
#' Execute SWAT run in parallel
#'
#' @description
#' This will run SWAT in parallel
#'
#' @param workingDirectory path to the working directory where all files created
#' by SWAT or SWAT+ will be saved
#' @param TxtInOutFolder path to the original TxtInOut folder of SWAT or SWAT+
#' @param outputExtraction a dataframe instructs which outputs should be
#' extracted after each run/iteration
#' @param ncores number of threads for parallel execution
#' @param swatExe path of the SWAT or SWAT+ execution file
# @name swatExe
#' @param parameterValue a dataframe contains all parametersets for running SWAT
#' or SWAT+
#' @param paraSelection a data frame
#' @param caliParam dataframe of parameters, number of row is number of samples,
#' the first column is the sample ID and the number of subsequence columns are
#' the number of parameters.
#' @param copyUnchangeFiles should unchanged files be copied
#' @param fileCioInfo a dataframe containing simulation time, warum up time
#' @param fromToDate a date vector with length of 2, indicating from which time
#' period the function should extract outputs
#' @param firstRun logical (set to TRUE if this is the first iteration/run),
#' in the first iteration, all files need to be copied to the new TxtInOut folder
#' for SWAT/SWAT+ run, in the subsequent iterations, only files that content
#' parameters (that need to be changed) are updated.
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel

#' @return no return value, results will be saved in the workingDirectory
#'
#' @examples
#'
#'\donttest{
#' runSWATpar(workingDirectory,
#'            TxtInOutFolder,
#'            outputExtraction,
#'            ncores,
#'            swatExe,
#'            parameterValue,
#'            paraSelection,
#'            caliParam,
#'            copyUnchangeFiles,
#'            fileCioInfo,
#'            fromToDate,
#'            firstRun)
#'}
#'
#' @export
#'
#'
runSWATpar <- function(workingDirectory,
                       TxtInOutFolder,
                       outputExtraction,
                       ncores,
                       swatExe,
                       parameterValue,
                       paraSelection,
                       caliParam,
                       copyUnchangeFiles,
                       fileCioInfo,
                       fromToDate,
                       firstRun){

  # Create n directory in R
  if(copyUnchangeFiles){
    createDirCopyUnchangeFile(workingDirectory,
                              ncores,
                              TxtInOutFolder,
                              caliParam$file,
                              swatExe,
                              firstRun)
  }

  # --------------------------------------------------------------------------
  subParameterSet <- splitParameterValue(ncores, parameterValue)

  if (ncores == 1){
    runSWATSequential(1,
                      workingDirectory,
                      swatExe,
                      caliParam,
                      paraSelection,
                      subParameterSet[[1]],
                      outputExtraction,
                      fileCioInfo,
                      fromToDate,
                      firstRun)
  } else {
    core <- NA
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    foreach::foreach(core = 1:ncores, .combine = 'c') %dopar% {
      runSWATSequential(core,
                        workingDirectory,
                        swatExe,
                        caliParam,
                        paraSelection,
                        subParameterSet[[core]],
                        outputExtraction,
                        fileCioInfo,
                        fromToDate,
                        firstRun)

    }
    parallel::stopCluster(cl)
  }

}

#------------------------------------------------------------------------------#
#                           Run SWAT in sequential                             #
#------------------------------------------------------------------------------#
#' Execute SWAT/SWAT+ run in sequential used in parallel function (or alone)
#'
#' @description
#' This function will run SWAT or SWAT+ in sequential, one by one
#'
#' @inheritParams runSWATpar
#' @param subParameterSet data frame contains subset of parameters if it is used
#' when run SWAT/SWAT+ parallel, all parameters if it is used alone
#' @param coreNumber Core number
#' @return no return value, results will be saved in the workingDirectory
#'
#' @examples
#'
#'\donttest{
#' runSWATSequential(coreNumber,
#'                   workingDirectory,
#'                   swatExe,
#'                   caliParam,
#'                   paraSelection,
#'                   subParameterSet,
#'                   outputExtraction,
#'                   fileCioInfo,
#'                   fromToDate,
#'                   firstRun)
#'}
#'
#' @export
#'
#'
runSWATSequential <- function(coreNumber,
                              workingDirectory,
                              swatExe,
                              caliParam,
                              paraSelection,
                              subParameterSet,
                              outputExtraction,
                              fileCioInfo,
                              fromToDate,
                              firstRun){

  # Set working directory
  setwd(file.path(workingDirectory, paste0('TxtInOut_', coreNumber)))
  # Get directory where new TxtInOut files are saved
  toDir <- getwd()

  # Number of parameter sets
  if(is.vector(subParameterSet)){
    subParameterSet <- matrix(subParameterSet, nrow = 1)
  }

  # Number of parameters
  nParam <- ncol(subParameterSet)

  # Loop over number of parameter sets
  for (i in 1:nrow(subParameterSet)) {

    # If this is SWAT project
    if (isTRUE(caliParam$file[1] != "calibration.cal")){

      # Assign parameter values to caliParam
      caliParam$applyValue <- getParameterValue(caliParam$paramFlag,
                                                subParameterSet[i,2:nParam])
    }

    # Update TxtInOut folder
    updateMultiFile(toDir, caliParam, subParameterSet[i,2:nParam], paraSelection)

    # Call swat.exe file
    exeFile <- basename(swatExe)
    if(!file.exists(exeFile) file.copy(swatExe, toDir)

    # Check platform
    if(.Platform$OS.type == "unix") {
      system(paste0("./", exeFile))
    } else {
      system(exeFile)
    }

    # Check first run
    if ((nrow(subParameterSet) > 1) & (i > 1)){
      firstRun = FALSE
    }

    # Read and write output
    saveOutput(workingDirectory,
               coreNumber,
               outputExtraction[,2],
               outputExtraction[,1],
               fromToDate,
               outputExtraction[,3],
               outputExtraction[,4],
               fileCioInfo,
               subParameterSet[i,1],
               firstRun)

    # Write .log file
    write(paste('Finished_simulation_number ',
                i,
                ' out_of ',
                nrow(subParameterSet),
                ' on_core ',
                coreNumber,
                ' ',
                Sys.time(),
                sep =''
    ),
    file= file.path(workingDirectory, 'Output', 'CurrentSimulationReport.log'),
    append=TRUE
    )
  }
}


#------------------------------------------------------------------------------#
#                       getParameterValue                                      #
#------------------------------------------------------------------------------#

#' Get parameter value
#' @param parameter parameter flag
#' @param parameterValue parameter value
#' @return parameter value
#' @keywords interal
#'
getParameterValue <- function(parameter, parameterValue){
  for (i in 1:length(parameter)){
    for (j in 1:length(parameter[[i]])){
      parameter[[i]][j] <- parameterValue[parameter[[i]][j]]
    }
  }
  return(parameter)
}

#------------------------------------------------------------------------------#
#                         updateMultiFile                                      #
#------------------------------------------------------------------------------#
#' Update multiple TxtInOut files with new parameter values
#'
#' @description
#' Rewrite the TxtInOut with new parameter values
#'
#' @inheritParams runSWATpar
#' @inheritParams getParameterValue
#' @param toDir path to the directory where the updated files will be saved
#' @return no return, results will be saved in todir directory
#' @examples
#'
#'\donttest{
#' updateMultiFile(toDir, caliParam, parameterValue, paraSelection)
#'}
#'
#' @export
#'
#'
updateMultiFile <-  function(toDir, caliParam, parameterValue, paraSelection){

  # If this is SWAT+ project
  if (isTRUE(caliParam$file == "calibration.cal")){
    updateCalibrationFile(paraSelection, parameterValue, toDir)

    # If this is SWAT project
  } else {
    # Loop over list of files
    for (i in 1:length(caliParam$file)){

      updateSingleFile(toDir,
                       caliParam$file[i],
                       caliParam$fileContent[[i]],
                       caliParam$atLine[[i]],
                       caliParam$atPosition[[i]],
                       caliParam$changeMethod[[i]],
                       caliParam$applyValue[[i]],
                       caliParam$numberFormat[[i]],
                       caliParam$absoluteMin[[i]],
                       caliParam$absoluteMax[[i]])
    }
  }
}

#------------------------------------------------------------------------------#
#                         updateSingleFile                                      #
#------------------------------------------------------------------------------#
#' Update multiple TxtInOut files with new parameter values
#'
#' @description
#' Rewrite the TxtInOut with new parameter values
#'
#' @inheritParams updateMultiFile
#' @param file name of the file which contains the updated parameter
#' @param fileContent content of the file which contains the updated parameter
#' @param atLine line number of the updated parameter
#' @param atPosition column location of the updated parameter
#' @param changeMethod method of change
#' @param applyValue the value which the original parameter should be modify
#' @param numberFormat format of the updated parameter
#' @param absoluteMin minimum value which the updated parameter should be greater
#' @param absoluteMax maximum value which the updated parameter should be exceed
#'
#' @return no return, results will be saved in todir directory
#'
#' @examples
#'
#'\donttest{
#' updateSingleFile(toDir,
#'                  file,
#'                  fileContent,
#'                  atLine,
#'                  atPosition,
#'                  changeMethod,
#'                  applyValue,
#'                  numberFormat,
#'                  absoluteMin,
#'                  absoluteMax)
#'}
#'
#' @export
#'
#'
updateSingleFile <- function(toDir,
                             file,
                             fileContent,
                             atLine,
                             atPosition,
                             changeMethod,
                             applyValue,
                             numberFormat,
                             absoluteMin,
                             absoluteMax){

  newfileContent <- fileContent

  for (i in 1:length(atLine)){

    # check if this is soil file (then change all soil layers)
    if (substr(file, nchar(file) - 2, nchar(file)) == 'sol'){
      if (atLine[i] >= 8){

        nchars <- atPosition[i,2] - atPosition[i,1]
        nlayer <- length(strsplit(trimws(substr(newfileContent[atLine[i]],
                                                atPosition[i,1],
                                                nchar(newfileContent[atLine[i]]))),
                                  split = "\\s+")[[1]])

        for (j in 1:nlayer){

          if (j > 1){
            atPosition[i,1] <- atPosition[i,2] + 1
            atPosition[i,2] <- atPosition[i,1] + nchars
          }
          parameterValue <- as.double(substr(newfileContent[atLine[i]],
                                             atPosition[i,1],
                                             atPosition[i,2]))

          if (changeMethod[i] == "relative"){
            newParameterValue <- (1.0 + applyValue[i]) * parameterValue
          } else if (changeMethod[i] == "replace") {
            newParameterValue <- applyValue[i]
          } else {
            newParameterValue <- applyValue[i] + parameterValue
          }

          if (newParameterValue < absoluteMin[i]) newParameterValue <- absoluteMin[i]
          if (newParameterValue > absoluteMax[i]) newParameterValue <- absoluteMax[i]

          toText <- sprintf(numberFormat[i], newParameterValue)

          substr(newfileContent[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText
        }

      } else {
        parameterValue <- as.double(substr(newfileContent[atLine[i]],
                                           atPosition[i,1],
                                           atPosition[i,2]))

        if (changeMethod[i] == "relative"){
          newParameterValue <- (1.0 + applyValue[i]) * parameterValue
        } else if (changeMethod[i] == "replace") {
          newParameterValue <- applyValue[i]
        } else {
          newParameterValue <- applyValue[i] + parameterValue
        }

        if (newParameterValue < absoluteMin[i]) newParameterValue <- absoluteMin[i]
        if (newParameterValue > absoluteMax[i]) newParameterValue <- absoluteMax[i]

        toText <- sprintf(numberFormat[i], newParameterValue)

        substr(newfileContent[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText

      }

    } else {
      parameterValue <- as.double(substr(newfileContent[atLine[i]],
                                         atPosition[i,1],
                                         atPosition[i,2]))

      if (changeMethod[i] == "relative"){
        newParameterValue <- (1.0 + applyValue[i]) * parameterValue
      } else if (changeMethod[i] == "replace") {
        newParameterValue <- applyValue[i]
      } else {
        newParameterValue <- applyValue[i] + parameterValue
      }

      if (newParameterValue < absoluteMin[i]) newParameterValue <- absoluteMin[i]
      if (newParameterValue > absoluteMax[i]) newParameterValue <- absoluteMax[i]

      toText <- sprintf(numberFormat[i], newParameterValue)

      substr(newfileContent[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText
    }

  }

  writeLines(newfileContent,paste(toDir, '/', file, sep = ''))
}






