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
    exeFile <- strsplit(swatExe, split="\\\\")[[1]]
    if(!file.exists(exeFile[length(exeFile)])) file.copy(swatExe, toDir)

    system(trimws(exeFile[length(exeFile)]))

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

