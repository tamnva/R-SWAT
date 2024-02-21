
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
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    foreach::foreach(i = 1:ncores, .combine = 'c') %dopar% {
      runSWATSequential(i,
                        workingDirectory,
                        swatExe,
                        caliParam,
                        paraSelection,
                        subParameterSet[[i]],
                        outputExtraction,
                        fileCioInfo,
                        fromToDate,
                        firstRun)

    }
    parallel::stopCluster(cl)
  }

}

