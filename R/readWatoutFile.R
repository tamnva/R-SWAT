#'
#' Read watout.dat file from SWAT
#'
#' @description
#' Read watout.dat file of SWAT. This file is always at daily timestep and
#' include the warmup period
#' @inherit readOutputRchFile return
#' @inheritParams readOutputRchFile
#'
#' @export
#'
#'
readWatoutFile <- function(workingDirectory,
                           coreNumber,
                           fileName,
                           fromToDate,
                           colNumber,
                           fileCioInfo,
                           output){

  fileName <- file.path(workingDirectory, "TxtInOut_", coreNumber, fileName)

  getWatoutData <- read.table(fileName, header = FALSE, sep = "", skip = 6)

  # by default, assume that simulated data does not include warm up period
  timeSeries <- seq(fileCioInfo$startEval, fileCioInfo$endSim, by="days")

  # Check whether simulated data including warm-up period
  if (nrow(getWatoutData) != length(timeSeries)){
    timeSeries <- seq(fileCioInfo$startSim, fileCioInfo$endSim, by="days")
  }

  counter <- length(output)
  trim <- c(which(timeSeries ==  fromToDate[1]),
            which(timeSeries ==  fromToDate[2]))

  for (i in 1:length(colNumber)){
    output[[counter + i]] <- getWatoutData[c(trim[1]: trim[2]), colNumber[i]]
  }

  return(output)
}
