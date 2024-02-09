
#' Read output from output.rch, output.sub, or output.hru files of SWAT
#' @inheritParams runSWATpar
#'
#' @param coreNumber which core number you are reading from
#' @param fileName which file name (output.rch, output.sub or output.hru)
#' @param colNumber which column number of the file
#' @param rchNumber which reach, or subbasin, or hru number
#' @param output a list object that results should be attached to
#'
#' @return a list object of output
#'
#' @export
#'
#'
readOutputRchFile <- function(workingDirectory,
                           coreNumber,
                           fileName,
                           fromToDate,
                           colNumber,
                           fileCioInfo,
                           rchNumber,
                           output){


  fileName <- file.path(workingDirectory, "TxtInOut_", coreNumber, fileName)

  getOutputRsvData <- read.table(fileName, header = FALSE, sep = "", skip = 9)

  # By default, assume that simulated data does not include warm up period
  # Daily output
  if (fileCioInfo$timeStepCode == 1){
    timeSeries <- seq(fileCioInfo$startEval, fileCioInfo$endSim, by="days")

  # Monthly output
  } else if (fileCioInfo$timeStepCode == 0) {
    timeSeries <- seq(as.Date(paste0(substr(fileCioInfo$startEval,1,7), "-01"), "%Y-%m-%d"),
                      fileCioInfo$endSim, by="months")

  # Yearly output
  } else {
    timeSeries <- seq(as.Date(paste0(substr(fileCioInfo$startEval,1,4), "-01-01"), "%Y-%m-%d"),
                      fileCioInfo$endSim, by="years")
  }


  nRch <- max(getOutputRsvData$V2)

  # Check whether simulated data including warm-up period - output daily
  if ((nrow(getOutputRsvData) != (nRch * length(timeSeries)))
      & (fileCioInfo$timeStepCode == 1)){
    timeSeries <- seq(fileCioInfo$startSim, fileCioInfo$endSim, by="days")
  }

  # Check whether simulated data including warm-up period - output monthly
  if ((nrow(getOutputRsvData) != (nRch * (length(timeSeries) + 1 + length(unique(substr(timeSeries,1,4))))))
      & (fileCioInfo$timeStepCode == 0)){
    timeSeries <- seq(as.Date(paste0(substr(fileCioInfo$startSim,1,7), "-01"), "%Y-%m-%d"),
                      fileCioInfo$endSim, by="months")
  }

  # Check whether simulated data including warm-up period - output yearly
  if ((nrow(getOutputRsvData) != (nRch * (length(timeSeries) + 1)))
      & (fileCioInfo$timeStepCode == 2)){
    timeSeries <- seq(as.Date(paste0(substr(fileCioInfo$startSim,1,4), "-01-01"), "%Y-%m-%d"),
                      fileCioInfo$endSim, by="years")
  }

  # remove summary data of the whole simulation time
  # Monthly
  if (fileCioInfo$timeStepCode == 0){
    getOutputRsvData <- getOutputRsvData[-c((nrow(getOutputRsvData) - nRch + 1):nrow(getOutputRsvData)),]
    getOutputRsvData <- getOutputRsvData[-yearlyOutputLoc(timeSeries, nRch), ]

  # Yearly output
  } else if (fileCioInfo$timeStepCode == 2) {
    getOutputRsvData <- getOutputRsvData[-c((nrow(getOutputRsvData) - nRch + 1):nrow(getOutputRsvData)),]
  } else {

  }

  ntimeStep <- nrow(getOutputRsvData)/nRch

  # Daily output
  if (fileCioInfo$timeStepCode == 1){
    trim <- c(which(timeSeries ==  fromToDate[1]),
              which(timeSeries ==  fromToDate[2]))
  # Monthly output
  } else if (fileCioInfo$timeStepCode == 0){
    trim <- c(which(timeSeries ==  as.Date(paste0(substr(fromToDate[1],1,7), "-01"), "%Y-%m-%d")),
              which(timeSeries ==  as.Date(paste0(substr(fromToDate[2],1,7), "-01"), "%Y-%m-%d")))
  # Yearly output
  } else {
    trim <- c(which(timeSeries ==  as.Date(paste0(substr(fromToDate[1],1,4), "-01-01"), "%Y-%m-%d")),
              which(timeSeries ==  as.Date(paste0(substr(fromToDate[2],1,4), "-01-01"), "%Y-%m-%d")))
  }

  varNumber <- length(output)

  for (i in 1:length(colNumber)){
    for (j in 1:length(rchNumber[[i]])){

        varNumber <- varNumber + 1
        output[[varNumber]] <- getOutputRsvData[seq(from = rchNumber[[i]][j],
                                                    to = nrow(getOutputRsvData),
                                                    by = nRch),
                                                colNumber[i]]
        output[[varNumber]] <- output[[varNumber]][c(trim[1]:trim[2])]
    }
  }

  return(output)
}
