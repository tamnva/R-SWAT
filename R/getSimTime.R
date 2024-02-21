
#' Get simulation time setting in the file.cio of SWAT or time.sim of SWAT+
#'
#' @description
#' Search for file.cio or time.sim file in the TxtInOut and get the simulation
#' time setting
#'
#' @param TxtInOut path to the TxtInOut directory
#' @return a list stores information about the simulation time (warmup and
#' simulation time)
#'
#' @examples
#'
#'\donttest{
#' # Create a directory and populate with TxtInOut of SWAT
#' extracExampleData(exampleData,"swatTxtInOut", tempdir())
#' TxtInOut <- file.path(tempdir(), "swatTxtInOut")
#'
#' # Now get hru information
#' getSimTime(TxtInOut)
#'}
#'
#' @export
#'
getSimTime <- function(TxtInOut){

  info <- list()

  if(substr(TxtInOut,(nchar(TxtInOut)-3), nchar(TxtInOut)) == ".cio"){
    fileCio <- readLines(TxtInOut, 60)
  } else {
    fileCio <- readLines(file.path(TxtInOut, "file.cio"), 60)
  }

  # Check whether this is TxtInOut of SWAT or SWAT+
  if (substr(fileCio[8], 21,44) == "| NBYR : Number of years"){
    startSim <- as.Date(paste(substr(fileCio[9],13,17), "0101", sep=""), "%Y%m%d")
    startSim <- startSim + as.numeric(substr(fileCio[10],13,17)) - 1

    endSim <- as.Date(paste(
      toString(
        as.numeric(substr(fileCio[9],13,17)) +
          as.numeric(substr(fileCio[8],13,17)) - 1
      ),
      "0101",
      sep=""
    ),
    "%Y%m%d")

    endSim <- endSim + as.numeric(substr(fileCio[11],13,17)) - 1
    nyearSkip  <- as.numeric(substr(fileCio[60],13,16))

    if(nyearSkip == 0){
      startEval <- startSim
    } else {
      startEval <- as.Date(paste(
        toString(
          as.numeric(substr(fileCio[9],13,17)) + nyearSkip
        ),
        "0101",
        sep=""
      ),
      "%Y%m%d")
    }

    # Display the range of selected day for output extraction
    info$startSim <- startSim
    info$startEval <- startEval
    info$endSim <- endSim
    info$timeStepCode <- as.numeric(substr(fileCio[59],13,16))

    # SWAT+ project, simulation time in different file
  } else {

    # Read simulation time information from the time.sim file
    simTime <- read.table(file.path(TxtInOut, "time.sim"), skip = 2, sep = "")

    # Find the starting date of simulation
    startSim <- as.Date(paste0(toString(as.numeric(simTime[2])), "0101"),
                        "%Y%m%d") + as.numeric(simTime[1])

    # Find the ending date of simulation
    if(as.numeric(simTime[3]) == 0){
      endSim <- as.Date(paste0(toString(as.numeric(simTime[4])),"1231"),
                        "%Y%m%d")
    } else {
      endSim <- as.Date(paste0(toString(as.numeric(simTime[4])),"0101"),
                        "%Y%m%d") + as.numeric(simTime[3]) - 1
    }


    # Step
    timeStepCode <- as.numeric(simTime[5])

    # Find start of eval or printing
    printTime <- readLines(file.path(TxtInOut, "print.prt"), 3)
    printTime <- as.numeric(strsplit(trimws(printTime[3]), " +")[[1]])

    startPrint <- max(printTime[1] + as.numeric(simTime[2]) , printTime[3])
    startPrint <- as.Date(paste0(startPrint, "0101"), "%Y%m%d") +
      max(1, printTime[2]) - 1

    if ((printTime[4] > 0) & (printTime[5] > 0)) {
      endPrint <- min(endSim, as.Date(paste0(printTime[5], "0101"), "%Y%m%d") +
                        max(1, printTime[4]) - 1)
    } else {
      if (printTime[5] == 0){
        if (printTime[4] == 0){
          endPrint <- endSim
        } else {
          endPrint <- as.Date(paste0(toString(as.numeric(simTime[4])),"0101"),
                              "%Y%m%d") + as.numeric(printTime[4]) - 1
        }
      } else {
        endPrint <- min(endSim, as.Date(paste0(printTime[5], "0101"),
                                        "%Y%m%d") + max(1, printTime[4]) - 1)
      }
    }

    # Display the range of selected day for output extraction
    info$startSim <- startSim
    info$startEval <- startPrint
    info$endSim <- endPrint
    info$timeStepCode <- timeStepCode

  }

  return(info)
}
