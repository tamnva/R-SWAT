#'
#' Read channel_sd... files and lsunit_wb... files of SWAT+
#' @description
#' Read channel outputs (any file starts with channel_sd) and landscape unit
#' file (any file starts with lsuni_wd) of SWAT+
#'
#' @inheritParams saveOutput
#' @inherit saveOutput return
#' @param output a list object containing another outputs and outputs from calling
#' this function
#'
#' @examples
#'
#' \donttest{
#' # Please see RSWAT Vignettes
#' vignette("SUFI2_without_GUI",package="RSWAT")
#' }
#'
#' @export
#'
#'
readChannelFile <- function(workingDirectory,
                            coreNumber,
                            fileName,
                            fromToDate,
                            colNumber,
                            fileCioInfo,
                            rchNumber,
                            output){

  fileType <- c("channel_sd_day.txt",
                "channel_sd_mon.txt",
                "channel_sd_yr.txt",
                "channel_sdmorph_day.txt",
                "channel_sdmorph_mon.txt",
                "channel_sdmorph_yr.txt",
                "lsunit_wb_day.txt",
                "lsunit_wb_mon.txt",
                "lsunit_wb_yr.txt")

  filePath <- file.path(workingDirectory, "TxtInOut_", coreNumber, fileName)

  # Get file content/data
  channelData <- read.table(filePath, header = FALSE, sep = "", skip = 3)
  nrows <- nrow(channelData)

  # Get number of units
  nunits <- max(channelData$V5)

  # Convert to to time series
  temp <- seq(1, nrows, nunits)
  timeSeries <- paste(channelData$V4[temp], "-", channelData$V2[temp], "-",
                      channelData$V3[temp], sep = "" )
  timeSeries <- as.Date(timeSeries, "%Y-%m-%d")
  timeStep <- length(seq(timeSeries[1], timeSeries[2], by= "days")) -1

  # Daily output file
  if(timeStep == 1){
    trim <- c(which(timeSeries ==  fromToDate[1]),
              which(timeSeries ==  fromToDate[2]))

    # Monthly output file
  } else if((timeStep > 27) & (timeStep < 31)){
    trim <- c(which(format(timeSeries,"%Y-%m") ==  format(fromToDate[1],"%Y-%m")),
              which(format(timeSeries,"%Y-%m") ==  format(fromToDate[2],"%Y-%m")))
  } else if((timeStep == 365) | (timeStep == 366)){
    trim <- c(which(format(timeSeries,"%Y") ==  format(fromToDate[1],"%Y")),
              which(format(timeSeries,"%Y") ==  format(fromToDate[2],"%Y")))
  } else {
    # TODO
    trim <- c("error", "error")
  }

  varNumber <- length(output)

  for (i in 1:length(colNumber)){
    for (j in 1:length(rchNumber[[i]])){

      varNumber <- varNumber + 1
      output[[varNumber]] <- channelData[seq(from = rchNumber[[i]][j],
                                             to = nrows,
                                             by = nunits),
                                         colNumber[i]]

      output[[varNumber]] <- output[[varNumber]][c(trim[1]:trim[2])]
    }
  }

  return(output)
}

