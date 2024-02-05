#'
#' Save output of SWAT/SWAT+ after each model run
#'
#' @inherit runSWATpar return
#' @inheritParams runSWATpar
#' @inheritParams readOutputRchFile
#' @importFrom utils write.table
#' @param fileType character, type of the file that are being read to extract
#' output, possible values are:
#' "watout.dat" \cr
#' "output.rch" \cr
#' "output.sub" \cr
#' "output.hru" \cr
#' "channel_sd_day.txt" \cr
#' "channel_sd_mon.txt" \cr
#' "channel_sd_yr.txt" \cr
#' "channel_sdmorph_day.txt"  \cr
#' "channel_sdmorph_mon.txt" \cr
#' "channel_sdmorph_yr.txt" \cr
#' "lsunit_wb_day.txt"  \cr
#' "lsunit_wb_mon.txt" \cr
#' "lsunit_wb_yr.txt"  \cr
#' "basin_wb_day.txt" \cr
#' "basin_wb_mon.txt" \cr
#' "basin_wb_yr.txt" \cr
#'
#' @param simulationNumber integer, the simulation number (not the number of
#' simulations)
#'
#' @export
#'
saveOutput <- function(workingDirectory,
                       coreNumber,
                       fileName,
                       fileType,
                       fromToDate,
                       colNumber,
                       rchNumber,
                       fileCioInfo,
                       simulationNumber,
                       firstRun){  #for output file name

  # Set output as list object
  output <- list()

  for (i in 1:length(fileType)){

    if (fileType[i] == "watout.dat"){

      # Read from watout.dat file type
      output <- readWatoutFile(workingDirectory,
                               coreNumber,
                               fileName[i],
                               fromToDate,
                               as.numeric(strsplit(colNumber[i],split = ",")[[1]]),
                               fileCioInfo,
                               output)

    } else if (fileType[i] == "output.rch" |
               fileType[i] == "output.sub" |
               fileType[i] == "output.hru" ){
      output <- readOutputRchFile(workingDirectory,
                                  coreNumber,
                                  fileName[i],
                                  fromToDate,
                                  as.numeric(strsplit(colNumber[i],split = ",")[[1]]),
                                  fileCioInfo,
                                  getRchNumber(rchNumber[i]),
                                  output)

    } else if (fileType[i] == "channel_sd_day.txt"      |
               fileType[i] == "channel_sd_mon.txt"      |
               fileType[i] == "channel_sd_yr.txt"       |
               fileType[i] == "channel_sdmorph_day.txt" |
               fileType[i] == "channel_sdmorph_mon.txt" |
               fileType[i] == "channel_sdmorph_yr.txt"  |
               fileType[i] == "lsunit_wb_day.txt"       |
               fileType[i] == "lsunit_wb_mon.txt"       |
               fileType[i] == "lsunit_wb_yr.txt"        |
               fileType[i] == "basin_wb_day.txt"       |
               fileType[i] == "basin_wb_mon.txt"       |
               fileType[i] == "basin_wb_yr.txt"){
      output <- readChannelFile(workingDirectory,
                                coreNumber,
                                fileName[i],
                                fromToDate,
                                as.numeric(strsplit(colNumber[i],split = ",")[[1]]),
                                fileCioInfo,
                                getRchNumber(rchNumber[i]),
                                output)

    } else if (fileType[i] == "userReadSwatOutput"){
      workingDir <- paste(workingDirectory, "/TxtInOut_", coreNumber, sep = "")
      setwd(workingDir)
      userExtractData <- userReadSwatOutput()
      output <- c(output, userExtractData)

    } else {
      warnings("Unkown output files, please modify saveOutput function")
    }
  }

  # Save output
  outputDirectory <- paste(workingDirectory, "/Output/Core_", coreNumber, sep = "")

  # Create directory if it does not exist
  if(!dir.exists(outputDirectory)) dir.create(outputDirectory)

  for (i in 1:length(output)){
    OutputFileName <- paste(outputDirectory, '/out_var_', i,
                            '.txt', sep ='')

    if (firstRun){file.create(OutputFileName)}

    # write simulation number
    write.table(as.character(simulationNumber), OutputFileName, append = TRUE,
                row.names = FALSE, col.names = FALSE)

    # write simulated data
    write.table(output[[i]], OutputFileName, append = TRUE,sep = '\t',
                row.names = FALSE, col.names = FALSE)

  }


}