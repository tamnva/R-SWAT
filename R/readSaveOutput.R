#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                         Read outputs from SWAT                               #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# ------------------------------------------------------------------------------
# Read output from output.rch, output.sub, output.hru files, 
# Output from these files are always at daily time step, without warm up period
# ------------------------------------------------------------------------------
readOutputRchFile <- function(workingDirectory, 
                           coreNumber, 
                           fileName, 
                           fromToDate, 
                           colNumber, 
                           fileCioInfo,
                           rchNumber,
                           output){
  
  
  fileName <- paste(workingDirectory, "/TxtInOut_", coreNumber, "/", 
                    fileName, sep = "")
  
  getOutputRsvData <- read.table(fileName, header = FALSE, sep = "", skip = 9)

  # by default, assume that simulated data does not include warm up period
  timeSeries <- seq(fileCioInfo$startEval, fileCioInfo$endSim, by="days")
  
  nRch <- max(getOutputRsvData$V2)

  # Check whether simulated data including warm-up period
  if (nrow(getOutputRsvData) != (nRch * length(timeSeries))){
    timeSeries <- seq(fileCioInfo$startSim, fileCioInfo$endSim, by="days")
  }
 
  ntimeStep <- nrow(getOutputRsvData)/nRch
  trim <- c(which(timeSeries ==  fromToDate[1]), 
            which(timeSeries ==  fromToDate[2]))
  
  
    
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

#-------------------------------------------------------------------------------
# Get reach (rch), subbasin, or hru number from output.rch ,hru, subbasin files
# ------------------------------------------------------------------------------
getRchNumber <- function(inputText){

  rsvRchNumber <- list()
  
  if(nchar(inputText) > 0){
    inputText <- strsplit(inputText, split = "*", fixed = TRUE)[[1]]
    for (i in 1:length(inputText)){

      rsvRchNumber[[i]] <- as.numeric(strsplit(inputText[i], 
                                            split = ",", 
                                            fixed = TRUE)[[1]]
      )
      rsvRchNumber[[i]] <-  rsvRchNumber[[i]][!is.na(rsvRchNumber[[i]])]
    }    
  } else {
    rsvRchNumber <- NULL
  }

  return(rsvRchNumber)
}

#-------------------------------------------------------------------------------
# Read watout.dat file, data is always at daily timestep, include warm-up period
# ------------------------------------------------------------------------------
readWatoutFile <- function(workingDirectory, 
                           coreNumber, 
                           fileName, 
                           fromToDate, 
                           colNumber, 
                           fileCioInfo,
                           output){
  
  fileName <- paste(workingDirectory, "/TxtInOut_", 
                    coreNumber, "/", fileName, sep = "")
  
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


#-------------------------------------------------------------------------------
# Save output
#-------------------------------------------------------------------------------
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
               fileType[i] == "channel_sdmorph_yr.txt"){
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
      output <- appendListObject(output, userExtractData)
        
    } else {
      print("Unkown output files, please modify saveOutput function")
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

#-------------------------------------------------------------------------------
# Get fileCio information (warmup + calibration period)
getFileCioInfo <- function(TxtInOut){
  
  info <- list()
  
  if(substr(TxtInOut,(nchar(TxtInOut)-3), nchar(TxtInOut)) == ".cio"){
    fileCio <- readLines(TxtInOut, 60)
  } else {
    fileCio <- readLines(paste(TxtInOut, "/file.cio", sep = ""), 60)
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
    simTime <- read.table(paste(TxtInOut, "/", "time.sim", sep = ""), skip = 2, sep = "")
    
    # Find the starting date of simulation
    startSim <- as.Date(paste(toString(as.numeric(simTime[2])),"0101", 
                              sep=""), "%Y%m%d") + as.numeric(simTime[1])
    
    # Find the ending date of simulation
    if(as.numeric(simTime[3]) == 0){
      endSim <- as.Date(paste(toString(as.numeric(simTime[4])),"1231",
                              sep=""), "%Y%m%d")
    } else {
      endSim <- as.Date(paste(toString(as.numeric(simTime[4])),"0101",
                              sep=""), "%Y%m%d") + as.numeric(simTime[3]) - 1      
    }

    
    # Step
    timeStepCode <- as.numeric(simTime[5])
    
    # Find start of eval or printing
    printTime <- readLines(paste(TxtInOut, "/print.prt", sep = ""), 3)
    printTime <- as.numeric(strsplit(trimws(printTime[3]), " +")[[1]])

    startPrint <- max(printTime[1] + as.numeric(simTime[2]) , printTime[3])
    startPrint <- as.Date(paste(startPrint, "0101", sep = ""), "%Y%m%d") + 
      max(1, printTime[2]) - 1

    if ((printTime[4] > 0) & (printTime[5] > 0)) {
      endPrint <- min(endSim, as.Date(paste(printTime[5], "0101", sep = ""), "%Y%m%d") + 
                        max(1, printTime[4]) - 1)      
    } else {
      if (printTime[5] == 0){
        if (printTime[4] == 0){
          endPrint <- endSim
        } else {
          endPrint <- as.Date(paste(toString(as.numeric(simTime[4])),"0101",
                                    sep=""), "%Y%m%d") + as.numeric(printTime[4]) - 1
        }
      } else {
        endPrint <- min(endSim, as.Date(paste(printTime[5], "0101", sep = ""), "%Y%m%d") + 
                          max(1, printTime[4]) - 1)         
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

#-------------------------------------------------------------------------------
# Append list object
#-------------------------------------------------------------------------------

appendListObject <- function(listA, listB){
  
  counter <- length(listA)
  
  for (i in 1:length(listB)){
    counter <- counter + 1
    listA[[counter]] <- listB[[i]]
  }
  
  return(listA)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                         Read outputs from SWAT +                             #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#-------------------------------------------------------------------------------
# Read channel_sd_xxx.txt file, data is always at daily timestep
#                 xxx could be "day", "month", or "yr"  readChannel_sd_dayFile
# ------------------------------------------------------------------------------
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
                "channel_sdmorph_yr.txt")
  
  filePath <- paste(workingDirectory, "/TxtInOut_", coreNumber, "/", 
                    fileName, sep = "")
  
  # Get file content/data
  channelData <- read.table(filePath, header = FALSE, sep = "", skip = 3)
  nrows <- nrow(channelData)
  
  # Get number of units
  nunits <- max(channelData$V5)
  
  # Convert to to time series
  temp <- seq(1, nrows, nunits)
  timeSeries <- paste(channelData$V4[temp], "-", channelData$V2[temp], "-", 
                      channelData$V3[temp], "-", sep = "" )
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