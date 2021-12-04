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
    OutputFileName <- paste(outputDirectory, '/Output_Variable_', i, 
                            '.txt', sep ='')
    
      if (!firstRun){file.create(OutputFileName)}
    
      write.table(as.character(simulationNumber), OutputFileName, append = TRUE,
                  row.names = FALSE,col.names = FALSE)
      write.table(output[[i]], OutputFileName, append = TRUE,sep = '\t', 
                  row.names = FALSE, col.names = FALSE)      

  }  
    
  
}

#-------------------------------------------------------------------------------
# Get fileCio information (warmup + calibration period)
getFileCioInfo <- function(TxtInOut){
  
  if(substr(TxtInOut,(nchar(TxtInOut)-3), nchar(TxtInOut)) == ".cio"){
    fileCio <- readLines(TxtInOut, 60)
  } else {
    fileCio <- readLines(paste(TxtInOut, "/file.cio", sep = ""), 60)
  }

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
  
  info <- list()
  info$startSim <- startSim
  info$startEval <- startEval
  info$endSim <- endSim
  info$timeStepCode <- as.numeric(substr(fileCio[59],13,16))
  
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