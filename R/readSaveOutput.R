
# ------------------------------------------------------------------------------
# Read output from reservoir (rsv), output is always at daily timestep
# ------------------------------------------------------------------------------
readOutputRchFile <- function(workingDirectory, 
                           coreNumber, 
                           fileName, 
                           fromToDate, 
                           colNumber, 
                           fileCioInfo,
                           rchNumber,
                           output){
  
  # rchNumber is a list with length of colNumber
  
  fileName <- paste(workingDirectory, "/TxtInOut_", coreNumber, "/", fileName, sep = "")
  
  getOutputRsvData <- read.table(fileName, header = FALSE, sep = "", skip = 9)
  timeSeries <- seq(fileCioInfo$startEval, fileCioInfo$endSim, by="days")
  
  nRch <- max(getOutputRsvData$V2)
  
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
        # Trim data to the calibration range
        # Output in .rsv file is always at daily
        # Data in this file does not include the  warm up
        output[[varNumber]] <- output[[varNumber]][c(trim[1]:trim[2])]        
    }
  }
  
  return(output)
}

#-------------------------------------------------------------------------------
# Function to get the reservoir number in the extract output table
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
# Read watout.dat file, output is always at daily timestep
# ------------------------------------------------------------------------------
readWatoutFile <- function(workingDirectory, 
                           coreNumber, 
                           fileName, 
                           fromToDate, 
                           colNumber, 
                           fileCioInfo,
                           output){
  
  fileName <- paste(workingDirectory, "/TxtInOut_", coreNumber, "/", fileName, sep = "")
  
  # Output in this file is always at daily time step and include warmup period
  getWatoutData <- read.table(fileName, header = FALSE, sep = "", skip = 6)
  timeSeries <- seq(fileCioInfo$startSim, fileCioInfo$endSim, by="days")
  
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
saveOutput <- function(workingDirectory,
                       coreNumber,                   
                       fileName,   
                       fileType,
                       fromToDate, 
                       colNumber,
                       rchNumber,
                       fileCioInfo,
                       simulationNumber){  #for output file name
  
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
      } else {
      print("Unkown output files, please modify saveOutput function")
    }
  }
  
  # Save output
  outputDirectory <- paste(workingDirectory, "/Output/Core_", coreNumber, sep = "")
  
  # Create directory if it does not exist
  if(!dir.exists(outputDirectory)) dir.create(outputDirectory)
  
  for (i in 1:length(output)){
    OutputFileName <- paste(outputDirectory, '/Output_Variable_', i, '.txt', sep ='')
    write.table(as.character(simulationNumber), OutputFileName, append = TRUE,row.names = FALSE,col.names = FALSE)
    write.table(output[[i]], OutputFileName, append = TRUE,sep = '\t', row.names = FALSE, col.names = FALSE)
  }  
    
  
}

#-------------------------------------------------------------------------------
# Get fileCio information (warmup + calibration period)
getFileCioInfo <- function(TxtInOut){
  fileCio <- readLines(paste(TxtInOut, "/file.cio", sep = ""), 60)
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
  
  # Output TimeStep code
  
  info <- list()
  info$startSim <- startSim
  info$startEval <- startEval
  info$endSim <- endSim
  info$timeStepCode <- as.numeric(substr(fileCio[59],13,16))
  
  #if (info$timeStepCode == 0){
  #  info$timeSeries = seq(startEval, endSim, by="months")
  #} else if (info$timeStepCode == 1){
  #  info$timeSeries =  seq(startEval, endSim, by="days")
  #} else {
  #  info$timeSeries = seq(startEval, endSim, by="years")
  #}
  
  
  return(info)
}