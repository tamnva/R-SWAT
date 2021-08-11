#-------------------------------------------------------------------------------
# Read watout.dat file
readWatoutFile <- function(workingDirectory, 
                           coreNumber, 
                           fileName, 
                           fromToDate, 
                           colNumber, 
                           fileCioInfo){
  
  fileName <- paste(workingDirectory,
                    "/TxtInOut_",
                    coreNumber,
                    "/",
                    fileName,
                    sep = ""
  )

  getWatoutData <- read.table(fileName, header = FALSE, sep = "", skip = 6)
  timeSeries <- seq(fileCioInfo$startSim, fileCioInfo$endSim, by="days")
  
  output <- list()
  counter <- 0

  for (i in 1:nrow(getWatoutData)){
    if ((timeSeries[i] >= fromToDate[1]) & (timeSeries[i] <= fromToDate[2])){
      counter <- counter + 1
      for (j in 1:length(colNumber)){
        if (counter == 1) {
          output[[j]] <- getWatoutData[i, colNumber[j]]
        } else {
          output[[j]] <- c(output[[j]], getWatoutData[i, colNumber[j]])
        }
      } #end for
      
    } # end if
  }   # end for
  
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
                       fileCioInfo,
                       simulationNumber){  #for output file name
  
  counter <- -1

  for (i in 1:length(fileType)){
    
    counter <- counter + 1
    
    if (fileType[i] == "watout.dat"){
      # Read from watout.dat file type

      watoutData <- readWatoutFile(workingDirectory, 
                                   coreNumber, 
                                   fileName[i], 
                                   fromToDate, 
                                   as.numeric(strsplit(colNumber[i],split = ",")[[1]]), 
                                   fileCioInfo)
      # Save output
      outputDirectory <- paste(workingDirectory, "/Output/Core_", coreNumber, sep = "")
      
      # Create directory if it does not exist
      if(!dir.exists(outputDirectory)) dir.create(outputDirectory)
      
      for (j in 1:length(watoutData)){
        counter <- counter + 1
        OutputFileName <- paste(outputDirectory, '/Output_Variable_', counter, '.txt', sep ='')
        write.table(as.character(simulationNumber), OutputFileName, append = TRUE,row.names = FALSE,col.names = FALSE)
        write.table(watoutData[[j]], OutputFileName, append = TRUE,sep = '\t', row.names = FALSE, col.names = FALSE)
      }
    } else {
      print("Current option is only read from watout.dat file type")
    }
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
  
  if (info$timeStepCode == 0){
    info$timeSeries = seq(startEval, endSim, by="months")
  } else if (info$timeStepCode == 1){
    info$timeSeries =  seq(startEval, endSim, by="days")
  } else {
    info$timeSeries = seq(startEval, endSim, by="years")
  }
  
  
  return(info)
}