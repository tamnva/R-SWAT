# ------------------------------------------------------------------------------
# Function to read watout.dat file type
# ------------------------------------------------------------------------------

readWatout <- function(fileName){
  # test
  # fileName <- "C:/Users/nguyenta/Documents/old/SWATShinyVisual/testData/TxtInOut/watout.dat"
  output <- read.table(fileName, header = TRUE, sep = "", skip = 6)
  return(output)
}

# ------------------------------------------------------------------------------
# Get variable number in watout.dat file type
# ------------------------------------------------------------------------------
readWatoutHeader <- function(fileName){
  
  watoutHeader <- readLines(fileName, 6)
  watoutHeader <-  gsub("CMETAL1mg/LCMETAL2mg/L", "  CMETAL1mg/L  CMETAL2mg/L  ", watoutHeader)
  
  watoutHeader <- strsplit(watoutHeader[6], "\\s+")[[1]]
  watoutHeader <- watoutHeader[watoutHeader != ""]
  
  return(watoutHeader)
}

# ------------------------------------------------------------------------------
# Get time series of watout.dat file type
# ------------------------------------------------------------------------------
getWatoutTime <- function(watoutData){
  nrows <- nrow(watoutData)
  sdate <- as.Date(paste(watoutData[1,1],"0101", sep=""), "%Y%m%d") + 
    + watoutData[1,2] - 1
  edate <- as.Date(paste(watoutData[nrows,1],"0101", sep=""), "%Y%m%d") + 
    + watoutData[nrows,2] - 1  
  output <- seq(sdate, edate, by ="days")
  return(output)
}

# ------------------------------------------------------------------------------
# Plot the selected data
# ------------------------------------------------------------------------------
plotWatout <- function(watoutData = watoutData, watoutHeader = watoutHeader, varName1 = varName1, 
                       observeData = observeData, varName2 = varName2){
  
  varColumn <- match(varName1, watoutHeader)
  
  output <- list()
  date <- getWatoutTime(watoutData)

  output$data <- data.frame(Date = date, 
                            Variable = rep(varName1,length(date)),
                            value = watoutData[,varColumn])

  if(!is.null(observeData)){
    if (is.data.frame(observeData)){
     
      varColumn <- match(varName2, colnames(observeData))
      
      observeData <- data.frame(Date = observeData[,1], 
                                Variable = rep(varName2,nrow(observeData)),
                                value = observeData[,varColumn])
      
      output$data <- rbind(output$data, observeData)        
    }
  }

  # Plot result
  output$plot <- ggplot(output$data, aes(x = Date, y = value,color = Variable)) +  
    geom_line(alpha = 0.8) +
    labs(x =" ", y = " ") +
    scale_x_date(date_labels = "%m-%Y") +
    theme_bw()
  
  output$plot <- ggplotly(output$plot)
  
  return(output)
  
}

# HRU --------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Get variable number in watout.dat file type
# ------------------------------------------------------------------------------
readOutputHruHeader <- function(fileName){
  
  outputHruHeader <- readLines(fileName, 9)

  outputHruHeader <- strsplit(outputHruHeader[9], "\\s+")[[1]]
  outputHruHeader <- outputHruHeader[outputHruHeader != ""]
  temp <- c('LULC', 'HRU', 'GIS', 'SUB', 'MGT', 'MO', 'DA', 'YR')
  outputHruHeader <- outputHruHeader[-match(temp,outputHruHeader )]
  
  return(outputHruHeader)
}

# ------------------------------------------------------------------------------
# Subset of HRU based on column name and temporal aggregation
# ------------------------------------------------------------------------------
subsetOutputHru <- function(hruData, sDate, eDate, colNam, tempAgg){
  
  # Deleteme
#  outputHruFile <- "C:/data/TxtInOut/output.hru"
#  hruData <- read.table(outputHruFile, header = TRUE, sep = "", skip = 8)
#  sDate <-  as.Date("20000101", "%Y%m%d")
#  eDate <-  as.Date("20071231", "%Y%m%d")
# tempAgg <- "Monthly"
# colNam <- 'AREAkm2'
  
  # number of hrus
  nHrus <- max(hruData[,2])
  nTimeSteps <- nrow(hruData)/nHrus
  colNr <- match(colNam, colnames(hruData))
  
  # Aggregated result
  hru <- as.data.frame(matrix(rep(NA, nHrus*nTimeSteps), ncol = nHrus, nrow = nTimeSteps))
 
  # Time step 
  plotPeriod <- seq(sDate, eDate, by ="days")
  byMonth <- strftime(plotPeriod, "%m")
  byYear <- strftime(plotPeriod, "%Y")
    
  # convert to vector
  for (i in 1:nTimeSteps){
    startLocation <- (i-1)*nHrus + 1
    endLocation <- i*nHrus
    hru[i,] <- as.vector(hruData[startLocation:endLocation, colNr])
  }
  
  if (tempAgg == "Monthly"){

    hru <- aggregate(x = hru, by = list(byMonth, byYear), FUN = "sum")
    hru <- hru[,-c(2)]
    hru[,1] <- unique(paste(byYear, "-", byMonth, sep =""))

    
  } else if (tempAgg == "Yearly"){
    hru <- aggregate(x = hru, by = list(byYear), FUN = "sum")
    hru[,1] <- unique(byYear)
    
  } else {
    hru <- cbind(as.character(plotPeriod), hru)
  }
  
  return(hru)
  
}


# ------------------------------------------------------------------------------
# Subset of HRU based on column name and temporal aggregation
# ------------------------------------------------------------------------------
hruRasterValue <- function(hruRaster, hruAggregate, selectedDate){

  temp <- hruAggregate[1,1]
  
  if (nchar(temp) == 7){
    selectedDate <- paste(strftime(selectedDate, "%Y"), "-", strftime(selectedDate, "%m"),
                          sep ="")
  } else if (nchar(temp) == 4) {
    selectedDate <- strftime(selectedDate, "%Y")  
  } else {
    selectedDate <- as.character(selectedDate)
  }
    

  rowNr <- match(selectedDate, hruAggregate[,1])
  output <- as.numeric(hruAggregate[rowNr, 2:ncol(hruAggregate)])
  
  val <- values(hruRaster)

 
  for(i in 1:length(val)){
    if(!is.na(val[i])){
      val[i] <- output[val[i]]      
    }
  }
  
  
  values(hruRaster) <- val
  
  return(hruRaster)
  
}

# ------------------------------------------------------------------------------
# Plot output.rch
# ------------------------------------------------------------------------------
readOutputRch <- function(outputRchFile){
  
  # outputRchFile <- "C:/Users/nguyenta/Documents/GitHub/SWATshiny/data/TxtInOut/output.rch"
  output <- read.table(outputRchFile, header = FALSE, sep = "", skip = 9)
  colnames(output)  <- paste("Column_", c(1:ncol(output)), sep = "")
  output <- output[,-c(1)]
  return(output)
}

# ------------------------------------------------------------------------------
# Subset and temperol aggreagation of output.rch
# ------------------------------------------------------------------------------
subsetOutputRch <- function(readOutputRch, tempAgg, colNam, dataPeriod, rchNr){
  
  # readOutputRch <- output
  # colNam <- c("Column_8")
  # dataPeriod <- c(as.Date("20000101", "%Y%m%d"), as.Date("20071231", "%Y%m%d"))
  # rchNr <- 2
  
  nRchs <- max(readOutputRch[,1])
  nTimeSteps <- nrow(readOutputRch)/nRchs
  colNr <- match(colNam, colnames(readOutputRch))
  
  # Aggregated result
  rchIdx <- seq(rchNr, nrow(readOutputRch), nRchs)
  readOutputRch <- readOutputRch[rchIdx,]
  output <- matrix(readOutputRch[, colNr], ncol = 1)
  
  # Time step 
  dataPeriod <- seq(dataPeriod[1], dataPeriod[2], by ="days")
  byMonth <- strftime(dataPeriod, "%m")
  byYear <- strftime(dataPeriod, "%Y")
  
  if (tempAgg == "Monthly"){
    
    output <- aggregate(x = output, by = list(byMonth, byYear), FUN = "sum")
    output <- output[,-c(2)]
    output[,1] <- unique(paste(byYear, "-", byMonth, sep =""))
    
    
  } else if (tempAgg == "Yearly"){
    output <- aggregate(x = output, by = list(byYear), FUN = "sum")
    output[,1] <- paste(output[,1],"01-01", sep = "")
  } else {
    output <- cbind(date = as.character(dataPeriod), data.frame(output))
  }
  
  result <- list()
  result$output <- output
  result$plot <- plot(output)
  
  return(result)
}


# ------------------------------------------------------------------------------
# Plot output.rch subset
# ------------------------------------------------------------------------------
plotOutputRchSubset <- function(subsetOutputRch, subsetObsRch){
  
  # subsetOutputRch <- output
  colnames(subsetOutputRch) <- c("Date", "Values")
  subsetOutputRch[,1] <- as.Date(subsetOutputRch[,1], "%Y-%m-%d")
  
  myPlot <- ggplot(subsetOutputRch, aes(x=Date,y=Values)) +
    geom_line(aes(x = Date, y = Values), alpha = 0.6) +
    labs(x ="Date", y = " ") +
    scale_x_date(date_labels = "%m-%Y") +
    theme_bw()
  
  return(ggplotly(myPlot))
}


#watoutData <- readWatout("C:/Users/nguyenta/Documents/GitHub/SWATshiny/data/TxtInOut/watout.dat")
#watoutHeader <- readWatoutHeader("C:/Users/nguyenta/Documents/GitHub/SWATshiny/data/TxtInOut/watout.dat")
#varName1 <- "FLOWm^3/s"
#observeData <- read.table("C:/Users/nguyenta/Documents/GitHub/SWATshiny/data/obs_var_1.txt", header = TRUE, sep = "")
#varName2 <- "Qoutlet"
#test <- plotWatout(watoutData, watoutHeader, varName1, 
#                  NULL, NULL)
#plotWatout(watoutData, watoutHeader, "Day", 
#           NULL, NULL)
