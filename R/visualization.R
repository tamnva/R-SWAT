# ------------------------------------------------------------------------------
# Function to read watout.dat file type
# ------------------------------------------------------------------------------

readWatout <- function(fileName){
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
# Read output.rch
# ------------------------------------------------------------------------------
readOutputRch <- function(outputRchFile){
  
  output <- read.table(outputRchFile, header = FALSE, sep = "", skip = 9)
  n <- nchar(outputRchFile)
  
  if (substr(outputRchFile, n-3, n)== ".sub"){
    header <- readLines(outputRchFile, 9)[9]
    colnames(output) <- c("BIGSUB", getOutputSubHeader(header))

  } else {
    header <- readLines(outputRchFile, 9)[9]
    
    colnames(output) <- c("REACH", getOutputRchHeader(header))
  }
  
  output <- output[,-c(1)]
  return(output)
}

# ------------------------------------------------------------------------------
# Subset and temperol aggreagation of output.rch
# ------------------------------------------------------------------------------
subsetOutputRch <- function(readOutputRch, tempAgg, colNam, dataPeriod, rchNr){
  
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
    output[,1] <- paste(output[,1],"-01", sep = "")
    
  } else if (tempAgg == "Yearly"){
    output <- aggregate(x = output, by = list(byYear), FUN = "sum")
    output[,1] <- paste(output[,1],"-01-01", sep = "")
  } else {
    output <- cbind(date = as.character(dataPeriod), data.frame(output))
  }
  colnames(output) <- c("date", colNam)
  return(output)
}


# ------------------------------------------------------------------------------
# Plot output.rch subset
# ------------------------------------------------------------------------------
plotOutputRchSubset <- function(subsetOutputRch, subsetObsRch){
  
  subsetObsRch[,1] <- as.Date(subsetObsRch[,1], "%Y-%m-%d")
  subsetOutputRch[,1] <- as.Date(subsetOutputRch[,1], "%Y-%m-%d")
  
  plotData <- data.frame(date = subsetOutputRch[,1],
                         values = subsetOutputRch[,2],
                         Variable = rep(colnames(subsetOutputRch)[2], nrow(subsetOutputRch)))
  
  plotData <- rbind(plotData, data.frame(date = subsetObsRch[,1],
                                         values = subsetObsRch[,2],
                                         Variable = rep(colnames(subsetObsRch)[2],
                                                        nrow(subsetObsRch)))
                    )

  myPlot <- ggplot(plotData, aes(date, values, colour = Variable, shape = Variable)) +
    geom_line() +     
    labs(x =" ", y = " ") +
    scale_x_date(date_labels = "%m-%Y") +
    theme_bw()
  
  return(ggplotly(myPlot))
}

# ------------------------------------------------------------------------------
# GetOutputSubHeader from output.sub file
# ------------------------------------------------------------------------------
# header <- readLines("C:/data/TxtInOut/output.sub", 9)[9]

getOutputSubHeader <- function(header){
  header <- gsub("[()]", "", header)
  header <- gsub("DOXQ mg/L", " DOXQ_mg/L ", header)
  header <- gsub("CBODU mg/L", " CBODU_mg/L ", header)
  header <- gsub("CBODU mg/L", " CBODU_mg/L ", header)
  header <- gsub("CHOLAmic/L", " CHOLAmic/L ", header)
  header <- gsub("GWNO3kg/ha", " GWNO3kg/ha ", header)
  header <- gsub("LATNO3kg/h", " LATNO3kg/h ", header)
  header <- gsub("LAT Qmm",  " LAT_Q(mm) ", header)
  header <- gsub("SEDPkg/ha", " SEDPkg/ha ", header)
  header <- gsub("SOLPkg/ha", " SOLPkg/ha ", header)
  header <- gsub("NSURQkg/ha", "NSURQkg/ha  ", header)
  header <- gsub("ORGPkg/ha", " ORGPkg/ha ", header)
  header <- gsub("ORGNkg/ha", " ORGNkg/ha ", header)
  header <- gsub("SYLDt/ha", " SYLDt/ha ", header)
  header <- gsub("WYLDmm", " WYLDmm ", header)
  header <- gsub("GW_Qmm", " GW_Qmm ", header)
  header <- gsub("SURQmm", " SURQmm ", header)
  header <- gsub("PERCmm", " PERCmm ", header)
  
  header <- strsplit(header, "\\s+")[[1]]
  header <- header[header != ""]
  
  return(header)
}


# ------------------------------------------------------------------------------
# GetOutputSubHeader from output.rcg file
# ------------------------------------------------------------------------------
# header <- readLines("C:/data/TxtInOut/output.rch", 9)[9]

getOutputRchHeader <- function(header){
  header <- gsub("TOT Pkg", " TOT_Pkg   ", header)
  header <- gsub("TOT Nkg", "  TOT_Nkg  ", header)
  header <- gsub("BACTLP_OUTct", "  BACTLP_OUTct  ", header)
  header <- gsub("BACTP_OUTct", "  BACTP_OUTct  ", header)
  header <- gsub("BED_PSTmg", "  BED_PSTmg  ", header)
  header <- gsub("BURYPSTmg", "  BURYPSTmg  ", header)
  header <- gsub("REACBEDPSTmg", "  REACBEDPSTmg  ", header)
  header <- gsub("DIFFUSEPSTmg", "  DIFFUSEPSTmg  ", header)
  header <- gsub("RESUSP_PSTmg", "  RESUSP_PSTmg  ", header)
  header <- gsub("SETTLPSTmg", "  SETTLPSTmg  ", header)
  header <- gsub("SORPST_OUTmg", "  SORPST_OUTmg  ", header)
  header <- gsub("SORPST_INmg", "  SORPST_INmg  ", header)
  header <- gsub("SOLPST_OUTmg", "  SOLPST_OUTmg  ", header)
  header <- gsub("SOLPST_INmg", "  SOLPST_INmg  ", header)
  header <- strsplit(header, "\\s+")[[1]]
  header <- header[header != ""]
  
  return(header)
}

# ------------------------------------------------------------------------------
# Plot output.sub subset
# ------------------------------------------------------------------------------
plotOutputSubSubset <- function(subsetOutputSub){
  
  subsetOutputSub[,1] <- as.Date(subsetOutputSub[,1], "%Y-%m-%d")
  
  plotData <- data.frame(date = subsetOutputSub[,1],
                         values = subsetOutputSub[,2],
                         Variable = rep(colnames(subsetOutputSub)[2], nrow(subsetOutputSub)))
  
  
  myPlot <- ggplot(plotData, aes(date, values, colour = Variable, shape = Variable)) +
    geom_line() +     
    labs(x =" ", y = " ") +
    scale_x_date(date_labels = "%m-%Y") +
    theme_bw()
  
  return(ggplotly(myPlot))
}
