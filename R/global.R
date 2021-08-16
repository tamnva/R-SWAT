# ------------------------------------------------------------------------------
# Default objective function
# ------------------------------------------------------------------------------
dataObjFunction = data.frame(Index = c('NSE'))

columnsObjFunction = data.frame(title = colnames(dataObjFunction),
                                width = c(900),
                                source = I(list(c('NSE', 'KGE', 'R2', 'RMSE', 'PBIAS'))),
                                type = c('dropdown')
                                )

# ------------------------------------------------------------------------------
# Default calibration/sensitivity approach
# ------------------------------------------------------------------------------
dataParaSampling = data.frame(samplingApproach = c("Sensi_Cali_(LHS)"),
                              InputInfo = c(10))

ColumnsParaSampling = data.frame(title = colnames(dataParaSampling),
                                 width = c(900,
                                           900),
                                 source = I(list(c('Sensi_Cali_(LHS)', 
                                                   'Sensi_(Morris)', 
                                                   'Cali_(DDS)_Under_Development_DO_NOT_TRY',
                                                   'Sensi_Cali_(UserInput)_Under_Development_DO_NOT_TRY'), 
                                                 NA)),
                                 type = c('dropdown',
                                          'text')
                                 )


# ------------------------------------------------------------------------------
# Default parameter change values
# ------------------------------------------------------------------------------
dataParaSelection = data.frame(Parameter = c('GW_DELAY.gw', 'CN2.mgt', 'SOL_K.sol', 
                                             'ALPHA_BF.gw', 'ESCO.hru', 'SURLAG.hru', 
                                             'CH_K2.rte', 'SURLAG.bsn'),
                               Change = c('absolute', 'relative', 'relative', 
                                          'replace', 'replace', 'replace', 'replace', 'replace'),
                               Min = c(50,-0.25, -0.25, 0.01, 0.5, 0.1, 0.0, 3.0),
                               Max = c(450,0.25, 0.25, 0.4, 0.99, 10.0, 0.5, 6.0),
                               Subbasin = c('All','All', 'All', 'All', '1,3,5', 'All', 'All', NA),
                               Landuse = c('All','All', 'All', 'All', 'FRSE, RNGB', 'All', NA, NA),
                               soil = c('B59H5401, B63H8301','All', 'All', 'All', 'All', 'All', NA, NA),
                               slope = c('0-5', 'All', 'All', '0-5, 5-9999', 'All', 'All', NA, NA))

columnsParaSelection = data.frame(title= colnames(dataParaSelection),
                                  source = I(list(NA ,
                                                  c('absolute', 'relative','replace'),
                                                  NA, 
                                                  NA, 
                                                  NA, 
                                                  NA,
                                                  NA, 
                                                  NA)),
                                  width= c(300, 
                                           300, 
                                           300, 
                                           300, 
                                           300, 
                                           300,
                                           300, 
                                           300),
                                  type=c('text', 
                                         'dropdown', 
                                         'numeric',
                                         'numeric', 
                                         'numeric', 
                                         'text',
                                         'text', 
                                         'text')
                                  )

# ------------------------------------------------------------------------------
# Default output variables
# ------------------------------------------------------------------------------
dataOutputExtraction = data.frame(FileType = c('watout.dat'),
                                  FileName = c('watout.dat'),
                                  Column = c("4"),
                                  Reach = c(NA))
                               

columnsOutputExtraction = data.frame(title= colnames(dataOutputExtraction),
                                  source = I(list(c('watout.dat', 'output.rch_Under_Development_DO_NOT_TRY'),
                                                  NA,
                                                  NA, 
                                                  NA)),
                                  width= c(300,
                                           300,
                                           300, 
                                           300),
                                  type=c('dropdown', 
                                         'text',
                                         'text',
                                         'text'))

# ------------------------------------------------------------------------------
# Default parameter change values
# ------------------------------------------------------------------------------
sortObservedDataFile <- function(observedDataFile){
  
  if (length(observedDataFile) > 1){
    
    check <- c()
    
    for (i in 1:length(observedDataFile)){
      wantName <- paste("obs_var_", i, ".txt", sep = "")
      for (j in 1:length(observedDataFile)){
        temp <- strsplit(observedDataFile[j], split = "/")
        temp <- temp[[1]][length(temp[[1]])]
        if(temp == wantName) check = c(check, j)
      }
    }
    
    out <- observedDataFile[check]
    
  } else {
    out <- observedDataFile
  }

  return(out)
  
}

# ------------------------------------------------------------------------------
# Merge data frame with different number of row
# ------------------------------------------------------------------------------
mergeDataFrameDiffRow <- function(inputDataFrameList){
  
  if (length(inputDataFrameList) > 1){
    maxVal <- nrow(inputDataFrameList[[1]])
    
    for (i in 2:length(inputDataFrameList)){
      if (nrow(inputDataFrameList[[i]]) > maxVal) maxVal <- nrow(inputDataFrameList[[i]])
    }
    
    for (i in 1:length(inputDataFrameList)){
      if (nrow(inputDataFrameList[[i]]) < maxVal){
        temp <- matrix(rep(NA,(maxVal - nrow(inputDataFrameList[[i]]))*2), ncol = 2)
        colnames(temp) <- c("Date", "Value")
        inputDataFrameList[[i]] <- rbind(inputDataFrameList[[i]], temp) 
      }
      if (i == 1) {
        out <- inputDataFrameList[[i]]
      } else {
        out <- cbind(out, inputDataFrameList[[i]])
      }
    }    
  } else {
    out <- inputDataFrameList[[1]]
  }
  
  return(out)
}

# ------------------------------------------------------------------------------
# Check if a directory and file exist
# ------------------------------------------------------------------------------
checkDirFileExist <- function(InputDir, fileName, fileExtention){
  
  check <- FALSE
  
  if (dir.exists(InputDir)) check = TRUE
  
  if (check){
    
    setwd(InputDir)
    
    filesExist <- FALSE
    
    files <- list.files(pattern = paste(fileName, fileExtention, sep =""))
    
    if (length(files) > 0) {
      check <- TRUE
    } else {
      check <- FALSE
    }
  }
  return(check)
  
}

# ------------------------------------------------------------------------------
# Find behavioral simulation
# ------------------------------------------------------------------------------
behaSimulation <- function(objValue, simData, parameterValue, behThreshold, 
                           varNumber, statIndex, observedData){
  
  # find index of simulation which are behavioral simulations
  if ((statIndex == "NSE") | (statIndex == "KGE") | (statIndex == "R2")){
    behaIndex <- which (objValue >= behThreshold)
  } else if(statIndex == "PBIAS") {
    behaIndex <- which (abs(objValue) <= abs(behThreshold))
  } else {
    behaIndex <- which (objValue <- behThreshold)
  }

  # find 2.5% and 97.5 percentile
  ncol <- length(behaIndex)
  nrow <- length(simData[[varNumber]][[1]])
  
  behaSimData <- matrix(rep(NA, ncol*nrow), ncol = ncol)
  behaParameter <- parameterValue[behaIndex, ]
  
  for (i in 1:length(behaIndex)){
    behaSimData[,i] <- simData[[varNumber]][[behaIndex[[i]]]]
  }

  ppuSimData <- matrix(rep(NA, 4*nrow), ncol = 4)
  for (i in 1:nrow){
    ppuSimData[i,c(1:3)] <- as.numeric(quantile(behaSimData[i,], c(0.025, 0.5, 0.975))) 
  }
  
  ppuSimData[,4] <- simData[[varNumber]][[which (objValue == max(objValue))]]
  

  ppuSimData <- as.data.frame(ppuSimData)
  ppuSimData <- cbind(observedData[[varNumber]]$Date, ppuSimData)
  colnames(ppuSimData) <- c('Date','lower_95PPU', 'median', 'upper_95PPU', 'bestSim')

  ppuParaRange <- matrix(rep(NA, (ncol(parameterValue)-1)*4), ncol = 4)
  
  
  for (i in 1:(ncol(behaParameter)-1)){
    ppuParaRange[i, c(1:3)] <- as.numeric(quantile(behaParameter[,i+1], c(0.025,0.5, 0.975))) 
  }
  
 
  #Best parameter
  ppuParaRange[,4] <- parameterValue[which (objValue == max(objValue)),2:ncol(parameterValue)]
  ppuParaRange <- as.data.frame(ppuParaRange)
  
  # Add parameter number to the data frame
  colnames(ppuParaRange) <- c('lower_95PPU', 'median', 
                              'upper_95PPU', 'bestParameter')
  output <- list()
  output$ppuSimData <- ppuSimData
  output$ppuParaRange <- ppuParaRange
  output$prFactor <- prFactor(observedData[[varNumber]]$Value,
                              ppuSimData$lower_95PPU,
                              ppuSimData$upper_95PPU)
  
  return(output)
}

# ------------------------------------------------------------------------------
# Check SWAT parameter table for calibration
# ------------------------------------------------------------------------------
# check if the parameter selection return null
#paraSelection <- globalVariable$paraSelection
#SWATParam <- globalVariable$SWATParam
# HRUinfo <- globalVariable$HRUinfo

checkSwatParameterName <- function(paraSelection, SWATParam, HRUinfo){

  check <- TRUE
  checkMessage <- "Error: "
  
  if (is.null(paraSelection)) {
    checkMessage <- paste(checkMessage, "Cannot find the parameter table that ",
                          "needs to be check, please add/modify something ", 
                          "in the table. ", 
                          sep ="")
    check <- check & FALSE
  }

  if (is.null(SWATParam)) {
    checkMessage <- paste(checkMessage, "Cannot find the SWAT parameter file, ",
                          "please go to General Setting and select the file. ", 
                          sep ="")
    check <- check & FALSE
  }

  if (check) {
    
    for (i in 1:length(paraSelection$Parameter)){
      
      if (!(paraSelection$Parameter[i] %in%  SWATParam$parameter)){
        checkMessage <- paste(checkMessage, "Cannot find parameter ",
                              paraSelection$Parameter[i], 
                              " in the list of SWAT parameter. ", 
                              sep = "")
        check <- check & FALSE
      }
    }
    
    #----------------------
    if (min(as.numeric(paraSelection$Max) - as.numeric(paraSelection$Min)) < 0.0){
      checkMessage <- paste(checkMessage, "Maximum value must be smaller or ", 
                            "equal with minimum value. ", 
                            sep = "")
      check <- check & FALSE
    }
    #----------------------    
    # check subbasin land use, soil, slope combination produce zero?
    
    if(is.null(HRUinfo)){
      checkMessage <- paste(checkMessage, "Cannot find HRU information (subbasin,",
                            " land use, soil, slope,for checking. Please go back", 
                            " to General Setting using input TxtInOut folder. "
                            , sep = "")
      check <- check & FALSE
    } else {
      
      tempCheck <- checkHRUCombination(paraSelection, SWATParam, HRUinfo)
      if (!tempCheck$check){
        checkMessage <- paste(checkMessage, tempCheck$checkMessage, sep = "")
        check <- check & FALSE        
      }
      
    }
  }
  
  output <- list()
  
  if (!check){
    output$checkMessage <- checkMessage
    output$check <- FALSE      
  } else {
    output$checkMessage <- "Check successfull! Input is correct"
    output$check <- check
  }
  
  return(output)
}

# ------------------------------------------------------------------------------
# Check if HRU combination return NULL
# ------------------------------------------------------------------------------
  checkHRUCombination <- function(paraSelection, SWATParam, HRUinfo){
    
    # List of all file types (according to the spatial resolution)
    hruBasedFile <- c("hru", "gw", "mgt", "chm", "sdr", "sep", "sol")
    subBasedFile <- c("sub", "rte", "swq", "pnd")
    basinBasedFile <- c("wwq", "bsn")
    
    # Convert paraSelection to list object
    selectCriteria <- list()
    change <- list()
    change$parameterRange <- list()
    change$atLine <-list()
    change$atPosition <- list()
    change$numberFormat <- list()
    change$changeMethod <- list()
    change$applyValue <- list()
    
    checkMessage <- " "
    check <- TRUE
    allFiles <- c()
    allParam <- c()
    
    for (i in 1:nrow(paraSelection)){
      para <- trimws(paraSelection[i,1])
      fileType <- strsplit(para, split = '.', fixed = TRUE)[[1]][2]
      changeMethod <- trimws(paraSelection[i,2])
      
      if (i == 1) {
        change$parameterRange <- matrix(as.numeric(paraSelection[i,3:4]), ncol = 2)
      } else {
        change$parameterRange <- rbind(change$parameterRange, 
                                       as.numeric(paraSelection[i,3:4]))
      }
      
      if(fileType %in% hruBasedFile){
        selectCriteria$sub <- trimws(strsplit(paraSelection[i,5], split=",")[[1]])
        selectCriteria$lu <- trimws(strsplit(paraSelection[i,6], split=",")[[1]])
        selectCriteria$soil <- trimws(strsplit(paraSelection[i,7], split=",")[[1]])
        selectCriteria$slope <- trimws(strsplit(paraSelection[i,8], split=",")[[1]])	
      } else if (fileType %in% subBasedFile){
        selectCriteria$sub <- trimws(strsplit(paraSelection[i,5], split=",")[[1]])
      } else {
      }
      
      
      #Get list of files
      if(fileType %in% hruBasedFile){
        files <- hruSubset(HRUinfo, selectCriteria)
        files <- gsub("hru", fileType, files)
      } else if (fileType %in% subBasedFile){
        if (selectCriteria$sub == "All") { 
          selectCriteria$sub <- c(1:max(HRUinfo$sub))
        } else {
          selectCriteria$sub <- as.numeric(selectCriteria$sub)
        }
        for(j in 1:length(selectCriteria$sub)){
          if (j == 1) {files <- NULL}
          files <- c(files, paste(subtofilename(
            as.integer(selectCriteria$sub[j])), ".", fileType, sep ="")) 
        }	        
      } else {
        files <- paste("basins.", fileType, sep="")
      }
      
      if (length(files) == 0){
        checkMessage <- paste(checkMessage, "There is no spatial unit with ", 
                              "such and HRU combination:", 
                               " Subbasin = ", selectCriteria$sub,
                               " Landuse = ", selectCriteria$lu,
                               " Soil = ", selectCriteria$soil,
                               " Slope = ", selectCriteria$slope,
                               " ",
                               sep = "")
        check <- FALSE
      }
      
      fileInFiles <- length(which(files %in% allFiles))
      paraInPara <- length(which(para %in% allParam))
      
      if((fileInFiles > 0) & (paraInPara > 0)){
        checkMessage <- paste(checkMessage, "The same parameter is modified more than 1 times ", 
                              para," ",sep = "")
        check <- FALSE        
      }
      
      allFiles <- c(allFiles, files)
      allParam <- c(allParam, para)
      
    }
      
    output <- list()
    output$checkMessage <- checkMessage
    output$check <- check
    
    return(output)
  }

# ------------------------------------------------------------------------------
# Check if HRU combination return NULL
# ------------------------------------------------------------------------------
getNumberOutputVar <- function(outputExtraction){
  nOutputVar <- 0
  for (i in 1:nrow(outputExtraction)) {
    if (outputExtraction[i,1] == "watout.dat"){
      nOutputVar <- nOutputVar + length(strsplit(outputExtraction[i,3], ",")[[1]])
    } else if(outputExtraction[i,1] == "output.rch"){
      nOutputVar <- nOutputVar + length(strsplit(outputExtraction[i,3], ",")[[1]])*
                                length(strsplit(outputExtraction[i,4], ",")[[1]])
    } else {
      #print("Error: Function getNumberOutputVar")
      #print("ERROR: current option is reading from watout.dat and output.rch")
      nOutputVar <- NULL
    }
  }
  
  return(nOutputVar)

}

# ------------------------------------------------------------------------------
# Calculating p and r factor
# ------------------------------------------------------------------------------
prFactor <- function(obs, low, up){
  
  naIndex <- which(is.na(obs))
  if (length(naIndex) > 0){
    obs <- obs[-c(naIndex)]
    low <- low[-c(naIndex)]
    up <- up[-c(naIndex)]    
  }

  pfactor <- 0
  count <- 0
  count_all <- 0
  rfactor <- 0
  
  for (i in 1:length(obs)){
    count_all <- count_all + 1
    rfactor <- rfactor + up[i] - low[i]
    if(obs[i] >= low[i]){
      if(obs[i] <= up[i]){
        count <- count + 1
      }
    }
  }
  
  pfactor = count/count_all
  rfactor <- rfactor/(count_all * sd(obs))
  
  return(c(pfactor, rfactor))
}

# ------------------------------------------------------------------------------
# Print out number of variables - required observed files
# ------------------------------------------------------------------------------
printVariableNameObservedFiles <- function(outputExtraction){
 
  output <- list()
  output$varNumber <- c()
  output$file <- c()
  output$column <- c()
  output$reach <- c()
  output$observedFile <- c()
  
  counter <- 0

  for (i in 1:nrow(outputExtraction)){

    if(outputExtraction$FileType[i] == "watout.dat"){
      columnNr <- strsplit(outputExtraction$Column[i], split = ",")[[1]]
      for (j in 1:length(columnNr)){
        counter <- counter + 1
        output$varNumber[counter] <- counter
        output$column[counter] <- columnNr[j]
        output$file[counter] <- outputExtraction$FileName[i]
        output$reach[counter] <- NA
        output$observedFile[counter] <- paste("obs_var_", counter ,".txt", sep = "")
      }
    } else if (outputExtraction$FileType[i] == "output.rch") {
      columnNr <- strsplit(outputExtraction$Column[i], split = ",")[[1]]
      reachNr <- strsplit(outputExtraction$Reach[i], split = ",")[[1]]
      for (j in 1:length(columnNr)){
        for (k in 1:length(reachNr)){
          counter <- counter + 1
          output$varNumber[counter] <- counter
          output$column[counter] <- columnNr[j]
          output$file[counter] <- outputExtraction$FileName[i]
          output$reach[counter] <- reachNr[k]
          output$observedFile[counter] <- paste("obs_var_", counter ,".txt", sep = "")          
        }
      }
    } else {
      counter <- counter + 1
      output$varNumber[counter] <- NA
      output$column[counter] <- NA
      output$file[counter] <- "Unknown file type"
      output$reach[counter] <- NA
      output$observedFile[counter] <- NA       
    }
  }
  
  # output data frame
  data <- data.frame(varNumber = output$varNumber,
                    file = output$file,
                    column = output$column,
                    reach = output$reach,
                    observedFile = output$observedFile)
  colnames(data) <- c("Variable number",
                     "Ouput file name",
                     "Column number",
                     "Reach number",
                     "Observed file name (see 4.1)")
  
  
  return(data)
}

# ------------------------------------------------------------------------------
# When user insert there own function to read the output
# ------------------------------------------------------------------------------
userReadOutput <- function(inputFiles){
  
  myDataFrame <- NULL
  
  
  return(myDataFrame)
  
}


