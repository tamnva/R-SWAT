# ------------------------------------------------------------------------------
# Convert subbasin number to subbasin name
# ------------------------------------------------------------------------------
  subtofilename <- function(sub){
    if(sub < 10){
      filename <- paste("0000", sub, "0000", sep="")
    } else if (sub < 100){
      filename <- paste("000", sub, "0000", sep="")      
    } else if (sub < 1000){
      filename <- paste("00", sub, "0000", sep="")  
    } else if (sub < 10000){
      filename <- paste("0", sub, "0000", sep="")  
    } else {
      filename <- paste( sub, "0000", sep="")  
    }
    return(filename)
  }

# ------------------------------------------------------------------------------
# Update numbers in a file
# ------------------------------------------------------------------------------

	updateSingleFile <- function(toDir,
	                             file,
	                             fileContent,
	                             atLine,
	                             atPosition,
	                             changeMethod,
	                             applyValue,
	                             numberFormat,
	                             absoluteMin,
	                             absoluteMax){
	  
	  newfileContent <- fileContent

	  for (i in 1:length(atLine)){

	    # check if this is soil file (then change all soil layers)
	    if (substr(file, nchar(file) - 2, nchar(file)) == 'sol'){
	      if (atLine[i] >= 8){
	        
	        nchars <- atPosition[i,2] - atPosition[i,1]
	        nlayer <- length(strsplit(trimws(substr(newfileContent[atLine[i]],
	                                                atPosition[i,1], 
	                                                nchar(newfileContent[atLine[i]]))), 
	                                  split = "\\s+")[[1]])
	        
	        for (j in 1:nlayer){
	          
	          if (j > 1){
	            atPosition[i,1] <- atPosition[i,2] + 1 
	            atPosition[i,2] <- atPosition[i,1] + nchars
	          }
	          parameterValue <- as.double(substr(newfileContent[atLine[i]],
	                                             atPosition[i,1],
	                                             atPosition[i,2]))
	          
	          if (changeMethod[i] == "relative"){
	            newParameterValue <- (1.0 + applyValue[i]) * parameterValue
	          } else if (changeMethod[i] == "replace") {
	            newParameterValue <- applyValue[i]
	          } else {
	            newParameterValue <- applyValue[i] + parameterValue
	          }
	          
	          if (newParameterValue < absoluteMin[i]) newParameterValue <- absoluteMin[i]
	          if (newParameterValue > absoluteMax[i]) newParameterValue <- absoluteMax[i]
	          
	          toText <- sprintf(numberFormat[i], newParameterValue)
	          
	          substr(newfileContent[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText	          
	        }
	        
	      } else {
	        parameterValue <- as.double(substr(newfileContent[atLine[i]],
	                                           atPosition[i,1],
	                                           atPosition[i,2]))
	        
	        if (changeMethod[i] == "relative"){
	          newParameterValue <- (1.0 + applyValue[i]) * parameterValue
	        } else if (changeMethod[i] == "replace") {
	          newParameterValue <- applyValue[i]
	        } else {
	          newParameterValue <- applyValue[i] + parameterValue
	        }
	        
	        if (newParameterValue < absoluteMin[i]) newParameterValue <- absoluteMin[i]
	        if (newParameterValue > absoluteMax[i]) newParameterValue <- absoluteMax[i]
	        
	        toText <- sprintf(numberFormat[i], newParameterValue)
	        
	        substr(newfileContent[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText	        
	        
	      }
	      
	    } else {
	      parameterValue <- as.double(substr(newfileContent[atLine[i]],
	                                         atPosition[i,1],
	                                         atPosition[i,2]))
	      
	      if (changeMethod[i] == "relative"){
	        newParameterValue <- (1.0 + applyValue[i]) * parameterValue
	      } else if (changeMethod[i] == "replace") {
	        newParameterValue <- applyValue[i]
	      } else {
	        newParameterValue <- applyValue[i] + parameterValue
	      }
	      
	      if (newParameterValue < absoluteMin[i]) newParameterValue <- absoluteMin[i]
	      if (newParameterValue > absoluteMax[i]) newParameterValue <- absoluteMax[i]
	        
	      toText <- sprintf(numberFormat[i], newParameterValue)
	      
	      substr(newfileContent[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText	      
	    }

	  }

	  writeLines(newfileContent,paste(toDir, '/', file, sep = ''))
	}

# ------------------------------------------------------------------------------
# Update multiple files in a folder
# ------------------------------------------------------------------------------

	updateMultiFile <-  function(toDir, caliParam, parameterValue, paraSelection){

	  # If this is SWAT+ project
	  if (isTRUE(caliParam$file == "calibration.cal")){
	    updateCalibrationFile(paraSelection, parameterValue, toDir)	    
	    
	  # If this is SWAT project
	  } else {
	    # Loop over list of files
	    for (i in 1:length(caliParam$file)){
	      
	      updateSingleFile(toDir,
	                       caliParam$file[i],
	                       caliParam$fileContent[[i]],
	                       caliParam$atLine[[i]],
	                       caliParam$atPosition[[i]],
	                       caliParam$changeMethod[[i]],
	                       caliParam$applyValue[[i]],
	                       caliParam$numberFormat[[i]],
	                       caliParam$absoluteMin[[i]],
	                       caliParam$absoluteMax[[i]])
	    }	    
	  }
	}

#-------------------------------------------------------------------------------
# Get HRU information from a single hru file
# ------------------------------------------------------------------------------
	getHruHeader <- function(hruHeader){

	  # Read information from header of HRU file
	  for (i in 1:nchar(hruHeader)){
	    if(substr(hruHeader,i,i+2) == "HRU"){
	      hruID <- i
	    }
	    if(substr(hruHeader,i,i+7) == "Subbasin"){
	      subID <- i
	    }
	    if(substr(hruHeader,i,i+3) == "Soil"){
	      soilID <- i
	    }
	    if(substr(hruHeader,i,i+4) == "Slope"){
	      slopeID <- i
	    }
	    if(substr(hruHeader,i,i+3) == "Luse"){
	      luseID <- i
	    }
	  }

	  result <- list()

	  # get subbasin number
	  result$sub <- substr(hruHeader,subID, hruID - 1)
    result$sub <- gsub("Subbasin", " ",result$sub )
    result$sub <- as.numeric(gsub(":", " ",result$sub ))

	  # get HRU number
	  result$hru <- substr(hruHeader,hruID , luseID - 1)
	  result$hru <- gsub("HRU", " ",result$hru )
	  result$hru <- as.numeric(gsub(":", " ",result$hru ))

	  # get soil name
	  result$soil <- substr(hruHeader,soilID, slopeID - 1)
	  result$soil <- gsub("Soil", " ",result$soil )
	  result$soil <- trimws(gsub(":", " ",result$soil ))

	  # get land use name
	  result$lu <- substr(hruHeader,luseID, soilID - 1)
	  result$lu <- gsub("Luse", " ",result$lu )
	  result$lu <- trimws(gsub(":", " ",result$lu ))
	  
	  # get slope class
	  result$slope <- substr(hruHeader, slopeID, nchar(hruHeader))
	  result$slope <- gsub("Slope", " ",result$slope )
	  result$slope <- trimws(gsub(":", " ",result$slope ))
	  result$slope <- strsplit(result$slope, "\\s+")[[1]][1]

	  return(result)
	}


#-------------------------------------------------------------------------------
# Get HRU information from all HRU files (in TxtInOut folder)
# ------------------------------------------------------------------------------
  getHruInfo <- function(TxtInOut){

    # list of .hru files in TxtInOut folder
    lstFiles <-  list.files(path = TxtInOut,
                            pattern = ".hru",
                            full.names = TRUE)

    # remove output.hru from list files
    file <- basename(lstFiles)
    
    # remove all files that are not hru files
    temp <- c()
    for (i in 1:length(file)){
      if (nchar(file[i]) != 13){
        temp <- c(temp, i)
      } else {
        fname <- substr(file[i],1,9)
        if (is.na(as.numeric(fname))){
          temp <- c(temp, i)
        }
      }
    }

    
    if(!is.null(temp)){
      file <- file[-temp]
      lstFiles <- lstFiles[-temp]      
    }

    # Initialized vector to store HRU information from headers of HRU files
    hru <- c()
    sub <- c()
    soil <- c()
    lu <- c()
    slope <- c()

    # Loop over all HRU files
    for (i in 1:length(lstFiles)){

      temp <- getHruHeader(readLines(lstFiles[i], 1, warn = FALSE))

      # HRU number
      hru <- c(hru, temp$hru)

      # Subbasin number
      sub <- c(sub, temp$sub)

      # Soil name
      soil <- c(soil, temp$soil)

      # Land use class
      lu <- c(lu, temp$lu)

      # Slope class
      slope <- c(slope, temp$slope)
    }

    # Store HRU information in a data frame
    hruInfo <- data.frame("file" = file, "hru" = hru, "sub" = sub, 
                          "soil" = soil, "lu" = lu, "slope" = slope)

    # Return result
    return(hruInfo)
  }

#-------------------------------------------------------------------------------
# Get subset of HRU based on land use, soil type, slope, subbasin selection
# ------------------------------------------------------------------------------
  
  hruSubset <- function(HRUinfo, selectCriteria){

    hru <- HRUinfo
    
    # Subset with land use selection
    if (is.na(which("All" %in% selectCriteria$lu)[1])) {
      hru <- hru[hru$lu %in% selectCriteria$lu, ]
    }

    # Subset with soil type selection
    if (is.na(which("All" %in% selectCriteria$soil)[1])) {
      hru <- hru[hru$soil %in% selectCriteria$soil, ]
    }

    # Subset with slope class selection
    if (is.na(which("All" %in% selectCriteria$slope)[1])) {
      hru <- hru[hru$slope %in% selectCriteria$slope, ]
    }

    # Subset with subbasin selection
    if (is.na(which("All" %in% selectCriteria$sub)[1])) {
      hru <- hru[hru$sub %in% selectCriteria$sub, ]
    }

    return(hru$file)

  }

#-------------------------------------------------------------------------------
# Read all SWAT parameters form "swat_param.txt" file
# ------------------------------------------------------------------------------

	loadSwatParam <- function(swatParamFile){
  
	  # check SWAT or SWAT+ parameter file
	  fileName <- substr(swatParamFile, nchar(swatParamFile) - 12, nchar(swatParamFile))
	  
	  # If this is SWAT parameter file
	  if (fileName == "swatParam.txt"){
	    
	    check <- c()
	    swat_param <- readLines(swatParamFile, warn = FALSE)
	    
	    for (i in 1:length(swat_param)){
	      checkCommentBlankLine <- substr(trimws(swat_param[i]), 1, 1)
	      if ((checkCommentBlankLine == "!") | (checkCommentBlankLine == "")) {
	        check <- c(check, i)
	      }
	    }
	    
	    swat_param <- swat_param[-check]
	    
	    param <- read.table(textConnection(swat_param))
	    colnames(param) <- c("parameter", "lineNumber", "startPosition",
	                         "endPosition","precision", "absoluteMin", "absoluteMax")	  
	    
	  # If this is a SWAT+ paramter file
	  } else {
	 
	    param <- read.table(swatParamFile, skip = 2, header = TRUE, sep = "")
	  }

	  return(param)
	}

#-------------------------------------------------------------------------------
# Load param_change.txt file
# ------------------------------------------------------------------------------	
	
	loadParamChangeFileContent <- function(HRUinfo, paraSelection, SWATParam, TxtInOutFolder){

	  change <- list()
	  
	  # If this is SWAT+ project
	  if (ncol(HRUinfo) == 6){
	    # List of all file types (according to the spatial resolution)
	    hruBasedFile <- c("hru", "gw", "mgt", "chm", "sdr", "sep", "sol")
	    subBasedFile <- c("sub", "rte", "swq", "pnd")
	    resFile <- c("res")
	    basinBasedFile <- c("wwq", "bsn")
	    
	    # Convert paraSelection to list object
	    selectCriteria <- list()
	    change$parameterRange <- list()
	    change$atLine <-list()
	    change$atPosition <- list()
	    change$numberFormat <- list()
	    change$changeMethod <- list()
	    change$applyValue <- list()
	    
	    counter <- 0
	    
	    for (i in 1:nrow(paraSelection)){
	      para <- trimws(paraSelection[i,1])
	      temp <-  strsplit(para, split = '.', fixed = TRUE)[[1]]
	      fileType <- temp[2]
	      changeMethod <- trimws(paraSelection[i,2])
	      
	      if (i == 1) {
	        change$parameterRange <- matrix(as.numeric(paraSelection[i,3:4]), ncol = 2)
	      } else {
	        change$parameterRange <- rbind(change$parameterRange, 
	                                       as.numeric(paraSelection[i,3:4]))
	      }
	      
	      # Check if there is new files/parameters added by user
	      if (length(temp) == 3){
	        files <- paste(temp[2], ".", temp[3], sep = "") 
	      } else {
	        if(fileType %in% hruBasedFile){
	          selectCriteria$sub <- trimws(strsplit(paraSelection[i,5], split=",")[[1]])
	          selectCriteria$lu <- trimws(strsplit(paraSelection[i,6], split=",")[[1]])
	          selectCriteria$soil <- trimws(strsplit(paraSelection[i,7], split=",")[[1]])
	          selectCriteria$slope <- trimws(strsplit(paraSelection[i,8], split=",")[[1]])	
	        } else if (fileType %in% subBasedFile){
	          selectCriteria$sub <- trimws(strsplit(paraSelection[i,5], split=",")[[1]])
	        } else if (fileType %in% resFile){
	          selectCriteria$sub <- trimws(strsplit(paraSelection[i,5], split=",")[[1]])
	        } else {
	        }
	        
	        
	        #Get list of files
	        if(fileType %in% hruBasedFile){
	          files <- hruSubset(HRUinfo, selectCriteria)
	          files <- gsub("hru", fileType, files)
	        } else if (fileType %in% subBasedFile){
	          if ("All" %in% selectCriteria$sub) { 
	            selectCriteria$sub <- c(1:max(HRUinfo$sub))
	          } else {
	            selectCriteria$sub <- as.numeric(selectCriteria$sub)
	          }
	          for(j in 1:length(selectCriteria$sub)){
	            if (j == 1) {files <- NULL}
	            files <- c(files, paste(subtofilename(
	              as.integer(selectCriteria$sub[j])), ".", fileType, sep ="")) 
	          }
	        } else if (fileType %in% resFile){
	          if ("All" %in% selectCriteria$sub) { 
	            selectCriteria$sub <- substring(list.files(TxtInOutFolder,'.res'), 1, 
	                                            nchar(list.files(TxtInOutFolder,'.res') ) - 8)
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
	      }
	      
	      
	      
	      # Find location of the parameters in the file
	      index <- which(SWATParam$parameter == para)
	      atLine <- SWATParam$lineNumber[[index]]
	      atPosition <- c(SWATParam$startPosition[[index]], 
	                      SWATParam$endPosition[[index]])
	      numberFormat <-   paste('%', atPosition[2] - atPosition[1] + 1, '.', 
	                              SWATParam$precision[[index]], 'f', sep ="")
	      absoluteMin <- SWATParam$absoluteMin[[index]]
	      absoluteMax <- SWATParam$absoluteMax[[index]]
	      
	      if (i == 1){
	        nfiles <- length(files)
	        change$file <- files
	        for (j in 1:nfiles){
	          change$atLine[[j]] <- atLine
	          change$atPosition[[j]] <- matrix(atPosition,ncol = 2, byrow = TRUE)
	          change$numberFormat[[j]] <- numberFormat
	          change$changeMethod[[j]] <- changeMethod
	          change$applyValue[[j]] <- i
	          change$absoluteMin[[j]] <- absoluteMin
	          change$absoluteMax[[j]] <- absoluteMax
	        }
	        
	      } else {
	        
	        # Find if old files need to be updated
	        intersectFiles <- intersect(change$file,files)
	        if (length(intersectFiles) >= 1){
	          idx <-  which(change$file %in% intersectFiles)	   
	          for (j in 1:length(idx)){
	            change$atLine[[idx[j]]] <- c(change$atLine[[idx[j]]], atLine)
	            change$atPosition[[idx[j]]] <- rbind(change$atPosition[[idx[j]]], c(atPosition))
	            change$numberFormat[[idx[j]]] <- c(change$numberFormat[[idx[j]]], numberFormat)
	            change$changeMethod[[idx[j]]] <- c(change$changeMethod[[idx[j]]], changeMethod)
	            change$applyValue[[idx[j]]] <- c(change$applyValue[[idx[j]]], i)
	            change$absoluteMin[[idx[j]]] <- c(change$absoluteMin[[idx[j]]], absoluteMin)
	            change$absoluteMax[[idx[j]]] <- c(change$absoluteMax[[idx[j]]], absoluteMax)
	          }	
	        }
	        
	        change$file <- unique(c(change$file, files))
	        nfiles <- length(change$file)
	        
	        if (nfiles > counter){
	          for (j in (counter+1):nfiles){
	            change$atLine[[j]] <- atLine
	            change$atPosition[[j]] <- matrix(atPosition, ncol = 2, byrow = TRUE) 
	            change$numberFormat[[j]] <- numberFormat
	            change$changeMethod[[j]] <- changeMethod
	            change$applyValue[[j]] <- i
	            change$absoluteMin[[j]] <- absoluteMin
	            change$absoluteMax[[j]] <- absoluteMax
	          }	        
	        }
	      }	    
	      counter <- nfiles	    
	    }
	    
	    change$fileContent <- readContentFiles(TxtInOutFolder, change$file)
	    change$paramFlag <- change$applyValue
	    
	  # This is SWAT project
	  } else {
	    change$file <- "calibration.cal"
	  }

	  
	  return(change)
	}
	
#-------------------------------------------------------------------------------	
# Get parameter value
#-------------------------------------------------------------------------------
	getParameterValue <- function(parameter, parameterValue){
	  for (i in 1:length(parameter)){
	    for (j in 1:length(parameter[[i]])){
	      parameter[[i]][j] <- parameterValue[parameter[[i]][j]]
	    }
	  }
	  return(parameter)
	}


#-------------------------------------------------------------------------------	
# Copy all files in a folder except files
#-------------------------------------------------------------------------------	
	copyAllExcept <- function(fromDir, toDir, exceptFiles){
	  
	  listFiles <- list.files(fromDir, full.names = FALSE)
	  listFiles <- listFiles[!listFiles %in% exceptFiles]
	  listFiles <- paste(fromDir, "/", listFiles, sep = "")
	  
	  file.copy(listFiles, toDir)
	  
	  # Copy files in subfolders (SWAT+ project)
	  listDir <- list.dirs(fromDir, recursive = FALSE)
	  if(length(listDir) > 0) {
	    for (i in 1:length(listDir)){
	      file.copy(listDir[i], toDir, recursive=TRUE)		      
	    }
	  }
	}	

#-------------------------------------------------------------------------------	
# Read content of all files that are going to be updated
#-------------------------------------------------------------------------------	
	readContentFiles <- function(fileDirectory, files){
	  
	  files <- paste(fileDirectory, "/", files, sep = "")
	  fileContent <- list()

	  for (i in 1:length(files)){
	    fileContent[[i]] <- readLines(files[i], -1, warn = FALSE)
	  }
	  
	  return(fileContent)
	}		

#-------------------------------------------------------------------------------
# Get parameter range from paraSelection matrix
#-------------------------------------------------------------------------------	
	getParamRange <- function(paraSelection){
	  
	  parameterRange <- as.matrix(paraSelection[,3:4])
	  temp <- matrix(rep(NA, nrow(parameterRange)*2), ncol = 2)
	  
	  for (i in 1:nrow(parameterRange)){
	    temp[i,] <- as.numeric(parameterRange[i,])
	  }
	  
	  return(temp)
	}
	

#-------------------------------------------------------------------------------
# update calibration.cal file with new parameter
#-------------------------------------------------------------------------------	
	updateCalibrationFile <- function(paraSelection, parameterValue, currentDirectory){
	  
	  # condition type
	  conTyp <- c("hsg=", "texture=", "plant=", "landuse=", "region=", "region_lte=")
	  
	  
	  fileContent <- c()
	  fileContent[1] <- "calibration.cal file is created by R-SWAT"
	  fileContent[2] <- as.character(nrow(paraSelection)) 
	  fileContent[3] <- "cal_parm            chg_typ       chg_val    conds soil_lyr1 soil_lyr2       yr1       yr2      day1      day2   obj_tot"

	  # Change to SWAT+ keywords
	  paraSelection[,2] <- gsub("replace", "absval", paraSelection[,2])  
	  paraSelection[,2] <- gsub("relative", "relchg", paraSelection[,2])  
	  paraSelection[,2] <- gsub("absolute", "abschg", paraSelection[,2])  
	  
	  # Remove the extension e.g., .hru in the parameter selection data
	  paraName <- strsplit(trimws(paraSelection[,c(1)]), '[.]')
	  for (i in 1:length(paraName)){
	    paraSelection[i,1] <- paraName[[i]][1]
	  }
	  
	  # remove spaces in object and conditions
	  paraSelection[,5] <- gsub(" ", "", paraSelection[,5], fixed = TRUE)
	  paraSelection[,6] <- gsub(" ", "", paraSelection[,6], fixed = TRUE)	  

	  lineCounter <- 3
	  for (i in 1:nrow(paraSelection)){
	    
	    # If "All" or "all" then select all object and conditions
	    if(grepl("All", paraSelection[i,6], fixed = TRUE) |
	       grepl("all", paraSelection[i,6], fixed = TRUE)){
	      paraSelection[i,6] <- ""
	    }

	    if(grepl("All", paraSelection[i,5], fixed = TRUE) |
	       grepl("all", paraSelection[i,5], fixed = TRUE)){
	      paraSelection[i,5] <- ""
	    }
	    
	    # Parameter value
	    val <- parameterValue[i]
	    
	    # Soil layer 1 value
	    lyr1 <- 0
	    if(grepl("lyr1=", paraSelection[i,6], fixed = TRUE)){
	      temp <- strsplit(paraSelection[i,6], ";")[[1]]
	      for (j in 1:length(temp)){
	        if (grepl("lyr1=", temp[j], fixed = TRUE)){
	          layerNr <- strsplit(temp[j], "=")[[1]]
	          lyr1 <- as.numeric(layerNr[length(layerNr)])
	        }
	      }
	    }

	    
	    # Soil layer 2 value
	    lyr2 <- 0
	    if(grepl("lyr2=", paraSelection[i,6], fixed = TRUE)){
	      temp <- strsplit(paraSelection[i,6], ";")[[1]]
	      for (j in 1:length(temp)){
	        if (grepl("lyr2=", temp[j], fixed = TRUE)){
	          layerNr <- strsplit(temp[j], "=")[[1]]
	          lyr2 <- as.numeric(layerNr[length(layerNr)])
	        }
	      }
	    }
	    
	    year1 <- 0
	    day1 <- 0
	    if(grepl("year1=", paraSelection[i,6], fixed = TRUE)){
	      temp <- strsplit(paraSelection[i,6], ";")[[1]]
	      for (j in 1:length(temp)){
	        if (grepl("year1=", temp[j], fixed = TRUE)){
	          time <- strsplit(temp[j], "=")[[1]]
	          year1 <- as.numeric(substr(time[length(time)], 1, 4))
	          day1 <- difftime(as.Date(time[length(time)], "%Y%m%d") + 1,
	                             as.Date(paste(substr(time[length(time)], 1, 4), 
	                                           "0101", sep = ""),  "%Y%m%d"), "days")
	          day1 <- as.numeric(day1)
	           
	        }
	      }
	    }	    
	    
	    year2 <- 0
	    day2 <- 0
	    if(grepl("year2=", paraSelection[i,6], fixed = TRUE)){
	      temp <- strsplit(paraSelection[i,6], ";")[[1]]
	      for (j in 1:length(temp)){
	        if (grepl("year2=", temp[j], fixed = TRUE)){
	          time <- strsplit(temp[j], "=")[[1]]
	          year2 <- as.numeric(substr(time[length(time)], 1, 4))
	          day2 <- difftime(as.Date(time[length(time)], "%Y%m%d") + 1,
	                           as.Date(paste(substr(time[length(time)], 1, 4), 
	                                         "0101", sep = ""),  "%Y%m%d"), "days")
	          day2 <- as.numeric(day2)
	          
	        }
	      }
	    }	
	    
	    # object total
	    if (paraSelection[i,5] == ""){
	      obj_tot <- 0
	      obj <-  " "
	    } else {
	      obj_tot <- length(eval(parse(text = paste("c(",paraSelection[i,5], ")", sep = "" ))))
	      obj <- gsub( ":", ",-", paraSelection[i,5])
	      obj <- as.numeric(strsplit(obj, ",")[[1]])
	    }
	    
	    condition <- c()
	    conds <- 0
	    # condition on hsg, texture, landuse, plant,...
	    if(grepl("hsg=", paraSelection[i,6], fixed = TRUE)){
	      temp <- strsplit(paraSelection[i,6], ";", fixed = TRUE)[[1]]
	      for (j in 1:length(temp)){
	        
	        # hsg condition
	        for (cond in 1:length(conTyp)){
	          if(grepl(conTyp[cond], temp[j], fixed = TRUE)){
	            condsplit <- strsplit(gsub(conTyp[cond], "", temp[j], fixed = TRUE), ",")[[1]]
	            condsplit <- sort(condsplit)
	            conds <- conds + length(condsplit)
	            for (k in 1:length(condsplit)){
	              condition[length(condition) + 1] <- paste(format(gsub("=", "", conTyp[cond], fixed = TRUE), width = 20, justify = "left"),
	                                                        format("=", width = 10, justify = "left"), 
	                                                        format(0, digits = 5, width = 10, justify = "right"),
	                                                        format(condsplit[k], width = 10, justify = "right"),
	                                                        sep = "")
	            }
	          }	          
	        }
	      }
	    }
	    
	    lineCounter <- lineCounter + 1
	    fileContent[lineCounter]  <- paste(format(paraSelection[i,1], width = 20, justify = "left"), 
	                                       format(paraSelection[i,2], width = 10, justify = "left"), 
	                                       format(round(parameterValue[i], 5), nsmall = 5, width = 10, justify = "right"),
	                                       format(conds, width = 10, justify = "right"),
	                                       format(lyr1, width = 10, justify = "right"),
	                                       format(lyr2, width = 10, justify = "right"),
	                                       format(year1, width = 10, justify = "right"),
	                                       format(year2, width = 10, justify = "right"),
	                                       format(day1, width = 10, justify = "right"),
	                                       format(day2, width = 10, justify = "right"),
	                                       format(obj_tot, width = 10, justify = "right"),
	                                       paste(format(obj, width = 10, justify = "right"), collapse = ""),
	                                       sep = "")
	    
	    if (conds > 0){
	      for (m in 1:conds){
	        lineCounter <- lineCounter + 1
	        fileContent[lineCounter]  <- condition[m]
	      }
	    }
	    
	  }

	  writeLines(fileContent, paste(currentDirectory, "/", "calibration.cal", sep = ""))
	}
