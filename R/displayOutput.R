# ------------------------------------------------------------------------------
# Sort observed data files
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
      if (nrow(inputDataFrameList[[i]]) > maxVal) {
        maxVal <- nrow(inputDataFrameList[[i]])
      } 
    }
    
    for (i in 1:length(inputDataFrameList)){
      if (nrow(inputDataFrameList[[i]]) < maxVal){
        temp <- matrix(rep(NA,(maxVal - nrow(inputDataFrameList[[i]]))*2), 
                       ncol = 2)
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
# Get number of output variables
# ------------------------------------------------------------------------------
getNumberOutputVar <- function(outputExtraction){
  nOutputVar <- 0
  userReadSwatOutput <- c()
  for (i in 1:nrow(outputExtraction)) {
    if (outputExtraction[i,1] == "watout.dat"){
      temp <- length(strsplit(outputExtraction[i,3], ",")[[1]])
      nOutputVar <- nOutputVar + temp
      
      if (!is.na(temp)){
        if (is.numeric(temp)){
          if(temp > 0){
            userReadSwatOutput <- c(userReadSwatOutput, rep(FALSE, temp))          
          }
        }        
      }

    } else if(outputExtraction[i,1] == "output.rch" |
              outputExtraction[i,1] == "output.hru" |
              outputExtraction[i,1] == "output.sub" |
              outputExtraction[i,1] == "output.rsv"){
      if ((nchar(outputExtraction[i,3]) > 0) & (nchar(outputExtraction[i,4]) > 0)){
        temp <- sum(lengths(getRchNumber(outputExtraction[i,4])))  
        nOutputVar <- nOutputVar + temp
        if (!is.na(temp)){
          if (is.numeric(temp)){
            if(temp > 0){
              userReadSwatOutput <- c(userReadSwatOutput, rep(FALSE, temp))          
            }
          }        
        }
      }
    } else if(outputExtraction[i,1] == "userReadSwatOutput") {
      temp <- as.numeric(outputExtraction[i,3])
      nOutputVar <- nOutputVar + temp
      if (!is.na(temp)){
        if (is.numeric(temp)){
          if(temp > 0){
            userReadSwatOutput <- c(userReadSwatOutput, rep(TRUE, temp))          
          }
        }        
      }
    } else {
      # TODO: add check if length column == length reach
      userReadSwatOutput <- NULL
      nOutputVar <- NULL
    }
  }
  
  output <- list()
  output$nOutputVar <- nOutputVar
  output$userReadSwatOutput <- userReadSwatOutput
  
  return(output)
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
        output$observedFile[counter] <- paste("obs_var_", counter ,
                                              ".txt", sep = "")
      }
    }else if ((outputExtraction$FileType[i] == "output.rsv") | 
              (outputExtraction$FileType[i] == "output.rch") | 
              (outputExtraction$FileType[i] == "output.hru") | 
              (outputExtraction$FileType[i] == "output.sub") | 
              (outputExtraction$FileType[i] == "output.swr")){
      columnNr <- strsplit(outputExtraction$Column[i], split = ",")[[1]]
      rsvNr <- getRchNumber(outputExtraction$Reach[i])
      
      if (length(columnNr) == length(rsvNr)){
        for (j in 1:length(columnNr)){
          for (k in 1:length(rsvNr[[j]])){
            counter <- counter + 1
            output$varNumber[counter] <- counter
            output$column[counter] <- columnNr[j]
            output$file[counter] <- outputExtraction$FileName[i]
            output$reach[counter] <- rsvNr[[j]][k]
            output$observedFile[counter] <- paste("obs_var_", counter ,
                                                  ".txt", sep = "") 
          }
        }        
      }
    } else if (outputExtraction$FileType[i] == "userReadSwatOutput"){
      
      if (!is.null(outputExtraction$Column[i]) & !is.na(outputExtraction$Column[i])){
        if (!is.numeric(outputExtraction$Column[i])){
          columnNr <- as.numeric(outputExtraction$Column[i])
          for (j in 1:columnNr){
            counter <- counter + 1
            output$varNumber[counter] <- counter
            output$column[counter] <- NA
            output$file[counter] <- paste("userReadSwatOutput_var_", j, sep ="")
            output$reach[counter] <- NA
            output$observedFile[counter] <- paste("obs_var_", counter ,
                                                  ".txt", sep = "")
          }        
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
                      "Output file name",
                      "Column number",
                      "Reach/HRU/Subbasin/Reservoir number",
                      "Observed file name should be (see 4.1)")
  
  return(data)
}



