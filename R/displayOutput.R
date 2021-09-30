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
      temp <- length(strsplit(as.character(outputExtraction[i,3]), ",")[[1]])
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

# ------------------------------------------------------------------------------
# Plot function for simulated values, observed and 95 PPU
# ------------------------------------------------------------------------------
plotSimulated <- function(inputDataFrame){
  inputDataFrame$date <- as.Date(inputDataFrame$date, "%Y-%m-%d")
  myplot <- ggplot(inputDataFrame) + 
    #95 PPU
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, color = "95PPU"), 
                size = 0.0, fill = "red", alpha = 0.3) +
    # Best simulation
    geom_line(aes(x = date, y = best, color = "Best simulation"), alpha = 0.6) +
    # Observed data
    geom_line(aes(x = date, y = median, color = "Median"), alpha = 0.6) +
    # Observed data
    geom_line(aes(x = date, y = observed, color = "Observed"), alpha = 0.6) +
    scale_colour_manual(name= '', values=c("95PPU" = "red", 
                                           "Best simulation" = "red",
                                           "Median" = "green",
                                           "Observed" = "darkblue")) +
    # Axis name
    labs(x ="  ", y = " ") +
    scale_x_date(date_labels = "%m-%Y") +
    theme_bw()
  return(myplot)
}

# ------------------------------------------------------------------------------
# Plot sensitivity LHS - morris
# ------------------------------------------------------------------------------
plotSensitivity <- function(xval, yval, para){
  myData <- data.frame(x = xval, y = yval, Parameters = para)
  
  myData$Parameters <- factor(myData$Parameters, 
                              levels = para[order(xval, decreasing = TRUE)])
    
    #95 PPU
    myplot <- ggplot(data = myData, aes(x=x, y=y, colour = Parameters)) + 
      geom_point() +
      scale_y_log10() + 
      scale_x_log10() +
      theme_bw()
  return(myplot)
}

# ------------------------------------------------------------------------------
# Plot objective function - parameter value
# ------------------------------------------------------------------------------
plotObjFuncParaValue <- function(globalVariable){
  nIter <- nrow(globalVariable$parameterValue)
  nPara <- nrow(globalVariable$paraSelection)
  Objective_function <- rep(globalVariable$objValue, nPara)
  Parameter_Value <- c()
  Parameter <- c()
  
  for(i in 1:length(globalVariable$paraSelection$Parameter)){
    Parameter_Value <- c(Parameter_Value, globalVariable$parameterValue[,i+1])
    Parameter <- c(Parameter, rep(globalVariable$paraSelection$Parameter[i], nIter))
  }
  
  temp <- data.frame(Objective_function, Parameter_Value, Parameter) 
  myPlot <- ggplot(temp, aes(x = Parameter_Value, y = Objective_function, color = Parameter)) + 
    geom_point()+ facet_wrap(vars(Parameter), scales = "free") 
  
  myPlot <- ggplotly(myPlot)
  
  return(myPlot)
}

# ------------------------------------------------------------------------------
# Plot sensitivity - sobol
# ------------------------------------------------------------------------------
plotSensiSobol <- function(paraSelection, tableSensitivity){

  data <- tableSensitivity[1:nrow(paraSelection),]
  colnames(data) <- c("paraName", "original", "bias", "stdErr", "min", "max")
  
  myPlot <- ggplot(data,aes(x=paraName,y=original)) +
    geom_point(shape=21, fill="blue", color="blue", size=3) +
    geom_errorbar(aes(x=paraName,ymax=max,ymin=min),position="dodge", width=.0, colour='blue', size=0.5) +
    xlab(" ") +
    ylab("Sensitivity --> Increasing sensitivity") +
    coord_flip() +
    theme_bw()
  
  myPlot <- ggplotly(myPlot)

  return(myPlot)
}

