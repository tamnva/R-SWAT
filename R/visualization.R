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

#watoutData <- readWatout("C:/Users/nguyenta/Documents/GitHub/SWATshiny/data/TxtInOut/watout.dat")
#watoutHeader <- readWatoutHeader("C:/Users/nguyenta/Documents/GitHub/SWATshiny/data/TxtInOut/watout.dat")
#varName1 <- "FLOWm^3/s"
#observeData <- read.table("C:/Users/nguyenta/Documents/GitHub/SWATshiny/data/obs_var_1.txt", header = TRUE, sep = "")
#varName2 <- "Qoutlet"
#test <- plotWatout(watoutData, watoutHeader, varName1, 
#                  NULL, NULL)
#plotWatout(watoutData, watoutHeader, "Day", 
#           NULL, NULL)
