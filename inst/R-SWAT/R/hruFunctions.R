# ------------------------------------------------------------------------------
getDate <- function(TxtInOut){
  file_cio <- readLines(paste(TxtInOut, "/file.cio", sep = ""), 60)
  start_sim <- as.Date(paste(substr(file_cio[9],13,17), "0101", sep=""), "%Y%m%d")
  start_sim <- start_sim + as.numeric(substr(file_cio[10],13,17)) - 1
  
  end_sim <- as.Date(paste(
    toString(
      as.numeric(substr(file_cio[9],13,17)) + 
        as.numeric(substr(file_cio[8],13,17)) - 1
    ), 
    "0101", 
    sep=""
  ), 
  "%Y%m%d")
  
  end_sim <- end_sim + as.numeric(substr(file_cio[11],13,17)) - 1
  nyear_skip  <- as.numeric(substr(file_cio[60],13,16))
  
  if(nyear_skip == 0){
    start_eval <- start_sim
  } else {
    start_eval <- as.Date(paste(
      toString(
        as.numeric(substr(file_cio[9],13,17)) + nyear_skip
      ), 
      "0101", 
      sep=""
    ), 
    "%Y%m%d")
  }
  
  return(c(start_sim, start_eval, end_sim))
}

#-------------------------------------------------------------------------------
# Convert data in output.hru to matrix of size [nrow = ndays, ncol = nhrus]
getOutputHru <- function(outputHRU){
  
  hru <- outputHRU[c(2)]
  nhrus <- max(hru)
  ncol <- dim(outputHRU)[2]
  ndays <- dim(hru)[1]/nhrus
  
  
  mylist <- list()
  
  for (i in 1:ncol){
    # columns 1 to 6 are: LULC -> HRU -> GIS -> SUB -> MGT -> AREA
    if(i < 7) {
      mylist[[i]] <- outputHRU[1:nhrus,i] 
    } else {
      mylist[[i]] <- matrix(rep(NA,dim(hru)[1]), ncol = nhrus, nrow = ndays)
      count <- 1
      for (j in 1:ndays){
        for (k in 1:nhrus){
          mylist[[i]][j,k] <- outputHRU[count,i]
          count <- count + 1
        }
      }
    }
  }
  
  return(mylist)
}

#-------------------------------------------------------Get observed data from file to compare with watout
getObsWatout <- function(obsWatoutFile){
  mydata <- read.table(obsWatoutFile , 
                       header = TRUE,
                       sep = ""
  )
  mydata[,1] <- as.Date(mydata[,1], "%Y-%m-%d")
  return(mydata)
}

#--------------------------------------------------------------Get watout header
getWatoutHeader <- function(watout){
  
  WatoutHeader <- readLines(watout, 6)
  WatoutHeader <- strsplit(WatoutHeader[6], "\\s+")[[1]]
  WatoutHeader <- WatoutHeader[WatoutHeader != ""]
  
  # there is no space between element 17 and later on in watout file
  WatoutHeader <- c(WatoutHeader[1:17],
                    paste(strsplit(WatoutHeader[18],"/L")[[1]], "/L", sep=""),
                    WatoutHeader[19:length(WatoutHeader)]
  )
  
  return(WatoutHeader)
}

#-------------------------------------------------------------------------------
# Aggregation
agg <- function(idata, time, method){
  
  #input data in form of matrix, output as list
  
  # aggregation method 1 = daily, 2 = monthly, 3 = yearly, 4 = sum
  if(method == 1){
    
    result <- idata
    
  } else if (method == 2){

    month <- as.numeric(strftime(time, "%m"))
    year <- as.numeric(strftime(time, "%Y"))
    idata <- data.frame(month, year, idata)
    result <- aggregate( .~ month + year, idata, FUN = sum)
    result <- data.matrix(result)
    
  } else if (method == 3){
    
    year <- as.numeric(strftime(time, "%Y"))
    idata <- data.frame(year, idata)
    result <- aggregate( .~ year, idata, FUN = sum)
    result <- data.matrix(result)

  } else if (method == 4){
    
    result <- colSums(idata)
  }
  
  return(result)
}

#-------------------------------------------------------------------------------
# Convert data in watout.dat to matrix of size [nrow = ndays, ncol = nfiles]
watoutData <- function(watoutFiles, column){
  
  watout <- list()
  
  for (i in 1:length(watoutFiles)){
    mydata <- read.table(watoutFiles[1] , 
                         header = FALSE,
                         sep = "",
                         skip = 6
    ) 
    
    if(i == length(watoutFiles)) {
      sdate <- as.Date(paste(mydata[1,1],"0101", sep=""), "%Y%m%d") + 
        + mydata[1,2] - 1
      edate <- as.Date(paste(mydata[dim(mydata)[1],1],"0101", sep=""), "%Y%m%d") + 
        + mydata[dim(mydata)[1],2] - 1 
      
      watout[[i+1]] <- seq(sdate, edate, by ="days")
      
    }
    watout[[i]] <- mydata[, column]
  }
  
  return(watout)
}