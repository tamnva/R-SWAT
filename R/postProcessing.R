# ------------------------------------------------------------------------------
# Find behavioral simulation/parameters
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
    ppuSimData[i,c(1:3)] <- as.numeric(quantile(behaSimData[i,], 
                                                c(0.025, 0.5, 0.975))) 
  }
  
  ppuSimData[,4] <- simData[[varNumber]][[which (objValue == max(objValue))]]
  
  
  ppuSimData <- as.data.frame(ppuSimData)
  ppuSimData <- cbind(observedData[[varNumber]]$Date, ppuSimData)
  colnames(ppuSimData) <- c('Date','lower_95PPU', 'median', 
                            'upper_95PPU', 'bestSim')
  
  ppuParaRange <- matrix(rep(NA, (ncol(parameterValue)-1)*4), ncol = 4)
  
  
  for (i in 1:(ncol(behaParameter)-1)){
    ppuParaRange[i, c(1:3)] <- as.numeric(quantile(behaParameter[,i+1], 
                                                   c(0.025,0.5, 0.975))) 
  }
  
  
  #Best parameter
  ppuParaRange[,4] <- parameterValue[which (objValue == max(objValue)),
                                     2:ncol(parameterValue)]
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


#-------------------------------------------------------------------------------
# Evaluate model performance (single variable)
# ------------------------------------------------------------------------------
  perCriteria <- function(obs, sim){

    missingValue <- which(is.na(obs))
    
    if (length(missingValue) > 0){
      obs <- obs[-missingValue]
      sim <- sim[-missingValue]      
    }
    
    mObs <- mean(obs)
    mSim <- mean(sim)
    obs_mObs <- obs - mObs
    sim_mSim <- sim - mSim
    sim_obs <-  sim - obs
    sumSim <- sum(sim)
    sumObs <- sum(obs)
    correlation <- cor(obs, sim)
    sdObs <- sd(obs)
    sdSim <- sd(sim)
    
    
    objNSE <- 1 - sum(sim_obs**2)/sum(obs_mObs**2)
    objR2 <- correlation ** 2
    objPBIAS <- 100 * (sumObs - sumSim)/sumObs
    obsKGE <- 1 - sqrt((correlation - 1)**2  + (sdSim/sdObs - 1)**2 + 
                         (mSim/mObs - 1)**2)
    
    rmse <- sqrt(mean(sim_obs**2))

    result <- matrix(c(objNSE, obsKGE, objR2, rmse, objPBIAS), nrow = 1)
    colnames(result) <- c('NSE', 'KGE', 'R2', 'RMSE', 'PBIAS')
    
    return(result)
  }
  


#------------------------------------------------------------------------------- 
# TODO 
# ------------------------------------------------------------------------------
  uncertaintyAnalysis <- function(outData, obsData, objFunction, 
                                  objCriteria, behaThreshold){
    
    result <- list()
    
    # Loop over number of output file types
    for (i in 1:length(outData$file)){
      
      #Loop over number of Iterations
      for (j in 1:length(outData$file[[i]]$iter)){
        
        #Loop over number of variable
        temp <- 0
        for (k in 1:length(outData$file[[i]]$iter[[j]]$variable)){
          
        }
        
        objFunctionValue[j,] <- objFunctionValue[j,] + temp/k
      }
    }
    
    return(result)
    
  }
  
#------------------------------------------------------------------------------- 
# Calculate objective function
# ------------------------------------------------------------------------------
calObjFunction <- function(parameterValue, ncores, 
                           nOutputVar,userReadSwatOutput, 
                           observedData, workingFolder, 
                           index, dateRangeCali){
  
  output <- list()
  
  nSim <- as.integer(nrow(parameterValue)/ncores)
  nSim <- rep(nSim, ncores)
  nSim[ncores] <- nSim[ncores] + nrow(parameterValue) - sum(nSim)
  
  simData <- list()
  
  counter <- rep(0, nOutputVar)
  output$objValue <-  rep(0, nrow(parameterValue))
  
  #Loop over number of cores
  for (i in 1:ncores){
    #loop over number of variables
    for (j in 1:nOutputVar){
      
      if (userReadSwatOutput[j]){
        ntimestep <- nrow(observedData[[j]]) + 1
      } else {
        ntimestep <- as.numeric(dateRangeCali[2]- dateRangeCali[1]) + 2
      }
      
      if ((i == 1)) {
        simData[[j]] <- list()
        output$perCriteria[[j]] <- list()
        output$simData[[j]] <- list()
      }
      
      #Loop over number of simulation
      fileNameSimData <- paste(workingFolder, "/Output/Core_", 
                               i, "/Output_Variable_", j, ".txt", sep = "")
      tempSimData <- read.table(fileNameSimData, header = FALSE, sep = "")
      for (k in 1:nSim[i]){
        sIndex <- (k-1)*ntimestep + 1
        eIndex <- k*ntimestep
        counter[j] <- counter[j] + 1
        
        simData[[j]][[counter[j]]] <- tempSimData[(sIndex + 1):eIndex, 1]
        output$simData[[j]][[counter[j]]] <- simData[[j]][[counter[j]]]
        
        output$perCriteria[[j]][[counter[j]]] <- perCriteria(observedData[[j]][,2],
                                                             simData[[j]][[counter[j]]])
        
        if((i == 1) & (j == 1)) {
          perIndex <- match(index, 
                            colnames(output$perCriteria[[j]][[counter[j]]]))
        }
        output$objValue[counter[j]] <- output$objValue[counter[j]] + 
          output$perCriteria[[j]][[counter[j]]][perIndex]         
      }
    }
    
  }
  
  # Calculate objective function values
  output$objValue <- output$objValue/nOutputVar
  
  return(output) 
}

#------------------------------------------------------------------------------- 
# Binding list object (with 3 level of list)
# ------------------------------------------------------------------------------
bindList <- function(toList, inList){
  
  nOutputVar <- length(lengths(toList))
  lengthToList <- length(lengths(toList[[1]]))
  lengthInList <- length(lengths(inList[[1]]))
  
  for (i in 1:nOutputVar){
    for (j in 1:lengthInList){
      toList[[i]][[lengthToList + j]] <- inList[[i]][[j]]   
    }
  }
  
  return(toList)
}
  
  