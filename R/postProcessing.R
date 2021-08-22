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
# Check if the extracted output is defined by user or not 
# ------------------------------------------------------------------------------
  checkUserReadSwatOutput <- function(outputExtraction, varNumber){
    
    findRow <- which(outputExtraction$FileType == "userReadSwatOutput")
    
    if (length(findRow > 0)){
      output <- TRUE
    } else {
      output <- FALSE
    }
}
  
  