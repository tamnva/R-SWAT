#-------------------------------------------------------------------------------
# Evaluate model performance (single variable)
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
    obsKGE <- 1 - sqrt((correlation - 1)**2  + (sdSim/sdObs - 1)**2 + (mSim/mObs - 1)**2)
    
    rmse <- sqrt(mean(sim_obs**2))

    result <- matrix(c(objNSE, obsKGE, objR2, rmse, objPBIAS), nrow = 1)
    colnames(result) <- c('NSE', 'KGE', 'R2', 'RMSE', 'PBIAS')
    
    return(result)
  }
  
#-------------------------------------------------------------------------------
  linearRegression <- function(objFunction, parameterValue, objCriteria){
    
    parameterValue <- as.data.frame(parameterValue)
    
    header <- strsplit(colnames(objFunction), split = "_")
    temp <- c()
    for (i in 1:length(header)){
      temp <- c(temp, header[[i]][1])
    }

    temp <- which(temp %in% objCriteria)
    
    if (length(temp) > 1) {
      parameterValue$V1 <- rowSums(objFunction[,temp])
    } else {
      parameterValue$V1 <- objFunction[,temp]
    }
    
    temp <- summary(lm(V1 ~ ., parameterValue))
    
    tValue <- as.numeric(temp$coefficients[,3])
    tValue <- tValue[2:length(tValue)]
    
    pValue <- as.numeric(temp$coefficients[,4])
    pvalue <- pValue[2:length(pValue)]
    
    result <- matrix(c(tValue, pvalue), byrow = FALSE, ncol = 2)
    colnames(result) <-  c('t-stat', 'p-value')

    rowName <- c()    
    for (i in 1:nrow(result)){
      rowName <- c(rowName, paste('Parameter_', i, sep = ''))
    }
    
    rownames(result) <- rowName
    
    return(result)
    
  }

#-------------------------------------------------------------------------------  
  uncertaintyAnalysis <- function(outData, obsData, objFunction, objCriteria, behaThreshold){
    
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