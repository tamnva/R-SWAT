# ------------------------------------------------------------------------------
# Find behavioral simulation/parameters
# ------------------------------------------------------------------------------
behaSimulation <- function(objValue, simData, parameterValue, behThreshold, 
                           varNumber, statIndex, observedData, minOrmax,
                           samplingApproach){
    
  # find index of simulation which are behavioral simulations
  if ((statIndex == "NSE") | (statIndex == "KGE") | (statIndex == "R2")){
    behaIndex <- which (objValue >= behThreshold)
  } else if(statIndex == "aBIAS" | statIndex == "RMSE") {
    behaIndex <- which (objValue <= abs(behThreshold))
  } else {
    if (minOrmax == "Maximize"){
      behaIndex <- which (objValue >= behThreshold)
    } else {
      behaIndex <- which (objValue <= behThreshold)
    }
  }

  # find 2.5% and 97.5% percentile
  ncol <- length(behaIndex)
  nrow <- length(simData[[varNumber]][[1]])
  
  behaSimData <- matrix(rep(NA, ncol*nrow), ncol = ncol)
  behaParameter <- parameterValue[behaIndex, ]
  
  for (i in 1:length(behaIndex)){
    behaSimData[,i] <- simData[[varNumber]][[behaIndex[[i]]]]
  }
  
  ppuSimData <- matrix(rep(NA, 4*nrow), ncol = 4)

  if (samplingApproach == 'Cali_(Generalized_Likelihood_Uncertainty_Estimation)'){
    # If GLUE then use different approach
    # Normalized the likelikhood
    normalizedLik <- which(objValue >= behThreshold)
    normalizedLik <- normalizedLik/sum(normalizedLik)
    for (i in 1:nrow){
      ppuSimData[i,c(1:3)] <- ecdf.pred(c(0.025, 0.5, 0.975),
                                   behaSimData[i,],
                                   normalizedLik)
    }
    
  } else {
    for (i in 1:nrow){
      ppuSimData[i,c(1:3)] <- as.numeric(quantile(behaSimData[i,], 
                                                  c(0.025, 0.5, 0.975))) 
    }   
  }

  # find best simulation
  if ((statIndex == "NSE") | (statIndex == "KGE") | (statIndex == "R2")){

    
    
    ppuSimData[,4] <- simData[[varNumber]][[which(objValue == max(objValue))[1]]]

  } else if(statIndex == "aBIAS" | statIndex == "RMSE") {
    ppuSimData[,4] <- simData[[varNumber]][[which (objValue == min(objValue))[1]]]
  } else {
    if (minOrmax == "Maximize"){
      ppuSimData[,4] <- simData[[varNumber]][[which (objValue == max(objValue))[1]]]
    } else {
      ppuSimData[,4] <- simData[[varNumber]][[which (objValue == min(objValue))[1]]]
    }
  }  
 
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
  ppuParaRange[,4] <- parameterValue[which (objValue == max(objValue))[1],
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
    
    if (length(missingValue) == length(obs)){
      NSE <- 0
      R2 <- 0
      aBIAS <- 0
      KGE <- 0
      RMSE <- 0      
    } else {
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
      
      
      NSE <- 1 - sum(sim_obs**2)/sum(obs_mObs**2)
      R2 <- correlation ** 2
      aBIAS <- abs((sumObs - sumSim)/sumObs)
      KGE <- 1 - sqrt((correlation - 1)**2  + (sdSim/sdObs - 1)**2 + (mSim/mObs - 1)**2)
      RMSE <- sqrt(mean(sim_obs**2))      
    }

    result <- matrix(c(NSE, KGE, R2, RMSE, aBIAS), nrow = 1)
    colnames(result) <- c('NSE', 'KGE', 'R2', 'RMSE', 'aBIAS')
    
    return(result)
  }
  
#------------------------------------------------------------------------------- 
# Calculate objective function
# ------------------------------------------------------------------------------
calObjFunction <- function(parameterValue, ncores, 
                           nOutputVar, userReadSwatOutput, 
                           observedData, workingFolder, 
                           index){
  
  output <- list()
  
  count <- 0
  
  nSim <- as.integer(nrow(parameterValue)/ncores)
  nSim <- rep(nSim, ncores)
  nSim[ncores] <- nSim[ncores] + nrow(parameterValue) - sum(nSim)
  
  simData <- list()
  
  counter <- rep(0, nOutputVar)
  output$objValue <-  rep(0, nrow(parameterValue))
  output$error <- FALSE
  
  #Loop over number of cores
  for (i in 1:ncores){
    #loop over number of variables
    for (j in 1:nOutputVar){
      
      # Number of time step + 1 (header of each simulation)
      ntimestep <- nrow(observedData[[j]]) + 1

      if ((i == 1)) {
        simData[[j]] <- list()
        output$perCriteria[[j]] <- list()
        output$simData[[j]] <- list()
      }
      
      #Loop over number of simulation
      fileNameSimData <- paste(workingFolder, "/Output/Core_", 
                               i, "/Output_Variable_", j, ".txt", sep = "")
      tempSimData <- read.table(fileNameSimData, header = FALSE, sep = "")
      
      #Check if length of observed and simulated data are the same or not
      if (nrow(tempSimData) %% ntimestep > 0){
        print(" Error: length of observed data # length of the extracted variable")
        print(fileNameSimData)
        print(" Please load the corrected observed data and rerun this step")
        output$objValue[] <- -999999
        output$error <- TRUE
      } else {
        for (k in 1:nSim[i]){
          sIndex <- (k-1)*ntimestep + 1
          eIndex <- k*ntimestep
          counter[j] <- counter[j] + 1
          
          simData[[j]][[counter[j]]] <- tempSimData[(sIndex + 1):eIndex, 1]
          output$simData[[j]][[counter[j]]] <- simData[[j]][[counter[j]]]
          
          if (index != 'userObjFunction'){
            
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
    }
    
    # In case of user defined objective function
    if (index == 'userObjFunction'){
      for (k in 1:nSim[i]){
        count <- count + 1
        output$objValue[count] <- userObjFunction(observedToList(observedData), 
                                                  simToList(simData, count))
      }
    }      
    
  }
  
  if (index != 'userObjFunction'){
    output$objValue <- output$objValue/nOutputVar
  }
  
  return(output) 
}


#------------------------------------------------------------------------------- 
# Convert simulated data to list object
# ------------------------------------------------------------------------------
simToList <- function(simData, k){
  output <- list()
  for (i in 1:length(simData)){
    output[[i]] <- simData[[i]][[k]]
  }
  return(output)
}

#------------------------------------------------------------------------------- 
# Convert observed data to list object
# ------------------------------------------------------------------------------
observedToList <- function(observedData){
  output <- list()
  for (i in 1:length(observedData)){
    output[[i]] <- observedData[[i]][,2]
  }
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

#------------------------------------------------------------------------------- 
# Remove output files
# ------------------------------------------------------------------------------
removeOutputFiles <- function(workingFolder, ncores, nOutputVar){
  for (i in 1:ncores){
    for (j in 1:nOutputVar){
      removeFileName <- paste(workingFolder, "/Output/Core_", i, "/Output_Variable_", j, ".txt", sep ="")
      file.remove(removeFileName)
    }
  }
}

#------------------------------------------------------------------------------- 
# Write output files
# ------------------------------------------------------------------------------
writeOutputFiles <- function(workingFolder, ncores, nOutputVar, simData){
  for (i in 1:ncores){
    for (j in 1:nOutputVar){
      fileName <- paste(workingFolder, "/Output/Core_", i, "/Output_Variable_", 
                        j, ".txt", sep ="")
      
      # Remove file
      file.remove(fileName)
      
      # Check which core and which simulation
      sim <- seq(from = i, to = length(simData[[nOutputVar]]), ncores)
      
      # Write simulation data
      for (k in 1:length(sim)){
        write.table(as.character(sim[k]), fileName, col.names = FALSE, 
                    row.names = FALSE, append = TRUE)
        write.table(simData[[j]][[sim[k]]], fileName, col.names = FALSE, 
                    row.names = FALSE, append = TRUE)
      }
    }
  }
}
