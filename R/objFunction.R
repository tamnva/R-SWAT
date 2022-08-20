# ------------------------------------------------------------------------------
# Find behavioral simulation/parameters
# ------------------------------------------------------------------------------
behaSimulation <- function(objValue, simData, parameterValue, behThreshold, 
                           varNumber, statIndex, observedData, minOrmax,
                           samplingApproach){
    
  # find index of simulations which are behavioral simulations
  if ((statIndex == "NSE") | (statIndex == "KGE") | (statIndex == "R2")){
    behaIndex <- which(objValue >= behThreshold)
  } else if(statIndex == "aBIAS" | statIndex == "RMSE") {
    behaIndex <- which(objValue <= abs(behThreshold))
  } else {
    if (minOrmax == "Maximize"){
      behaIndex <- which(objValue >= behThreshold)
    } else {
      behaIndex <- which(objValue <= behThreshold)
    }
  }

  # create a matrix to store behavioral simulations/parameters
  ncol <- length(behaIndex)
  nrow <- length(simData[[varNumber]][[1]])
  behaSimData <- matrix(rep(NA, ncol*nrow), ncol = ncol)
  behaParameter <- parameterValue[behaIndex, ]
  
  # loop over number of behavioral simulations
  for (i in 1:ncol){
    behaSimData[,i] <- simData[[varNumber]][[behaIndex[[i]]]]
  }
  
  # calculate 2.5% and 97.5% percentiles
  ppuSimData <- matrix(rep(NA, 4*nrow), ncol = 4)

  # check which approach is used for calculate the 2.5% and 97.5% percentiles 
  if (samplingApproach == 'Cali_(Generalized_Likelihood_Uncertainty_Estimation)'){

    # If GLUE then use different approach, normalized the likelikhood
    normalizedLik <- which(objValue >= behThreshold)
    normalizedLik <- normalizedLik/sum(normalizedLik)
    
    # loop over number of behavioral simulations to finds 95ppu
    for (i in 1:nrow){
      ppuSimData[i,c(1:3)] <- ecdf.pred(c(0.025, 0.5, 0.975),
                                   behaSimData[i,],
                                   normalizedLik)
    }
    
  } else {
    
    # loop over number of behavioral simulations to finds 95ppu
    for (i in 1:nrow){
      ppuSimData[i,c(1:3)] <- as.numeric(quantile(behaSimData[i,], 
                                                  c(0.025, 0.5, 0.975))) 
    }   
  }

  # find the best simulation
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
 
  # convert matrix to data frame
  ppuSimData <- as.data.frame(ppuSimData)
  
  # add date to the first column
  ppuSimData <- cbind(observedData[[varNumber]]$Date, ppuSimData)
  
  # add column names
  colnames(ppuSimData) <- c('Date','lower_95PPU', 'median', 
                            'upper_95PPU', 'bestSim')
  
  # find behavioral parameter range (2.5%, 50%, and 97.5% percentiles)
  ppuParaRange <- matrix(rep(NA, (ncol(parameterValue)-1)*4), ncol = 4)
  
  
  for (i in 1:(ncol(behaParameter)-1)){
    ppuParaRange[i, c(1:3)] <- as.numeric(quantile(behaParameter[,i+1], 
                                                   c(0.025,0.5, 0.975))) 
  }
  
  # best parameters
  ppuParaRange[,4] <- parameterValue[which (objValue == max(objValue))[1],
                                     2:ncol(parameterValue)]
  
  # convert matrix to data frame
  ppuParaRange <- as.data.frame(ppuParaRange)
  
  # add parameter number to the data frame
  colnames(ppuParaRange) <- c('lower_95PPU', 'median', 'upper_95PPU', 'bestParameter')
  
  # save output as list object
  output <- list()
  
  # behavioral simulated results
  output$ppuSimData <- ppuSimData
  
  # behavioral parameter range
  output$ppuParaRange <- ppuParaRange
  
  # p- and r-factors
  # Calibration period
  iloc <- which(observedData[[varNumber]]$Flag %in% c("C", "c"))
  output$prFactorCali <- prFactor(observedData[[varNumber]]$Value[iloc],
                              ppuSimData$lower_95PPU[iloc],
                              ppuSimData$upper_95PPU[iloc])

  # Calibration period
  iloc <- which(observedData[[varNumber]]$Flag %in% c("V", "v"))
  output$prFactorValid <- prFactor(observedData[[varNumber]]$Value[iloc],
                              ppuSimData$lower_95PPU[iloc],
                              ppuSimData$upper_95PPU[iloc])

  return(output)
}

# ------------------------------------------------------------------------------
# Function to calculate p and r factor
# ------------------------------------------------------------------------------
prFactor <- function(obs, low, up){
  
  naIndex <- which(is.na(obs))
  
  if (length(naIndex) > 0){
    obs <- obs[-c(naIndex)]
    low <- low[-c(naIndex)]
    up <- up[-c(naIndex)]    
  }
  
  # if all data in the observed files are missing values
  if (length(obs) == 0){
    
    # in this case cannot calculate p and r factor
    pfactor <- NA
    rfactor <- NA
    
  } else {
    
    # variable to store the number of simulated values within the 95ppu
    count <- 0
    count_all <- 0
    
    # initially rfactor is 0
    rfactor <- 0
    
    # loop over the length of observed data
    for (i in 1:length(obs)){
      count_all <- count_all + 1
      rfactor <- rfactor + up[i] - low[i]
      if(obs[i] >= low[i]){
        if(obs[i] <= up[i]){
          count <- count + 1
        }
      }
    }
    
    # calculate p factor
    pfactor <- count/count_all
    
    # calculate r factor
    rfactor <- rfactor/(count_all * sd(obs))    
  }
  
  return(c(pfactor, rfactor))
}


#-------------------------------------------------------------------------------
# Evaluate model performance (single variable)
# ------------------------------------------------------------------------------
  perCriteria <- function(obs, sim){

    missingValue <- which(is.na(obs))
    
    if (length(missingValue) == length(obs)){
      NSE <- NA
      R2 <- NA
      aBIAS <- NA
      KGE <- NA
      RMSE <- NA      
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
  output$objValueCali <-  rep(0, nrow(parameterValue))
  output$objValueValid <-  rep(0, nrow(parameterValue))
  output$error <- FALSE
  
  #Loop over number of cores
  for (i in 1:ncores){
    #loop over number of variables
    for (j in 1:nOutputVar){
      
      # Number of time step + 1 (header of each simulation)
      ntimestep <- nrow(observedData[[j]]) + 1

      if ((i == 1)) {
        simData[[j]] <- list()
        output$perCriteriaCali[[j]] <- list()
        output$perCriteriaValid[[j]] <- list()
        output$simData[[j]] <- list()
      }
      
      #Loop over number of simulation
      fileNameSimData <- paste(workingFolder, "/Output/Core_", 
                               i, "/out_var_", j, ".txt", sep = "")
      tempSimData <- read.table(fileNameSimData, header = FALSE, sep = "")
      
      #Check if length of observed and simulated data are the same or not
      if (nrow(tempSimData) %% ntimestep > 0){
        print(" Error: length of observed data # length of the extracted variable")
        print(fileNameSimData)
        print(" Please load the corrected observed data and rerun this step")
        output$objValueCali[] <- -999999
        output$objValueValid[] <- -999999
        output$error <- TRUE
      } else {
        for (k in 1:nSim[i]){
          sIndex <- (k-1)*ntimestep + 1
          eIndex <- k*ntimestep
          counter[j] <- counter[j] + 1
          
          simData[[j]][[counter[j]]] <- tempSimData[(sIndex + 1):eIndex, 1]
          output$simData[[j]][[counter[j]]] <- simData[[j]][[counter[j]]]
          
          # Calculate objective function value (NSE, KGE, RMSE, R2, aBIAS)
          if (index != 'userObjFunction'){
            
            # Objective function value for calibration period, flag C or c
            iloc <- which(observedData[[j]][,3] %in% c("C", "c"))
            
            # calculate 
            output$perCriteriaCali[[j]][[counter[j]]] <- 
              perCriteria(observedData[[j]][iloc,2], simData[[j]][[counter[j]]][iloc])
 
            # Objective function value for calibration period, flag V or v
            iloc <- which(observedData[[j]][,3] %in% c("V", "v"))
            
            # Objective function value for validation period
            output$perCriteriaValid[[j]][[counter[j]]] <- 
              perCriteria(observedData[[j]][iloc,2], simData[[j]][[counter[j]]][iloc])
            
            # Check which performance criteria the user selected
            if((i == 1) & (j == 1)) {
              perIndex <- match(index, colnames(output$perCriteriaCali[[j]]
                                                [[counter[j]]]))
            }   
            
            # Only get the user-selected performance criteria for
            # Calibration period
            output$objValueCali[counter[j]] <- output$objValueCali[counter[j]] + 
              output$perCriteriaCali[[j]][[counter[j]]][perIndex]
            
            # Validation period
            output$objValueValid[counter[j]] <- output$objValueValid[counter[j]] + 
              output$perCriteriaValid[[j]][[counter[j]]][perIndex]
          }
          
        }        
      }
    }
    
    # In case of user defined objective function
    if (index == 'userObjFunction'){
      for (k in 1:nSim[i]){
        count <- count + 1
        
        # Select calibration data with flag "C" or "c"
        flag <- c("C", "c")
        output$objValueCali[count] <- userObjFunction(observedToList(observedData, flag), 
                                                  simToList(simData, count, flag))

        # Select validation data with flag "V" or "v"
        flag <- c("V", "v")        
        output$objValueCali[count] <- userObjFunction(observedToList(observedData, flag), 
                                                      simToList(simData, count, observedData, flag))
      }
    }      
    
  }
  
  if (index != 'userObjFunction'){
    output$objValueCali <- output$objValueCali/nOutputVar
    output$objValueValid <- output$objValueValid/nOutputVar
  }
  
  return(output) 
}


#------------------------------------------------------------------------------- 
# Convert simulated data to list object
# ------------------------------------------------------------------------------
simToList <- function(simData, k, observedData, flag){
  output <- list()
  for (i in 1:length(simData)){
    output[[i]] <- simData[[i]][[k]][which(observedData[[i]][,3] %in% flag)]
  }
  return(output)
}

#------------------------------------------------------------------------------- 
# Convert observed data to list object
# ------------------------------------------------------------------------------
observedToList <- function(observedData, flag){
  output <- list()
  for (i in 1:length(observedData)){
    output[[i]] <- observedData[[i]][which(observedData[[i]][,3] %in% flag),2]
  }
  return(output)
}

#------------------------------------------------------------------------------- 
# Binding list object (with 3 levels of list)
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
      removeFileName <- paste(workingFolder, "/Output/Core_", i, 
                              "/out_var_", j, ".txt", sep ="")
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
      fileName <- paste(workingFolder, "/Output/Core_", i, "/out_var_", 
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

