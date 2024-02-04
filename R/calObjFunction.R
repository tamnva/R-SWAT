
#' Read output data from R-SWAT and calculate objective functions for
#' calibration and validation period and for each variable
#'
#' @inheritParams runSWATpar
#' @param nOutputVar number of output variables
#' @param userReadSwatOutput TODO
#' @param observedData list of observed data, each observed data is a data frame
#' @param index performance criteria (NSE, KGE, RMSE, R2, or aBIAS)
#'
#' @return dataframe of objective function values
#' @importFrom utils read.table
#' @examples
#'
#' \donttest{
#' # Please see RSWAT Vignettes
#' vignette("SUFI2_without_GUI",package="RSWAT")
#' }
#'
#' @export

calObjFunction <- function(parameterValue, ncores,
                           nOutputVar, userReadSwatOutput,
                           observedData, workingDirectory,
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
      fileNameSimData <- paste(workingDirectory, "/Output/Core_",
                               i, "/out_var_", j, ".txt", sep = "")
      tempSimData <- read.table(fileNameSimData, header = FALSE, sep = "")

      #Check if length of observed and simulated data are the same or not
      if (nrow(tempSimData) %% ntimestep > 0){
        warnings(" Error: length of observed data # length of the extracted variable")
        message(fileNameSimData)
        message(" Please load the corrected observed data and rerun this step")
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
        temp  <- userObjFunction(observedToList(observedData, flag),
                                 simToList(simData, count, observedData, flag))
        output$objValueCali[count] <- temp$overalPerCriteria

        for (j in 1:nOutputVar){
          output$perCriteriaCali[[j]][[count]] <- temp$perCriteria[[j]]
        }


        # Select validation data with flag "V" or "v"
        flag <- c("V", "v")
        temp  <- userObjFunction(observedToList(observedData, flag),
                                 simToList(simData, count, observedData, flag))
        output$objValueValid[count] <- temp$overalPerCriteria
        for (j in 1:nOutputVar){
          output$perCriteriaValid[[j]][[count]] <- temp$perCriteria[[j]]
        }
      }
    }
  }

  if (index != 'userObjFunction'){
    output$objValueCali <- output$objValueCali/nOutputVar
    output$objValueValid <- output$objValueValid/nOutputVar
  }



  return(output)
}


#------------------------------------------------------------------------------#
#                         Calculate performance criteria                       #
#------------------------------------------------------------------------------#
#' Calculate NSE, KGE, RMSE, R2, and aBIAS
#'
#' @param obs vector of observed data (NA values are allowed)
#' @param sim vector of simulated data (NA values are allowed)
#' @return dataframe with 5 columns (NSE, KGE, RMSE, R2, and aBIAS) and 1 row
#'
#' @examples
#'
#' perCriteria(obs=runif(10), sim=runif(10))
#'
#' @importFrom stats cor
#' @importFrom stats sd
#'
#' @export
#'
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

#------------------------------------------------------------------------------#
#                       Subset of observed data according to flag              #
#------------------------------------------------------------------------------#
#' Subset of observed data according to flag
#'
#' @inheritParams calObjFunction
#' @param flag character ("C" for calibration and "V" for validation)
#' @return list of calibrated or validated data
#' @keywords internal
#' @examples
#' \donttest{
#' observedToList(observedData, flag)
#' }
#'
observedToList <- function(observedData, flag){
  output <- list()
  for (i in 1:length(observedData)){
    output[[i]] <- observedData[[i]][which(observedData[[i]][,3] %in% flag),2]
  }
  return(output)
}


#------------------------------------------------------------------------------#
#                       Subset of observed data according to flag              #
#------------------------------------------------------------------------------#
#' Subset of simulated data according to flag
#'
#' @inheritParams observedToList
#' @inheritParams calObjFunction
#' @param k column number of simulated data
#' @return list of simulated data  for the calibration or validation period
#' @keywords internal
#' @examples
#' \donttest{
#' simToList(simData, k, observedData, flag)
#' }
#'
#'
simToList <- function(simData, k, observedData, flag){
  output <- list()
  for (i in 1:length(simData)){
    output[[i]] <- simData[[i]][[k]][which(observedData[[i]][,3] %in% flag)]
  }
  return(output)
}


