#' Find behavioral simulation statistics
#'
#' @description
#' Calculate the median, 95PPU, p-factor, r-factor of simulated variables and
#' parameters
#'
#' @inheritParams runSWATpar
#' @inheritParams calObjFunction
#' @param objValue numeric vector of the objective function values
#' @param simData A list of list object containing simulated data, the first key
#' is the variable number and the second key is the simulation number, for example
#' \code{simData[[variable_number]][[simulation_number]]}
#'
#' @param behThreshold the behavioral threshold
#'
#' @param varNumber the index of the variable number
#'
#' @param statIndex model performance index, for example:
#' "NSE",
#' "KGE",
#' "R2",
#' "aBIAS",
#' "RMSE"
#'
#' @param minOrmax character, "Maximize" or "Minimize" the objective function
#' should be maximized/minimized. So it depends on the objective (statIndex)
#' function used in here, for example:
#'  NSE: "Maximize"
#'  KGE: "Maximize"
#'  R2: "Maximize"
#'  aBIAS: "Minimize"
#'  RMSE: "Minimize"
#'
#' @param samplingApproach the parameter sampling approach, possible values are:
#' 'Sensi_Cali_(uniform_Latin_Hypercube_Sampling)' \cr
#' 'Cali_(from_optimization_package)'\cr
#' 'Cali_(from_nloptr_package)')\cr
#' 'Cali_(Dynamically_Dimensioned_Search)'\cr
#' 'Cali_(Generalized_Likelihood_Uncertainty_Estimation)'\cr
#' 'Read_User_Parameter_File'\cr
#' 'Sensi_(from_userDefined_package)'\cr
#' 'Cali_(from_userDefined_package)'
#'
#' @return a list of dataframes showing the 95PPU, median,
#' p-factor, and r-factor.
#'
#' @importFrom stats quantile
#'
#' @examples
#'
#' \donttest{
#' # Please see RSWAT Vignettes
#' }
#'
#'
#' @export
#'
#'
#'

behaSimulation <- function(objValue, simData, parameterValue, behThreshold,
                           varNumber, statIndex, observedData, minOrmax,
                           samplingApproach){

  # find index of simulations which are behavioral simulations
  if (statIndex %in% c("NSE", "KGE", "R2")){
    behaIndex <- which(objValue >= behThreshold)
  } else if(statIndex %in% c("aBIAS", "RMSE")) {
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
      ppuSimData[i,c(1:3)] <- glue(c(0.025, 0.5, 0.975),
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
  if (statIndex %in% c("NSE", "KGE", "R2")){
    ppuSimData[,4] <- simData[[varNumber]][[which(objValue == max(objValue))[1]]]

  } else if(statIndex %in% c("aBIAS","RMSE")) {
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


