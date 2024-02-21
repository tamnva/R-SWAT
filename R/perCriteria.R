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
