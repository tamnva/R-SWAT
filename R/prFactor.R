#' Calculate p-factor and r-factor
#'
#' @description
#' Calculate p-factor and r-factor
#'
#' @param obs observed data (vector)
#' @param low lower values of the 95PPU (at 2.5\%)
#' @param up upper values of the 95PPU (at 97.5\%)
#'
#' @return vector of two values c(p-factor, r-factor)
#'
#' @examples
#'
#'\donttest{
#' # Create aritifical simulated streamflow (95PPU) and observed streamflow
#' Q_sim_95ppu_low <-  2 + sin(seq(0,10,0.1))
#' Q_sim_95ppu_high <- 2.2 + sin(seq(0,10,0.1))
#' Q_obs <- (Q_sim_95ppu_low + Q_sim_95ppu_high)/2 + runif(101)*0.2
#'
#' # Plot to see data
#' plot(Q_sim_95ppu_low, col = "black", type = "l", xlab = "Time (days)",
#'      ylab = "Streamflow (m3/s)")
#' lines(Q_sim_95ppu_high, col = "red")
#' points(Q_obs, col = "blue")
#'
#' # Calculate p and r factor
#' prFactor(Q_obs, Q_sim_95ppu_low, Q_sim_95ppu_high )
#'}
#'
#' @importFrom stats sd
#' @export
#'
#'
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










