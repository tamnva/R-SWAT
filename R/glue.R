#'
#' Generalized likelihood uncertainty estimation (GLUE)
#'
#' @description
#' Calculate the percentiles according Generalized likelihood uncertainty
#' estimation (GLUE). The code was taken from
#' \url{http://www.uncertain-future.org.uk/wp-content/uploads/2016/06/R-GLUE.zip}
#'
#'
#' @param perc percentiles
#' @param x values (numeric vector). In R-SWAT x is simulated values at a specific
#' time step (only from behavioral simulations). So to get, for example, the 95PPU
#' of all time step, need to put this function into a loop.
#'
#' @param w weights (numeric vector, sum = 1). This is the objective function values
#' of behavioral simulations, this must be positive number, current code does not
#' support negative objective function values when calculating 95PPU using GLUE.
#' In other works, the behavioral threshold should be positive in this case.
#'
#' @return the value of x at given percentiles
#'
#' @examples
#' # Simulated streamflow Qt at day t = t from 100 behavioral simulations
#' Qt = runif(100)
#'
#' # NSE of 100 behavioral simulations
#' NSE = runif(100)
#' normalize_NSE = NSE/sum(NSE)
#'
#'# Want to have streamflow at 2.5 and 97.5 percentiles according to GLUE
#'
#' percentiles = c(0.025, 0.975)
#'
#' glue(perc=percentiles, x = Qt, w = normalize_NSE)
#'
#' @export
#'
#'
#-------------------------------------------------------------------------------
# 95% GLUE prediction uncertainty, x is only behavioral simulation
# Please see the original code in this link
# http://www.uncertain-future.org.uk/wp-content/uploads/2016/06/R-GLUE.zip
#-------------------------------------------------------------------------------
glue <- function(perc,x,w){

  #perc = percentile
  #x = values
  #w = weights - sum to 1

  # trim x so only have values with positive weights
  x <- x[w > 0]
  w <- w[w > 0]

  # form the empirical cdf
  sort.x <- sort(x,index=TRUE)
  ecdf <- cumsum(w[sort.x$ix])

  # calculate the percentiles
  out <- rep(NA,length(perc))
  for(ii in 1:length(perc)){
    jj <- which.min(abs(ecdf - perc[ii]))
    flag <- TRUE
    while(flag == TRUE){
      if(perc[ii] <= 0.5){
        if(ecdf[jj] > perc[ii]){
          if (jj == 1) {
            flag <- FALSE
          } else {
            jj = jj - 1
          }
        }else{
          flag <- FALSE
        }
      }else{
        if(ecdf[jj] < perc[ii]){
          if (jj == length(ecdf)){
            flag = FALSE
          } else {
            jj <- jj + 1
          }
        }else{
          flag <- FALSE
        }
      }
    }
    out[ii] <- sort.x$x[jj]
  }

  return(out)
}
