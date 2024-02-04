#'
#' Find location of yearly output summary in output.rch, output.sub, output.hru
#'
#' @description
#' In files output.rch/.sub/.hru of SWAT, the outputs are summarized every year
#' and at the end of the simulation, therefore, when reading time series from
#' these files, we need to skip these rows. This function will find these rows
#' @param timeSeries vector of data in format (YYYY-MM-DD)
#' @param nRch total number of reaches/subbasins/hrus
#' @return location of yearly summary outputs
#'
#' @examples
#'
#' timeSeries = seq(as.Date("2000-01-01", "%Y-%m-%d"),
#'                  as.Date("2005-01-01", "%Y-%m-%d"),
#'                  by='days')
#'
#' nRch = 3
#' yearlyOutputLoc(timeSeries, nRch)
#'
#' @export
#'
yearlyOutputLoc <- function(timeSeries, nRch){
  # Location of the ending month
  endingYearLoc <- substr(timeSeries, 6, 7)
  endingYearLoc <- which(endingYearLoc %in% "12")

  # Find location of yearly summary
  iloc <- c()
  for (i in 1:length(endingYearLoc)){
    temp <- endingYearLoc[i] * nRch + 1 + (i-1)* nRch
    iloc <- c(iloc, c(temp:(temp + nRch - 1)))
  }

  return(iloc)

}
