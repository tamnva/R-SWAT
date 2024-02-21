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
