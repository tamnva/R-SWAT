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
