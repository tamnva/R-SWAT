#' Get range (min, max) of selected parameter
#'
#' @inheritParams runSWATpar
#' @return a matrix with 2 columns (min, max) and nrows (= number of parameters)

#' @examples
#'
#' # Example of parameter selection dataframe of SWAT and SWAT+
#' exampleData$paraSelectionSWAT
#' exampleData$paraSelectionSWATPlus
#'
#' getParamRange(exampleData$paraSelectionSWAT)
#' getParamRange(exampleData$paraSelectionSWATPlus)
#'
#' @export
#'
getParamRange <- function(paraSelection){

  parameterRange <- as.matrix(paraSelection[,3:4])
  temp <- matrix(rep(NA, nrow(parameterRange)*2), ncol = 2)

  for (i in 1:nrow(parameterRange)){
    temp[i,] <- as.numeric(parameterRange[i,])
  }

  return(temp)
}
