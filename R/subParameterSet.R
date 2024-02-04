
#' Split parametersets (simulations) among threads
#' @description
#' When run SWAT/SWAT+ parallel, this function will split the parameterset to each
#' core, for example if there is the number of parametersets is nparams and the
#' number of cores/threads is ncores. Then each cores will performe nparams/ncores
#' simulations, if nparams is not devisable to ncores, then the last cores will
#' take all remaining parametersets
#'
#'
#' @inheritParams runSWATpar
#' @return A list contain different subset of the parametersets
#'
#' @examples
#'
#'ncores <- 3
#'parameterValue <- matrix(runif(20), ncol=2)
#'splitParameterValue(ncores, parameterValue)
#'
#' @export
#'
#'
splitParameterValue <- function(ncores, parameterValue){
  subParameterSet <- list()
  numberParameterSet <- nrow(parameterValue)
  numberSubset <- as.integer(numberParameterSet/ncores)

  if (ncores > 1){
    for (i in 1:(ncores)){
      istart <- (i-1) * numberSubset + 1
      iend <- i * numberSubset

      if((numberSubset < numberParameterSet/ncores) & (i == ncores)){
        subParameterSet[[i]] <- parameterValue[istart:nrow(parameterValue),]
      } else {
        subParameterSet[[i]] <- parameterValue[istart:iend,]
      }
    }
  } else {
    subParameterSet[[1]] <- parameterValue[,]
  }

  return(subParameterSet)
}
