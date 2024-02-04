
#' Random sample of n parameters within a given range using Latin Hypercube Sampling
#'
#' @param nIter The number of samples
#' @param paramRange Dataframe with two columns, the first row and second row
#' are the minimum and maximum range. The number of rows is the number of
#' parameters, each parameter in one row

#' @return Data frame with the first columns is the is the sample ID, followed
#' by n columns correspond to n parameters. The number of rows is the number
#' of sample (nIter)

#' @examples
#'
#' lhsRange(nIter=5, paramRange=data.frame(min=c(0,0,0), max=c(2,2,2)))
#'
#' @importFrom lhs randomLHS
#' @export
#'
lhsRange <- function(nIter, paramRange){

  nParam <- nrow(paramRange)
  paramSampling <- lhs::randomLHS(nIter, nParam)

  for (i in 1:nParam){
    paramSampling[,i] <- paramRange[i,1] +  paramSampling[,i] *
      (paramRange[i,2] - paramRange[i,1])
  }

  paramSampling <- cbind(c(1:nrow(paramSampling)), paramSampling)
  return(paramSampling)
}








