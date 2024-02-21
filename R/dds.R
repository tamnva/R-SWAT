#'
#' Parameter optimization using Dynamically Dimensioned Search (DDS)
#'
#' @param para data frame of parameter, its mean, max and actual parameter values
#' @param ncores number of cores for parallel run
#' @param iter current iteration number
#' @param nIters total number of iterations
#' @param r TOO
#' @param parallelMode TODO
#'
#' @examples
#'\donttest{
#'para <- data.frame(min = c(0.1, 0.2, 0.3, 0.4),
#'                   max = c(1,2,3,4),
#'                   paraValue = c(0.3, 0.7, 1.3, 2.4))
#' ncores <- 3
#' iter <- 3
#' nIters <- 20
#' r <- 0.2
#' parallelMode <- 0
#'}
#'
#' @importFrom stats runif
#' @importFrom stats rnorm
#' @export
#'
#'
dds <- function(para,
                ncores,
                iter,
                nIters,
                r,
                parallelMode){

  # Number of parameters
  nPara <- nrow(para)


  # Probability for including in the perturbation list
  p <- 1 - log(iter)/log(nIters)

  # output parameters
  outPara <- matrix(rep(NA, ncores*nPara), nrow = ncores)

  for (core in 1:ncores){

    # Add parameter to the perturb list with probability p
    idx <- which(runif(nPara) < p)

    # Select random parameter for perturbation list if it is empty
    if (length(idx) == 0) {
      idx <- sample.int(nPara, 1)
    }

    # Calculate sigma
    sigma <- r * (para[[2]][idx] - para[[1]][idx])

    # Pertub parameters that in the perturbation list
    if (parallelMode == 1) {  #Single input paraValue
      paraNew <- para[[core + 2]][idx] + sigma * rnorm(length(idx), mean = 0, sd = 1)
    } else {
      paraNew <- para[[3]][idx] + sigma * rnorm(length(idx), mean = 0, sd = 1)
    }

    # Check if min/max boundary condition is satisfied
    for (i in 1:length(idx)){
      # Check min
      if (paraNew[i] < para[[1]][idx[i]]) {
        paraNew[i] = para[[1]][idx[i]] + (para[[1]][idx[i]] - paraNew[i])
        if (paraNew[i] > para[[2]][idx[i]]) {paraNew[i] = para[[1]][idx[i]]}
      }

      # Check max
      if (paraNew[i] > para[[2]][idx[i]]) {
        paraNew[i] = para[[2]][idx[i]] - (paraNew[i] - para[[2]][idx[i]])
        if (paraNew[i] < para[[1]][idx[i]]) { paraNew[i] = para[[2]][idx[i]]}
      }
    }

    # update new parameter after checking min max
    if (parallelMode == 1) {
      outPara[core, ] <- para[[core + 2]]
      outPara[core, idx] <- paraNew
    } else {
      outPara[core, ] <- para[[3]]
      outPara[core, idx] <- paraNew
    }

  }

  outPara <- cbind(c(1:nrow(outPara)), outPara)
  return(outPara)
}
