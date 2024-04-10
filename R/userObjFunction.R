
#' User-defined objective function
#'
#' @description
#' This is a dummy function. There are countless number of objective functions,
#' which RSWAT cannot include all of them in here. Instead, RSWAT only
#' provides some of the most commonly used objective functions, such as the NSE,
#' KGE, aBIAS, R2. User can implement their own objective function and replace
#' this function. For more detail see RSWAT vignettes (user_objective_function).
#'
#'@param obs a list of vectors containing observed data
#'@param sim a list of vectors containing simulated data
#'
#' @examples
#' \donttest{
#' # Create example data of observed and simulated
#' obs <- list()
#' obs[[1]] <- runif(100)
#' obs[[2]] <- runif(100)
#'
#' # Simulated data
#' sim <- list()
#' sim[[1]] <- runif(100)
#' sim[[2]] <- runif(100)
#'
#' # Lets say now our objective function is the correlation R2 and the weight
#' # for the first variable (obs[[1]]) is 1 and for the second variable is 2
#'
#' # Create a custom objective function for this task
#' userObjFunction <- function(obs, sim){
#'
#'   # Define output variable
#'   output <- list()
#'   output$perCriteria <- list()
#'
#'   # R2 of the first variable
#'   output$perCriteria[[1]] <- cor(obs[[1]], sim[[1]])**2
#'
#'   # R2 of the second variable
#'   output$perCriteria[[2]] <- cor(obs[[2]], sim[[2]])**2
#'
#'   # Final objective function value (with different weights), must be with the name "output"
#'   output$overalPerCriteria <- (1 * output$perCriteria[[1]] + 2 * output$perCriteria[[2]])/3
#'
#'   for (i in 1:length(output$perCriteria)){
#'     output$perCriteria[[i]] <- data.frame(userObjFunc = output$perCriteria[[i]])
#'   }
#'
#'   return(output)
#' }
#'
#' # Overwrite the userObjFunction with our userObjFunction
#' environment(userObjFunction) <- asNamespace('RSWAT')
#' assignInNamespace("userObjFunction", userObjFunction, ns = "RSWAT")
#' }
#'
#' @export

userObjFunction <- function(obs, sim){
  output <- NA
}
