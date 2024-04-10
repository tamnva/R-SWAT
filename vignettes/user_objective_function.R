## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, eval = FALSE----------------------------------------------
#  # TODO change file path to your userObjFunction.R file
#  source("C:/example/userObjFunction.R")

## ----message=FALSE, eval = FALSE----------------------------------------------
#  #' User-defined objective function - DO NOT USE AS IT IS NOW
#  #'
#  #' @description
#  #' This is a user-defined objective function, do not use as it is now, please
#  #' modify the source code of this function if you use according to your need.
#  #' DO NOT CHANGE the name of this function (mus be userObjFunction). ONLY CHANGE
#  #' the code at the place where indicate "START TO CHANGE FROM HERE" and STOP
#  #' at "END OF CHANGE".
#  #'
#  #' @param obs a list of observed data, for example, time series of the first
#  #' observed variable is obs[[1]], of the second observed variable is obs[[2]],
#  #' and so on...
#  #' @param sim a list of simulated data, sim[[1]] corresponds to obs[[1]], and so
#  #' on...
#  #'
#  #' @return a data frame contains a single objective function value
#  #' @importFrom stats cor
#  #' @examples
#  #'
#  #'\donttest{
#  #' userObjFunction(obs, sim)
#  #'}
#  #'
#  #' @export
#  #'
#  
#  userObjFunction <- function(obs, sim){
#  
#    # obs is a list of observed variables
#    # obs[[1]] is a vector of the first observed variable
#    # obs[[2]] is a vector of the second observed variable, and so on...
#    # obs[[...]]
#  
#    # sim is a list of simulated variables
#    # sim[[1]] is a vector of the first simulated variable
#    # sim[[2]] is a vector of the second simulated variable, and so on...
#    # sim[[...]]
#  
#    # The actual number of variables is defined in step 4. Run SWAT -> 1. Output extraction
#  
#    # Remove missing values from the observed and the corresponding values in simulated data
#  
#    for (i in 1:length(obs)){
#  
#      # Find index of missing values in observed data
#      missingValue <- which(is.na(obs[[i]]))
#  
#      # Remove missing values
#      if (length(missingValue) > 0){
#        obs[[i]] <- obs[[i]][-missingValue]
#        sim[[i]] <- sim[[i]][-missingValue]
#      }
#    }
#  
#    # Define output variable
#    output <- list()
#    output$perCriteria <- list()
#  
#    # -------------------------------------------------------------------------#
#    #  TODO: MODIFY the existing code below and add your code below this line  #
#    #                 START TO CHANGE FROM HERE                                #
#    # -------------------------------------------------------------------------#
#    # In this example, lets say you extracted two outputs and want to calculate
#    #                  R square and put more weight to the second variable
#    #
#    #  output <- (1 * R2_Q_1 + 2 * R2_Q_2)/(1 + 2)
#  
#    # MUST USE output$perCriteria[[x]] where x indicate the the objective
#    #                                 function values for variable x^th
#  
#    # MUST USE output$overalPerCriteria for the overall objective function value
#  
#    # R2 of the first variable
#    output$perCriteria[[1]] <- cor(obs[[1]], sim[[1]])**2
#  
#    # R2 of the second variable
#    output$perCriteria[[2]] <- cor(obs[[2]], sim[[2]])**2
#  
#    # Final objective function value
#    output$overalPerCriteria <- (1*output$perCriteria[[1]] + 2*output$perCriteria[[2]])/3
#  
#    # ----------------------------------------------------------------------------
#    # END OF CHANGE: Don't modify anything after this line
#    # ----------------------------------------------------------------------------
#    for (i in 1:length(output$perCriteria)){
#      output$perCriteria[[i]] <- data.frame(userObjFunc = output$perCriteria[[i]])
#    }
#  
#    return(output)
#  }
#  
#  
#  # Overwrite the userObjFunction with our userObjFunction
#  environment(userObjFunction) <- asNamespace('RSWAT')
#  assignInNamespace("userObjFunction", userObjFunction, ns = "RSWAT")
#  

