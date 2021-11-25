# In this file you can define your own objective function
# Your defined function MUST follow these rules
# ----------------------------------------------------------------------------
# Name of the function:  userObjFunction
# Input of the function: simulated and observed values of all variables
# Return: a scalar object (single value)
# ----------------------------------------------------------------------------

# IMPORTANT: TEST YOUR FUNCTION BEFORE RUNNING RSWAT

userObjFunction <- function(obs, sim){
  
  # obs is a list of observed variables
  # obs[[1]] is a vector of the first observed variable
  # obs[[2]] is a vector of the second observed variable, and so on...
  # obs[[...]]

  # sim is a list of simulated variables
  # sim[[1]] is a vector of the first simulated variable
  # sim[[2]] is a vector of the second simulated variable, and so on...
  # sim[[...]]
  
  # The number of variables is defined in step 4. Run SWAT -> 1. Output extraction

  # Remove missing values from the observed and the corresponding values in simulated data

  for (i in 1:length(obs)){

    # Find index of missing values in observe 
    missingValue <- which(is.na(obs[[i]])) 
 
    # Remove missing values
    if (length(missingValue) > 0){
      obs[[i]] <- obs[[i]][-missingValue]
      sim[[i]] <- sim[[i]][-missingValue] 
    }    
  }
  
  # Calculate your user defined objective function here: START HERE
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # In this example, lets say you extracted two outputs and want to calculate 
  #                  R square and put more weight to the second variable
  #
  #  output <- (1 * R2_var_1 + 2 * R2_var_2)/3
  
  # R2 of the first variable
  R2_var_1 <- cor(obs[[1]], sim[[1]])**2
  
  # R2 of the second variable
  R2_var_2 <- cor(obs[[2]], sim[[2]])**2
  
  # Final objective function value (with different weights), must be with the name "output"
  output <- (1 * R2_var_1 + 2 * R2_var_2)/3
  
  # Calculate your user defined objective function here: END HERE
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------

  return(output)
  
}