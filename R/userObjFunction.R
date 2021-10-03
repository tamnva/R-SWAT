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

  # sim is a list of simulated variables
  # sim[[1]] is a vector of the first simulated variable
  # sim[[2]] is a vector of the second simulated variable, and so on...

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
  
  # Calulate your objective function here
  # DELETE THE FOLLOWING CODE AND REPLACE WITH YOUR OWN CODE
  # -------------------------------------------------------------------------
  output <- 0
  for (i in 1:length(obs)){
    output <- output + sqrt((obs - sim)**2/length(obs))
  }

  output <- output/length(obs)
  # -------------------------------------------------------------------------

  return(output)
  
}