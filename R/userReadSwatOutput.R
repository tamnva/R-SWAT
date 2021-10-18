# This is an example of how to extract SWAT output data that are not available
# in the given options in RSWAT. RSWAT will use this function to get 
# SWAT outputs (from the respective core) after each model run and save it in 
# combination  with other outputs

# Your defined function MUST follow these rules
# ----------------------------------------------------------------------------
# Name of the function:  userReadSwatOutput
# Input of the function: This function take no input, eveything is hard coded
#                        inside the function
# Return: a list() object
# ----------------------------------------------------------------------------

# NOTE: BY DEFAULT YOU ARE INSDIE THE TxtInOut FOLDER, so you can read any files there 
#       just using its name unless your change your working directory by setting setwd("??")

# IMPORTANT: TEST YOUR FUNCTION BEFORE RUNNING Rshiny

userReadSwatOutput <- function(){
  
  output <- list()
  # ----------------------------------------------------------------------------  
  # Delete the exising code below and add your code below this line
  # ----------------------------------------------------------------------------
  # Example: This function will return the first 20 values in the columns 4 and
  #          the first 30 values in column 5 in file watout.dat after each model run 
  # 
  # NOTE: The obs_var_xx.txt should have a length of 20 and 30, repectively
  #       SWAT shiny will evaluate these outputs with data from the respective 
  #       obs_var_xx.txt files
  
   getOutputRsvData <- read.table("watout.dat", header = FALSE, sep = "", skip = 9)
   output[[1]] <- getOutputRsvData[1:20, 4]  # get 20 first values in column 4th
   output[[2]] <- getOutputRsvData[1:30, 5]  # get 30 first values in column 5th
  
  # ----------------------------------------------------------------------------  
  # End: Don't modify anything after this line
  # ----------------------------------------------------------------------------
  
  return(output)
  
}