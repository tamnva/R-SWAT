# This is an example of how to extract SWAT output data that are not available
# in the given options in RSWAT. RSWAT will use this function to get 
# SWAT outputs (from the respective core) after each model run and save it in 
# combination  with other outputs

# Your defined function MUST follow these rules
# ----------------------------------------------------------------------------
# Name of the function:  userReadSwatOutput
# Input of the function: This function take no input, everything is hard coded
#                        inside the function
# Return: a list() object
# ----------------------------------------------------------------------------

# NOTE: BY DEFAULT YOU ARE INSDIE THE TxtInOut FOLDER, so you can read any files there 
#       just using its name

# IMPORTANT: TEST YOUR FUNCTION BEFORE RUNNING R-SWAT

userReadSwatOutput <- function(){
  
  output <- list()
  # ----------------------------------------------------------------------------  
  # Delete the existing code below and add your code below this line
  # ----------------------------------------------------------------------------
  # Example: This function extracts data in the 7th column of the file  output.rch 
  #          and aggregate to monthly
  
  # Read all data from output.rch
  getOutputRsvData <- read.table("output.rch", header = FALSE, sep = "", skip = 9)
   
  # Only select column 7 (just as example)
  output[[1]] <- getOutputRsvData[, 7]  
   
  # only select reach number 2. In this example, we have 7 reaches in total
  reachNumber <- 2
  totalNumberOfReach <- 7
  output[[1]] <- output[[1]][seq(reachNumber, length(output[[1]]), totalNumberOfReach)]
   
  # Time frame of this data (please see the file.cio)
  date <- seq(from = as.Date("2000-01-01", "%Y-%m-%d"), 
               to = as.Date("2007-12-31", "%Y-%m-%d"),
               by = 1)
  year <- as.numeric(format(date, "%Y"))
  month <- as.numeric(format(date, "%m")) 
  
  # Create a frame for this data
  dataFrame <- data.frame(year = year,
                              month = month,
                              Q = output[[1]])
   
  # Aggregate to monthly
  dataFrameMonthly <- aggregate( .~ year + month, dataFrame, FUN = sum)
   
  # Only get the values of Q
  output[[1]] <- dataFrameMonthly$Q
  
  # ----------------------------------------------------------------------------  
  # End: Don't modify anything after this line
  # If you need to extract other simulate variables, please assign it to output[[2]]
  # ----------------------------------------------------------------------------
  
  return(output)
  
}