# This is an example of how to extract SWAT output data that are not available
# in the given options in RSWAT. RSWAT will use this function to get 
# SWAT outputs (from the respective core) after each model run and save it in 
# combination  with other outputs
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
shinyCatch(
  userReadSwatOutput <- function(){
    
    output <- list()
    # ----------------------------------------------------------------------------  
    # Delete the existing code below and add your code below this line
    # ----------------------------------------------------------------------------
    # Example: This function extracts data in the 7th column of the file  output.rch 
    #          and aggregate to monthly
    
    # --------------------------------------------------------------------------
    # PLEASE ADJUST THE STARTING AND ENDING DATE CORRESPONDING TO YOUR DATA
    date <- seq(from = as.Date("2010-01-01", "%Y-%m-%d"), 
                to = as.Date("2020-12-31", "%Y-%m-%d"),
                by = 1)
    year <- as.numeric(format(date, "%Y"))
    month <- as.numeric(format(date, "%m")) 
    
    # PLEASE SELECT THE REACH NUMBERS YOUR WANT TO EXTRACT (in this eg. are 4 reaches)
    reachNumber <- c(17, 42, 48, 61)
    
    # --------------------------------------------------------------------------
    
    # Read all data from output.rch
    getOutputRsvData <- read.table("output.rch", header = FALSE, sep = "", skip = 9)
    
    # Only select column 7 (this is the outflow column)
    flowOut <- getOutputRsvData[,7]  
  
    # Total number of reach
    totalNumberOfReach <- max(getOutputRsvData[,2])
    
    # Get daily outflow of each reach and aggregate to monthly flow (unit m3/month)
    for (i in 1:length(reachNumber)){
      
      # Get daily outflow of reach reachNumber[i]
      output[[i]] <- flowOut[seq(reachNumber[i], length(flowOut), totalNumberOfReach)]    
      
      # Put daily outflow to a data frame of 3 columns (month, year, and daily Q)
      dataFrame <- data.frame(month = month,
                              year = year,
                              Q = output[[i]])
      
      # Aggregate to monthly
      dataFrameMonthly <- aggregate(.~ month + year, dataFrame, FUN = sum)
      
      # Only get the values of Q
      output[[i]] <- dataFrameMonthly$Q
      
    }
    
    # ----------------------------------------------------------------------------  
    # End: Don't modify anything after this line
    # If you need to extract other simulate variables, please assign it to output[[2]]
    # ----------------------------------------------------------------------------
    
    return(output)
    
  }, 
  blocking_level = "error"
)
