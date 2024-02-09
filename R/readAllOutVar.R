
#'
#' This function is used to read all output files in the working folder
#'
#' @description
#' When running RSWAT, output files will be saved in the working folder/Output/Core
#' depending number of cores/threads and number of output variables, there will
#' be many out_var_xx.txt generated in these folders. This function is used to read
#' these files.
#'
#'
#' @param workingFolder path to the working folder
#' @param numTimesteps a vector of integer numbers, number of output timestep for
#' each output variables, for example, the first output variables has 3000 output
#' time step, the second output variables has 4000 time step, then
#' numTimesteps <- c(3000,4000)
#'
#' @return a list of data frame for each output variable. Each data frame has n
#' rows (number of rows are number of time steps) and m columns (m is the number
#' of iterations)
#'
#' @examples
#'
#'\donttest{
#' workingFolder <- "C:/data/workingFolder"
#' # Assume that I have only 1 observed data with number of output time step = 3000
#' numTimesteps <- c(3000)
#'
#' #  if you have two output variables with different number of time steps
#' #  numTimesteps <- c(3000, 4000)
#'
#' readAllOutVar("C:/data/workingFolder")
#'}
#'
#' @export
#'
#'
readAllOutVar <- function(workingFolder, numTimesteps){

  # List of folders with Core_ where output_var_xx.txt files are stored
  coreFolders <- dir(file.path(workingFolder, "Output"), pattern = "Core_",
                    full.names = TRUE)

  # List of files in each coreFolders (all coreFolders have the same file list)
  outVarFiles <- list.files(coreFolders[1], full.names = TRUE)

  output <- list()
  for (ifolder in 1:length(coreFolders)){
    # Loop over each outVarfiles
    for (var in 1:length(outVarFiles)){
      # Read content of each file and store
      data <- read.table(outVarFiles[var], header = FALSE, sep="")

      # convert to matrix
      data <- matrix(data[,1], nrow = numTimesteps[var] + 1)

      # Remove the first rows
      data <- data[-c(1),]

      if ((var == 1) & (ifolder == 1)){
        output[[var]] <- list()
        output[[var]] <- data
      } else {
        output[[var]] <- cbind(output[[var]],data)
      }
    }
  }

  return(output)
}
