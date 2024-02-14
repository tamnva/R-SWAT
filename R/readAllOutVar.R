
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
#' of iterations: column 1 = result from iteration 1 and column i = result from
#' iteration i)
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
#' output <- readAllOutVar("C:/data/workingFolder", numTimesteps)
#'}
#'
#' @export
#'
#'

readAllOutVar <- function(workingFolder, numTimesteps){

  # Find number of cores
  numberOfCores <- length(dir(file.path(workingFolder, "Output"),
                              pattern = "Core_"))

  # List of folder names starts with "Core_1" in the Workingfolder/Output
  coreFolders <- paste0("Core_", c(1:numberOfCores))

  # Number of output variables = number of files in  Workingfolder/Output/Core_1
  numOuputVariables <- length(list.files(file.path(workingFolder,
                                                   "Output",
                                                   coreFolders[1])
                                         )
                              )
  # List of output files starts in  Workingfolder/Output/Core_1
  outVarFiles <- paste0("out_var_",
                        c(1:numOuputVariables),
                        ".txt")

  # Now read all output files (out_var_xx.txt) in all Core_xx folders
  output <- list()
  for (ifolder in 1:length(coreFolders)){

    # Loop over each outVarfiles
    for (var in 1:length(outVarFiles)){

      # Read content of each file and store
      outFile <- file.path(workingFolder,
                           "Output",
                           coreFolders[ifolder],
                           outVarFiles[var])

      data <- read.table(outFile, header = FALSE, sep="")

      # convert to matrix
      data <- matrix(data[,1], nrow = numTimesteps[var] + 1)

      # Remove the first rows
      data <- data[-c(1),]

      if (ifolder == 1){
        output[[var]] <- list()
        output[[var]] <- data
      } else {
        output[[var]] <- cbind(output[[var]], data)
      }
    }
  }

  return(output)
}
