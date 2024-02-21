
#' Read output data from R-SWAT and calculate objective functions for
#' calibration and validation period and for each variable
#'
#' @inheritParams runSWATpar
#' @param nOutputVar integer - number of output variables
#' @param userReadSwatOutput TODO
#' @param observedData list of observed data, each observed data is a data frame
#' @param index performance criteria (NSE, KGE, RMSE, R2, or aBIAS)
#'
#' @return dataframe of objective function values
#' @importFrom utils read.table
#' @examples
#'
#' \donttest{
#' # Please see RSWAT Vignettes
#' }
#'
#' @export

calObjFunction <- function(parameterValue,
                           ncores,
                           nOutputVar,
                           userReadSwatOutput,
                           observedData,
                           workingDirectory,
                           index){

  output <- list()

  count <- 0

  nSim <- as.integer(nrow(parameterValue)/ncores)
  nSim <- rep(nSim, ncores)
  nSim[ncores] <- nSim[ncores] + nrow(parameterValue) - sum(nSim)

  simData <- list()

  counter <- rep(0, nOutputVar)
  output$objValueCali <- rep(0, nrow(parameterValue))
  output$objValueValid <-  rep(0, nrow(parameterValue))
  output$error <- FALSE

  #Loop over number of cores
  for (i in 1:ncores){
    #loop over number of variables
    for (j in 1:nOutputVar){

      # Number of time step + 1 (header of each simulation)
      ntimestep <- nrow(observedData[[j]]) + 1

      if ((i == 1)) {
        simData[[j]] <- list()
        output$perCriteriaCali[[j]] <- list()
        output$perCriteriaValid[[j]] <- list()
        output$simData[[j]] <- list()
      }

      # Loop over number of simulation
      fileNameSimData <- file.path(workingDirectory, "Output", paste0("Core_", i),
                                   paste0("out_var_", j, ".txt"))

      tempSimData <- read.table(fileNameSimData, header = FALSE, sep = "")

      # check if length of observed and simulated data are the same or not
      if (nrow(tempSimData) %% ntimestep > 0){

        warnings(" Error: length of observed data # length of the extracted variable")
        message(fileNameSimData)
        message(" Please load the corrected observed data and rerun this step")
        output$objValueCali[] <- -999999
        output$objValueValid[] <- -999999
        output$error <- TRUE

      } else {
        for (k in 1:nSim[i]){
          sIndex <- (k-1)*ntimestep + 1
          eIndex <- k*ntimestep
          counter[j] <- counter[j] + 1

          simData[[j]][[counter[j]]] <- tempSimData[(sIndex + 1):eIndex, 1]
          output$simData[[j]][[counter[j]]] <- simData[[j]][[counter[j]]]

          # Calculate objective function value (NSE, KGE, RMSE, R2, aBIAS)
          if (index != 'userObjFunction'){

            # Objective function value for calibration period, flag C or c
            iloc <- which(observedData[[j]][,3] %in% c("C", "c"))

            # calculate
            output$perCriteriaCali[[j]][[counter[j]]] <-
              perCriteria(observedData[[j]][iloc,2], simData[[j]][[counter[j]]][iloc])

            # Objective function value for calibration period, flag V or v
            iloc <- which(observedData[[j]][,3] %in% c("V", "v"))

            # Objective function value for validation period
            output$perCriteriaValid[[j]][[counter[j]]] <-
              perCriteria(observedData[[j]][iloc,2], simData[[j]][[counter[j]]][iloc])

            # Check which performance criteria the user selected
            if((i == 1) & (j == 1)) {
              perIndex <- match(index, colnames(output$perCriteriaCali[[j]]
                                                [[counter[j]]]))
            }

            # Only get the user-selected performance criteria for
            # Calibration period
            output$objValueCali[counter[j]] <- output$objValueCali[counter[j]] +
              output$perCriteriaCali[[j]][[counter[j]]][perIndex]

            # Validation period
            output$objValueValid[counter[j]] <- output$objValueValid[counter[j]] +
              output$perCriteriaValid[[j]][[counter[j]]][perIndex]
          }
        }
      }
    }

    # In case of user defined objective function
    if (index == 'userObjFunction'){
      for (k in 1:nSim[i]){
        count <- count + 1

        # Select calibration data with flag "C" or "c"
        flag <- c("C", "c")
        temp  <- userObjFunction(observedToList(observedData, flag),
                                 simToList(simData, count, observedData, flag))
        output$objValueCali[count] <- temp$overalPerCriteria

        for (j in 1:nOutputVar){
          output$perCriteriaCali[[j]][[count]] <- temp$perCriteria[[j]]
        }


        # Select validation data with flag "V" or "v"
        flag <- c("V", "v")
        temp  <- userObjFunction(observedToList(observedData, flag),
                                 simToList(simData, count, observedData, flag))
        output$objValueValid[count] <- temp$overalPerCriteria
        for (j in 1:nOutputVar){
          output$perCriteriaValid[[j]][[count]] <- temp$perCriteria[[j]]
        }
      }
    }
  }

  if (index != 'userObjFunction'){
    output$objValueCali <- output$objValueCali/nOutputVar
    output$objValueValid <- output$objValueValid/nOutputVar
  }

  return(output)
}
