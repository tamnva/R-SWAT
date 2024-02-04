
#' Get the number of output variables
#'
#' @inheritParams runSWATpar
#'
#' @return number of output variables (nOutputVar) and boolen variables indicate
#' whether a specific output is extracted by the user defined extraction function
#' or not
#'
#' @examples
#' # Here is the example of output extraction data frame of SWAt
#' outputExtractionSWAT <- exampleData$dataOutputExtractionSWAT
#' outputExtractionSWATPlus <- exampleData$dataOutputExtractionSWATPlus
#'
#' # Get number of output variables
#' getNumberOutputVar(outputExtractionSWAT)
#' getNumberOutputVar(outputExtractionSWATPlus)
#'
#' @export
#'
getNumberOutputVar <- function(outputExtraction){
  nOutputVar <- 0
  userReadSwatOutput <- c()
  for (i in 1:nrow(outputExtraction)) {
    if (outputExtraction[i,1] == "watout.dat"){
      temp <- length(strsplit(as.character(outputExtraction[i,3]), ",")[[1]])
      nOutputVar <- nOutputVar + temp

      if (!is.na(temp)){
        if (is.numeric(temp)){
          if(temp > 0){
            userReadSwatOutput <- c(userReadSwatOutput, rep(FALSE, temp))
          }
        }
      }

    } else if(outputExtraction[i,1] == "output.rch" |
              outputExtraction[i,1] == "channel_sd_day.txt" |
              outputExtraction[i,1] == 'channel_sd_mon.txt' |
              outputExtraction[i,1] == 'channel_sd_yr.txt'  |
              outputExtraction[i,1] == 'channel_sdmorph_day.txt' |
              outputExtraction[i,1] == 'channel_sdmorph_mon.txt' |
              outputExtraction[i,1] == 'channel_sdmorph_yr.txt'  |
              outputExtraction[i,1] == 'lsunit_wb_day.txt' |
              outputExtraction[i,1] == 'lsunit_wb_mon.txt' |
              outputExtraction[i,1] == 'lsunit_wb_yr.txt'  |
              outputExtraction[i,1] == 'basin_wb_day.txt' |
              outputExtraction[i,1] == 'basin_wb_mon.txt' |
              outputExtraction[i,1] == 'basin_wb_yr.txt'  |
              outputExtraction[i,1] == "output.hru" |
              outputExtraction[i,1] == "output.sub" |
              outputExtraction[i,1] == "output.rsv"){
      if ((nchar(outputExtraction[i,3]) > 0) & (nchar(outputExtraction[i,4]) > 0)){
        temp <- sum(lengths(getRchNumber(outputExtraction[i,4])))
        nOutputVar <- nOutputVar + temp
        if (!is.na(temp)){
          if (is.numeric(temp)){
            if(temp > 0){
              userReadSwatOutput <- c(userReadSwatOutput, rep(FALSE, temp))
            }
          }
        }
      }
    } else if(outputExtraction[i,1] == "userReadSwatOutput") {
      temp <- as.numeric(outputExtraction[i,3])
      nOutputVar <- nOutputVar + temp
      if (!is.na(temp)){
        if (is.numeric(temp)){
          if(temp > 0){
            userReadSwatOutput <- c(userReadSwatOutput, rep(TRUE, temp))
          }
        }
      }
    } else {
      # TODO: add check if length column == length reach
      userReadSwatOutput <- NULL
      nOutputVar <- NULL
    }
  }

  output <- list()
  output$nOutputVar <- nOutputVar
  output$userReadSwatOutput <- userReadSwatOutput

  return(output)
}
