#'
#' Extract example data
#' @param exampleData is example data, which contains different data, identify
#' by dataName
#'
#' @param dataName name of the data, use one of the following keywords to
#' extract the data that needed.\cr
#'
#' "swatTxtInOut" - example TxtInOut folder (and its files) of SWAT \cr
#' "swatParam" - example parameter file (swatParam.txt) of SWAT \cr
#' "swatObservedData" - example observed streamflow data (obs_var_1.txt) of SWAT \cr
#' "swatPlusTxtInOut" - example TxtInOut folder (and its files) of SWAT+ \cr
#' "swatPlusParam" - example parameter file (cal_parms.cal) of SWAT+ \cr
#' "swatPlusObservedData" - example observed streamflow data (obs_var_1.txt) of SWAT+ \cr
#' "all" - extract all data
#'
#' @param toDir path to the directory that the example data will be extracted
#'
#' @return the files content will be extracted to the given path
#' @seealso \code{\link{exampleData}} for data description
#' @examples
#'
#' \donttest{
#' extracExampleData(exampleData, "swatParam", tempdir())
#' }
#'
#' @export
#'

extracExampleData <- function(exampleData, dataName, toDir){

  if(dataName =="swatTxtInOut" | dataName =="all"){
    dir.create(file.path(toDir, "swatTxtInOut"))
    for (i in 1:length(exampleData$swatTxtInOut)){
      writeLines(exampleData$swatTxtInOut[[i]],
                 file.path(file.path(toDir, "swatTxtInOut"),
                           exampleData$swatFiles[i])
                 )
    }

    # Write message
    message("Created TxtInOut for SWAT in ",
            file.path(toDir, "swatTxtInOut"))

  }


  if(dataName =="swatPlusTxtInOut" | dataName =="all"){
    dir.create(file.path(toDir, "swatPlusTxtInOut"))
    for (i in 1:length(exampleData$swatPlusTxtInOut)){
      writeLines(exampleData$swatPlusTxtInOut[[i]],
                 file.path(file.path(toDir, "swatPlusTxtInOut"),
                           exampleData$swatPlusFiles[i])
      )
    }

    # Write message
    message("Created TxtInOut for SWAT+ in ",
            file.path(toDir, "swatPlusTxtInOut"))

  }

  if(dataName =="swatParam" | dataName =="all"){
    writeLines(exampleData$swatParam, file.path(toDir, "swatParam.txt"))
    message("Created swatParam.txt file in  ", file.path(toDir, "swatParam.txt"))

  }

  if(dataName =="swatObservedData" | dataName =="all"){
    dir.create(file.path(toDir, "observedSWAT"))
    writeLines(exampleData$swatObservedData, file.path(toDir,
                                                       "observedSWAT",
                                                       "obs_var_1.txt"))
    message("Created obs_var_1.txt file in ", file.path(toDir,
                                                        "observedSWAT",
                                                        "obs_var_1.txt"))

  }

  if(dataName =="swatPlusObservedData" | dataName =="all"){
    dir.create(file.path(toDir, "observedSWATPlus"))
    writeLines(exampleData$swatPlusObservedData, file.path(toDir,
                                                           "observedSWATPlus",
                                                           "obs_var_1.txt"))
    message("Created obs_var_1.txt file in ", file.path(toDir,
                                                        "observedSWATPlus",
                                                        "obs_var_1.txt"))

  }

  if(dataName =="swatPlusParam" | dataName =="all"){
    writeLines(exampleData$swatPlusParam, file.path(toDir, "cal_parms.cal"))
    message("Created cal_parms.cal file in ", file.path(toDir, "cal_parms.cal"))
  }

}
