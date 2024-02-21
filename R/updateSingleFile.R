#' Update multiple TxtInOut files with new parameter values
#'
#' @description
#' Rewrite the TxtInOut with new parameter values
#'
#' @inheritParams updateMultiFile
#' @param file name of the file which contains the updated parameter
#' @param fileContent content of the file which contains the updated parameter
#' @param atLine line number of the updated parameter
#' @param atPosition column location of the updated parameter
#' @param changeMethod method of change
#' @param applyValue the value which the original parameter should be modify
#' @param numberFormat format of the updated parameter
#' @param absoluteMin minimum value which the updated parameter should be greater
#' @param absoluteMax maximum value which the updated parameter should be exceed
#'
#' @return no return, results will be saved in todir directory
#'
#' @examples
#'
#'\donttest{
#' updateSingleFile(toDir,
#'                  file,
#'                  fileContent,
#'                  atLine,
#'                  atPosition,
#'                  changeMethod,
#'                  applyValue,
#'                  numberFormat,
#'                  absoluteMin,
#'                  absoluteMax)
#'}
#'
#' @export
#'
#'
updateSingleFile <- function(toDir,
                             file,
                             fileContent,
                             atLine,
                             atPosition,
                             changeMethod,
                             applyValue,
                             numberFormat,
                             absoluteMin,
                             absoluteMax){

  newfileContent <- fileContent

  for (i in 1:length(atLine)){

    # check if this is soil file (then change all soil layers)
    if (substr(file, nchar(file) - 2, nchar(file)) == 'sol'){
      if (atLine[i] >= 8){

        nchars <- atPosition[i,2] - atPosition[i,1]
        nlayer <- length(strsplit(trimws(substr(newfileContent[atLine[i]],
                                                atPosition[i,1],
                                                nchar(newfileContent[atLine[i]]))),
                                  split = "\\s+")[[1]])

        for (j in 1:nlayer){

          if (j > 1){
            atPosition[i,1] <- atPosition[i,2] + 1
            atPosition[i,2] <- atPosition[i,1] + nchars
          }
          parameterValue <- as.double(substr(newfileContent[atLine[i]],
                                             atPosition[i,1],
                                             atPosition[i,2]))

          if (changeMethod[i] == "relative"){
            newParameterValue <- (1.0 + applyValue[i]) * parameterValue
          } else if (changeMethod[i] == "replace") {
            newParameterValue <- applyValue[i]
          } else {
            newParameterValue <- applyValue[i] + parameterValue
          }

          if (newParameterValue < absoluteMin[i]) newParameterValue <- absoluteMin[i]
          if (newParameterValue > absoluteMax[i]) newParameterValue <- absoluteMax[i]

          toText <- sprintf(numberFormat[i], newParameterValue)

          substr(newfileContent[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText
        }

      } else {
        parameterValue <- as.double(substr(newfileContent[atLine[i]],
                                           atPosition[i,1],
                                           atPosition[i,2]))

        if (changeMethod[i] == "relative"){
          newParameterValue <- (1.0 + applyValue[i]) * parameterValue
        } else if (changeMethod[i] == "replace") {
          newParameterValue <- applyValue[i]
        } else {
          newParameterValue <- applyValue[i] + parameterValue
        }

        if (newParameterValue < absoluteMin[i]) newParameterValue <- absoluteMin[i]
        if (newParameterValue > absoluteMax[i]) newParameterValue <- absoluteMax[i]

        toText <- sprintf(numberFormat[i], newParameterValue)

        substr(newfileContent[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText

      }

    } else {
      parameterValue <- as.double(substr(newfileContent[atLine[i]],
                                         atPosition[i,1],
                                         atPosition[i,2]))

      if (changeMethod[i] == "relative"){
        newParameterValue <- (1.0 + applyValue[i]) * parameterValue
      } else if (changeMethod[i] == "replace") {
        newParameterValue <- applyValue[i]
      } else {
        newParameterValue <- applyValue[i] + parameterValue
      }

      if (newParameterValue < absoluteMin[i]) newParameterValue <- absoluteMin[i]
      if (newParameterValue > absoluteMax[i]) newParameterValue <- absoluteMax[i]

      toText <- sprintf(numberFormat[i], newParameterValue)

      substr(newfileContent[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText
    }

  }

  writeLines(newfileContent, file.path(toDir, file))
}
