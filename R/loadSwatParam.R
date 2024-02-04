
#' Read swatParam.txt file of SWAT or cal_parms.cal of SWAT+
#'
#' @param swatParamFile swatParam.txt or cal_parms.cal file
#'
#' @return Dataframe of swatParam.txt or cal_parms.cal

#' @examples
#'
#'\donttest{
#' # Create SWAT parameter file
#' extracExampleData(exampleData,"swatParam", tempdir())
#'
#' # Read SWAT prameter file
#' loadSwatParam(file.path(tempdir(), "swatParam.txt"))
#'
#' # Create SWAT+ parameter file
#' extracExampleData(exampleData,"swatPlusParam", tempdir())
#'
#' # Read SWAT+ prameter file
#' loadSwatParam(file.path(tempdir(), "cal_parms.cal"))
#'}
#'
#' @export
#'

loadSwatParam <- function(swatParamFile){

  # check SWAT or SWAT+ parameter file
  fileName <- substr(swatParamFile, nchar(swatParamFile) - 12, nchar(swatParamFile))

  # If this is SWAT parameter file
  if (fileName == "swatParam.txt"){

    check <- c()
    swat_param <- readLines(swatParamFile, warn = FALSE)

    for (i in 1:length(swat_param)){
      checkCommentBlankLine <- substr(trimws(swat_param[i]), 1, 1)
      if ((checkCommentBlankLine == "!") | (checkCommentBlankLine == "")) {
        check <- c(check, i)
      }
    }

    swat_param <- swat_param[-check]

    param <- read.table(textConnection(swat_param))
    colnames(param) <- c("parameter", "lineNumber", "startPosition",
                         "endPosition","precision", "absoluteMin", "absoluteMax")

    # If this is a SWAT+ paramter file
  } else {

    param <- read.table(swatParamFile, skip = 2, header = TRUE, sep = "")
  }

  return(param)
}
