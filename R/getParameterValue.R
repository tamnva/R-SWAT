#' Get parameter value
#' @param parameter parameter flag
#' @param parameterValue parameter value
#' @return parameter value
#' @keywords internal
#'
getParameterValue <- function(parameter, parameterValue){
  for (i in 1:length(parameter)){
    for (j in 1:length(parameter[[i]])){
      parameter[[i]][j] <- parameterValue[parameter[[i]][j]]
    }
  }
  return(parameter)
}
