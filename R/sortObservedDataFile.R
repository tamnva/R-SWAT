#' Sort observed data files by it number before the file extention
#'
#' @param observedDataFile a character vector, containing the NAME different
#' observed data files, all observed data files must have the name starting with
#' 'obs_var_', followed by an integer number, indicating the variable number, and
#' the '.txt' extension. Please see example below.
#'
#' @return a character vector, containing the sorted names of observed data files

#' @examples
#'
#' observedDataFile <- c("obs_var_2.txt", "obs_var_3.txt", "obs_var_1.txt")
#'
#' sortObservedDataFile(observedDataFile)
#'
#' @export

sortObservedDataFile <- function(observedDataFile){

  if (length(observedDataFile) > 1){

    check <- c()

    for (i in 1:length(observedDataFile)){
      wantName <- paste("obs_var_", i, ".txt", sep = "")
      for (j in 1:length(observedDataFile)){
        temp <- substr(observedDataFile[j],
                       nchar(observedDataFile[j]) - nchar(wantName) + 1,
                       nchar(observedDataFile[j]))
        if(temp == wantName) check = c(check, j)
      }
    }

    out <- observedDataFile[check]

  } else {
    out <- observedDataFile
  }

  return(out)
}
