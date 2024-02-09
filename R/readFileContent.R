#'
#' Read content of multiple files
#'
#' @param fileDirectory a directory where these files are located
#' @param files a character vector of file names (only file names, without
#' full path) that will be read
#' @return a list store the content of each file
#'
#' @keywords internal

#' @examples
#'
#'\donttest{
#' readFileContent(fileDirectory, files)
#'}

readFileContent <- function(fileDirectory, files){

  files <- file.path(fileDirectory, files)
  fileContent <- list()

  for (i in 1:length(files)){
    fileContent[[i]] <- readLines(files[i], -1, warn = FALSE)
  }

  return(fileContent)
}
