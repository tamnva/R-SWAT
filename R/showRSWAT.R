#'
#' Display the Graphical User Interface (GUI) of R-SWAT
#'
#' @description
#' Calling this functions will show the GUI of R-SWAT
#'
#' @return GUI of R-SWAT

#' @examples
#'
#'\donttest{
#' showRSWAT()
#' }
#'
#' @export
#'
showRSWAT <- function(){
  # Path to R-SWAT app
  appDir <- system.file("R-SWAT", package = "RSWAT")
  # Run app
  shiny::runApp(appDir, display.mode = "normal")
}








