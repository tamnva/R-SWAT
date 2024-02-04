
# Introduction text

introductionUI <- function(id) {

  # shiny::NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.

  ns <- shiny::NS(id)

  tagList(

    fluidRow(
      # Input UI
      column(width = 10,
             HTML(readLines(file.path(system.file("R-SWAT", package = "RSWAT"),
                                      "HTML","introduction.html"),
                            warn=FALSE))
      ),

    )

  )}

