
# Module hruUI function

introductionUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow( 
      # Input UI 
      column(width = 10,
             includeHTML("introduction.html")
      ),

    )
    
  )}

