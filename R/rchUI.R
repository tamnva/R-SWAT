
# Module rchUI function

rchUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    # Input data -----------------------------------------------------
    fluidRow(
      column(width = 4,
             fileInput("rchFileCio",
                       "1. Select file.cio file", 
                       multiple = FALSE)
      ),
             
      column(width = 4,
             fileInput("outputRchFile",
                       "2. Select output.rch file", 
                       multiple = FALSE)
      ),
      
      column(width = 4,
             fileInput("rchObservedFile",
                       "3. Select observed data file", 
                       multiple = TRUE)
      ),
      
      column(width = 4,
             selectInput("rchSelectedReach", 
                         "Select Reach number(s) to plot", 
                         choices = NULL,
                         multiple = TRUE, 
                         width = "100%")
      ), 
      column(width = 4,
             selectInput("rchSelectedVariable", 
                         "Select variable(s) to plot", 
                         choices = NULL,
                         multiple = TRUE, 
                         width = "100%")
      ),


    ),

    fluidRow(
      column(width = 12,
             verbatimTextOutput("rchPlotTitle", placeholder = TRUE)),
      
      column(width = 12,
             plotOutput("rchPlotRch",
                        width = "100%", 
                        height = "650")
             ),
    )
    
    #----------------
  )}

