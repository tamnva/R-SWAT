
# Module rchUI function

rchUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    # Input data -----------------------------------------------------
    fluidRow(
      column(width = 3,
             fileInput("rchFileCio",
                       "1. Select file.cio file", 
                       multiple = FALSE)
      ),
      
      column(width = 3,
             fileInput("rchObservedFile",
                       "2. Select observed data file", 
                       multiple = TRUE)
      ),
      
      column(width = 3,
             selectInput("rchObsVariable", 
                         "3. Select observed variable to plot", 
                         choices = NULL,
                         multiple = FALSE, 
                         width = "100%")
      ), 
    ),
    
    fluidRow(
        
      column(width = 3,
             fileInput("outputRchFile",
                       "4. Select output.rch file", 
                       multiple = FALSE)
      ),
      
      column(width = 3,
             selectInput("rchSelectedReach", 
                         "5. Select Reach number to plot", 
                         choices = NULL,
                         multiple = FALSE, 
                         width = "100%")
      ), 
      column(width = 3,
             selectInput("rchSelectedVariable", 
                         "6. Select variable to plot", 
                         choices = NULL,
                         multiple = FALSE, 
                         width = "100%")
      ),
      
      column(width = 3,
             selectInput("rchTempAgg", 
                         "7. Temporal aggregation", 
                         choices = c("Daily", "Monthly", "Yearly"),
                         multiple = FALSE, 
                         width = "100%")
      ),
    ),

    fluidRow(
      column(width = 12,
             plotlyOutput("rchPlotRch",
                          width = "100%",
                          height = "400px"),
      )
    )
    
    #----------------
  )}

