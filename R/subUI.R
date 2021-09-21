
# Module rchUI function

subUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    # Input data -----------------------------------------------------
    fluidRow(
      column(width = 3,
             fileInput("subFileCio",
                       "1. Select file.cio file", 
                       multiple = FALSE)
      ),
        
      column(width = 3,
             fileInput("outputSubFile",
                       "2. Select output.sub file", 
                       multiple = FALSE)
      ),
      
      column(width = 3,
             selectInput("subSelectedSub", 
                         "3. Select subbasin number to plot", 
                         choices = NULL,
                         multiple = FALSE, 
                         width = "100%")
      ), 
      column(width = 3,
             selectInput("subSelectedVariable", 
                         "4. Select variable to plot", 
                         choices = NULL,
                         multiple = FALSE, 
                         width = "100%")
      ),
    ),
    
    fluidRow(
      column(width = 3,
             selectInput("subTempAgg", 
                         "5. Temporal aggregation", 
                         choices = c("Daily", "Monthly", "Yearly"),
                         multiple = FALSE, 
                         width = "100%")
      ),
    ),

    fluidRow(
      column(width = 12,
             plotlyOutput("subPlotSub",
                          width = "100%",
                          height = "400px"),
      )
    )
    
    #----------------
  )}

