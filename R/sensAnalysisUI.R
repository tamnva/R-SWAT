
# Graphical user interface for Sensitivity analysis

sensAnalysisUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
    #---------------------------------------------------------------------------
    # 1. Display parameter sensitivity ranking
    #---------------------------------------------------------------------------     
    column(width = 10,
           HTML("<b>","1. Display parameter sensitivity analysis","</b>"),
    ),

    column(width = 10,
           actionButton("calSensitivity", "Click here to do sensitivity analysis"),
    ),
    

    column(width = 10,
           dataTableOutput('tableSensitivity'),
           verbatimTextOutput("displaySensitivityReport")
    ),

    
    #--------------------------------------------------------------------------- 
    ),
    )}

