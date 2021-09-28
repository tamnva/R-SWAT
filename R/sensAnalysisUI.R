
# Graphical user interface for Sensitivity analysis

sensAnalysisUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      #-------------------------------------------------------------------------
      # 1. Display the sensitivity analysis approach
      #------------------------------------------------------------------------- 
      column(width = 10,
             HTML("<b>","1. Display the sensitivity analysis approach","</b>"),
      ),
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:15px;'>
                   You cannot change this option, it is automatically link to 
                   '2.2. Pameter sampling' approach
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ), 
      
      column(width = 10,
             verbatimTextOutput("printSelectedParaSensiApproach", 
                                placeholder = TRUE),
      ),

    #---------------------------------------------------------------------------
    # 2. Display parameter sensitivity ranking
    #---------------------------------------------------------------------------     
    column(width = 10,
           HTML("<b>","2. Display parameter sensitivity ranking","</b>"),
    ),
    
    column(width = 1,
           tippy("Help?", tooltip = "<span style='font-size:15px;'>
                   Check this box to display parameter sensitivity ranking 
                   results in of form of figure and table
                   <span>", 
                 allowHTML = TRUE, 
                 trigger = "click",
                 theme = "translucent"),
    ), 

    column(width = 10,
           checkboxInput('checkDisplayTableSensitivity', 'Diplay parameter sensitivity plot', 
                         value = FALSE, width = NULL),
    ),
    
    conditionalPanel(
      condition = "input.checkDisplayTableSensitivity == 1",
      column(width = 10,
             actionButton("savePlotlySensitivity", "Click here to save plot"),
             plotlyOutput("plotlySensitivity"), 
      ),
    ),

    column(width = 10,
           checkboxInput('checkDisplayTableSensitivityTable', 'Diplay parameter sensitivity table', 
                         value = FALSE, width = NULL),
    ),
    
    conditionalPanel(
      condition = "input.checkDisplayTableSensitivityTable == 1",
      column(width = 10, 
             excelOutput("tableSensitivity", 
                         width = "100%", 
                         height = "1px"),
      ),
    ),    
    #--------------------------------------------------------------------------- 
    ),
    )}

