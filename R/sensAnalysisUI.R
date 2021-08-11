
# Module hruUI function

sensAnalysisUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
    #---------------------------------------------------------------------------
    column(width = 10,
           HTML("<b>","1. Select sensitivity analyis approach","</b>"),
    ),
    column(width = 1,
           tippy("Help?", tooltip = "<span style='font-size:15px;'>
                   Curently 'Multivariable regression' (like SUFI 2) is the only 
                   option. Other options are under development. The results does
                   not change if you change to other options becuase the option
                   'Multivariable regression' is automatically selected in the code
                   <span>", 
                 allowHTML = TRUE, 
                 trigger = "click",
                 theme = "light"),
    ), 
    
    column(width = 10,
           selectInput("sensApproach", " ",
                       list(`Sensi_Cali_(LHS)` = list("Multivariable regression"),
                            `One at a time` = list("Morris"),
                            `Other` = list("Todo"))
          ),
    ),
    
    column(width = 10,
           HTML("<b>","2. Display parameter sensitivity ranking","</b>"),
    ),
    
    column(width = 1,
           tippy("Help?", tooltip = "<span style='font-size:15px;'>
                   Check this box to display parameter sensitivity ranking in 
                   of figure and table
                   <span>", 
                 allowHTML = TRUE, 
                 trigger = "click",
                 theme = "light"),
    ), 

    column(width = 10,
           checkboxInput('checkDisplayTableSensitivity', 'Diplay', value = FALSE, width = NULL),
    ),
    
    conditionalPanel(
      condition = "input.checkDisplayTableSensitivity == 1",
      column(width = 10,
             plotlyOutput("plotlySensitivity"),
             
      ),
      
      column(width = 10,
             excelOutput("tableSensitivity", 
                         width = "100%", 
                         height = "1px"),
      ),

    ),
    
    #--------------------------------------------------------------------------- 
    ),
      
    )}

