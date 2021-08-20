
# Module hruUI function

objFunctionUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(

      # -----------------------------------------------------Objective function 
      column(width = 10,
             HTML("<b>","1. Objective function","</b>"),
      ),
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   Input information in case of Sensi_Cali_(LHS) is the number of model evaluations. In case of
                   Parameter from external file is the path to the file
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),  
      
      column(width = 10,
             excelOutput("tableObjFunction", 
                         width = "100%", 
                         height = "1px"),
      ),
      
      #-------------------------------------------------Load observed data files
      
      column(width = 10,
             HTML("<b>","2. Get observed data files","</b>"),
      ),
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   Load observed data, single file for each variable. 
                   Files should be in ASCII format with two columns and header,
                   the first column is the date (format dd/mm/yyyy) and 
                   the second column is the data. Please the file name in section 3.1
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),
      
      column(width = 10,
             shinyFilesButton("getObservedDataFile", "Click here to select" ,
                              title = "Please select observed data file",
                              multiple = TRUE,
                              buttonType = "default", 
                              class = NULL),
             verbatimTextOutput("printObservedDataFile", placeholder = TRUE),
      ),
      
      column(width = 10,
             checkboxInput('checkDisplayObsVar', 'Display obseved variable', value = FALSE, width = NULL),
      ),
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   Show all observed variable here
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),
      
      conditionalPanel(
        condition = "input.checkDisplayObsVar == 1",
      
        column(width = 10,
               dataTableOutput('tableObsVarDisplay'),
               
        ),
      ),
      div( style = "margin-top: 15em",  
           column(width = 10,
                  HTML("<b>","3. Calculate objective function","</b>"),
           ),
      ),

      column(width = 10,
             actionButton("calObjFunction", "Click here to calculate objection"),
             verbatimTextOutput('printObjFunction'),
      ),
      
      column(width = 10,
             checkboxInput('checkDisplayObjFunction', 'Check here to display result', value = FALSE, width = NULL),
      ),
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   Todo
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),
      
      conditionalPanel(
        condition = "input.checkDisplayObjFunction == 1",
        verbatimTextOutput('printMessageObjectiveFunction'),
        column(width = 10,
               excelOutput("tableCalObjFunction", 
                           width = "100%", 
                           height = "1px"),
        ),
      ), 
      
    )
    
  )}

