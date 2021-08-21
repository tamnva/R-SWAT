
# Graphical user interface for Objective function

objFunctionUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(

      #-------------------------------------------------------------------------
      # 1. Objective function
      #-------------------------------------------------------------------------  
      column(width = 10,
             HTML("<b>","1. Objective function","</b>"),
      ),
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   Please select the objective function: NSE = Nash–Sutcliffe 
                   efficiency, KGE = Kling-Gupta efficiency, R2 = R squared, 
                   RMSE = Root-mean-square error, PBIAS =  Percent bias = 
                   100 * (sumObserved - sumSimulated)/sumObserved. The objective 
                   function currently just the average selected (e.g., NSE) for all 
                   variables with equal weights. Future options will let the user to write
                   their own objective function here
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),  
      
      column(width = 10,
             excelOutput("tableObjFunction", 
                         width = "100%", 
                         height = "1px"),
      ),
      
      #-------------------------------------------------------------------------
      # 2. Get observed data files
      #------------------------------------------------------------------------- 
      
      column(width = 10,
             HTML("<b>","2. Get observed data files","</b>"),
      ),
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   Load observed data, single file for each variable. 
                   Files should be in ASCII format with two columns and header,
                   the first column is the date (format dd/mm/yyyy) and 
                   the second column is the data. If there is missing value, simply
                   put NA in that position. File name MUST be followed section 3.1,
                   check 'Display your selected outputs'
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
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
             checkboxInput('checkDisplayObsVar', 'Display obseved variable', 
                           value = FALSE, width = NULL),
      ),
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   Show all observed variable here, each observed values have 
                   2 columns, the first 2 columns are for 1st variable in the file
                   obs_var_1.txt, the next 2 columns are for the 2nd variable in the
                   file obs_var_2.txt and so on for the next 2 columns
                   <span>",
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
      conditionalPanel(
        condition = "input.checkDisplayObsVar == 1",
      
        column(width = 10,
               dataTableOutput('tableObsVarDisplay'),
               
        ),
      ),
      
      
      #-------------------------------------------------------------------------
      # 3. Calculate objective function
      #------------------------------------------------------------------------- 
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
             checkboxInput('checkDisplayObjFunction', 'Check here to display result', 
                           value = FALSE, width = NULL),
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

