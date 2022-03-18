
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
             selectInput("objFunction",
                         width = "40%",
                         label = "1. Select objective function", 
                         choices = c('NSE', 'KGE', 'R2', 'RMSE', 'aBIAS', 
                                     'userObjFunction'),
                         multiple = FALSE),
      ),

      conditionalPanel(
        
        condition = "input.objFunction == 'userObjFunction'",
        column(width = 10,
               selectInput("minOrmax",
                           width = "40%",
                           label = "Minimize or maximize the objective function?", 
                           choices = c('Minimize', 'Maximize'),
                           multiple = FALSE),
        ),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   Please select the objective function: NSE = Nash-Sutcliffe 
                   efficiency, KGE = Kling-Gupta efficiency, R2 = R squared, 
                   RMSE = Root-mean-square error, aPBIAS =  Absolute Percent bias = 
                   abs(100 * (sumObserved - sumSimulated))/sumObserved. The same weights
                   are applied for all variables. Users can define their own objective
                   function using the option 'userObjFunction' by modifying the file
                   ./R/userObjFunction.R
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),  
      
      #-------------------------------------------------------------------------
      # 2. Get observed data files
      #------------------------------------------------------------------------- 
      
      column(width = 10,
             HTML("<b>","2. Get observed data files","</b>"),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   Load observed data, single file for each variable. 
                   Files should be in ASCII format with two columns and header,
                   the first column is the date (format yyyy-mm-dd) and 
                   the second column is the data. Use NA for missing values. 
                   File name MUST be followed then name shown in 3.Run SWAT => 
                   Display corresponding observed file names
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
      column(width = 10,
             shinyFilesButton("getObservedDataFile", "Click here to select" ,
                              title = "Please select observed data file(s)",
                              multiple = TRUE,
                              buttonType = "default", 
                              class = NULL),
             verbatimTextOutput("printObservedDataFile"),
             textOutput("checkGetObservedDataFile"),
      ),
      
      column(width = 10,
             checkboxInput('checkDisplayObsVar', 'Display observed data', 
                           value = FALSE, width = NULL),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   Show all observed data here, each observed data have 
                   2 columns, the first 2 columns are data of the file
                   obs_var_1.txt, the next 2 columns are data of the 
                   file obs_var_2.txt and so on
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
             actionButton("calObjFunction", "Click here to calculate the objective function"),
             verbatimTextOutput("printCalObjFunction"),
      ),

      column(width = 10, 
             checkboxInput('checkDisplayObjFunctionPlot', 'Check here to display plot', 
                           value = FALSE, width = NULL),
      ),
      
      conditionalPanel(
        condition = "input.checkDisplayObjFunctionPlot == 1",
        column(width = 10, 
               plotlyOutput("plotObjFunction", height = "500px"),
        ),
        
        column(width = 10, 
               checkboxInput('checkDisplayObjFunction', 'Check here to display result', 
                             value = FALSE, width = NULL),
        ),
      ), 
      
      conditionalPanel(
        condition = "input.checkDisplayObjFunction == 1",
        column(width = 10,
               excelOutput("tableCalObjFunction", 
                           width = "100%", 
                           height = "1px"),
        ),
      ), 
      
    )
    
  )}

