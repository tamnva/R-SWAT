
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
                   ./R/userObjFunction.R. See section 5 of R-SWAT wiki page
                   https://github.com/tamnva/R-SWAT/wiki/R-SWAT-User-Manual
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),  
      
      #-------------------------------------------------------------------------
      # 2. Get observed data files
      #------------------------------------------------------------------------- 
      
      column(width = 10,
             HTML("<b>","2. Get observed data files (IMPORTANT CHANGES: Please read 'Help')","</b>"),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   Load observed data, single file for each variable. 
                   Files should be in ASCII format with 4 columns and header,
                   <br />
                   IMPORTANT: From version 2.0.0, the 1st column is the
                   date (format yyyy-mm-dd), 2nd column is the time (format hh:mm),
                   3rd column is the data, and 4th column is the Flag for  
                   calibration please write C and validation write please write V.
                   If there are missing data, please write NA, if data are not used for
                   calibration and validation, also write NA in the 4th column. 
                   See ./data/obs_var_1.txt as an example.
                   <br />
                   File name MUST be followed then name shown in 3.Run SWAT => 
                   Display corresponding observed file names
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),

      column(width = 10,
             conditionalPanel(
               condition = sprintf("'%s' != 'windows'", .Platform$OS.type),
               shinyFilesButton("getObservedDataFile", "Click here to select" ,
                                title = "Please select observed data file(s)",
                                multiple = TRUE,
                                buttonType = "default",
                                style="background-color: #87CEFA; border-color: #2e6da4",
                                class = NULL),
             ),             
      ),

      column(width = 10,
             conditionalPanel(
               condition = sprintf("'%s' == 'windows'", .Platform$OS.type),
               actionButton("getObservedDataFileWindow", 
                            "Please select observed data file(s)",
                            buttonType = "default",
                            style="background-color: #87CEFA; border-color: #2e6da4",
                            class = NULL),
             ),             
      ),
      
      column(width = 10,
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
             actionButton("calObjFunction", 
                          "Click here to calculate objective function",
                          style="background-color: #87CEFA; border-color: #2e6da4"),
             verbatimTextOutput("printCalObjFunction"),
      ),

      column(width = 10, 
             checkboxInput('checkDisplayObjFunctionPlot', 'Display plot of objective function values', 
                           value = FALSE, width = NULL),
      ),
      
      conditionalPanel(
        condition = "input.checkDisplayObjFunctionPlot == 1",
        column(width = 10, 
               plotlyOutput("plotObjFunction", height = "500px"),
        ),
        
      ), 
      
      column(width = 10, 
             checkboxInput('checkDisplayObjFunction', 'Display table of objective function values', 
                           value = FALSE, width = NULL),
      ),
      
      conditionalPanel(
        condition = "input.checkDisplayObjFunction == 1",
        column(width = 10,
               excelOutput("tableCalObjFunction", 
                           width = "100%", 
                           height = "1px"),
        ),
      ), 

      column(width = 10, 
             checkboxInput('ObjEachVar', 'Display table of obj. func. each variable', 
                           value = FALSE, width = NULL),
      ),
      
      conditionalPanel(
        condition = "input.ObjEachVar == 1",
        column(width = 10,
               excelOutput("tableObjEachVar", 
                           width = "100%", 
                           height = "1px"),
        ),
      ),       
    )
    
  )}

