
# Graphical interface for Gerernal setting

generalSettingUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow( 
      #-------------------------------------------------------------------------
      # 1. Load previous result
      #------------------------------------------------------------------------- 
      column(width = 10,
             HTML("<b>",
                  "1. Load R-SWAT settings from the previous R-SWAT project (optional)",
                  "</b>"),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   1. Load R-SWAT settings from the previous R-SWAT project: 
                   If users have set up the R-SWAT project before, there is a file
                   RSWATproject.rds was created and saved in the working folder.
                   When this file is loaded,  all settings in 1. General 
                   Setting to 4.1. Evaluate output will be updated)
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
      column(width = 10,
             shinyFilesButton("getRSWATProjectFile", "Click here to select the RSWATproject.rds file" ,
                              title = "Please select 'RSWATproject.rds' file",
                              multiple = FALSE,
                              buttonType = "default",
                              style="background-color: #87CEFA; 
                              border-color: #2e6da4",
                              class = NULL),
             verbatimTextOutput("printRSWATProjectFile", placeholder = TRUE),
      ),
      #-------------------------------------------------------------------------
      # 2. Working folder
      #-------------------------------------------------------------------------
      column(width = 10,
             textInput("workingFolder",
                       "2. Working folder",
                       "C:/workingFolder",
                       width = "100%"
             ),
             textOutput("checkWorkingFolder"),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   All files created by this tool are saved in this folder
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),

      #-------------------------------------------------------------------------
      # 3. TxtInOut folder
      #-------------------------------------------------------------------------
      column(width = 10,
             textInput("TxtInOutFolder",
                       "3. TxtInOut folder",
                       "C:/Scenarios/Default/TxtInOut",
                       width = "100%"
             ),
             textOutput("checkTxtInOutFolder"),
             
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
             Link to the TxtInOut directory which contains all 
             original SWAT input files. These files will not be changed by R-SWAT
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
      column(width = 10,
             checkboxInput('checkTxtInOutFolder', 'Display HRU info from TxtInOut 
                           folder', value = FALSE, width = NULL),
      ),

      conditionalPanel(
        condition = "input.checkTxtInOutFolder == 1",
        column(width = 10,
               dataTableOutput('tableHRUinfo'),
        ),
      ),
      

      #-------------------------------------------------------------------------
      # 4. Select executable SWAT file 
      #------------------------------------------------------------------------- 
      column(width = 10,
             HTML("<b>",
                        "4. Select executable SWAT file",
                        "</b>"),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                  Select the executable SWAT file, for example, swat_32debug.exe
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),

      column(width = 10,
             shinyFilesButton("getSWATexe", "Click here to select" ,
                              title = "Please select the executable SWAT file:",
                              multiple = FALSE,
                              buttonType = "default",
                              style="background-color: #87CEFA; 
                              border-color: #2e6da4",
                              class = NULL),
             verbatimTextOutput("printSWATexe", placeholder = TRUE),
      ),

      #-------------------------------------------------------------------------
      # 5. Files with list of all SWAT parameters 
      #-------------------------------------------------------------------------
      div( style = "margin-top: 15em",  
           column(width = 10,
                  HTML("<b>",
                             "5. Files with list of all SWAT parameters",
                             "</b>"),
           ),
      ),

      column(width = 1,
             tippy("Help", tooltip = "<span style ='font-size:16px'>
                            Select the file 'swatParam.txt' with list of all SWAT parameters. 
                            If there are  parameters that you want to calibrate 
                            but are missing in this file, 
                            simply open this file manually and add to the file. 
                            The theoretical min and max are used to 
                            restrict the parameters that you are going to modify
                            will not out of this range. Please check again 
                            the theoretical min and max of your selected 
                            parameters.
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
      column(width = 10,
             shinyFilesButton("getSWATParamFile", "Click here to select" ,
                              title = "Please select the 'swatParam.txt' file:",
                              multiple = FALSE,
                              buttonType = "default", 
                              style="background-color: #87CEFA;
                              border-color: #2e6da4",
                              class = NULL),

             
             verbatimTextOutput("printSWATParamFile", placeholder = TRUE),
      ),
      
      column(width = 10,
             checkboxInput('checkSWATParamFile', 
                           "Display content of the 'swatParam.txt' file", 
                           value = FALSE, width = NULL),
      ),
      
      conditionalPanel(
        condition = "input.checkSWATParamFile == 1",
        column(width = 10,
               dataTableOutput('tableSWATParam'),
        ),
      ),
      
      #----------------------------------------------------add more content here
  
    )
    
  )}

