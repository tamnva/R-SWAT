
# Graphical interface for Gerernal setting

generalSettingUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow( 
      #-------------------------------------------------------------------------
      # 1. Working folder
      #-------------------------------------------------------------------------
      column(width = 10,
             textInput("workingFolder",
                       "1. Working folder",
                       "C:/data/workingFolder",
                       width = "100%"
             ),
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
      # 2. TxtInOut folder
      #-------------------------------------------------------------------------
      column(width = 10,
             textInput("TxtInOutFolder",
                       "2. TxtInOut folder",
                       "C:/data/Scenarios/Default/TxtInOut",
                       width = "100%"
             ),
             
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
             Link to the TxtInOut directory which contains all SWAT input files
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
      # 3. Select executable SWAT file 
      #------------------------------------------------------------------------- 
      column(width = 10,
             HTML("<b>",
                        "3. Select executable SWAT file",
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
      # 4. Files with list of all SWAT parameters 
      #-------------------------------------------------------------------------
      div( style = "margin-top: 15em",  
           column(width = 10,
                  HTML("<b>",
                             "4. Files with list of all SWAT parameters",
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
                              title = "Please select SWAT parameter file:",
                              multiple = FALSE,
                              buttonType = "default", 
                              style="background-color: #87CEFA;
                              border-color: #2e6da4",
                              class = NULL),
             verbatimTextOutput("printSWATParamFile", placeholder = TRUE),
      ),
      
      column(width = 10,
             checkboxInput('checkSWATParamFile', 
                           'Display content of the SWAT parameter file', 
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

