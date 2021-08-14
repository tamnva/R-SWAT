
# Module hruUI function

generalSettingUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow( 
      # Input UI 
      column(width = 10,
             textInput("workingFolder",
                       "1. Working folder",
                       "C:/Users/nguyenta/Documents/DemoSWATshiny",
                       width = "100%"
             ),
      ),
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   1. Working folder: All files created by this tool are saved 
                   in this folder. IMPORTANT: Use <span> <span style=
                   'color: red;'> FORWARD SLASH '/' instead of BACKSLASH
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),
      column(width = 10,
             textInput("TxtInOutFolder",
                       "2. TxtInOut folder",
                       "C:/Users/nguyenta/Documents/DemoSWATshiny/TxtInOut",
                       width = "100%"
             ),
             
      ),
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
             2. TxtInOut folder: The folder which contains TxtInOut files created 
             by ArcSWAT or QSWAT, etc...
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),
      
      column(width = 10,
             checkboxInput('checkTxtInOutFolder', 'Display HRU info from TxtInOut 
                           folder', value = FALSE, width = NULL),
      ),

      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   If this is a TxtInOut folder, a table with a list of HRUs 
                   and their properties (landuse, soil, slope,..) will appear 
                   if you tick the checkbox
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),
      
      conditionalPanel(
        condition = "input.checkTxtInOutFolder == 1",
        column(width = 10,
               dataTableOutput('tableHRUinfo'),
        ),
      ),
      
      
      column(width = 10,
             HTML("<b>",
                        "3. Select executable SWAT file",
                        "</b>"),
      ),
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   3. Select executable SWAT file: Select the executable swat 
                   files, for example, swat_32debug.exe
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
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

      div( style = "margin-top: 15em",  
           column(width = 10,
                  HTML("<b>",
                             "4. Files with list of all SWAT parameters",
                             "</b>"),
           ),
      ),

      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                            4. Files with list of all SWAT parameters: 
                            The file name must be 'swatParam.txt'
                            ASCII file format that contains a description 
                            (parameter name, location, number format, 
                            theoretical min and max values) of all SWAT 
                            parameters. If any parameter that you want to calibrate
                            but missing in this file, simply open this file 
                            manually and add to the file. The theoretical min 
                            and max values are used to restrict the parameters
                            that you are going to modified for calibration or 
                            sensitivity analysis will not out of this range. I 
                            would suggest checking the theoretical min and max values of 
                            your selected parameters
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
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
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   The file name must be 'swatParam.txt'. If the format is 
                   correct, a table with a list of parameters and their 
                   properties (location, number format,min, max values,..) will 
                   appear when you tick this box.
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),
      
      conditionalPanel(
        condition = "input.checkSWATParamFile == 1",
        column(width = 10,
               dataTableOutput('tableSWATParam'),
        ),
      ),
  
    )
    
  )}

