
# Graphical interface for Gerernal setting

generalSettingUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow( 
      
      #-------------------------------------------------------------------------
      # 1. SWAT or SWAT+ project
      #-------------------------------------------------------------------------
      column(width = 10,
             selectInput("SWATorSWATplus",
                         width = "50%",
                         label = "1. SWAT or SWAT+ project", 
                         choices = c('SWAT', 'SWAT+'),
                         multiple = FALSE),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   Please select which project are you going to use: SWAT or SWAT+
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
      #-------------------------------------------------------------------------
      # 2. Working folder
      #-------------------------------------------------------------------------
      column(width = 10,
             textInput("workingFolder",
                       "2. Working folder",
                       "C:/RSWAT_demo/workingFolder",
                       width = "100%"
             ),
             span(textOutput("checkWorkingFolder"), style="color:red"),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   All files created by R-SWAT will be saved in this folder
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
                       "C:/RSWAT_demo/R-SWAT-master/data/TxtInOut",
                       width = "100%"
             ),
             span(textOutput("checkTxtInOutFolder"), style="color:red"),
             
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
             Link to the TxtInOut directory which contains all 
             original SWAT or SWAT+ input files. These files will 
             not be changed by R-SWAT
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
      # 4. Select executable SWAT or SWAT+ file 
      #-------------------------------------------------------------------------
      # Change the display text according to SWAT or SWAT+
      conditionalPanel(
        condition = "input.SWATorSWATplus == 'SWAT'",
        column(width = 10,
               HTML("<b>",
                    "4. Select executable SWAT file",
                    "</b>"),
        ),
      ),
      
      # If SWAT+ project (step 1) is selected
      conditionalPanel(
        condition = "input.SWATorSWATplus == 'SWAT+'",
        column(width = 10,
               HTML("<b>",
                    "4. Select executable SWAT+ file",
                    "</b>"),
        ),
      ),
      
      # Help text for selecting executable SWAT or SWAT+ file
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                  Select the executable SWAT or SWAT+ file, for example, swat_32debug.exe
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),

      # Pop up window for selecting the executable SWAT or SWAT+ 
      column(width = 10,
             shinyFilesButton("getSWATexe", "Click here to select" ,
                              title = "Please select the executable file:",
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
      # Adjust the title of this section depending on SWAT or SWAT+ project
      div( style = "margin-top: 15em", 
           
           # If this is a SWAT project
           conditionalPanel(
             condition = "input.SWATorSWATplus == 'SWAT'",
             column(width = 10,
                    HTML("<b>",
                         "5. File with list of all SWAT parameters",
                         "</b>"),
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
           ),
           
           
           # If this is a SWAT+ project
           conditionalPanel(
             condition = "input.SWATorSWATplus == 'SWAT+'",
             column(width = 10,
                    HTML("<b>",
                         "5. File with list of all SWAT+ parameters",
                         "</b>"),
             ),
             column(width = 1,
                    tippy("Help", tooltip = "<span style ='font-size:16px'>
                            This is a SWAT+ project, please select the file 
                            'cal_parms.cal' in your SWAT+ TxtInOur folder.
                   <span>", 
                          allowHTML = TRUE, 
                          trigger = "click",
                          theme = "translucent"),
             ),
           ),
      ),

      # Select the paramter file
      column(width = 10,
             shinyFilesButton("getSWATParamFile", "Click here to select" ,
                              title = "Please select the 'swatParam.txt' for SWAT project or 
                              'cal_parms.cal' for SWAT+ project file ",
                              multiple = FALSE,
                              buttonType = "default", 
                              style="background-color: #87CEFA;
                              border-color: #2e6da4",
                              class = NULL),
      ),
      
      # Display file name
      column(width = 10,
             verbatimTextOutput("printSWATParamFile", placeholder = TRUE),
      ),
      
      # Check box to display content of the parameter file
      column(width = 10,
             checkboxInput('checkSWATParamFile', 
                           "Display content of the parameter file", 
                           value = FALSE, width = NULL),
      ),
      
      # If the box was checked, then display content of the parameter file
      conditionalPanel(
        condition = "input.checkSWATParamFile == 1",
        column(width = 10,
               dataTableOutput('tableSWATParam'),
        ),
      ),
      
      #----------------------------------------------------add more content here
  
    )
    
  )}

