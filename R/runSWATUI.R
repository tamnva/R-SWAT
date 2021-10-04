
# Graphical user interface for Run SWAT

runSwatUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      #-------------------------------------------------------------------------
      # 1. Define model outputs for extraction
      #-------------------------------------------------------------------------           
      column(width = 10,
             HTML("<b>","1. Define model outputs for extraction","</b>"),
      ),
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   You can read from multiple 'watout.dat' file type with different 
                   names, add more rows if you do so. The column specifies which 
                   columns from that file you want to extract (e.g., column 4 and 
                   5 and 7) -> the input should be '4,5,7' with comma to seperate 
                   the column name. In case of reading from watout.dat file type,
                   leave the reach number empty. In case of reading from .rch, 
                   .sub, .hru, the reach number is also the subbasin or hru number.
                   For example, you want to read from column 4 reach number 1,2 and
                   from columnn 5 reach number 1,2,3, column 7 reach number 3. Input
                   to the Column should be '4,5,7', to the Reach should be 
                   '1,2 * 1,2,3 * 3' Note * must be use to seperate this expression. Currently
                   incase of reading from .hru, .sub, .rch the file name and file type 
                   must be identical. Please check the tick box below to ensure that 
                   all numbers you enter here appear in the display table
                   <span> ", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
      column(width = 10,
             excelOutput("tableOutputExtraction", 
                         width = "100%", 
                         height = "1px"),
      ),

      column(width = 5,
             checkboxInput('checkOutputExtractionDisplayOnly', 
                           'Display corresponding observed file names', 
                           value = FALSE, width = NULL),
      ),
      
      
      conditionalPanel(
        condition = "input.checkOutputExtractionDisplayOnly == 1",
        column(width = 10,
               dataTableOutput('tableOutputExtractionDisplayOnly'),
        ),
      ),
      
      #-------------------------------------------------------------------------
      # 2. Select date range for calibration
      #-------------------------------------------------------------------------  
      column(width = 10,
             dateRangeInput("dateRangeCali", "2. Select date range",
                            start = "2001-01-01",
                            end   = "2010-12-31"),
             
      ),
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   Select the time range for sensitivity analysis/calibration. 
                   The min and maximum dates is automatically detect by this 
                   program by reading the 'file.cio'. Feel free to select any 
                   dates that this tool allows you
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),

      #-------------------------------------------------------------------------
      # 3. Select number of parallel runs (cores)
      #-------------------------------------------------------------------------       
      column(width = 10,
             sliderInput("ncores", 
                         "3. Select number of parallel runs (cores)", 
                         value = 4, 
                         min = 1, 
                         max = detectCores(),
                         step = 1,
                         round = TRUE),
             ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:15px;'>
                   The maximum number of parallel runs is the number of 
                   logical processors in this computer. Please check how much CPU 
                   or Memory you need for each simulation. Then you can select the
                   optimal number of cores. Otherwise your computer will be overloaded 
                   if you use 100% CPU or Memory. You can check this manually by 
                   runing your SWAT (in the way that you have done) project and 
                   see changes in CPU and Memory (see Task Manager if you used Window)
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),

      #-------------------------------------------------------------------------
      # 4. Run SWAT
      #-------------------------------------------------------------------------        
      div( style = "margin-top: 5em",  
           column(width = 10,
                  HTML("<b>","4. Run SWAT","</b>"),
           ),
      ),

      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:15px;'>
                   When you click this button, all settings are saved to the file 
                   'SWATShinyObject.rds' in the working directory folder. The parameter
                   sets are generated and SWAT are run in parallel. Running can
                   can take time, R is busy while calling SWAT on the background
                   therefore, it might not response to any anything. Don't turn of R.
                   You can check the current simulation status in the file 
                   ./Output/CurrentSimulationReport.log You still go to step 4 and click 
                   the 'Open file CurrentSimulationReport.log'. When all simulations
                   are finished, you will see a table appear
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
      column(width = 10,
             actionButton("runSWAT", "Click here to run SWAT"),
             verbatimTextOutput('printRuning'),
      ),

      #-------------------------------------------------------------------------
      # 5. See simulation report
      #-------------------------------------------------------------------------       
      div( style = "margin-top: 15em",  
           column(width = 10,
                  HTML("<b>","5. See simulation report","</b>"),
           ),
      ),

      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:15px;'>
                   You can only open this files when all SWAT simulations are 
                   finished. If SWAT is running on background, you can manually 
                   go to the working folder .\\Output\\CurrentSimulationReport.log
                   and open it with any text editor (e.g., notepad)
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
      column(width = 5,
             checkboxInput('checkCurrentSimulation', 
                           'Open file CurrentSimulationReport.log', 
                           value = FALSE, width = NULL),
      ),
      
      column(width = 5,
             checkboxInput('checkDisplayParameterSet', 
                           'Display all parameter sets', 
                           value = FALSE, width = NULL),
      ),
      
      
      conditionalPanel(
        condition = "input.checkCurrentSimulation == 1",
        column(width = 10,
               uiOutput('tableCurrentSimulation'),
               
        ),
      ),
      
      conditionalPanel(
        condition = "input.checkDisplayParameterSet == 1",
        column(width = 10,
               excelOutput("tableDisplayParameterSet", 
                           width = "100%", 
                           height = "1px"),
        ),
      ),
      
      #-------------------------------------------------------------------------
      # 6. Load previous result
      #------------------------------------------------------------------------- 
      column(width = 10,
             HTML("<b>",
                  "6. Load previous results (optional - for loading previous results)",
                  "</b>"),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   6. Load previous results: This option can be used in the case 
                   that you have finised step 5, close your project and want to open
                   the project again without starting from the first step. It only 
                   works if you have finished step 5 (meaning that all 
                   simulations were finished. If the model is interupted during model  
                   run, this option cannot be used). In addition, if you changed your 
                   working directory, please go back to 'General Setting' and update your
                   working directory before proceding to the next step. All other
                   settings in the user interface will not be considered by this 
                   program unless you click it.
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
      column(width = 10,
             shinyFilesButton("getSWATShinyObject", "Click here to select SWATShinyObject.rds file from previous run" ,
                              title = "Please select 'SWATShinyObject.rds' file",
                              multiple = FALSE,
                              buttonType = "default",
                              style="background-color: #87CEFA; 
                              border-color: #2e6da4",
                              class = NULL),
             verbatimTextOutput("printSWATShinyObject", placeholder = TRUE),
      ),
      #-------------------------------------------------------------------------
      
    )
  )}

