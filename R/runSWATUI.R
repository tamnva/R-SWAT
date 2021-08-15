
# Module runSWATUI function

runSwatUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      # -----------------------------------------------------------Model outputs            
      column(width = 10,
             HTML("<b>","1. Define model outputs for extraction","</b>"),
      ),
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   Current option is reading from file type 'watout.dat'. You 
                   can read from multiple 'watout.dat' file type, just add as many 
                   rows as you do so. In this case, input the column number
                   as many as you want, seperated by comma (for example 4,5), 
                   leave the reach number empty as reading watout.dat files does
                   not need this.Reading from output.rch is under developement 
                   (don't try this). <span> ", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),
      
      column(width = 10,
             excelOutput("tableOutputExtraction", 
                         width = "100%", 
                         height = "1px"),
      ),

      column(width = 5,
             checkboxInput('checkOutputExtractionDisplayOnly', 
                           'Display your selected outputs', 
                           value = FALSE, width = NULL),
      ),
      
      
      conditionalPanel(
        condition = "input.checkOutputExtractionDisplayOnly == 1",
        column(width = 10,
               dataTableOutput('tableOutputExtractionDisplayOnly'),
        ),
      ),
      
      # -------------------------------------------------------Evaluation period
      column(width = 10,
             dateRangeInput("dateRangeCali", "3. Select date range",
                            start = "2001-01-01",
                            end   = "2010-12-31"),
             
      ),
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   Select the time range for sensitivity analysis/calibration. 
                   The min and maximum dates is automatically detect by this 
                   program by reading the 'file.cio'. Feel free to select any 
                   dates that this tool allows you
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),
      
      column(width = 10,
             sliderInput("ncores", 
                         "2. Select number of parallel runs (cores)", 
                         value = 4, 
                         min = 1, 
                         max = detectCores(),
                         step = 1,
                         round = TRUE),
             ),
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:15px;'>
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
                   theme = "light"),
      ),
      
      div( style = "margin-top: 5em",  
           column(width = 10,
                  HTML("<b>","3. Run SWAT","</b>"),
           ),
      ),

      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:15px;'>
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
                   theme = "light"),
      ),
      
      column(width = 10,
             actionButton("runSWAT", "Click here to run SWAT"),
             verbatimTextOutput('printRuning'),
      ),
      
      div( style = "margin-top: 15em",  
           column(width = 10,
                  HTML("<b>","4. See simulation report","</b>"),
           ),
      ),

      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:15px;'>
                   You can only open this files when all SWAT simulations are 
                   finished. If SWAT is running on background, you can manually 
                   go to the working folder .\\Output\\CurrentSimulationReport.log
                   and open it with any text editor (e.g., notepad)
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "light"),
      ),
      #-------------------------------------------------------------------------
      # Display report of the simulation when all simulations are finished
      #-------------------------------------------------------------------------
      
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
               dataTableOutput('tableCurrentSimulation'),
               
        ),
      ),

      #-------------------------------------------------------------------------
      # Display parameter sets
      #-------------------------------------------------------------------------
      
      conditionalPanel(
        condition = "input.checkDisplayParameterSet == 1",
        column(width = 10,
               excelOutput("tableDisplayParameterSet", 
                           width = "100%", 
                           height = "1px"),
        ),
      ),      
      
    )
    
    #----------------
  )}

