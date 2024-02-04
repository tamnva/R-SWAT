
# Graphical user interface for Run SWAT

runSwatUI <- function(id) {

  # shiny::NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- shiny::NS(id)
  
  tagList(
    
    fluidRow(
      #-------------------------------------------------------------------------
      # 1. Define model outputs for extraction
      #-------------------------------------------------------------------------           
      column(width = 10,
             HTML("<b>","1. Define model outputs for extraction","</b>"),
      ),

      column(width = 1,
             actionButton("helpOutputExtraction", 
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),
      
      column(width = 10,
             excelR::excelOutput("tableOutputExtraction", 
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
             actionButton("helpDateRangeCali", 
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),
      
      #-------------------------------------------------------------------------
      # 3. Select number of parallel runs (cores)
      #-------------------------------------------------------------------------       
      column(width = 10,
             sliderInput("ncores", 
                         "3. Select number of parallel runs (threads)", 
                         value = 4, 
                         min = 1, 
                         max = parallel::detectCores(),
                         step = 1,
                         round = TRUE),
             ),
      
      column(width = 1,
             actionButton("helpNumberofThreads", 
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
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
             tippy::tippy("Help", tooltip = "<span style='font-size:15px;'>
                   When you click this button, all settings are saved to the file 
                   'RSWATObject.rds' in the working directory folder. The parameter
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
             actionButton("runSWAT",
                          "Click here to run SWAT",
                          style="background-color: #87CEFA; border-color: #2e6da4"),
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
             tippy::tippy("Help", tooltip = "<span style='font-size:15px;'>
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
               dataTableOutput("tableDisplayParameterSet"),
        ),
      ),
      #-------------------------------------------------------------------------
      
    )
  )}

