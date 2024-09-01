
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
             span(textOutput("messageUserReadSwatOutput"), style="color:red"),
      ),

      column(width = 5,
             checkboxInput('checkOutputExtractionDisplayOnly',
                           'Display corresponding observed file names',
                           value = FALSE, width = NULL),
      ),


      conditionalPanel(
        condition = "input.checkOutputExtractionDisplayOnly == 1",
        column(width = 10,
               actionButton("getUserReadSwatOutput",
                            "Load userReadSwatOutput.R",
                            buttonType = "default",
                            style="background-color: #87CEFA; border-color: #0d0c0c",
                            class = NULL),
        ),
        # Display file name
        column(width = 10,
               verbatimTextOutput("userReadSwatOutputFile", placeholder = TRUE),
        ),

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
             actionButton("helpRunSWAT",
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
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
      div( style = "margin-top: 20em",
           column(width = 10,
                  HTML("<b>","5. See simulation report","</b>"),
           ),
      ),

      column(width = 1,
             actionButton("helpCheckCurrentSimulation",
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),

      column(width = 10,
             checkboxInput('checkCurrentSimulation',
                           'Open file CurrentSimulationReport.log',
                           value = FALSE, width = NULL),
      ),

      conditionalPanel(
        condition = "input.checkCurrentSimulation == 1",
        column(width = 10,
               uiOutput('tableCurrentSimulation'),

        ),
      ),

      column(width = 10,
             checkboxInput('checkDisplayParameterSet',
                           'Display all parameter sets',
                           value = FALSE, width = NULL),
      ),

      conditionalPanel(
        condition = "input.checkDisplayParameterSet == 1",
        column(width = 10,
               dataTableOutput("tableDisplayParameterSet"),
        ),
      ),

      # Check box display all simulation results
      column(width = 10,
             checkboxInput("checkSaveSimTocsv",
                           "Save simulated results as .csv files"),
      ),

      column(width = 1,
             actionButton("helpCheckSaveSimTocsv",
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),
      #-------------------------------------------------------------------------

    )
  )}

