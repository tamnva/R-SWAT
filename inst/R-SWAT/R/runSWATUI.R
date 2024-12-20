
# Graphical user interface for Run SWAT

runSwatUI <- function(id) {

  # shiny::NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.

  tagList(

    fluidRow(
      #-------------------------------------------------------------------------
      # 1. Define model outputs for extraction
      #-------------------------------------------------------------------------
      column(width = 10,
             HTML("<b>","1. Define model outputs for extraction","</b>"),
      ),

      column(width = 1,
             actionButton(shiny::NS(id, "helpOutputExtraction"),
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),

      column(width = 10,
             excelR::excelOutput(shiny::NS(id, "tableOutputExtraction"),
                                 width = "100%",
                                 height = "1px"),
             span(textOutput(shiny::NS(id, "messageUserReadSwatOutput")),
                  style="color:red"),
      ),

      column(width = 5,
             checkboxInput(shiny::NS(id, 'checkOutputExtractionDisplayOnly'),
                           'Display corresponding observed file names',
                           value = FALSE, width = NULL),
      ),


      conditionalPanel(
        condition = "input.checkOutputExtractionDisplayOnly == 1",
        column(width = 10,
               actionButton(shiny::NS(id, "getUserReadSwatOutput"),
                            "Load userReadSwatOutput.R",
                            buttonType = "default",
                            style="background-color: #87CEFA; border-color: #0d0c0c",
                            class = NULL),
        ),
        # Display file name
        column(width = 10,
               verbatimTextOutput(shiny::NS(id, "userReadSwatOutputFile"),
                                  placeholder = TRUE),
        ),

        column(width = 10,
               dataTableOutput(shiny::NS(id, 'tableOutputExtractionDisplayOnly')),
        ),
        ns = shiny::NS(id)
      ),

      #-------------------------------------------------------------------------
      # 2. Select date range for calibration
      #-------------------------------------------------------------------------
      column(width = 10,
             dateRangeInput(shiny::NS(id, "dateRangeCali"),
                            "2. Select date range",
                            start = "2001-01-01",
                            end   = "2010-12-31"),

      ),

      column(width = 1,
             actionButton(shiny::NS(id, "helpDateRangeCali"),
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),

      #-------------------------------------------------------------------------
      # 3. Select number of parallel runs (cores)
      #-------------------------------------------------------------------------
      column(width = 10,
             sliderInput(shiny::NS(id, "ncores"),
                         "3. Select number of parallel runs (threads)",
                         value = 4,
                         min = 1,
                         max = parallel::detectCores(),
                         step = 1,
                         round = TRUE),
      ),

      column(width = 1,
             actionButton(shiny::NS(id, "helpNumberofThreads"),
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
             actionButton(shiny::NS(id, "helpRunSWAT"),
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),


      column(width = 10,
             actionButton(shiny::NS(id, "runSWAT"),
                          "Click here to run SWAT",
                          style="background-color: #87CEFA; border-color: #2e6da4"),
             verbatimTextOutput(shiny::NS(id, 'printRuning')),
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
             actionButton(shiny::NS(id, "helpCheckCurrentSimulation"),
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),

      column(width = 10,
             checkboxInput(shiny::NS(id, 'checkCurrentSimulation'),
                           'Open file CurrentSimulationReport.log',
                           value = FALSE, width = NULL),
      ),

      conditionalPanel(
        condition = "input.checkCurrentSimulation == 1",
        column(width = 10,
               uiOutput(shiny::NS(id, 'tableCurrentSimulation')),

        ),
        ns = shiny::NS(id)
      ),

      column(width = 10,
             checkboxInput(shiny::NS(id, 'checkDisplayParameterSet'),
                           'Display all parameter sets',
                           value = FALSE, width = NULL),
      ),

      conditionalPanel(
        condition = "input.checkDisplayParameterSet == 1",
        column(width = 10,
               dataTableOutput(shiny::NS(id, "tableDisplayParameterSet")),
        ),
        ns = shiny::NS(id)
      ),

      # Check box display all simulation results
      column(width = 10,
             checkboxInput(shiny::NS(id, "checkSaveSimTocsv"),
                           "Save simulated results as .csv files"),
      ),

      column(width = 1,
             actionButton(shiny::NS(id, "helpCheckSaveSimTocsv"),
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),
      #-------------------------------------------------------------------------

    )
  )}
