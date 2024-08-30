
# Graphical user interface for Optimization/Uncertainty

paraOptUncerUI <- function(id) {

  # shiny::NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.

  ns <- shiny::NS(id)

  tagList(

    fluidRow(

      #-------------------------------------------------------------------------
      # 1. Input behavioral threshold
      #-------------------------------------------------------------------------
      column(width = 10,
             numericInput("behThreshold",
                          "1. Input behavioral threshold",
                          value = 0.5,
                          step = 0.05,
                          min = 0,
                          max = 1,
                          width = "25%"),
      ),

      column(width = 10,
             verbatimTextOutput("printMaxBehThreshold"),
      ),


      #-------------------------------------------------------------------------
      # 4. Input variable number to plot
      #-------------------------------------------------------------------------
      column(width = 10,
             sliderInput("plotVarNumber",
                         "2. Input variable number to plot",
                         value = 1,
                         min = 1,
                         max = 2,
                         step = 1,
                         round = TRUE,
                         width = "25%"),
             ),

      column(width = 10,
             checkboxInput("checkPlotVariableNumber",
                           "Display plot",
                           width = '100%'),
             ),

      conditionalPanel(

        condition = "input.checkPlotVariableNumber == 1",
        column(width = 10,
               plotly::plotlyOutput("PlotVariableNumber"),

        ),
      ),

      # Display behavioral simulation table
      column(width = 10,
             checkboxInput("checkTableBehaSim", "Display table of the above plot"),
      ),
      conditionalPanel(
        condition = "input.checkTableBehaSim== 1",
        column(width = 10,
               excelR::excelOutput("tableBehaSim",
                           width = "100%",
                           height = "1px"),
        ),
      ),

      # Display behavioral parameter range
      column(width = 10,
             checkboxInput("checkTableBehaParamRange",
                           "Display table of behavioral parameter range"),
      ),
      conditionalPanel(
        condition = "input.checkTableBehaParamRange== 1",
        column(width = 10,
               excelR::excelOutput("tableBehaParamRange",
                           width = "100%",
                           height = "1px"),
        ),
      ),

      # Display all behavioral parameter sets
      column(width = 10,
             checkboxInput("checkTableBehaParamSet",
                           "Display behavioral parameter sets"),
      ),
      conditionalPanel(
        condition = "input.checkTableBehaParamSet== 1",
        column(width = 10,
               excelR::excelOutput("tableBehaParamSet",
                                   width = "100%",
                                   height = "1px"),
        ),
      ),

      # Display behavioral parameter range
      column(width = 10,
             checkboxInput("checkPandRFactor", "Display p-factor and r-factor"),
      ),
      conditionalPanel(
        condition = "input.checkPandRFactor== 1",
        column(width = 10,
               verbatimTextOutput("printPandRFactor", placeholder = TRUE),
        ),
      ),

      #-------------------------------------------------------------------------
    ),

  )

}
