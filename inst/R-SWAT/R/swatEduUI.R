
# Graphical user interface for Run SWAT

swatEduUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.

  ns <- shiny::NS(id)

  tagList(

    fluidRow(

      column(width = 12,
             checkboxInput('checkBoxHelp', 'Show User guide', value = FALSE,
                           width = NULL),
      ),

      conditionalPanel(
        condition = "input.checkBoxHelp == 1",
        column(12,
               shinydashboard::box(width = NULL,
                   title = "Project Information/User guide",
                   status = "success",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   HTML(readLines(file.path(system.file("R-SWAT",
                                                        package = "RSWAT"),
                                            "HTML","manualCalibration.html"),
                                  warn=FALSE))
               )),
      ),

      column(3,
             shinydashboard::box(width = NULL,
                 title = p("1. Select parameter value",
                           actionButton("helpSwatEduSelectParam",
                                        " ",
                                        icon = icon("circle-info"),
                                        style="background-color: #f20505",
                                        class = "btn-xs",
                                        title = "Update")),
                 status = "warning",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 sliderInput(
                         inputId = "parameter1",
                         label = "Parameter 1",
                         min = 0, max = 1, value = 0.5
                       )
                 )),

      column(6,
             shinydashboard::box(width = NULL,
                 title = p("2. Model run",
                           actionButton("helpModelRunPerf",
                                        " ",
                                        icon = icon("circle-info"),
                                        style="background-color: #f20505",
                                        class = "btn-xs",
                                        title = "Update")),
                 status = "success",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 #HTML("<b>","2.1 Click here to run SWAT/SWAT+","</b>"),
                 #HTML("<br>"," ","</br>"),
                 actionButton("runSwatEdu",
                              "Run SWAT/SWAT+",
                              style="background-color: #b1fa87; border-color: #2e6da4")
             ),
             shinydashboard::box(
               width = NULL,
               title = "3. Plot simulated variables",
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               selectInput("showPlotVariables",
                           label = "",
                           choices = c('variable 1', 'variable 2'),
                           selected = 1,
                           selectize = TRUE,
                           multiple = TRUE)),
             lapply(1:10, function(i) {
               plotly::plotlyOutput(paste0('SwatEduPlot', i), height = "250px")
             }
             )),

      column(3,
             shinydashboard::box(width = NULL,
                 title = "4. Model performance statistics",
                 status = "warning",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 tableOutput('swatEduModelPerf')
             ),
             shinydashboard::box(width = NULL,
                 title = "5. Water/Nutrients balance",
                 status = "warning",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 HTML("<b>","5.1 Water balance entire period","</b>"),
                 HTML("<br>"," ","</br>"),
                 dataTableOutput('swatEduWaterBalance'),
                 HTML("<br>"," ","</br>"),
                 HTML("<b>","5.2 Nutrient balance entire period","</b>"),
                 dataTableOutput('swatEduNutrientBalance')
             )),
    )
  )}

