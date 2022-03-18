# Graphical user interface for visualize subbasin

subUI <- function(id) {
  
  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow( 
      
      # Input UI
      
      column(width = 4,
             shinyFilesButton("getSubShape", "1. Click here to select subbasin shapefile" ,
                              title = "Please select the subbasin shape file:",
                              multiple = FALSE,
                              buttonType = "default",
                              style="font-weight: bold",
                              # border-color: #2e6da4",
                              class = NULL),
             verbatimTextOutput("printSubShape", placeholder = TRUE),
      ),
      
      
      column(width = 4,
             textInput("subTxtInOutFolder",
                       "2. Select TxtInOut directory",
                       "C:/data/testData/TxtInOut",
                       width = "100%"
             )
      ),
      
      column(width = 4,
             selectInput("subSelectCol",
                         "3. Select data to plot",
                         choices = NULL,
                         width = "100%"
             )
      ),
      
    ),
    
    
    fluidRow(      
      
      column(width = 4,
             selectInput("subTempAgg",
                         "4. Temporal aggregation",
                         choices = list("Daily",
                                        "Monthly",
                                        "Yearly"
                         ),
                         selected = 1,
                         width = "100%"
             )
      ),
      
      column(width = 4,
             conditionalPanel(
               condition = "input.subTempAgg == 'Daily'",
               dateRangeInput("subInputDateRange",
                              "5. Select date range",
                              width = "100%",
                              format = "yyyy-mm-dd")
             ),
             
             conditionalPanel(
               condition = "input.subTempAgg == 'Monthly'",
               dateRangeInput("subInputMonthRange",
                              "5. Select date range",
                              width = "100%",
                              format = "yyyy-mm")
             ),
             
             conditionalPanel(
               condition = "input.subTempAgg == 'Yearly'",
               dateRangeInput("subInputYearRange",
                              "5. Select date range",
                              width = "100%",
                              format = "yyyy")
             ),
             
      ),
      
      column(width = 4,
             conditionalPanel(
               condition = "input.subTempAgg == 'Daily'",
               dateInput("subPlotDate",
                         "6. Select date/month/or year to plot",
                         width = "100%",
                         format = "yyyy-mm-dd")
             ),
             
             conditionalPanel(
               condition = "input.subTempAgg == 'Monthly'",
               dateInput("subPlotMonth",
                         "6. Select date/month/or year to plot",
                         width = "100%",
                         format = "yyyy-mm")
             ),
             
             conditionalPanel(
               condition = "input.subTempAgg == 'Yearly'",
               dateInput("subPlotYear",
                         "6. Select date/month/or year to plot",
                         width = "100%",
                         format = "yyyy")
             ),
             
      ),
      column(width = 10,
             sliderInput("subPlotRange", "7. Set value range for plot",
                         min = 0, max = 100, value = c(10, 90), step = 1)
      )
      
    ),
    
    # Plot -----------------------------------------------------------              
    fluidRow(
      column(width = 12,
             verbatimTextOutput("subPlotTitle")),
      
      column(width = 12,
             plotOutput("subPlotSub",
                        width = "100%", 
                        height = "650")
      ),
      
      
    )
  )}

