# Graphical user interface for visualize HRU

hruUI <- function(id) {
  
  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow( 
      
      # Input UI
      
      column(width = 4,
             shinyFilesButton("getHruShp", "1. Click here to select HRU shape file" ,
                              title = "Please select the Hru shape file:",
                              multiple = FALSE,
                              buttonType = "default",
                              style="font-weight: bold",
                              # border-color: #2e6da4",
                              class = NULL),
             verbatimTextOutput("printHruShp", placeholder = TRUE),
      ),
      
      
      column(width = 4,
             textInput("hruTxtInOutFolder",
                       "2. Select TxtInOut directory",
                       "C:/data/testData/TxtInOut",
                       width = "100%"
             )
      ),
      
      column(width = 4,
             selectInput("hruSelectCol",
                         "3. Select data to plot",
                         choices = NULL,
                         width = "100%"
             )
      ),

    ),
    
             
    fluidRow(      

      column(width = 4,
             selectInput("hruTempAgg",
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
               condition = "input.hruTempAgg == 'Daily'",
               dateRangeInput("hruInputDateRange",
                              "5. Select date range",
                              width = "100%",
                              format = "yyyy-mm-dd")
             ),
             
             conditionalPanel(
               condition = "input.hruTempAgg == 'Monthly'",
               dateRangeInput("hruInputMonthRange",
                              "5. Select date range",
                              width = "100%",
                              format = "yyyy-mm")
             ),
             
             conditionalPanel(
               condition = "input.hruTempAgg == 'Yearly'",
               dateRangeInput("hruInputYearRange",
                              "5. Select date range",
                              width = "100%",
                              format = "yyyy")
             ),
             
      ),
      
      column(width = 4,
             conditionalPanel(
               condition = "input.hruTempAgg == 'Daily'",
               dateInput("hruPlotDate",
                         "6. Select date/month/or year to plot",
                         width = "100%",
                         format = "yyyy-mm-dd")
             ),
             
             conditionalPanel(
               condition = "input.hruTempAgg == 'Monthly'",
               dateInput("hruPlotMonth",
                         "6. Select date/month/or year to plot",
                         width = "100%",
                         format = "yyyy-mm")
             ),
             
             conditionalPanel(
               condition = "input.hruTempAgg == 'Yearly'",
               dateInput("hruPlotYear",
                         "6. Select date/month/or year to plot",
                         width = "100%",
                         format = "yyyy")
             ),
             
      ),
      
      column(width = 10,
             sliderInput("hruPlotRange", "7. Set value range for plot",
                         min = 0, max = 100, value = c(10, 90), step = 1)
             )
    
    ),
    
    # Plot -----------------------------------------------------------              
    fluidRow(
      column(width = 12,
             verbatimTextOutput("hruPlotTitle")),
      
      column(width = 12,
             plotOutput("hruPlotHRU",
                        width = "100%", 
                        height = "650")
             ),
    )
  )}

