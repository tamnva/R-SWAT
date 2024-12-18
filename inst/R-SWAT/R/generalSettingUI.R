
# Graphical interface for Gerernal setting

generalSettingUI <- function(id) {

  tagList(

    fluidRow(
      #------------------------------------------------------------------------#
      # 1. SWAT or SWAT+ project                                               #
      #------------------------------------------------------------------------#
      column(width = 10,
             selectInput(shiny::NS(id, "SWATorSWATplus"),
                         width = "50%",
                         label = "1. SWAT or SWAT+ project",
                         choices = c('SWAT', 'SWAT+'),
                         multiple = FALSE),
      ),

      column(width = 1,
             actionButton(shiny::NS(id, "helpSWATorSWATplus"),
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),

      #------------------------------------------------------------------------#
      # 2. Working folder                                                      #
      #------------------------------------------------------------------------#
      column(width = 10,
             textInput(shiny::NS(id, "workingFolder"),
                       "2. Working folder",
                       "C:/RSWAT_demo/workingFolder",
                       width = "100%"
             ),

             span(textOutput(shiny::NS(id, "checkWorkingFolder")),
                  style="color:red"),
      ),

      column(width = 1,
             actionButton(shiny::NS(id, "helpworkingFolder"),
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),

      #------------------------------------------------------------------------#
      # 3. TxtInOut folder                                                     #
      #------------------------------------------------------------------------#
      column(width = 10,
             textInput(shiny::NS(id, "TxtInOutFolder"),
                       "3. TxtInOut folder",
                       "C:/RSWAT_demo/R-SWAT-master/data/TxtInOut",
                       width = "100%"
             ),

             span(textOutput("checkTxtInOutFolder"), style="color:red"),
      ),

      column(width = 1,
             actionButton(shiny::NS(id, "helpTxtInOutFolder"),
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),

      ),

      column(width = 10,
             checkboxInput(shiny::NS(id, 'checkTxtInOutFolder'),
             'Display HRU info from TxtInOut folder',
             value = FALSE, width = NULL),
      ),

      conditionalPanel(
        condition = "input.checkTxtInOutFolder == 1",
        column(width = 10,
               dataTableOutput(shiny::NS(id, 'tableHRUinfo')),
        ),
        ns = shiny::NS(id)
      ),

      #------------------------------------------------------------------------#
      # 4. Select executable SWAT or SWAT+ file                                #
      #------------------------------------------------------------------------#
      # Change the display text according to SWAT or SWAT+
      conditionalPanel(
        condition = "input.SWATorSWATplus == 'SWAT'",
        column(width = 10,
               HTML("<b>",
                    "4. Select executable SWAT file",
                    "</b>"),
        ),
        ns = shiny::NS(id)
      ),

      # If SWAT+ project (step 1) is selected
      conditionalPanel(
        condition = "input.SWATorSWATplus == 'SWAT+'",
        column(width = 10,
               HTML("<b>",
                    "4. Select executable SWAT+ file",
                    "</b>"),
        ),
        ns = shiny::NS(id)
      ),

      column(width = 1,
             actionButton(shiny::NS(id, "helpgetSWATexe"),
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),

      # Pop up window for selecting the executable SWAT or SWAT+
      column(width = 10,
             actionButton(shiny::NS(id, "getSWATexe"),
                          "Click here to select executable file",
                          buttonType = "default",
                          style="background-color: #87CEFA; border-color: #0d0c0c",
                          class = NULL),
             verbatimTextOutput("printSWATexe", placeholder = TRUE),
      ),


      #------------------------------------------------------------------------#
      # 5. Files with list of all SWAT parameters                              #
      #------------------------------------------------------------------------#
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
             ns = shiny::NS(id)
           ),

           # If this is a SWAT+ project
           conditionalPanel(
             condition = "input.SWATorSWATplus == 'SWAT+'",
             column(width = 10,
                    HTML("<b>",
                         "5. File with list of all SWAT+ parameters",
                         "</b>"),
             ),
             ns = shiny::NS(id)
           ),

           column(width = 1,
                  actionButton(shiny::NS(id, "helpSWATparamFile"),
                               "Help",
                               buttonType = "default",
                               style="background-color: none; border-color: none",
                               class = NULL),

           ),


      ),

      # Select the paramter file
      column(width = 10,
             actionButton(shiny::NS(id, "getSWATParamFile"),
                          "Click here to select parameter file",
                          buttonType = "default",
                          style="background-color: #87CEFA; border-color: #0d0c0c",
                          class = NULL),
      ),

      # Display file name
      column(width = 10,
             verbatimTextOutput(shiny::NS(id, "printSWATParamFile"),
                                placeholder = TRUE),
      ),

      # Check box to display content of the parameter file
      column(width = 10,
             checkboxInput(shiny::NS(id, 'checkSWATParamFile'),
                           "Display content of the parameter file",
                           value = FALSE, width = NULL),
      ),

      # If the box was checked, then display content of the parameter file
      conditionalPanel(
        condition = "input.checkSWATParamFile == 1",
        column(width = 10,
               dataTableOutput(shiny::NS(id, 'tableSWATParam')),
        ),
        ns = shiny::NS(id)
      ),

      #-----------------------------------Add more GUI elements here (if needed)

    )

  )}

