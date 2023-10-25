
# Graphical interface for parameter sampling

paramSamplingUI <- function(id) {

  # NS(id) returns a namespace function, which was saved as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(

      #-------------------------------------------------------------------------
      # 1. Parameter selection table
      #-------------------------------------------------------------------------
      column(width = 10,
             HTML("<b>","1. Select SWAT parameters for calibration and/or 
                  sensitivity analysis","</b>"),
      ),
  
      column(width = 10,
             checkboxInput('helpParameterSelection', 
                           'Display help for parameter selection', 
                           value = FALSE, width = NULL),
      ),
      
      column(width = 1,
             actionButton("helpParam", 
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
             
      ),
      
      conditionalPanel(
        condition = "input.helpParameterSelection == 1",
        column(width = 10,
               dataTableOutput('tableHelpParameterSelection'),
        ),
      ),
      
      column(width = 10,
             excelOutput("tableParaSelection", 
                         width = "100%", 
                         height = "1px"),
      ),
      
      column(width = 1,
             actionButton("helpParamSelection", 
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),
      
      
      column(width = 10,
             actionButton("checkParameterTableButton", 
                          "Click here to check the input table (IMPORTANT)",
                          #icon("paper-plane"), 
                          style="background-color: #fa8787; 
                          border-color: #2e6da4"),
             verbatimTextOutput('checkParameterTableTxtOuput'),
      ),
      
      div( style = "margin-bottom: 10em",  
           column(width = 10,
           ),
      ),

      #-------------------------------------------------------------------------
      # 2. Select sensitivity or calibration approach
      #-------------------------------------------------------------------------
      column(width = 10,
             selectInput("samplingApproach",
                         width = "50%",
                         label = "2. Select sensitivity or calibration approach", 
                         choices = c('Sensi_Cali_(uniform_Latin_Hypercube_Sampling)', 
                                     'Sensi_(from_sensitivity_package)',
                                     'Sensi_(from_userDefined_package)',
                                     'Cali_(from_optimization_package)',
                                     'Cali_(from_nloptr_package)',
                                     'Cali_(Dynamically_Dimensioned_Search)',
                                     'Cali_(Generalized_Likelihood_Uncertainty_Estimation)',
                                     'Cali_(from_userDefined_package)',
                                     'Read_User_Parameter_File'),
                         selected = 'Sensi_Cali_(uniform_Latin_Hypercube_Sampling)',
                         multiple = FALSE),
      ),

      column(width = 1,
             actionButton("helpSelectingApproach", 
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),
    
      #-------------------------------------------------------------------------
      # 3. Additional information for sensitivity/calibration
      #-------------------------------------------------------------------------      
      column(width = 10,
             
             textAreaInput("inputInfo", "3. Additional infomation about the selected sensitivity/calibration approach", 
                           " ", 
                           width = "100%",
                           height = "100px",
                           resize = "vertical") %>%
               shiny::tagAppendAttributes(style = 'width: 100%;'),
             verbatimTextOutput("displayInputInfo"),
             ),
      
      
      
      column(width = 1,
             actionButton("helpAdditionalInfo", 
                          "Help",
                          buttonType = "default",
                          style="background-color: none; border-color: none",
                          class = NULL),
      ),
      
    )

  )}

