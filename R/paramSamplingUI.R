
# Graphical interface for parameter sampling

paramSamplingUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(

      #-------------------------------------------------------------------------
      # 1. Parameter selection table
      #-------------------------------------------------------------------------
      column(width = 10,
             HTML("<b>","1. Select SWAT parameters for calibration and/or 
                  sensitivity analyis","</b>"),
      ),
      
      column(width = 10,
             checkboxInput('helpParameterSelection', 
                           'Display help for parameter selection', 
                           value = FALSE, width = NULL),
      ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   If you are not sure about the name of the parameter, subbasin, 
                   land use, soil type, slope, check this box
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
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
             tippy("Help", tooltip = "<span style='font-size:16px;text-align: left;'>
                   Parameters defined at the hru level (in files .hru, .gw, .mgt,
                   .chm, .sdr, .sep, .sol files) you should fill in all fields 
                   of this table. <br> Parameter defined at the subbasin level 
                   (.sub, .rte, .wq, .pnd .res files), the land use, soil, slope 
                   can be left empty. Basin parameters (.wwq, .bsn files), the 
                   subbasin, landuse, soil, slope cells can be left emtpy. If 
                   you select all subbasins/landuse/soil or slope, just type 
                   'All'. Note: 'relative' change means
                   x_new = x_old*(1 + appliedValue). 'absolute' change means
                   x_new = x_old + appliedValue. 'replace' means
                   x_new = appliedValue
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      

      column(width = 10,
             actionButton("checkParameterTableButton", 
                          "Click here to check the input table (IMPORTANT)",
                          #icon("paper-plane"), 
                          style="background-color: #87CEFA; 
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
                                     'Cali_(from_hydroPSO_package)',
                                     'Cali_(from_nloptr_package)',
                                     'Cali_(Dynamically_Dimensioned_Search)',
                                     'Cali_(Generalized_Likelihood_Uncertainty_Estimation)',
                                     'Cali_(from_userDefined_package)',
                                     'Read_User_Parameter_File'),
                         multiple = FALSE),
      ),

      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;'>
                   'Sensi', 'Cali', and 'Sensi_Cali' mean for sensitivity, calibration,
                   and both sensitivity and calibration simulations respectively,
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
    
      #-------------------------------------------------------------------------
      # 3. Additional information for sensitivity/calibration
      #-------------------------------------------------------------------------      
      column(width = 10,
             
             textAreaInput("inputInfo", "3. Additional infomation about the selected sensitivity/calibration approach", 
                           " ", 
                           width = "100%",
                           height = "200px",
                           resize = "vertical") %>%
               shiny::tagAppendAttributes(style = 'width: 100%;'),
             
             verbatimTextOutput("displayInputInfo"),
             ),
      
      column(width = 1,
             tippy("Help", tooltip = "<span style='font-size:16px;text-align: left;'>
                   Please use the keyword 'SWAT', 'nParam', 'minCol', 'maxCol' when you want to access variables
                   from R-SWAT and use it in the senstitivity or optimization function. These keywords mean the 
                   SWAT function, number of parameters, minimum and maximum ranges of these parameters (table above).
                   SWAT function takes input is the parameter set (provided by the sensitivity or optimization function)
                   and return the objective function value (with the obejctive function defined in Step 4.1). 
                   The objective in here is ALWAYS minimize the objective function value (e.g., if you select the NSE, 
                   the objective fucntion will automatically return - NSE).  
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
      
    )

  )}

