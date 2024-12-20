#------------------------------------------------------------------------------#
#                      2. Parameter sampling server module                     #
#------------------------------------------------------------------------------#
paramSamplingServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    #--------------------------------------------------------------------------#
    # Default parameter table                                                  #
    #--------------------------------------------------------------------------#
    observe({
      if(globalVariable$SWATProject){
        output$tableParaSelection <- excelR::renderExcel(
          excelR::excelTable(data = dataParaSelectionSWAT,
                             columns = columnsParaSelectionSWAT,
                             editable = TRUE,
                             allowInsertRow = TRUE,
                             allowInsertColumn = TRUE,
                             allowDeleteColumn = TRUE,
                             allowDeleteRow = TRUE,
                             rowDrag = TRUE,
                             columnResize = FALSE,
                             wordWrap = TRUE))
      } else {
        output$tableParaSelection <- excelR::renderExcel(
          excelR::excelTable(data = dataParaSelectionSWATPlus,
                             columns = columnsParaSelectionSWATPlus,
                             editable = TRUE,
                             allowInsertRow = TRUE,
                             allowInsertColumn = TRUE,
                             allowDeleteColumn = TRUE,
                             allowDeleteRow = TRUE,
                             rowDrag = TRUE,
                             columnResize = FALSE,
                             wordWrap = TRUE))
      }
    })



    #--------------------------------------------------------------------------#
    # Show list of all parameters                                              #
    #--------------------------------------------------------------------------#
    observe({

      req(input$helpParameterSelection)

      # This first if command prevents the app crashed when no input is given
      if (is.data.frame(globalVariable$SWATParam)){

        # Check if there is no input SWAT parameter file
        if (is.null(globalVariable$SWATParam$parameter)){
          output$tableHelpParameterSelection <-
            renderDT(displayOutput$uniqueHruProperties)

        } else {
          SWATParamName <- globalVariable$SWATParam$parameter
          nSWATParamName <- length(SWATParamName)

          if(is.null(displayOutput$uniqueHruProperties)){
            output$tableHelpParameterSelection <- NULL
          } else {
            nRowUniqueHRU <- nrow(displayOutput$uniqueHruProperties)
            compareLength <- max(nRowUniqueHRU, nSWATParamName)
            if (nRowUniqueHRU < compareLength){
              newRow <- data.frame(
                matrix(rep(NA, (compareLength - nRowUniqueHRU)*4),ncol = 4))
              names(newRow) <- names(displayOutput$uniqueHruProperties)
              tempUniqueHruProperties <- rbind(displayOutput$uniqueHruProperties,
                                               newRow)

            } else {
              SWATParamName <- c(SWATParamName, rep(NA, compareLength - nSWATParamName))
            }

            tempUniqueHruProperties <- cbind(SWATParamName,tempUniqueHruProperties)

            output$tableHelpParameterSelection <- renderDT(tempUniqueHruProperties)
          }

        }

        # If this is a SWAT+ project
        if (ncol(globalVariable$SWATParam) != 7){

          # Get SWAT+ parameter name
          temp <- paste(globalVariable$SWATParam[, 1], ".", globalVariable$SWATParam[, 2],
                        sep = "")
          # Display SWAT+ help table
          output$tableHelpParameterSelection <-
            renderDT(data.frame(Parameter = temp))
        }
      } else {
        # Display SWAT+ help table
        output$tableHelpParameterSelection <- NULL
      }

    })

    #--------------------------------------------------------------------------#
    # Help parameter selection                                                 #
    #--------------------------------------------------------------------------#
    observeEvent(input$helpParam, {

      showModal(modalDialog(
        title = "Help: 1. Display help for parameter selection",
        "If you don't know the parameter, subbasin, land use, and slope names,
      check this box. We will get this information from the TxtInOut folder for you",
        easyClose = TRUE
      ))
    })

    #--------------------------------------------------------------------------#
    # Check parameter table                                                    #
    #--------------------------------------------------------------------------#
    observe({

      req(input$checkParameterTableButton)

      spsComps::shinyCatch(
        checkParameterTable <- checkSwatParameterName(globalVariable$paraSelection,
                                                      globalVariable$SWATParam,
                                                      globalVariable$HRUinfo,
                                                      globalVariable$SWATProject),
        blocking_level = "error"
      )

      output$checkParameterTableTxtOuput <- renderText(checkParameterTable$checkMessage)
    })

    #--------------------------------------------------------------------------#
    # Save parameter table to global variables                                 #
    #--------------------------------------------------------------------------#
    observe({

      req(input$tableParaSelection)

      # Parameter selection
      paraSelection <-  excelR::excel_to_R(input$tableParaSelection)

      # Check if no input data
      if(is.null(paraSelection)) {

        # Check if this is SWAT or SWAT+ project
        if(globalVariable$SWATProject){
          paraSelection <- dataParaSelectionSWAT
        } else {
          paraSelection <- dataParaSelectionSWATPlus
        }
      }

      # Save parameter selection to the global variable
      globalVariable$paraSelection  <- paraSelection
      globalVariable$paraSelection[,1] <- trimws(paraSelection[,1])

      spsComps::shinyCatch(
        if(check_null_na_empty(globalVariable$paraSelection$Min[1]) &
           check_null_na_empty(globalVariable$paraSelection$Max[1])){

          # Min and max value
          minVal <- as.numeric(globalVariable$paraSelection$Min[1])
          maxVal <- as.numeric(globalVariable$paraSelection$Max[1])

          # Update slider input
          updateSliderInput(session,
                            inputId = "parameter1",
                            label = globalVariable$paraSelection$Parameter[1],
                            min = minVal,
                            max = maxVal,
                            value = minVal,
                            step = (maxVal - minVal)/50
          )
        },
        blocking_level = "warning"
      )

      # remove slider input when parameter selection was updated
      spsComps::shinyCatch(
        if(globalVariable$nCaliParam > 1){
          for (i in 2:globalVariable$nCaliParam){
            removeUI(selector = paste0("div:has(> #parameter", i, ")"),
                     multiple = TRUE,
                     immediate = TRUE,
                     session)
          }
        },
        blocking_level = "warning"
      )

      # Update number of calibrated parameter
      globalVariable$nCaliParam <- nrow(globalVariable$paraSelection)

      # Add slider input for other parameters
      spsComps::shinyCatch(
        if (nrow(globalVariable$paraSelection) > 1){
          lapply(1:(nrow(globalVariable$paraSelection)-1), FUN = function(i) {
            if (check_null_na_empty(globalVariable$paraSelection$Min[i+1]) &
                check_null_na_empty(globalVariable$paraSelection$Max[i+1])){

              # Min and max value
              minVal <- as.numeric(globalVariable$paraSelection$Min[i+1])
              maxVal <- as.numeric(globalVariable$paraSelection$Max[i+1])

              # Update slider input
              insertUI(
                selector = "#parameter1",
                where = "afterEnd",
                ui = sliderInput(
                  inputId = paste0("parameter", i+1),
                  label = globalVariable$paraSelection$Parameter[i+1],
                  min = minVal,
                  max = maxVal,
                  value = minVal,
                  step = (maxVal - minVal)/50
                )
              )
            }
          })
        },
        blocking_level = "error"
      )
    })

    # ****************************************************************************
    # Help button parameter change selection
    # ****************************************************************************
    observeEvent(input$helpParamSelection, {

      showModal(modalDialog(
        title = "Help: Parameter Selection",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "helpParamSelection.html"),warn=FALSE)),
        easyClose = TRUE
      ))

    })

    # ****************************************************************************
    # Parameter sampling: Get user input for parameter sampling
    # ****************************************************************************
    observe({

      req(input$samplingApproach)

      # Save sampling approach to the global variable
      globalVariable$samplingApproach <- input$samplingApproach

      # SUFI2 approach
      if (input$samplingApproach == 'Sensi_Cali_(uniform_Latin_Hypercube_Sampling)'){

        updateTextAreaInput(session, "inputInfo",
                            paste("3. Additional infomation about the selected",
                                  "sensitivity/calibration approach"),
                            "100")
        outputText <- paste("Please input the number of iterations, e.g., 100")

        # From optimization package
      } else if (input$samplingApproach == 'Cali_(from_optimization_package)'){

        updateTextAreaInput(session, "inputInfo",
                            paste("3. Additional infomation about the selected",
                                  "sensitivity/calibration approach"),
                            paste("optimization::optim_sa(fun = SWAT, start = c(runif(nParam,",
                                  "min = minCol, max = maxCol)), lower = minCol,",
                                  "upper = maxCol, trace = TRUE, control = list(t0 = 10,",
                                  "nlimit = 5,t_min = 0.1, dyn_rf = FALSE,rf = 1,r = 0.7)))"))
        outputText <- helpTextCali

        # From nloptr_package
      } else if (input$samplingApproach == 'Cali_(from_nloptr_package)'){

        updateTextAreaInput(session, "inputInfo",
                            paste("3. Additional infomation about the selected",
                                  "sensitivity/calibration approach"),
                            paste("nloptr::bobyqa(runif(nParam, min = minCol, max = maxCol),",
                                  "SWAT, lower = minCol, upper = maxCol,",
                                  "control = list(maxeval = 100))"))
        outputText <- helpTextCali

      } else if (input$samplingApproach == 'Cali_(Dynamically_Dimensioned_Search)'){

        updateTextAreaInput(session, "inputInfo",
                            paste("3. Additional infomation about the selected",
                                  "sensitivity/calibration approach"),
                            "100, 1")
        outputText <- paste("Please write two INTEGER numbers in a SINGLE line,",
                            "separated by a comma", "\n",
                            "The first number is the number of iterations", "\n",
                            "The second number (either 1 or 2) is the parallel approach", "\n",
                            "  1 indicates the normal parallel approach",
                            "    (DDS run independently in each core) ", "\n",
                            "  2 indictes that the intermediate best parameter set",
                            "from all cores is selected", "\n",
                            "and is assigned as an inital parameter set for the",
                            "next run with all cores")

      } else if (input$samplingApproach == 'Cali_(Generalized_Likelihood_Uncertainty_Estimation)'){

        updateTextAreaInput(session, "inputInfo",
                            paste("3. Additional infomation about the selected",
                                  "sensitivity/calibration approach"),
                            "100")
        outputText <- paste("Please input the number of iterations, e.g., 100", sep ="")

      } else if (input$samplingApproach == 'Read_User_Parameter_File'){

        updateTextAreaInput(session, "inputInfo",
                            paste("3. Additional infomation about the selected",
                                  "sensitivity/calibration approach"),
                            "C:/data/myParameterFile.txt")
        outputText <- paste("Please input a link to the parameter set file, ",
                            "formatted as follows:","\n",
                            "   First row: Name of the parameters","\n",
                            "   From the second row: Parameter value", "\n",
                            " ", "\n",
                            "   As many column as the number of parameters","\n",
                            "   Columns are seperated by one or more spaces or tabs","\n",
                            "   The order of parameters in these columns follows ",
                            "the order of the parameters in the above Table", "\n",
                            " ", "\n",
                            "Example (content of myParameterFile.txt)", "\n",
                            " ", "\n",
                            "   GW_DELAY.gw  CN2.mgt   SOL_K.sol    ALPHA_BF.gw  ESCO.hru    SURLAG.hru  CH_K2.rte     SURLAG.bsn", "\n",
                            "   228.03       0.05     -0.21         0.07         0.80        0.14        0.41          3.39", "\n",
                            "   96.27       -0.12      0.17         0.35         0.66        0.34        0.03          3.00", "\n",
                            sep = "")

      } else if (input$samplingApproach == 'Sensi_(from_userDefined_package)'){

        updateTextAreaInput(session, "inputInfo",
                            paste("3. Additional infomation about the selected",
                                  "sensitivity/calibration approach"),
                            paste("Write_your_first_R_command_here \n",
                                  "Write_your_second_R_command_here", sep = ""))
        outputText <- helpTextSensi

      } else if (input$samplingApproach == 'Cali_(from_userDefined_package)'){

        updateTextAreaInput(session, "inputInfo",
                            paste("3. Additional infomation about the selected",
                                  "sensitivity/calibration approach"),
                            "Write_your_SINGLE_line_R_command_here")
        outputText <- helpTextCali

      } else {
        updateTextAreaInput(session, "inputInfo",
                            paste("3. Additional infomation about the selected",
                                  "sensitivity/calibration approach"),
                            paste0("sensCaliObject <- sensitivity::morris(model = SWAT, ",
                                   "factors = nParam, binf = minCol, bsup = maxCol, r = 4, ",
                                   "design = list(type = 'oat', levels = 5, grid.jump = 3))\n",
                                   "print(sensCaliObject)[]"))
        outputText <- helpTextSensi
      }
      output$displayInputInfo <- renderText(outputText)
    })

    # ****************************************************************************
    # Help: Selecting sensitivity, calibration approach
    # ****************************************************************************
    observeEvent(input$helpSelectingApproach, {
      showModal(modalDialog(
        title = "Help: 1. Selecting calibration and/or sensitivity approach",
        "Sensi', 'Cali', and 'Sensi_Cali' mean for sensitivity, calibration,
      and both sensitivity and calibration simulations respectively",
        easyClose = TRUE
      ))
    })

    # ****************************************************************************
    # Parameter sampling: get input information
    # ****************************************************************************
    observe({

      req(input$inputInfo)

      # Check if user need to input R command
      if (input$samplingApproach %in% c('Sensi_(from_sensitivity_package)',
                                        'Cali_(from_optimization_package)',
                                        'Cali_(from_nloptr_package)',
                                        'Sensi_(from_userDefined_package)',
                                        'Cali_(from_userDefined_package)')){

        # Save input as text
        globalVariable$sensCaliCommand <- input$inputInfo

        # Remove comments and split R command
        globalVariable$sensCaliCommand <- splitRemoveComment(
          globalVariable$sensCaliCommand)
      } else {

        # If input is not R command, then just save as text
        globalVariable$sensCaliCommand <- input$inputInfo
      }

    })

    # ****************************************************************************
    # Help: Additional infomation about the selected sensitivity/calibration
    # ****************************************************************************
    observeEvent(input$helpAdditionalInfo, {

      showModal(modalDialog(
        title = "Help: 3. Additional infomation for sensitivity/calibration",
        "Default input for each method is given, please modify the text if necessary
      (see help text below for modifying). Please see also Sections 2.1 and 2.2
      in the R-SWAT wiki page: https://github.com/tamnva/R-SWAT/wiki/R-SWAT-User-Manual",
        easyClose = TRUE
      ))
    })
  })
}
