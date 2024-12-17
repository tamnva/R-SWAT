

objFunctionServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ****************************************************************************
    # 4.1. Get user input Objective function
    # ****************************************************************************
    observeEvent(input$objFunction, {
      print("ok34")
      # Get the type of objective function
      globalVariable$objFunction  <- input$objFunction

      # Check when the objective function needs to be maximize or minimize
      if (input$objFunction %in% c('NSE', 'KGE', 'R2')){
        updateSelectInput(session, 'minOrmax',
                          label = 'Minimize or maximize the objective function?',
                          selected = 'Maximize')
      } else if (input$objFunction %in% c('aBIAS', 'RMSE')){
        updateSelectInput(session, 'minOrmax',
                          label = 'Minimize or maximize the objective function?',
                          selected = 'Minimize')
      } else {
        updateSelectInput(session, 'minOrmax',
                          label = 'Minimize or maximize the objective function?',
                          selected = ' ')
      }
      print("ok35")
    })

    # ****************************************************************************
    # Target min or max of the objective function
    # ****************************************************************************
    observeEvent(input$minOrmax, {
      # Assign the maximum and miminum objective fuction values to the global variables
      globalVariable$minOrmax  <- input$minOrmax
    })

    # ****************************************************************************
    # Get userObjFunction.R file file
    # ****************************************************************************
    observeEvent(input$getUserObjFunction, {

      # Get full path to userObjFunction.R file
      shinyjs::disable("getUserObjFunction")
      spsComps::shinyCatch(globalVariable$objFunctionScript <- file.choose(),
                           blocking_level = "none")
      shinyjs::enable("getUserObjFunction")

      spsComps::shinyCatch(
        if (basename(globalVariable$objFunctionScript) == "userObjFunction.R"){
          output$userObjFunctionFile <- renderText(globalVariable$objFunctionScript)
          source(globalVariable$objFunctionScript)
        } else {
          output$userObjFunctionFile <- renderText("Error: The selected file must be 'userObjFunction.R'")
          globalVariable$objFunctionScript <- NULL
        },
        blocking_level = "error")
    })

    # ****************************************************************************
    # Get observed data files
    # ****************************************************************************
    observeEvent(input$getObservedDataFileWindow, {
      print("ok36")
      if(.Platform$OS.type == 'windows'){
        req(input$getObservedDataFileWindow)
      } else {
        req(input$getObservedDataFile)
      }

      if(.Platform$OS.type == 'windows'){
        shinyjs::disable("getObservedDataFileWindow")
        observedDataFile <- choose.files()
        shinyjs::enable("getObservedDataFileWindow")

      } else {

        # Get volumes
        volumes <- getVolumes()

        # Display shinyFileChoose
        shinyFileChoose(input, "getObservedDataFile",
                        roots = volumes,
                        filetypes=c('', 'txt'),
                        session = session)

        # Get full path to the observed data files
        observedDataFile <- parseFilePaths(volumes, input$getObservedDataFile)
        observedDataFile <- observedDataFile$path
      }



      # Get observed data
      spsComps::shinyCatch(
        if(TRUE){
          # Assign observed data file paths to the global variables
          globalVariable$observedDataFile <- sortObservedDataFile(observedDataFile)

          # Display observed data file paths
          output$printObservedDataFile <- renderText(as.character(globalVariable$observedDataFile))



          # Observed data
          globalVariable$observedData <- list()

          checkGetObservedDataFileMessage <- " "
          # Check number of output variable files

          if (globalVariable$nOutputVar != length(globalVariable$observedDataFile)){

            # Print out message if there are more/less number of observed data files
            checkGetObservedDataFileMessage <- paste("Error: Number of observed files should be: ",
                                                     globalVariable$nOutputVar,
                                                     sep ="")

          } else {

            # Get content of observed data files
            for (i in 1:length(globalVariable$observedDataFile)){

              if (!grepl(paste("obs_var_", i, ".txt", sep =""),
                         globalVariable$observedDataFile[i], fixed = TRUE)){

                checkGetObservedDataFileMessage <- paste("Error: change file name ",
                                                         globalVariable$observedDataFile[i],
                                                         " to ",
                                                         paste("obs_var_", i, ".txt",
                                                               sep =""), sep = "")
              } else {

                # Read observed data
                temp <- read.table(globalVariable$observedDataFile[i], skip = 1, sep = "")

                # check observed data
                checkGetObservedDataFileMessage <- checkObservedData(temp)

                if (checkGetObservedDataFileMessage == ""){
                  # Store observed data in the global variables
                  temp <- data.frame(Date = as.POSIXct(paste(temp[,1], temp[,2], sep = " "),
                                                       format = "%Y-%m-%d %H:%M", tz = ""),
                                     Value = temp[,3],
                                     Flag = temp[,4])

                  # Assign back observed data to global variable
                  globalVariable$observedData[[i]] <- temp
                }
              }
            }
          }

          # Display check message
          output$checkGetObservedDataFile <- renderText(checkGetObservedDataFileMessage)
        },
        blocking_level = "error")
      print("ok37")
    })

    # ****************************************************************************
    # Display list of input observed data files
    # ****************************************************************************
    observeEvent(input$checkDisplayObsVar, {

      # Display observed data in table
      spsComps::shinyCatch(
        output$tableObsVarDisplay <- renderDT(mergeDataFrameDiffRow(globalVariable$observedData),
                                              server = FALSE,
                                              extensions = c("Buttons"),
                                              rownames = FALSE,
                                              options = list(dom = 'Bfrtip',
                                                             scroller = TRUE,
                                                             scrollX = TRUE,
                                                             searching = FALSE,
                                                             fixedHeader = TRUE,
                                                             buttons = c('csv', 'excel'))
        ),
        blocking_level = "error"
      )
    })

    # ****************************************************************************
    # Calculate objective function values
    # ****************************************************************************
    observeEvent(input$calObjFunction, {
      print("ok38")
      # Check if all simulations are finished
      if (globalVariable$checkSimComplete){

        # check which parameter sampling approach is selected
        if (globalVariable$samplingApproach %in% c('Cali_(Dynamically_Dimensioned_Search)',
                                                   'Cali_(from_userDefined_package)',
                                                   'Cali_(from_optimization_package)',
                                                   'Cali_(from_nloptr_package)',
                                                   'Sensi_(from_sensitivity_package)',
                                                   'Sensi_(from_userDefined_package)')){

          # Display message
          showModal(modalDialog(
            title = "Important message",
            "The objective function was/will be calculated after each model run (Step 3)",
            easyClose = TRUE,
            size = "l"
          ))

          # If the below approaches are selected, then calculate objective function
        } else if (globalVariable$samplingApproach %in% c('Sensi_Cali_(uniform_Latin_Hypercube_Sampling)',
                                                          'Cali_(Generalized_Likelihood_Uncertainty_Estimation)',
                                                          'Read_User_Parameter_File')){

          withProgress(message = 'Calculating objective function...', {

            # Calculate objective function
            output$printCalObjFunction <- NULL

            # Objective function in the intermediate step
            spsComps::shinyCatch(
              temp <- calObjFunction(globalVariable$parameterValue,
                                     globalVariable$ncores,
                                     globalVariable$nOutputVar,
                                     globalVariable$userReadSwatOutput,
                                     globalVariable$observedData,
                                     globalVariable$workingFolder,
                                     globalVariable$objFunction),
              blocking_level = "error"
            )

            # If there is error in the observed data, display an error message
            if(temp$error) output$printCalObjFunction <- renderText("ERROR in input data - please see R console")

            # Assign the intermediate objective function values to the global variable
            globalVariable$objValueCali <- temp$objValueCali
            globalVariable$perCriteriaCali <- temp$perCriteriaCali
            globalVariable$objValueValid <- temp$objValueValid
            globalVariable$perCriteriaValid <- temp$perCriteriaValid
            globalVariable$simData <- temp$simData

            # Update numeric input (threshold objective function)
            minObjValue <- min(globalVariable$objValueCali)
            maxObjValue <- max(globalVariable$objValueValid)

            updateNumericInput(session = session, "behThreshold",
                               label = "1. Input behavioral threshold",
                               value = minObjValue,
                               min = minObjValue,
                               max = maxObjValue,
                               step = (maxObjValue - minObjValue)/20)

            # Update select variable number
            updateSliderInput(session = session,
                              "plotVarNumber",
                              "2. Input variable number to plot",
                              value = 1,
                              min = 1,
                              max = globalVariable$nOutputVar,
                              step = 1)

            # Some cases the calculate is too fast, add 1 second of delay
            Sys.sleep(1)
            incProgress(1/2)
          })
        } else {
          showModal(modalDialog(
            title = "Important message",
            "Unknown sensitivity or optimization approach",
            easyClose = TRUE,
            size = "l"
          ))
        }

      } else {
        showModal(modalDialog(
          title = "Important message",
          "Not all simulations were finised ...",
          easyClose = TRUE,
          size = "l"
        ))
      }

      print("ok39")
    })

    # ****************************************************************************
    # Calculate objective function: Display objective function values
    # ****************************************************************************
    observeEvent(input$checkDisplayObjFunctionPlot, {

      spsComps::shinyCatch(
        if(!is.null(globalVariable$parameterValue) & !is.null(globalVariable$objValueCali)){
          output$plotObjFunction <- plotly::renderPlotly(plotObjFuncParaValue(globalVariable))
        } else {
          output$plotObjFunction <- NULL
        },
        blocking_level = "error"
      )
    })

    # ****************************************************************************
    # Calculate objective function: Display objective function values
    # ****************************************************************************
    observeEvent(input$checkDisplayObjFunction, {

      # Get parameter values
      spsComps::shinyCatch(
        if(!is.null(globalVariable$parameterValue) & !is.null(globalVariable$objValueCali)){
          tableParaObj <- globalVariable$parameterValue

          # Replace the first row as it is the simulation number with the obj function value
          tableParaObj <- cbind(tableParaObj[,1],
                                globalVariable$objValueCali,
                                globalVariable$objValueValid,
                                matrix(tableParaObj[,-c(1)], nrow = nrow(tableParaObj)))
          tableParaObj <- as.data.frame(tableParaObj)

          colnames(tableParaObj) <- c("SimNr", "objCalibration", "objValidation", globalVariable$paraSelection$Parameter)

          # round up to 3 decimal digits
          is.num <- sapply(tableParaObj, is.numeric)
          tableParaObj[is.num] <- lapply(tableParaObj[is.num], round, 3)

          # Fill output tables with parameter and objective function values
          output$tableCalObjFunction <- renderDT(tableParaObj,
                                                 server = FALSE,
                                                 extensions = c("Buttons"),
                                                 rownames = FALSE,
                                                 options = list(dom = 'Bfrtip',
                                                                scroller = TRUE,
                                                                scrollX = TRUE,
                                                                searching = TRUE,
                                                                fixedHeader = TRUE,
                                                                buttons = c('csv', 'excel')
                                                 ))
        } else {
          output$tableCalObjFunction <- NULL
        },
        blocking_level = "error"
      )

    })

    # ****************************************************************************
    # Calculate objective function: Display objective function values
    # ****************************************************************************
    observeEvent(input$ObjEachVar, {

      spsComps::shinyCatch(
        tableParaObjEachVar <- objFunctionEachVarCaliValid(globalVariable$perCriteriaCali,
                                                           globalVariable$perCriteriaValid),
        blocking_level = "none")


      # Fill output tables with parameter and objective function values
      spsComps::shinyCatch(
        output$tableObjEachVar <- renderDT(tableParaObjEachVar,
                                           server = FALSE,
                                           extensions = c("Buttons"),
                                           rownames = FALSE,
                                           options = list(dom = 'Bfrtip',
                                                          scroller = TRUE,
                                                          scrollX = TRUE,
                                                          searching = TRUE,
                                                          fixedHeader = TRUE,
                                                          buttons = c('csv', 'excel')
                                           )),
        blocking_level = "none")

    })








  })
}
