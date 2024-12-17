
runSWATServer <- function(id) {
  moduleServer(id, function(input, output, session) {


    # ****************************************************************************
    # Output extraction: Default setting
    # ****************************************************************************
    if(!globalVariable$loadProject){
      print("ok22")
      if (globalVariable$SWATProject){
        output$tableOutputExtraction <-
          excelR::renderExcel(excelR::excelTable(data = dataOutputExtractionSWAT,
                                                 columns = columnsOutputExtractionSWAT,
                                                 editable = TRUE,
                                                 allowInsertRow = TRUE,
                                                 allowInsertColumn = FALSE,
                                                 allowDeleteColumn = FALSE,
                                                 allowDeleteRow = TRUE,
                                                 rowDrag = FALSE,
                                                 columnResize = FALSE,
                                                 wordWrap = TRUE))
      } else {

        output$tableOutputExtraction <-
          excelR::renderExcel(excelR::excelTable(data = dataOutputExtractionSWATPlus,
                                                 columns = columnsOutputExtractionSWATPlus,
                                                 editable = TRUE,
                                                 allowInsertRow = TRUE,
                                                 allowInsertColumn = FALSE,
                                                 allowDeleteColumn = FALSE,
                                                 allowDeleteRow = TRUE,
                                                 rowDrag = FALSE,
                                                 columnResize = FALSE,
                                                 wordWrap = TRUE))
      }
    }

    # ****************************************************************************
    # Get user output extraction
    # ****************************************************************************
    observe({
      print("ok23")
      # Get table of output extraction
      outputExtraction <- excelR::excel_to_R(input$tableOutputExtraction)

      # Check if there is not such a table, then take default table
      if(is.null(outputExtraction)) {
        if (globalVariable$SWATProject){
          outputExtraction <- dataOutputExtractionSWAT
        } else {
          outputExtraction <- dataOutputExtractionSWATPlus
        }
      }

      # Save this table to the global variable
      globalVariable$outputExtraction <- outputExtraction

      # Number of output variables
      spsComps::shinyCatch(OutputVar <- getNumberOutputVar(outputExtraction),
                           blocking_level = "error")
      globalVariable$nOutputVar <- OutputVar$nOutputVar

      # Check if this option is activated
      globalVariable$userReadSwatOutput <- OutputVar$userReadSwatOutput

      # Display message
      spsComps::shinyCatch(
        if (TRUE %in% globalVariable$userReadSwatOutput){
          output$messageUserReadSwatOutput <- renderText(
            "MUST check the box below to load 'userReadSwatOutput.R' file")
          output$userReadSwatOutputFile <- renderText(
            "Please load 'userReadSwatOutput.R' file")
        } else {
          output$messageUserReadSwatOutput <- renderText(" ")
          output$userReadSwatOutputFile <- renderText(
            "No input file is needed")
        },
        blocking_level = "error")

      # Display table of observed file names needed for calibration/optimization
      output$tableOutputExtractionDisplayOnly <- renderDT(
        printVariableNameObservedFiles(outputExtraction)
      )

      # Get number of output plots
      spsComps::shinyCatch(
        updateSelectInput(session,
                          "showPlotVariables",
                          label = "",
                          choices = paste0("variable ", c(1:globalVariable$nOutputVar)),
                          selected = 1),
        blocking_level = "error")
      print("ok24")
    })

    # ****************************************************************************
    # Get userObjFunction.R file file
    # ****************************************************************************
    observeEvent(input$getUserReadSwatOutput, {

      print("ok25")
      # Display message
      spsComps::shinyCatch(
        if (TRUE %in% globalVariable$userReadSwatOutput){
          output$userReadSwatOutputFile <- renderText(
            "Please load 'userReadSwatOutput.R' file")
        } else {
          output$userReadSwatOutputFile <- renderText(
            "No input file is needed")
        },
        blocking_level = "error")


      # Get full path to userObjFunction.R file
      shinyjs::disable("getUserReadSwatOutput")
      spsComps::shinyCatch(globalVariable$readOutputScript <- file.choose(),
                           blocking_level = "none")
      shinyjs::enable("getUserReadSwatOutput")

      spsComps::shinyCatch(
        if (basename(globalVariable$readOutputScript) == 'userReadSwatOutput.R'){
          output$userReadSwatOutputFile <- renderText(globalVariable$readOutputScript)
          source(globalVariable$readOutputScript)
        } else {
          output$userReadSwatOutputFile <- renderText(
            "Error: The selected file must be 'userReadSwatOutput.R'")
          globalVariable$readOutputScript <- NULL
        },
        blocking_level = "error")
    })

    # ****************************************************************************
    # Help output extraction
    # ****************************************************************************
    observeEvent(input$helpOutputExtraction, {
      print("ok26")
      showModal(modalDialog(
        title = "Help: Parameter Selection",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "helpOutputExtraction.html"),warn=FALSE)),
        easyClose = TRUE
      ))

    })

    # ****************************************************************************
    # Update select input range based on file.cio in the TxtInOutFolder
    # ****************************************************************************
    observeEvent(input$TxtInOutFolder, {

      print("ok27")
      # Check if there is a file called file.cio in this TxtInOut
      if (checkDirFileExist(trimws(input$TxtInOutFolder), "file.cio", "")){

        # Get simulation dates
        myDate <- getSimTime(trimws(input$TxtInOutFolder))

        # Assign simulation dates to the global variale
        globalVariable$fileCioInfo <- myDate

        if (!globalVariable$loadProject){
          # Update selected date range for calibration/sensitivity
          updateDateRangeInput(session, "dateRangeCali",
                               start = myDate$startEval,
                               end = myDate$endSim,
                               min = myDate$startEval,
                               max = myDate$endSim)
        }
      }
      print("ok28")
    })

    # ****************************************************************************
    # Get user input range for calibration
    # ****************************************************************************
    observeEvent(input$dateRangeCali, {
      # Save selected date range for calibration/sensitivity to the global variables
      globalVariable$dateRangeCali <- input$dateRangeCali
    })

    # ****************************************************************************
    # Help button select file SWAT (or SWAT+) parameter file
    # ****************************************************************************
    observeEvent(input$helpDateRangeCali, {
      print("ok29")
      showModal(modalDialog(
        title = "Help: 2. Select date range for calibration",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "helpDateRangeCali.html"),warn=FALSE)),
        easyClose = TRUE
      ))
    })

    # ****************************************************************************
    # Help button select file SWAT (or SWAT+) parameter file
    # ****************************************************************************
    observeEvent(input$helpDateRangeCali, {
      print("ok30")
      showModal(modalDialog(
        title = "Help: 2. Select date range for calibration",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "helpDateRangeCali.html"),warn=FALSE)),
        easyClose = TRUE
      ))
    })

    # ****************************************************************************
    # Get user input number of cores/threads
    # ****************************************************************************
    observeEvent(input$ncores, {
      # Assign the number of selected cores to the global variables
      globalVariable$ncores <- input$ncores
    })

    # ****************************************************************************
    # Help button select file SWAT (or SWAT+) parameter file
    # ****************************************************************************
    observeEvent(input$helpNumberofThreads, {

      showModal(modalDialog(
        title = "Help: 3. Select number of parallel runs (threads)",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "helpNumberofThreads.html"),warn=FALSE)),
        easyClose = TRUE
      ))
    })

    # ****************************************************************************
    # Help button helpRunSWAT
    # ****************************************************************************
    observeEvent(input$helpRunSWAT, {

      showModal(modalDialog(
        title = "Help: 4. Run SWAT",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "helpRunSWAT.html"),warn=FALSE)),
        easyClose = TRUE
      ))
    })

    # ****************************************************************************
    # Help button helpCheckCurrentSimulation
    # ****************************************************************************
    observeEvent(input$helpCheckCurrentSimulation, {

      showModal(modalDialog(
        title = "Help: 5. See Simulation report",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "helpCheckCurrentSimulation.html"),warn=FALSE)),
        easyClose = TRUE
      ))
    })

    # ****************************************************************************
    # Help button helpCheckSaveSimTocsv
    # ****************************************************************************
    observeEvent(input$helpCheckSaveSimTocsv, {

      showModal(modalDialog(
        title = "Help: Save simulated results as .csv files",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "helpCheckSaveSimTocsv.html"),warn=FALSE)),
        easyClose = TRUE
      ))
    })

    # ****************************************************************************
    # Run SWAT
    # ****************************************************************************
    observeEvent(input$runSWAT, {
      print("ok29")
      # Refresh the parameter values in case of loading the projects
      globalVariable$parameterValue <- c()

      # Display progress bar
      withProgress(message = 'Running SWAT...', {

        # Perform various checks to activate SWAT run
        checkList <- TRUE
        checkList <- checkList & !is.null(globalVariable$workingFolder)
        checkList <- checkList & !is.null(globalVariable$paraSelection)
        checkList <- checkList & !is.null(globalVariable$HRUinfo)
        checkList <- checkList & !is.null(globalVariable$SWATParam)
        checkList <- checkList & !is.null(globalVariable$TxtInOutFolder)
        checkList <- checkList & !is.null(globalVariable$outputExtraction)
        checkList <- checkList & !is.null(globalVariable$ncores)
        checkList <- checkList & !is.null(globalVariable$SWATexeFile)
        checkList <- checkList & !is.null(globalVariable$fileCioInfo)
        checkList <- checkList & !is.null(globalVariable$dateRangeCali)

        if (checkList){

          # Print out message when click Run SWAT
          showModal(modalDialog(
            title = "SWAT is running...",
            HTML("SWAT is running, dismiss this message. You can open the text file
        'Output/CurrentSimulationReport.log' in the working folder to see the current simulation."),
            easyClose = TRUE,
            size = "l"
          ))

          # First get or generate parameter set
          if (globalVariable$samplingApproach == 'Cali_(Dynamically_Dimensioned_Search)'){

            # Generate initial parameter set
            spsComps::shinyCatch(
              globalVariable$parameterValue <- lhsRange(globalVariable$ncores,
                                                        getParamRange(globalVariable$paraSelection)),
              blocking_level = "error"
            )
          }


          # Load all files that are going to be updated
          spsComps::shinyCatch(
            globalVariable$caliParam <- updatedFileContent(globalVariable$HRUinfo,
                                                           globalVariable$paraSelection,
                                                           globalVariable$SWATParam,
                                                           globalVariable$TxtInOutFolder),
            blocking_level = "error"
          )


          # Save global variables
          spsComps::shinyCatch(
            saveRDS(globalVariable, file = paste(globalVariable$workingFolder, '/',
                                                 'RSWATproject.rds', sep ='')),
            blocking_level = "error"
          )

          # Copy unchanged file for the first simulation
          copyUnchangeFiles <- TRUE
          firstRun <- TRUE

          # Run SWAT for all iteration ---------------------------------------------
          if (globalVariable$samplingApproach %in% c('Sensi_Cali_(uniform_Latin_Hypercube_Sampling)',
                                                     'Cali_(Generalized_Likelihood_Uncertainty_Estimation)',
                                                     'Read_User_Parameter_File')){

            # Generate parameter values
            if(input$samplingApproach == 'Sensi_Cali_(uniform_Latin_Hypercube_Sampling)'){

              globalVariable$parameterValue <- lhsRange(as.numeric(input$inputInfo), getParamRange(globalVariable$paraSelection))

            } else if(input$samplingApproach == 'Cali_(Generalized_Likelihood_Uncertainty_Estimation)'){
              globalVariable$parameterValue <- runifSampling(as.numeric(input$inputInfo),
                                                             as.numeric(globalVariable$paraSelection$Min),
                                                             as.numeric(globalVariable$paraSelection$Max))

            } else if (globalVariable$samplingApproach == "Read_User_Parameter_File"){
              parameterValue <- as.matrix(read.table(file = trimws(input$inputInfo),
                                                     header = TRUE, sep =""))
              parameterValue <- cbind(c(1:nrow(parameterValue)),parameterValue)
              colnames(parameterValue) <- NULL
              rownames(parameterValue) <- NULL

              globalVariable$parameterValue <- parameterValue
            } else {
              globalVariable$parameterValue <- NULL
            }

            # Check max number of cores
            globalVariable$ncores <- min(globalVariable$ncores, nrow(globalVariable$parameterValue))

            # Run SWAT in parallel
            spsComps::shinyCatch(
              runSWATpar(globalVariable$workingFolder,
                         globalVariable$TxtInOutFolder,
                         globalVariable$outputExtraction,
                         globalVariable$ncores,
                         globalVariable$SWATexeFile,
                         globalVariable$parameterValue,
                         globalVariable$paraSelection,
                         globalVariable$caliParam,
                         copyUnchangeFiles,
                         globalVariable$fileCioInfo,
                         globalVariable$dateRangeCali,
                         firstRun,
                         globalVariable$readOutputScript),
              blocking_level = "error"
            )


          } else if (globalVariable$samplingApproach == 'Cali_(Dynamically_Dimensioned_Search)') {

            if(is.null(globalVariable$observedData)){

              # Display message box if there is no observed data provided
              showModal(modalDialog(
                title = "Not enough information to perform SWAT run",
                span("You selected 'Cali_(DDS)', please defined objective function ",
                     "and load observed data (Step 4.1) before running SWAT"),
                easyClose = TRUE,
                size = "l"
              ))

            } else {
              # Save results between iteration
              saveIterationResult <- list()

              # Run SWAT with init
              spsComps::shinyCatch(
                runSWATpar(globalVariable$workingFolder,
                           globalVariable$TxtInOutFolder,
                           globalVariable$outputExtraction,
                           globalVariable$ncores,
                           globalVariable$SWATexeFile,
                           globalVariable$parameterValue,
                           globalVariable$paraSelection,
                           globalVariable$caliParam,
                           copyUnchangeFiles,
                           globalVariable$fileCioInfo,
                           globalVariable$dateRangeCali,
                           firstRun,
                           globalVariable$readOutputScript),
                blocking_level = "error"
              )


              # Set firstRun = FALSE -> don't need to copy unchanged files again
              firstRun = FALSE

              # Run SWAT with first initial parameter set
              nIters <- as.numeric(strsplit(input$inputInfo,
                                            split = ",",
                                            fixed = TRUE)[[1]][1])

              # Get parallel mode
              parallelMode <- as.numeric(strsplit(input$inputInfo,
                                                  split = ",",
                                                  fixed = TRUE)[[1]][2])

              # Objective function with initial parameter set
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


              # Set new parameter
              newPar <- globalVariable$parameterValue

              # Set the best objective and parameter values
              best <- list()
              if(parallelMode == 1){
                best$objValueCali <- temp$objValueCali
                best$perCriteriaCali <- temp$perCriteriaCali
                best$objValueValid <- temp$objValueValid
                best$perCriteriaValid <- temp$perCriteriaValid
                best$simData <- temp$simData
              } else {

                idBest <- ilocMinMax(temp$objValueCali, globalVariable$minOrmax)

                if (length(idBest) > 1) {idBest = idBest[1]}

                # Take the better parameter value and objective function values
                best$parameterValue <- best$parameterValue[idBest, ]
                best$objValueCali <- temp$objValueCali[idBest]
                best$objValueValid <- temp$objValueValid[idBest]
              }

              # Loop over number of iteration
              for (i in 1:nIters){

                if (i == 1){
                  # Save iteration result
                  saveIterationResult$parameterValue <- globalVariable$parameterValue
                  saveIterationResult$objValueCali <- temp$objValueCali
                  saveIterationResult$objValueValid <- temp$objValueValid
                  saveIterationResult$perCriteriaCali <- temp$perCriteriaCali
                  saveIterationResult$perCriteriaValid <- temp$perCriteriaValid
                  saveIterationResult$simData <- temp$simData
                  print(paste("Iteration - Best objective function value (from the",
                              "respective core if parallel mode = 1)"))
                }

                print(c(i-1, round(best$objValueCali, digits = 3)))

                # Take better parameter + Generate new parameter set with DDS
                if (parallelMode == 1) {
                  # Take the better parameter set/data if exist
                  for (j in 1:nrow(newPar)){
                    if(isNewSimBetter(temp$objValueCali[j], best$objValueCali[j], globalVariable$minOrmax)){
                      globalVariable$parameterValue[j, ] <- newPar[j, ]
                      best$objValueCali[j] <- temp$objValueCali[j]
                      best$objValueValid[j] <- temp$objValueValid[j]
                    }
                  }

                  # Generate new parameter set
                  newPar <- data.frame(min = as.numeric(globalVariable$paraSelection$Min),
                                       max = as.numeric(globalVariable$paraSelection$Max))
                  parameterValue <- saveIterationResult$parameterValue[,2:ncol(saveIterationResult$parameterValue)]
                  newPar <- cbind(newPar, t(parameterValue))
                  newPar <- dds(newPar, globalVariable$ncores, i, nIters, 0.2, parallelMode)

                } else {

                  idBest <- ilocMinMax(temp$objValueCali, globalVariable$minOrmax)

                  if (length(idBest) > 1) {idBest = idBest[1]}

                  if(isNewSimBetter(temp$objValueCali[idBest], best$objValueCali, globalVariable$minOrmax)){
                    globalVariable$parameterValue <- newPar[idBest, ]
                    best$objValueCali <- temp$objValueCali[idBest]
                    best$objValueValid <- temp$objValueValid[idBest]
                  }

                  # Generate new parameter set
                  newPar <- data.frame(min = as.numeric(globalVariable$paraSelection$Min),
                                       max = as.numeric(globalVariable$paraSelection$Max))
                  parameterValue <- saveIterationResult$parameterValue[,2:ncol(saveIterationResult$parameterValue)]
                  newPar <- cbind(newPar, t(parameterValue))
                  newPar <- dds(newPar, globalVariable$ncores, i, nIters, 0.2, parallelMode)

                }

                # Remove output variables
                removeOutputFiles(globalVariable$workingFolder,
                                  globalVariable$ncores,
                                  globalVariable$nOutputVar)

                # Run SWAT in parallel
                spsComps::shinyCatch(
                  runSWATpar(globalVariable$workingFolder,
                             globalVariable$TxtInOutFolder,
                             globalVariable$outputExtraction,
                             globalVariable$ncores,
                             globalVariable$SWATexeFile,
                             newPar,
                             globalVariable$paraSelection,
                             globalVariable$caliParam,
                             FALSE,
                             globalVariable$fileCioInfo,
                             globalVariable$dateRangeCali,
                             firstRun,
                             globalVariable$readOutputScript),
                  blocking_level = "error"
                )



                # Caculate objective function
                spsComps::shinyCatch(
                  temp <- calObjFunction(newPar,
                                         globalVariable$ncores,
                                         globalVariable$nOutputVar,
                                         globalVariable$userReadSwatOutput,
                                         globalVariable$observedData,
                                         globalVariable$workingFolder,
                                         globalVariable$objFunction),
                  blocking_level = "error"
                )

                # Save iteration result
                saveIterationResult$parameterValue <- rbind(saveIterationResult$parameterValue,newPar)
                saveIterationResult$objValueCali <- c(saveIterationResult$objValueCali, temp$objValueCali)
                saveIterationResult$objValueValid <- c(saveIterationResult$objValueValid, temp$objValueValid)
                saveIterationResult$perCriteriaCali <- bindList(saveIterationResult$perCriteriaCali,temp$perCriteriaCali)
                saveIterationResult$perCriteriaValid <- bindList(saveIterationResult$perCriteriaValid,temp$perCriteriaValid)
                saveIterationResult$simData <- bindList(saveIterationResult$simData,temp$simData)
              }

              if (parallelMode == 1) {
                # Take the better parameter set/data if exists
                for (j in 1:nrow(newPar)){
                  if(isNewSimBetter(temp$objValueCali[j],best$objValueCali[j], globalVariable$minOrmax)){
                    best$objValueCali[j] <- temp$objValueCali[j]
                    best$objValueValid[j] <- temp$objValueValid[j]
                  }

                }
              } else {
                idBest <- ilocMinMax(temp$objValueCali, globalVariable$minOrmax)
                if (length(idBest) > 1) {idBest = idBest[1]}

                if(isNewSimBetter(temp$objValueCali[idBest],best$objValueCali, globalVariable$minOrmax)){
                  best$objValueCali <- temp$objValueCali[idBest]
                  best$objValueValid <- temp$objValueValid[idBest]
                }
              }

              # print out intermediate results
              print(c(i, round(best$objValueCali, digits = 3)))

              # Assign back to global variables
              globalVariable$parameterValue <- saveIterationResult$parameterValue
              globalVariable$objValueCali <- saveIterationResult$objValueCali
              globalVariable$objValueValid <- saveIterationResult$objValueValid
              globalVariable$perCriteriaCali <- saveIterationResult$perCriteriaCali
              globalVariable$perCriteriaValid <- saveIterationResult$perCriteriaValid
              globalVariable$simData <- saveIterationResult$simData
              globalVariable$parameterValue[,1] <- c(1:nrow(globalVariable$parameterValue))

              #Save output to text file
              writeOutputFiles(globalVariable$workingFolder, globalVariable$ncores,
                               globalVariable$nOutputVar, globalVariable$simData)
            }

            # Update numeric input (threshold objective function)
            minObjValue <- min(globalVariable$objValueCali)
            maxObjValue <- max(globalVariable$objValueCali)
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

          } else if (globalVariable$samplingApproach %in% c('Sensi_(from_sensitivity_package)',
                                                            'Sensi_(from_userDefined_package)',
                                                            'Cali_(from_optimization_package)',
                                                            'Cali_(from_hydroPSO_package)',
                                                            'Cali_(from_nloptr_package)',
                                                            'Cali_(from_userDefined_package)')){

            if(is.null(globalVariable$observedData)){

              # Show message box if there is no observed data
              showModal(modalDialog(
                title = "Not enough information to perform SWAT run",
                span("Please defined objective function and load ",
                     "observed data (Step 4.1) before running SWAT"),
                easyClose = TRUE,
                size = "l"
              ))

            } else {
              # There is no simulated data and obj function values when the model has not been run
              globalVariable$simData <- NULL
              globalVariable$objValueCali <- NULL
              globalVariable$objValueValid <- NULL

              # First run is true
              globalVariable$copyUnchangeFiles <- TRUE
              globalVariable$firstRun <- TRUE

              globalVariable$sensCaliObject <- eval(parse(text = globalVariable$sensCaliCommand[1]))

              # Print output to screen
              print(globalVariable$sensCaliObject)
              print(globalVariable$objValueCali)

              # Update numeric input (threshold objective function)
              minObjValue <- min(globalVariable$objValueCali)
              maxObjValue <- max(globalVariable$objValueCali)
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

              # Update select variable number
              updateSliderInput(session = session,
                                "showSimResultsVariable",
                                "Select variable",
                                value = 1,
                                min = 1,
                                max = globalVariable$nOutputVar,
                                step = 1)
            }

          } else {
            # Show message box if there is no observed data
            showModal(modalDialog(
              title = "Not enough information to perform SWAT run",
              HTML("Unkown method"),
              easyClose = TRUE,
              size = "l"
            ))
          }

          # End run SWAT for all iterations
          globalVariable$checkSimComplete <- TRUE

          # Save all results as RSWAT project
          spsComps::shinyCatch(
            saveRDS(globalVariable, file = paste(globalVariable$workingFolder, '/',
                                                 'RSWATproject.rds', sep ='')),
            blocking_level = "error"
          )


        } else {

          # Show message box if there is no observed data
          showModal(modalDialog(
            title = "Input data/information is missing, please check again",
            easyClose = TRUE,
            size = "l"
          ))
        }
        # Update progress
        incProgress(1/nrow(globalVariable$parameterValue))
      })
    })



    # ****************************************************************************
    # See simulation report - Open file CurrentSimulationReport.log
    # ****************************************************************************
    observeEvent(input$checkCurrentSimulation, {
      print("ok30")
      if (!is.null(globalVariable$CurrentSimulationReportFile)){
        # Check if the CurrentSimulationReport.log file exists
        if (file.exists(globalVariable$CurrentSimulationReportFile)){

          # Read CurrentSimulationReport.log content
          fileContent <- readLines(globalVariable$CurrentSimulationReportFile, -1,
                                   warn = FALSE)

          # Display content of the CurrentSimulationReport.log
          printFileContent <- list()
          for (i in 1:length(fileContent)){
            printFileContent[[i]] <- fileContent[i]
          }
          output$tableCurrentSimulation <- renderUI({
            return(lapply(printFileContent, p))
          })
        }
      }
      print("ok31")
    })


    # ****************************************************************************
    # Display simulated results
    # ****************************************************************************
    observeEvent(input$checkSaveSimTocsv, {

      # Display observed data in table
      spsComps::shinyCatch(
        if (TRUE){
          # Get simulated data
          for (var in 1:globalVariable$nOutputVar){

            data <- c()
            for (core in 1:globalVariable$ncores){
              data <- rbind(data, read.csv(file = file.path(globalVariable$workingFolder,
                                                            "Output",
                                                            paste0("Core_", core),
                                                            paste0("out_var_", var, ".txt")),
                                           header = FALSE))
            }
            # Convert to data frame
            data <- matrix(data$V1, ncol = nrow(globalVariable$parameterValue), byrow = FALSE)
            colnames(data) <- paste0("sim_", data[1,])

            fileName <- file.path(globalVariable$workingFolder,
                                  paste0("sim_variable_", var, ".csv"))
            write.csv(data[-c(1),], file = fileName,
                      quote = FALSE, row.names = FALSE)

            message(paste0("Simulated output variable ", var, " was saved as: ", fileName))
          }
        },
        blocking_level = "error"
      )
    })


    # ****************************************************************************
    # Display parameter sets used for simulations
    # ****************************************************************************
    observeEvent(input$checkDisplayParameterSet, {
      print("ok32")
      # Check if the parameterValue exists
      if (!is.null(globalVariable$parameterValue)){

        # Column names, round up to 3 decimal digits
        colnames(globalVariable$parameterValue) = c("Simulation Nr.",
                                                    globalVariable$paraSelection$Parameter)
        # Display output table
        output$tableDisplayParameterSet <- renderDT(
          round(globalVariable$parameterValue, 3),
          server = FALSE,
          extensions = c("Buttons"),
          rownames = FALSE,
          options = list(dom = 'Bfrtip',
                         scroller = TRUE,
                         scrollX = TRUE,
                         searching = TRUE,
                         fixedHeader = TRUE,
                         buttons = c('csv', 'excel')))
      } else {

        # If there is no parameter, don't display anything
        output$tableDisplayParameterSet <- NULL
      }
      print("ok33")
    })



  })
}
