

# maximum upload file 500 MB
options(shiny.maxRequestSize = 500*1024^2)

# Creating server
server <- function(input, output, session) {

  # Stop the app when user close the browser
  session$onSessionEnded(function(){
    stopApp()
  })

  #-----------------------------------------------------------------------------
  # Global variables (use <<- for saving globalVariable inside observe)
  # These variables are set by default, when opening the tool, will be updated
  # according the user define in later steps
  #-----------------------------------------------------------------------------
  globalVariable <<- list()
  displayOutput <<- list()
  displayOutput$plotHru <<- FALSE
  displayOutput$plotSub <<- FALSE
  globalVariable$checkSimComplete <<- FALSE
  globalVariable$loadProject <<- FALSE
  globalVariable$TxtInOutSWAT <<- FALSE
  globalVariable$TxtInOutSWATPlus <<- FALSE
  globalVariable$SWATProject <<- TRUE
  globalVariable$paraSelection <<- dataParaSelectionSWAT
  globalVariable$HTMLdir <<- system.file("R-SWAT", package = "RSWAT")
  globalVariable$nCaliParam <<- nrow(globalVariable$paraSelection)
  globalVariable$runManualCaliSuccess <<- FALSE

  #-----------------------------------------------------------------------------
  # Global function for running SWAT
  #-----------------------------------------------------------------------------
  SWAT <- function(parameterValue){

    # Make sure that parameterValue is data frame or matrix
    if (is.matrix(parameterValue) |
        is.data.frame(parameterValue)){

      # Numbering the parameter set number and add it to the 1st column
      nrow <- nrow(parameterValue)
      parameterValue <- cbind(c(1:nrow), parameterValue)

    } else {
      # Convert input vector to matrix, add 1 to 1st column as the parameter set number
      parameterValue <- matrix(c(1,parameterValue), nrow = 1)
    }

    #Remove row and column names
    rownames(parameterValue) <- NULL
    colnames(parameterValue) <- NULL

    # Save parameter value to the globalVariable
    globalVariable$parameterValue <<- rbind(globalVariable$parameterValue, parameterValue)

    # Number of parallel runs cannot be higher than number of input parameter sets
    globalVariable$ncores <<- min(globalVariable$ncores, nrow(parameterValue))

    # Convert input parameter to matrix
    parameterValue <- as.matrix(parameterValue)

    # Run SWAT model
    runSWATpar(globalVariable$workingFolder,
               globalVariable$TxtInOutFolder,
               globalVariable$outputExtraction,
               globalVariable$ncores,
               globalVariable$SWATexeFile,
               parameterValue,
               globalVariable$paraSelection,
               globalVariable$caliParam,
               globalVariable$copyUnchangeFiles,
               globalVariable$fileCioInfo,
               globalVariable$dateRangeCali,
               globalVariable$firstRun)

    # Objective function with initial parameter set
    temp <- calObjFunction(parameterValue,
                           globalVariable$ncores,
                           globalVariable$nOutputVar,
                           globalVariable$userReadSwatOutput,
                           globalVariable$observedData,
                           globalVariable$workingFolder,
                           globalVariable$objFunction)

    # If this is a first run
    if (is.null(globalVariable$simData)){
      globalVariable$simData <<- temp$simData
      globalVariable$objValueCali <<- temp$objValueCali
      globalVariable$objValueValid <<- temp$objValueValid

    # If this is not the first run, then combine result with the existing data
    } else {
      globalVariable$simData <<- bindList(globalVariable$simData, temp$simData)
      globalVariable$objValueCali <<- c(globalVariable$objValueCali, temp$objValueCali)
      globalVariable$objValueValid <<- c(globalVariable$objValueValid, temp$objValueValid)
    }

   # Next run, no need to copy unchanged SWAT input files
   globalVariable$copyUnchangeFiles <<- FALSE

   # Parameter optimization: convert to the minimize problem, regardless of input
   if (globalVariable$minOrmax == "Minimize"){
     output <- temp$objValueCali
   } else {
     output <- - temp$objValueCali
   }

   # Return output
   return(output)
  }

  #-----------------------------------------------------------------------------
  # Save project setting
  #-----------------------------------------------------------------------------
  observeEvent(input$saveProject, {

    # Check if working directory exists
    if(dir.exists(globalVariable$workingFolder)){

      # Display message that save was done
      showNotification("Project settings were saved as 'RSWATproject.rds'
      in the working folder", type = "message", duration = 10)

      # Save project setting
      spsComps::shinyCatch(
        saveRDS(globalVariable, file = paste(globalVariable$workingFolder, '/',
                                             'RSWATproject.rds', sep ='')),
        blocking_level = "error"
      )

    } else {

      # Display error message
      showNotification("Working folder does not exist, please define in
                       1.General setting => 1. Working folder",
                       type = "error", duration = 10)
    }

  })

  #-----------------------------------------------------------------------------
  # Load project setting
  #-----------------------------------------------------------------------------
  observe({

    req(input$loadProject)

    # Get full link of the selected file
    shinyjs::disable("loadProject")
    spsComps::shinyCatch(RSWATProjectFile <- file.choose(), blocking_level = "none")
    shinyjs::enable("loadProject")

    spsComps::shinyCatch(
      if(substr(RSWATProjectFile, nchar(RSWATProjectFile) - 15,
                nchar(RSWATProjectFile)) == "RSWATproject.rds"){

        # Read data in this file
        globalVariable <<- readRDS(RSWATProjectFile)

        # Now the load project is true (because the RSWATproject.rds was given)
        globalVariable$loadProject <<- TRUE

        #-------------------------------------------------------------------------
        # Update Tab 1: General Setting
        #-------------------------------------------------------------------------

        # Update select SWAT project
        if(globalVariable$SWATProject){
          updateSelectInput(session,
                            "SWATorSWATplus",
                            label = "1. SWAT or SWAT+ project",
                            choices = c('SWAT', 'SWAT+'),
                            selected = "SWAT")
        } else {
          updateSelectInput(session,
                            "SWATorSWATplus",
                            label = "1. SWAT or SWAT+ project",
                            choices = c('SWAT', 'SWAT+'),
                            selected = "SWAT+")
        }


        # Update working folder
        updateTextInput(session, "workingFolder",
                        label = "2. Working folder",
                        value = globalVariable$workingFolder)

        # Update TxtInOut folder
        updateTextInput(session, "TxtInOutFolder",
                        label = "3. TxtInOut folder",
                        value = globalVariable$TxtInOutFolder)


        # Update Select executable SWAT file Help
        output$printSWATexe <- renderText(globalVariable$SWATexeFile)

        # Update Files with list of all SWAT parameters
        output$printSWATParamFile <- renderText(globalVariable$SWATParamFile)

        # Update display content of the SWAT parameter file
        output$tableSWATParam <- renderDataTable(globalVariable$SWATParam)

        #-------------------------------------------------------------------------
        # Update Tab 2: Parameter sampling
        #-------------------------------------------------------------------------

        # Update Select SWAT parameters for calibration and/or sensitivity analysis
        if(globalVariable$SWATProject){
          output$tableParaSelection <- excelR::renderExcel(
            excelR::excelTable(data = globalVariable$paraSelection,
                       columns = columnsParaSelectionSWAT,
                       editable = TRUE,
                       allowInsertRow = TRUE,
                       allowInsertColumn = TRUE,
                       allowDeleteColumn = TRUE,
                       allowDeleteRow = TRUE,
                       rowDrag = TRUE,
                       columnResize = FALSE,
                       wordWrap = TRUE)
          )
        } else {
          output$tableParaSelection <- excelR::renderExcel(
            excelR::excelTable(data = globalVariable$paraSelection,
                       columns = columnsParaSelectionSWATPlus,
                       editable = TRUE,
                       allowInsertRow = TRUE,
                       allowInsertColumn = TRUE,
                       allowDeleteColumn = TRUE,
                       allowDeleteRow = TRUE,
                       rowDrag = TRUE,
                       columnResize = FALSE,
                       wordWrap = TRUE)
          )
        }

        # Update Select sensitivity or calibration approach
        updateSelectInput(session,
                          "samplingApproach",
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
                          selected = globalVariable$samplingApproach)

        # Update Additional information about the selected sensitivity/calibration approach
        updateTextAreaInput(session,
                            "inputInfo",
                            "3. Additional infomation about the selected sensitivity/calibration approach",
                            globalVariable$sensCaliCommand)

        # Update define model output for extraction
        if (globalVariable$SWATProject){
          output$tableOutputExtraction <- excelR::renderExcel(
            excelR::excelTable(data = globalVariable$outputExtraction,
                       columns = columnsOutputExtractionSWAT,
                       editable = TRUE,
                       allowInsertRow = TRUE,
                       allowInsertColumn = FALSE,
                       allowDeleteColumn = FALSE,
                       allowDeleteRow = TRUE,
                       rowDrag = FALSE,
                       columnResize = FALSE,
                       wordWrap = TRUE)
          )
        } else {
          output$tableOutputExtraction <- excelR::renderExcel(
            excelR::excelTable(data = globalVariable$outputExtraction,
                       columns = columnsOutputExtractionSWATPlus,
                       editable = TRUE,
                       allowInsertRow = TRUE,
                       allowInsertColumn = FALSE,
                       allowDeleteColumn = FALSE,
                       allowDeleteRow = TRUE,
                       rowDrag = FALSE,
                       columnResize = FALSE,
                       wordWrap = TRUE)
          )
        }

        # Update display corresponding observed file names
        output$tableOutputExtractionDisplayOnly <- renderDataTable(
          printVariableNameObservedFiles(globalVariable$outputExtraction))

        # Update select date range
        updateDateRangeInput(session,
                             "dateRangeCali",
                             "2. Select date range",
                             start = globalVariable$dateRangeCali[1],
                             end   = globalVariable$dateRangeCali[2])

        # Update number of parallel runs
        updateSliderInput(session,
                          "ncores",
                          "3. Select number of parallel runs (cores)",
                          value = globalVariable$ncores,
                          min = 1,
                          max = parallel::detectCores())

        # Update objective function
        updateSelectInput(session,
                          "objFunction",
                          label = "1. Select objective function",
                          choices = c('NSE', 'KGE', 'R2', 'RMSE', 'aBIAS',
                                      'userObjFunction'),
                          selected = globalVariable$objFunction)

        # Update get observed data files
        output$printObservedDataFile <<- renderText(globalVariable$observedDataFile)

        # remove slider input when parameter selection was updated
        for (i in 2:30){
          removeUI(selector = paste0("div:has(> #parameter", i, ")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session)
        }

        # Min and max value of parameter 1
        minVal <- as.numeric(globalVariable$paraSelection$Min[1])
        maxVal <- as.numeric(globalVariable$paraSelection$Max[1])

        # Update slider input
        updateSliderInput(session,
                          inputId = "parameter1",
                          label = globalVariable$paraSelection$Parameter[1],
                          min = minVal,
                          max = maxVal,
                          value = minVal,
                          step = (maxVal - minVal)/50)


        # Update number of calibrated parameter
        globalVariable$nCaliParam <<- nrow(globalVariable$paraSelection)

        # Add slider input for other parameters
        if (nrow(globalVariable$paraSelection) > 1){
          lapply(1:(nrow(globalVariable$paraSelection)-1), FUN = function(i) {
            if (check_null_na_empty(globalVariable$paraSelection$Min[i+1]) &
                check_null_na_empty(globalVariable$paraSelection$Max[i+1])){

              # Min and max value
              minVal <- as.numeric(globalVariable$paraSelection$Min[i+1])
              maxVal <- as.numeric(globalVariable$paraSelection$Max[i+1])

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
        }

        # Show meesage
        showNotification("All project settings were loaded",
                         type = "message",
                         duration = 10)

      } else {
        # Show meesage
        showNotification("Error: Input file must be 'RSWATproject.rds'",
                         type = "error",
                         duration = 10)
      },
      blocking_level = "error")

    # Update number of plots in Manual calibration
    spsComps::shinyCatch(
      updateSelectInput(session,
                        "showPlotVariables",
                        label = "",
                        choices = paste0("variable ", c(1:globalVariable$nOutputVar)),
                        selected = 1),
      blocking_level = "error")
  })

  #-----------------------------------------------------------------------------
  # Tab 1. General Setting
  #-----------------------------------------------------------------------------
  # ****************************************************************************
  # Check SWAT or SWAT+ project
  # ****************************************************************************
  observe({
    req(input$SWATorSWATplus)

    # Check SWAT or SWAT+ project
    if (input$SWATorSWATplus == "SWAT"){
      globalVariable$SWATProject <<- TRUE
      globalVariable$SWATPlusProject <<- FALSE
    } else {
      globalVariable$SWATProject <<- FALSE
      globalVariable$SWATPlusProject <<- TRUE
    }

    # Initially there is no warning message
    output$checkTxtInOutFolder <- renderText(" ")

    # Check if the TxtInOut matches the SWAT project
    if (globalVariable$SWATPlusProject){
      if(globalVariable$TxtInOutSWAT){
        output$checkTxtInOutFolder <- renderText(
          "ERROR: This should be TxtInOut of SWAT+"
        )
      }
    } else {
      if(globalVariable$TxtInOutSWATPlus){
        output$checkTxtInOutFolder <- renderText(
          "ERROR: This should be TxtInOut of SWAT"
        )
      }
    }

    # ****************************************************************************
    # Select SWAT parameters for calibration and/or sensitivity: Default setting
    # ****************************************************************************
    if (!globalVariable$loadProject){

      # Update template for parameter and output extraction according to the SWAT project
      if (globalVariable$SWATProject){

        # Example of parameter selection for SWAT project
        output$tableParaSelection <-
          excelR::renderExcel(excelR::excelTable(data = dataParaSelectionSWAT,
                                 columns = columnsParaSelectionSWAT,
                                 editable = TRUE,
                                 allowInsertRow = TRUE,
                                 allowInsertColumn = TRUE,
                                 allowDeleteColumn = TRUE,
                                 allowDeleteRow = TRUE,
                                 rowDrag = TRUE,
                                 columnResize = FALSE,
                                 wordWrap = TRUE))

        # Example of output extraction for SWAT project
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

      # If this is SWAT+ project
      } else {

        # Example of parameter selection for SWAT+ project
        output$tableParaSelection <-
          excelR::renderExcel(excelR::excelTable(data = dataParaSelectionSWATPlus,
                                 columns = columnsParaSelectionSWATPlus,
                                 editable = TRUE,
                                 allowInsertRow = TRUE,
                                 allowInsertColumn = TRUE,
                                 allowDeleteColumn = TRUE,
                                 allowDeleteRow = TRUE,
                                 rowDrag = TRUE,
                                 columnResize = FALSE,
                                 wordWrap = TRUE))

        # Example of output extraction for SWAT+ project
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

        globalVariable$paraSelection <<- dataParaSelectionSWATPlus
      }

    }

  })

  # ****************************************************************************
  # Help button select SWAT or SWAT+ project
  # ****************************************************************************
  observe({
    req(input$helpSWATorSWATplus)

    showModal(modalDialog(
      title = "Help: 1. SWAT or SWAT+ project",
      "Please select SWAT or SWAT+ model, the graphical user interface will be
      changed after the selection",
      easyClose = TRUE
    ))

  })

  # ****************************************************************************
  # Get working folder
  # ****************************************************************************
  observe({
    req(input$workingFolder)

    # Save working directory in the global variable
    globalVariable$workingFolder <<- trimws(input$workingFolder)

    # Save link to the simulation report log file to the global variable
    globalVariable$CurrentSimulationReportFile <<- paste(
      globalVariable$workingFolder,
      '/Output/CurrentSimulationReport.log',
      sep =''
    )
    # Check if working folder exists
    if(!dir.exists(globalVariable$workingFolder)){

      # Print out message if working dir does not exist
      output$checkWorkingFolder <- renderText("Input folder does not exist")
    } else {

      # If exists, does not display anything
      output$checkWorkingFolder <- renderText(" ")
    }

  })

  # ****************************************************************************
  # Help button select working folder
  # ****************************************************************************
  observe({
    req(input$helpworkingFolder)

    showModal(modalDialog(
      title = "Help: 2. Working folder",
      "All files created by R-SWAT will be saved in this folder, for example,
      project setting file 'RSWATproject.rds' when you save your project,
      this file also can be used to load project settings from previous setup,
      simulation outputs, TxtInOut folders for parallel runs, etc...",
      easyClose = TRUE
    ))

  })

  # ****************************************************************************
  # TxtInOut folder: Display HRU info from TxtInOut folder
  # ****************************************************************************
  observe({
    req(input$TxtInOutFolder)

    # Check if TxtInOut folder exists
    if(!dir.exists(trimws(input$TxtInOutFolder))){
      output$checkTxtInOutFolder <- renderText("Input folder does not exist")
    } else {
      output$checkTxtInOutFolder <- renderText(" ")
    }

    # Check if .hru files exist in this folder
    if (checkDirFileExist(trimws(input$TxtInOutFolder), "", ".cio")){

      # Is this TxtInOut of SWAT or SWAT plus
      globalVariable$TxtInOutSWAT <<- checkSWATorSWATplus(trimws(input$TxtInOutFolder))$SWAT
      globalVariable$TxtInOutSWATPlus <<- checkSWATorSWATplus(trimws(input$TxtInOutFolder))$SWATPlus

      # Check if the TxtInOut matches the SWAT project
      if (globalVariable$SWATPlusProject){
        if(globalVariable$TxtInOutSWAT){
          output$checkTxtInOutFolder <- renderText(
            "ERROR: This should be TxtInOut of SWAT+"
          )
        }
      } else {
        if(globalVariable$TxtInOutSWATPlus){
          output$checkTxtInOutFolder <- renderText(
            "ERROR: This should be TxtInOut of SWAT"
          )
        }
      }

      # Save link to TxtInout Folder to the global variable
      globalVariable$TxtInOutFolder <<- trimws(input$TxtInOutFolder)

      # If this is a SWAT project
      if (globalVariable$SWATProject & globalVariable$TxtInOutSWAT){
        # Get HRU information (land use, slope, soil, sub)
        globalVariable$HRUinfo <<- getHruInfo(globalVariable$TxtInOutFolder)

        # Get unique soil, land use, slope, max number of subbasins
        uniqueSoil <- unique(globalVariable$HRUinfo$soil)
        uniqueLandUse <- unique(globalVariable$HRUinfo$lu)
        uniqueSlope <- unique(globalVariable$HRUinfo$slope)
        minMaxSubbasin <- range(globalVariable$HRUinfo$sub)

        # Number of soil, land use, slope
        nSoil <- length(uniqueSoil)
        nLU <- length(uniqueLandUse)
        nSlope <- length(uniqueSlope)

        nRow <- max(nSoil, nLU, nSlope, 2)


        # Display unique land use, soil, slope
        displayOutput$uniqueHruProperties <<- data.frame(
          minMaxSubbasin = c(minMaxSubbasin,rep(NA, nRow - 2)),
          Landuse = c(uniqueLandUse,rep(NA, nRow - nLU)),
          soilName = c(uniqueSoil,rep(NA, nRow - nSoil)),
          slopeClass = c(uniqueSlope,rep(NA, nRow - nSlope))
        )

      # If this is a SWAT+ project
      } else if (globalVariable$SWATPlusProject & globalVariable$TxtInOutSWATPlus){
        displayOutput$uniqueHruProperties <<- NULL
        globalVariable$HRUinfo <<- read.table(
          file = paste(globalVariable$TxtInOutFolder,
                       "/hru-data.hru", sep =""),
          header = TRUE, skip = 1, sep = ""
        )
        # Find maximum number of HRU


      } else {
        displayOutput$uniqueHruProperties <<- NULL
        globalVariable$HRUinfo <<- NULL
      }

      # Display table of HRU information (land use, soil, slope)
      output$tableHRUinfo <<- renderDataTable(globalVariable$HRUinfo)


    } else {

      # If this is not TxtInOut folder, assign global variables to NULL
      globalVariable$HRUinfo <<- NULL
      globalVariable$TxtInOutFolder <<- NULL
      output$tableHRUinfo <<- NULL
      displayOutput$uniqueHruProperties <<- NULL
    }

  })

  # ****************************************************************************
  # Help button select TxtInOut folder
  # ****************************************************************************
  observe({
    req(input$helpTxtInOutFolder)

    showModal(modalDialog(
      title = "Help: 3. TxtInOut folder",
      "Path to the TxtInOut directory which contains all original SWAT (or SWAT+)
      input files. These files will not be changed by R-SWAT",
      easyClose = TRUE
    ))
  })

  # ****************************************************************************
  # Get executable SWAT file
  # ****************************************************************************
  observe({

    req(input$getSWATexe)

    # Get full path to SWAT exe file
    shinyjs::disable("getSWATexe")
    spsComps::shinyCatch(globalVariable$SWATexeFile <<- file.choose(),
               blocking_level = "none")
    shinyjs::enable("getSWATexe")

    spsComps::shinyCatch(
      if (grepl(".exe", globalVariable$SWATexeFile, fixed = TRUE)){
        output$printSWATexe <- renderText(globalVariable$SWATexeFile)
      } else {
        output$printSWATexe <- renderText("Error: The selected file must have '.exe' extention")
      },
      blocking_level = "error")


  })


  # ****************************************************************************
  # Help button select executable SWAT
  # ****************************************************************************
  observe({
    req(input$helpgetSWATexe)

    showModal(modalDialog(
      title = "Help: 4. Select executable SWAT",
      "Select the executable SWAT or SWAT+ file, for example, swat_32debug.exe",
      easyClose = TRUE
    ))
  })

  # ****************************************************************************
  # Files with list of all SWAT parameters (get file) + display content of file
  # ****************************************************************************
  observe({

    req(input$getSWATParamFile)

    # Get full path to SWAT exe file
    shinyjs::disable("getSWATParamFile")
    spsComps::shinyCatch(globalVariable$SWATParamFile <<- file.choose(),
               blocking_level = "none")
    shinyjs::enable("getSWATParamFile")

    spsComps::shinyCatch(
    if (grepl("swatParam.txt", globalVariable$SWATParamFile, fixed = TRUE) |
        grepl("cal_parms.cal", globalVariable$SWATParamFile, fixed = TRUE)){

      globalVariable$SWATParam <<- loadSwatParam(globalVariable$SWATParamFile)
      output$printSWATParamFile <- renderText(globalVariable$SWATParamFile)
      output$tableSWATParam <- renderDataTable(globalVariable$SWATParam)

    } else {
      output$printSWATParamFile <- renderText(
        paste("Error: The selected file must be either 'swatParam.txt'",
                                                    "or 'cal_parms.cal'")
        )
    },
    blocking_level = "error")

  })

  # ****************************************************************************
  # Help button select file SWAT (or SWAT+) parameter file
  # ****************************************************************************
  observe({
    req(input$helpSWATparamFile)

    showModal(modalDialog(
      title = "Help: 5. File with list of SWAT or SWAT+ parameters",
      HTML(readLines(file.path(globalVariable$HTMLdir,"HTML",
	  "helpSWATparamFile.html"),warn=FALSE)),
      easyClose = TRUE
    ))
  })

  #-----------------------------------------------------------------------------
  # Tab 2. Parameter sampling
  #-----------------------------------------------------------------------------

  # ****************************************************************************
  # Display help for parameter selection
  # ****************************************************************************

  observe({

    # Requirement for activation these command
    req(input$helpParameterSelection)

    # This first if command prevents the app crashed when no input is given
    if (is.data.frame(globalVariable$SWATParam)){
      # Check if there is no input SWAT parameter file
      if (is.null(globalVariable$SWATParam$parameter)){
        output$tableHelpParameterSelection <-
          renderDataTable(displayOutput$uniqueHruProperties)
      } else {

        SWATParamName <- globalVariable$SWATParam$parameter
        nSWATParamName <- length(SWATParamName)

        if(is.null(displayOutput$uniqueHruProperties)){
          output$tableHelpParameterSelection <- NULL
        } else {
          nRowUniqueHRU <- nrow(displayOutput$uniqueHruProperties)
          compareLength <- max(nRowUniqueHRU, nSWATParamName)
          if (nRowUniqueHRU < compareLength){
            newRow <- data.frame(matrix(rep(NA, (compareLength - nRowUniqueHRU)*4),
                                        ncol = 4))
            names(newRow) <- names(displayOutput$uniqueHruProperties)
            tempUniqueHruProperties <- rbind(displayOutput$uniqueHruProperties,
                                             newRow)

          } else {
            SWATParamName <- c(SWATParamName, rep(NA, compareLength - nSWATParamName))
          }

          tempUniqueHruProperties <- cbind(SWATParamName,tempUniqueHruProperties)

          output$tableHelpParameterSelection <- renderDataTable(tempUniqueHruProperties)
        }

      }

      # If this is a SWAT+ project
      if (ncol(globalVariable$SWATParam) != 7){

        # Get SWAT+ parameter name
        temp <- paste(globalVariable$SWATParam[, 1], ".", globalVariable$SWATParam[, 2],
                      sep = "")
        # Display SWAT+ help table
        output$tableHelpParameterSelection <-
          renderDataTable(data.frame(Parameter = temp))
      }
    } else {
      # Display SWAT+ help table
      output$tableHelpParameterSelection <- NULL
    }
  })

  # ****************************************************************************
  # Help parameter selection
  # ****************************************************************************
  observe({
    req(input$helpParam)

    showModal(modalDialog(
      title = "Help: 1. Display help for parameter selection",
      "If you don't know the parameter, subbasin, land use, and slope names,
      check this box. We will get this information from the TxtInOut folder for you",
      easyClose = TRUE
    ))
  })

  # ****************************************************************************
  # Check input 'Select SWAT parameters for calibration'
  # ****************************************************************************
  observeEvent(input$checkParameterTableButton, {

    spsComps::shinyCatch(
      checkParameterTable <- checkSwatParameterName(globalVariable$paraSelection,
                                                    globalVariable$SWATParam,
                                                    globalVariable$HRUinfo,
                                                    globalVariable$SWATProject),
      blocking_level = "error"
    )

    output$checkParameterTableTxtOuput <- renderText(checkParameterTable$checkMessage)
  })

  # ****************************************************************************
  # Save "SWAT parameters for calibration" to global variable
  # ****************************************************************************
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
    globalVariable$paraSelection  <<- paraSelection
    globalVariable$paraSelection[,1] <<- trimws(paraSelection[,1])

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
    globalVariable$nCaliParam <<- nrow(globalVariable$paraSelection)

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
  observe({

    req(input$helpParamSelection)
    showModal(modalDialog(
      title = "Help: Parameter Selection",
	  HTML(readLines(file.path(globalVariable$HTMLdir,"HTML",
	  "helpParamSelection.html"),warn=FALSE)),
      easyClose = TRUE
    ))

  })
  # ****************************************************************************
  # Parameter sampling: Default setting
  # ****************************************************************************
  output$tableParaSampling <- excelR::renderExcel(excelR::excelTable(data = dataParaSampling,
                                                     columns = ColumnsParaSampling,
                                                     editable = TRUE,
                                                     allowInsertRow = FALSE,
                                                     allowInsertColumn = FALSE,
                                                     allowDeleteColumn = FALSE,
                                                     allowDeleteRow = FALSE,
                                                     rowDrag = FALSE,
                                                     columnResize = FALSE,
                                                     wordWrap = TRUE))

  # ****************************************************************************
  # Parameter sampling: Get user input for parameter sampling
  # ****************************************************************************
  observe({
    req(input$samplingApproach)

    # Save sampling approach to the global variable
    globalVariable$samplingApproach <<- input$samplingApproach

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
                          "   Leave one row empty at the end of the file", "\n",
                          " ", "\n",
                          "   As many column as the number of parameters","\n",
                          "   The order of parameters in these columns follows ",
                          "the order of the parameters in the above Table", "\n",
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
  observe({
    req(input$helpSelectingApproach)

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
      globalVariable$sensCaliCommand <<- input$inputInfo

      # Remove comments and split R command
      globalVariable$sensCaliCommand <<- splitRemoveComment(
        globalVariable$sensCaliCommand)
    } else {

      # If input is not R command, then just save as text
      globalVariable$sensCaliCommand <<- input$inputInfo
    }

    })

  # ****************************************************************************
  # Help: Additional infomation about the selected sensitivity/calibration
  # ****************************************************************************
  observe({
    req(input$helpAdditionalInfo)

    showModal(modalDialog(
      title = "Help: 3. Additional infomation for sensitivity/calibration",
      "Default input for each method is given, please modify the text if necessary
      (see help text below for modifying). Please see also Sections 2.1 and 2.2
      in the R-SWAT wiki page: https://github.com/tamnva/R-SWAT/wiki/R-SWAT-User-Manual",
      easyClose = TRUE
    ))
  })

  #-----------------------------------------------------------------------------
  # Tab 3. Run SWAT
  #-----------------------------------------------------------------------------

  # ****************************************************************************
  # Output extraction: Default setting
  # ****************************************************************************
  if(!globalVariable$loadProject){
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
    globalVariable$outputExtraction <<- outputExtraction

    # Number of output variables
    spsComps::shinyCatch(OutputVar <- getNumberOutputVar(outputExtraction),
               blocking_level = "error")
    globalVariable$nOutputVar <<- OutputVar$nOutputVar

    # Check if this option is activated
    globalVariable$userReadSwatOutput <<- OutputVar$userReadSwatOutput

    # Display table of observed file names needed for calibration/optimization
    output$tableOutputExtractionDisplayOnly <- renderDataTable(
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

  })

  # ****************************************************************************
  # Help output extraction
  # ****************************************************************************
  observe({

    req(input$helpOutputExtraction)

    showModal(modalDialog(
      title = "Help: Parameter Selection",
	  HTML(readLines(file.path(globalVariable$HTMLdir,"HTML",
	  "helpOutputExtraction.html"),warn=FALSE)),
      easyClose = TRUE
    ))

  })

  # ****************************************************************************
  # Update select input range based on file.cio in the TxtInOutFolder
  # ****************************************************************************
  observe({

    # Require the TxtInOut Folder input field
    req(input$TxtInOutFolder)

    # Check if there is a file called file.cio in this TxtInOut
    if (checkDirFileExist(trimws(input$TxtInOutFolder), "file.cio", "")){

      # Get simulation dates
      myDate <- getSimTime(trimws(input$TxtInOutFolder))

      # Assign simulation dates to the global variale
      globalVariable$fileCioInfo <<- myDate

      if (!globalVariable$loadProject){
        # Update selected date range for calibration/sensitivity
        updateDateRangeInput(session, "dateRangeCali",
                             start = myDate$startEval,
                             end = myDate$endSim,
                             min = myDate$startEval,
                             max = myDate$endSim)
      }
    }
  })

  # ****************************************************************************
  # Get user input range for calibration
  # ****************************************************************************
  observe({
    req(input$dateRangeCali)
    # Save selected date range for calibration/sensitivity to the global variables
    globalVariable$dateRangeCali <<- input$dateRangeCali
  })


  # ****************************************************************************
  # Help button select file SWAT (or SWAT+) parameter file
  # ****************************************************************************
  observe({
    req(input$helpDateRangeCali)

    showModal(modalDialog(
      title = "Help: 2. Select date range for calibration",
		HTML(readLines(file.path(globalVariable$HTMLdir,"HTML",
		"helpDateRangeCali.html"),warn=FALSE)),
      easyClose = TRUE
    ))
  })

  # ****************************************************************************
  # Get user input number of cores/threads
  # ****************************************************************************
  observe({
    req(input$ncores)
    # Assign the number of selected cores to the global variables
    globalVariable$ncores <<- input$ncores
  })

  # ****************************************************************************
  # Help button select file SWAT (or SWAT+) parameter file
  # ****************************************************************************
  observe({
    req(input$helpNumberofThreads)

    showModal(modalDialog(
      title = "Help: 3. Select number of parallel runs (threads)",
		HTML(readLines(file.path(globalVariable$HTMLdir,"HTML",
		"helpNumberofThreads.html"),warn=FALSE)),
      easyClose = TRUE
    ))
  })

  # ****************************************************************************
  # Run SWAT
  # ****************************************************************************
  observeEvent(input$runSWAT, {

    # Refresh the parameter values in case of loading the projects
    globalVariable$parameterValue <<- c()

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
          globalVariable$parameterValue <<- lhsRange(globalVariable$ncores,
                                                     getParamRange(globalVariable$paraSelection)),
                   blocking_level = "error"
          )
      }


      # Load all files that are going to be updated
      spsComps::shinyCatch(
        globalVariable$caliParam <<- updatedFileContent(globalVariable$HRUinfo,
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

          globalVariable$parameterValue <<- lhsRange(as.numeric(input$inputInfo), getParamRange(globalVariable$paraSelection))

        } else if(input$samplingApproach == 'Cali_(Generalized_Likelihood_Uncertainty_Estimation)'){
          globalVariable$parameterValue <<- runifSampling(as.numeric(input$inputInfo),
                                                          as.numeric(globalVariable$paraSelection$Min),
                                                          as.numeric(globalVariable$paraSelection$Max))

        } else if (globalVariable$samplingApproach == "Read_User_Parameter_File"){
          parameterValue <- as.matrix(read.table(file = trimws(input$inputInfo),
                                                 header = TRUE, sep =""))
          parameterValue <- cbind(c(1:nrow(parameterValue)),parameterValue)
          colnames(parameterValue) <- NULL
          rownames(parameterValue) <- NULL

          globalVariable$parameterValue <<- parameterValue
        } else {
          globalVariable$parameterValue <<- NULL
        }

        # Check max number of cores
        globalVariable$ncores <<- min(globalVariable$ncores, nrow(globalVariable$parameterValue))

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
                     firstRun),
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
                       firstRun),
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
            idBest <- which(temp$objValueCali == max(temp$objValueCali))
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
                if(temp$objValueCali[j] > best$objValueCali[j]){
                  globalVariable$parameterValue[j, ] <<- newPar[j, ]
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

              idBest <- which(temp$objValueCali == max(temp$objValueCali))
              if (length(idBest) > 1) {idBest = idBest[1]}

              if(temp$objValueCali[idBest] > best$objValueCali){
                globalVariable$parameterValue <<- newPar[idBest, ]
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
                         firstRun),
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
              if(temp$objValueCali[j] > best$objValueCali[j]){
                best$objValueCali[j] <- temp$objValueCali[j]
                best$objValueValid[j] <- temp$objValueValid[j]
              }
            }
          } else {
            idBest <- which(temp$objValueCali == max(temp$objValueCali))
            if (length(idBest) > 1) {idBest = idBest[1]}

            if(temp$objValueCali[idBest] > best$objValueCali){
              best$objValueCali <- temp$objValueCali[idBest]
              best$objValueValid <- temp$objValueValid[idBest]
            }
          }

          # print out intermediate results
          print(c(i, round(best$objValueCali, digits = 3)))

          # Assign back to global variables
          globalVariable$parameterValue <<- saveIterationResult$parameterValue
          globalVariable$objValueCali <<- saveIterationResult$objValueCali
          globalVariable$objValueValid <<- saveIterationResult$objValueValid
          globalVariable$perCriteriaCali <<- saveIterationResult$perCriteriaCali
          globalVariable$perCriteriaValid <<- saveIterationResult$perCriteriaValid
          globalVariable$simData <<- saveIterationResult$simData
          globalVariable$parameterValue[,1] <<- c(1:nrow(globalVariable$parameterValue))

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
          globalVariable$simData <<- NULL
          globalVariable$objValueCali <<- NULL
          globalVariable$objValueValid <<- NULL

          # First run is true
          globalVariable$copyUnchangeFiles <<- TRUE
          globalVariable$firstRun <<- TRUE

          globalVariable$sensCaliObject <<- eval(parse(text = globalVariable$sensCaliCommand[1]))

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
      globalVariable$checkSimComplete <<- TRUE

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
  observe({
    req(input$checkCurrentSimulation)

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
  })


  # ****************************************************************************
  # Display parameter sets used for simulations
  # ****************************************************************************
  observe({
    req(input$checkDisplayParameterSet)

    # Check if the parameterValue exists
    if (!is.null(globalVariable$parameterValue)){

      # Column names, round up to 3 decimal digits
      colnames(globalVariable$parameterValue) = c("Simulation Nr.",
                                                  globalVariable$paraSelection$Parameter)
      # Display output table
      output$tableDisplayParameterSet <- renderDataTable(
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
  })

  #-----------------------------------------------------------------------------
  # Tab 4. Evaluate output
  #-----------------------------------------------------------------------------
  # ****************************************************************************
  # 4.1. Get user input Objective function
  # ****************************************************************************
  observe({
    req(input$objFunction)

    # Get the type of objective function
    globalVariable$objFunction  <<- input$objFunction

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

  })

  # ****************************************************************************
  # Target min or max of the objective function
  # ****************************************************************************
  observe({
    req(input$minOrmax)

    # Assign the maximum and miminum objective fuction values to the global variables
    globalVariable$minOrmax  <<- input$minOrmax
  })

  # ****************************************************************************
  # Get executable SWAT file
  # ****************************************************************************
  observe({

    req(input$getUserObjFunction)

    # Get full path to SWAT exe file
    shinyjs::disable("getUserObjFunction")
    spsComps::shinyCatch(globalVariable$getUserObjFunction <<- file.choose(),
                         blocking_level = "none")
    shinyjs::enable("getUserObjFunction")

    spsComps::shinyCatch(
      if (grepl(".R", globalVariable$getUserObjFunction, fixed = TRUE)){
        output$userObjFunctionFile <- renderText(globalVariable$getUserObjFunction)
        source(globalVariable$getUserObjFunction)
      } else {
        output$userObjFunctionFile <- renderText("Error: The selected file must have '.R' extention")
      },
      blocking_level = "error")
  })

  # ****************************************************************************
  # Get observed data files
  # ****************************************************************************
  observe({

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
        globalVariable$observedDataFile <<- sortObservedDataFile(observedDataFile)

        # Display observed data file paths
        output$printObservedDataFile <- renderText(as.character(globalVariable$observedDataFile))



        # Observed data
        globalVariable$observedData <<- list()

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
                globalVariable$observedData[[i]] <<- temp
              }
            }
          }
        }

        # Display check message
        output$checkGetObservedDataFile <- renderText(checkGetObservedDataFileMessage)
      },
      blocking_level = "error")
  })

  # ****************************************************************************
  # Display list of input observed data files
  # ****************************************************************************
  observe({
    req(input$checkDisplayObsVar)

    # Display content of the observed data files
    observedData <- mergeDataFrameDiffRow(globalVariable$observedData)

    # Display observed data in table
    spsComps::shinyCatch(
      output$tableObsVarDisplay <- renderDataTable(observedData,
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
          globalVariable$objValueCali <<- temp$objValueCali
          globalVariable$perCriteriaCali <<- temp$perCriteriaCali
          globalVariable$objValueValid <<- temp$objValueValid
          globalVariable$perCriteriaValid <<- temp$perCriteriaValid
          globalVariable$simData <<- temp$simData

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


  })

  # ****************************************************************************
  # Calculate objective function: Display objective function values
  # ****************************************************************************
  observe({

    req(input$checkDisplayObjFunctionPlot)

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
  observe({
    req(input$checkDisplayObjFunction)

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
        output$tableCalObjFunction <- renderDataTable(tableParaObj,
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
  observe({
    req(input$ObjEachVar)

    spsComps::shinyCatch(
      tableParaObjEachVar <- objFunctionEachVarCaliValid(globalVariable$perCriteriaCali,
                                                         globalVariable$perCriteriaValid),
      blocking_level = "none")


    # Fill output tables with parameter and objective function values
    spsComps::shinyCatch(
      output$tableObjEachVar <- renderDataTable(tableParaObjEachVar,
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

  # 4.2. Sensitivity Analysis
  # ****************************************************************************
  # Display parameter sensitivity ranking: need to break this to smaller code
  # ****************************************************************************
  observeEvent(input$calSensitivity, {
    spsComps::shinyCatch(
    # check if all simluations are finised
    if (globalVariable$checkSimComplete & !is.null(globalVariable$objValueCali)){

      # Check which parameter sampling approach is used
      if (globalVariable$samplingApproach == 'Sensi_Cali_(uniform_Latin_Hypercube_Sampling)'){

        withProgress(message = 'Performing sensitivity analysis...', {

          # Table with parameter and objective function values
          tableSensitivity <- globalVariable$parameterValue
          tableSensitivity[,1] <- globalVariable$objValueCali

          # Column names
          colnames(tableSensitivity) <- c("objFunction", globalVariable$paraSelection[,1])
          tableSensitivity <- as.data.frame(tableSensitivity)

          # Parameter sensitivity using multivariate regression analysis
          tableSensitivity <- summary(lm(
              formula = objFunction ~ ., tableSensitivity))[4]$coefficients[,3:4]

          # Remove the first row because it is the intercept
          tableSensitivity <- tableSensitivity[-c(1),]

          # Assign result to the global variables

          tableSensitivity <- as.data.frame(tableSensitivity)

          # Creat data frame with the results from parameter sensi. analysis
          globalVariable$tableSensitivity <<- data.frame(Parameter = rownames(tableSensitivity),
                                                         t_stat = tableSensitivity[,1],
                                                         absolute_t_stat = abs(tableSensitivity[,1]),
                                                         p_value = tableSensitivity[,2])

          # Fill output table with values
          output$tableSensitivity <- renderDataTable(globalVariable$tableSensitivity,
                                                     server = FALSE,
                                                     extensions = c("Buttons"),
                                                     rownames = FALSE,
                                                     options = list(dom = 'Bfrtip',
                                                                    scroller = TRUE,
                                                                    scrollX = TRUE,
                                                                    searching = TRUE,
                                                                    fixedHeader = TRUE,
                                                                    buttons = c('csv', 'excel')))

          # Set 1 second delay for the progress bar
          Sys.sleep(1)
          incProgress(1/2)
        })

      # Check if users do sensi. analysis with R packages
      } else if (globalVariable$samplingApproach %in% c('Sensi_(from_sensitivity_package)',
                                                        'Sensi_(from_userDefined_package)')){

        # Text for the progress bar
        withProgress(message = 'Performing sensitivity analysis...', {

          # Prepare output table of sensitivity analysis
          sensCaliObject <- globalVariable$sensCaliObject
          sensiReport <- eval(parse(text = globalVariable$sensCaliCommand[2]))

          # Print to screen the result
          print(sensiReport)[]

          # Assign names for the table row
          if ("X1" %in% rownames(sensiReport)) {
            sensiReport <- cbind(parameters = globalVariable$paraSelection[,1],sensiReport)
          }

          # Save result table to the global variable
          globalVariable$tableSensitivity <<- sensiReport
          output$tableSensitivity <- renderDataTable(globalVariable$tableSensitivity)

          # Set 1 second delay for the progress bar
          Sys.sleep(1)
          incProgress(1/2)
        })

      } else if(globalVariable$samplingApproach == "Read_User_Parameter_File"){
        output$tableSensitivity <- NULL

        # Message show all input was saved
        showModal(modalDialog(
          title = "Sensitivity analysis",
          span("You used parameter set from the extermal file, ",
               "NO sensitivity analysis will be performed.",
               " Please use external program"),
          easyClose = TRUE,
          size = "l"
        ))

      } else {
        showModal(modalDialog(
          title = "Sensitivity analysis",
          span("The approach you selected in step 2 is not for parameter ",
               "sensitivity analysis - skip this step"),
          easyClose = TRUE,
          size = "l"
        ))

        # Set output = NUll in other cases
        output$tableSensitivity <- NULL
      }

    } else {

      # Display a message that all simulated hasn't been finished
      showModal(modalDialog(
        title = "Sensitivity analysis",
        HTML("Not all simulations are finished"),
        easyClose = TRUE,
        size = "l"
      ))
    },
    blocking_level = "none")
  })


  # 4.3. Optimization/Uncertainty
  # ****************************************************************************
  # Input behavioral threshold: Check if the input behavioral threshold is valid
  # ****************************************************************************
  observe({

    # By default the input behavioral threshold value is not valid
    globalVariable$isBehThresholdValid <<- FALSE

    # Check if the user-defined behavioral threshold is in a valid range
    spsComps::shinyCatch(
      if(!is.null(input$behThreshold)){
        if(!is.null(globalVariable$objValueCali)){

          if (!is.numeric(input$behThreshold)){
            output$printMaxBehThreshold <- renderText("please input numeric value")
          } else {
            if (input$behThreshold > max(globalVariable$objValueCali)){

              # Display check message
              output$printMaxBehThreshold <- renderText(paste("The selected value is ",
                                                              "greater than the maximum value ",
                                                              max(globalVariable$objValueCali),
                                                              sep =""))
            } else {
              # Display check message
              globalVariable$isBehThresholdValid <<- TRUE
              output$printMaxBehThreshold <- renderText("check threshold value OK")
            }
          }
        } else {
          output$printMaxBehThreshold <- NULL
        }
      } else {
        output$printMaxBehThreshold <- NULL
      },
      blocking_level = "error"
    )
  })

  # ****************************************************************************
  # Input variable number to plot: Calculate values of all tables for display
  # ****************************************************************************
  observe({
    req(input$checkPlotVariableNumber)

    # Check if there are parameter values and the selected behavioral threshold is valid
    if(!is.null(globalVariable$parameterValue) & globalVariable$isBehThresholdValid){

      # Find behavioral simulations - 95PPU
      spsComps::shinyCatch(
        globalVariable$dataPlotVariableNumber <<- behaSimulation(
          globalVariable$objValueCali,
          globalVariable$simData,
          globalVariable$parameterValue,
          input$behThreshold,
          input$plotVarNumber,
          globalVariable$objFunction,
          globalVariable$observedData,
          globalVariable$minOrmax,
          globalVariable$samplingApproach
        ),
        blocking_level = "error"
      )

      # Store 95PPU in a temporary variable
      tempVar <- globalVariable$dataPlotVariableNumber$ppuSimData
      tempVar <- cbind(tempVar, globalVariable$observedData[[input$plotVarNumber]]$Value)

      # Give column names for the 95PPU table
      colnames(tempVar) <- c("date", "lower", "median", "upper", "best", "observed")
      globalVariable$PlotVariableNumber <<- plotSimulated(tempVar)

      # Plot the 95PPU
      output$PlotVariableNumber <- plotly::renderPlotly(
        plotly::ggplotly(globalVariable$PlotVariableNumber +
                   ggplot2::theme(text = ggplot2::element_text(size=10)))
        )

      # Set format for the 95PPU table
      columnsTableBehaSim <- data.frame(title = c('Date','Lower 95PPU', 'Median',
                                                  'Upper 95PPU', 'Best Simulation'),
                                        source = rep(NA, 5),
                                        width = rep(300, 5),
                                        type = rep('numeric', 5))

      # Fill the 95PPU table with values
      output$tableBehaSim <- excelR::renderExcel(excelR::excelTable(
        data = globalVariable$dataPlotVariableNumber$ppuSimData,
        columns = columnsTableBehaSim,
        editable = FALSE,
        allowInsertRow = FALSE,
        allowInsertColumn = FALSE,
        allowDeleteColumn = FALSE,
        allowDeleteRow = FALSE,
        rowDrag = FALSE,
        columnResize = FALSE,
        wordWrap = FALSE))

      #Table behavioral parameter range
      columnsTableBehaParam <- data.frame(title = c('parameter', 'lower_95PPU',
                                                    'median','upper_95PPU',
                                                    'bestParameter'),
                                          source = rep(NA, 5),
                                          width = rep(300, 5),
                                          type = rep('numeric', 5))

      # Set format for the behavioral parameter range table
      output$tableBehaParam <- excelR::renderExcel(excelR::excelTable(
        data = cbind(globalVariable$paraSelection$Parameter,
                     globalVariable$dataPlotVariableNumber$ppuParaRange),
        columns = columnsTableBehaParam,
        editable = FALSE,
        allowInsertRow = FALSE,
        allowInsertColumn = FALSE,
        allowDeleteColumn = FALSE,
        allowDeleteRow = FALSE,
        rowDrag = FALSE,
        columnResize = FALSE,
        wordWrap = FALSE
        ))

      # Show p- and r-factor
      output$printPandRFactor <- renderText(
        paste("p-factor-cali(valid) = ", round(globalVariable$dataPlotVariableNumber$prFactorCali[1], 2),
                                         " (",
                                         round(globalVariable$dataPlotVariableNumber$prFactorValid[1], 2),
                                         ");",
              " r-factor-cali(valid) = ", round(globalVariable$dataPlotVariableNumber$prFactorCali[2], 2),
                                         " (",
                                         round(globalVariable$dataPlotVariableNumber$prFactorValid[2], 2),
                                         ")",
              sep =""))
    }

  })

  # ****************************************************************************
  # 5. R-SWAT Education
  # ****************************************************************************

  # ****************************************************************************
  # Default list of parameters for manual calibration
  # ****************************************************************************
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

  # Add slider input for other parameters
  spsComps::shinyCatch(
    if (nrow(globalVariable$paraSelection) > 1){
      lapply(1:(nrow(globalVariable$paraSelection)-1), FUN = function(i) {
        if (check_null_na_empty(globalVariable$paraSelection$Min[i+1]) &
            check_null_na_empty(globalVariable$paraSelection$Max[i+1])){

          # Min and max value
          minVal <- as.numeric(globalVariable$paraSelection$Min[i+1])
          maxVal <- as.numeric(globalVariable$paraSelection$Max[i+1])

          # Insert Slider input for other parameters
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


  # ****************************************************************************
  # Help button R-SWAT Education select parameter value
  # ****************************************************************************
  observe({
    req(input$helpSwatEduSelectParam)

    showModal(modalDialog(
      title = "1. Selecting parameter values",
		HTML(readLines(file.path(globalVariable$HTMLdir,"HTML",
		"selectParameterValue.html"),warn=FALSE)),
      easyClose = TRUE
    ))
  })

  # ****************************************************************************
  # Help button R-SWAT Education run model and model perforamnce
  # ****************************************************************************
  observe({
    req(input$helpModelRunPerf)

    showModal(modalDialog(
      title = "2. Model run",
	  HTML(readLines(file.path(globalVariable$HTMLdir,"HTML",
	  "modelRun.html"),warn=FALSE)),
      easyClose = TRUE
    ))

  })

  # ****************************************************************************
  # 5.2. Run SWAT for manual calibration
  # ****************************************************************************
  observeEvent(input$runSwatEdu, {
    spsComps::shinyCatch(
      for (ii in 1:1){

        # Load all files that are going to be updated
        globalVariable$caliParam <<- updatedFileContent(globalVariable$HRUinfo,
                                                                globalVariable$paraSelection,
                                                                globalVariable$SWATParam,
                                                                globalVariable$TxtInOutFolder)

        # Get prameter value
        selecParameterValue <- c()
        for (i in 1:nrow(globalVariable$paraSelection)){
          temp <- eval(parse(text = "paste0('input$parameter', i)"))
          selecParameterValue <- c(selecParameterValue,
                                   eval(parse(text = temp)))
        }
        parameterValue <- c(1, selecParameterValue)

        # Copy unchange file
        createDirCopyUnchangeFile(globalVariable$workingFolder,
                                  1,
                                  globalVariable$TxtInOutFolder,
                                  globalVariable$caliParam$file,
                                  globalVariable$SWATexeFile,
                                  TRUE)

        # Run SWAT
        withProgress(message = 'SWAT is running', {
        runSWATSequential(1,
                          globalVariable$workingFolder,
                          globalVariable$SWATexeFile,
                          globalVariable$caliParam,
                          globalVariable$paraSelection,
                          parameterValue,
                          globalVariable$outputExtraction,
                          globalVariable$fileCioInfo,
                          globalVariable$dateRangeCali,
                          TRUE)
          incProgress(1/2)
        })

        # Read output data
        globalVariable$SwatEduSimData <<- list()
        for (i in 1:globalVariable$nOutputVar){
          globalVariable$SwatEduSimData[[i]] <<- read.csv(file = paste0(globalVariable$workingFolder,
                                         "/Output/Core_1/out_var_", i, ".txt"),
                           header = TRUE)[,1]
        }

        # Calculate objective function
        temp <- calObjFunction(as.data.frame(0),
                               1,
                               globalVariable$nOutputVar,
                               globalVariable$userReadSwatOutput,
                               globalVariable$observedData,
                               globalVariable$workingFolder,
                               globalVariable$objFunction)

        #Update model performance
        rnames <- c()
        for (i in 1:globalVariable$nOutputVar){
          if (i == 1) {
            swatEduModelPerf <- temp$perCriteriaCali[[i]][[1]]
          } else {
            swatEduModelPerf <- rbind(swatEduModelPerf, temp$perCriteriaCali[[i]][[1]])
          }
          swatEduModelPerf <- rbind(swatEduModelPerf, temp$perCriteriaValid[[i]][[1]])
          rnames <- c(rnames, paste0("cali_var_", i), paste0("valid_var_", i))
        }
        rownames(swatEduModelPerf) <- rnames

        #not include column 4 and 5 => table too long - TODO improve GUI later
        output$swatEduModelPerf <- renderTable({swatEduModelPerf[,-c(4,5)]},
                                               rownames = TRUE,
                                               width = "100%",
                                               digits = 2)

        # convert to ggplotly with legend on top
        lapply(1:globalVariable$nOutputVar, function(i) {
          temp_plot <- plotly::ggplotly(plotSwatEdu(globalVariable, i))
          outID <- paste0("SwatEduPlot", i)
          output[[outID]] <- plotly::renderPlotly(temp_plot)
        })

        globalVariable$runManualCaliSuccess <<- TRUE

        # Get water balance and nutrient balance
        if (globalVariable$SWATProject){
          output_std <- paste0(globalVariable$workingFolder, "/TxtInOut_1/output.std")
          output_std <- read_output_std(output_std)
          output$swatEduWaterBalance <- renderDataTable(output_std$waterbalance,
                                                        options = list(scrollX = TRUE, searching = FALSE))
          output$swatEduNutrientBalance <- renderDataTable(output_std$nutrientblance,
                                                           options = list(scrollX = TRUE, searching = FALSE))
        } else {
          # TODO
        }

      },
      blocking_level = "error"
    )
  })

  # ****************************************************************************
  # Plot simulated variables
  # ****************************************************************************
  observe({

    req(input$showPlotVariables)
    print("THIS was run")
    if (is.null(input$showPlotVariables)) temp <- "variable 1"

    if(globalVariable$runManualCaliSuccess){

      # Convert string to numeric
      temp <- input$showPlotVariables
      temp <- as.numeric(gsub("variable", "", temp))
      temp <- sort(temp, decreasing = FALSE)

      lapply(1:20, function(i) {
        outputId <- paste0("SwatEduPlot", i)
        output[[outputId]] <- NULL
      })

      lapply(1:length(temp), function(i) {
        temp_plot <-  plotly::ggplotly(plotSwatEdu(globalVariable, temp[i]))
        outputId <- paste0("SwatEduPlot", i)
        output[[outputId]] <- plotly::renderPlotly(temp_plot)
      })

    }
  })
  #-----------------------------------------------------------------------------
}
