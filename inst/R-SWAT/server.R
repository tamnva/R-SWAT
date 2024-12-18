

# maximum upload file 500 MB
options(shiny.maxRequestSize = 500*1024^2)

# Creating server
server <- function(input, output, session) {

  # Stop the app when user close the browser
  session$onSessionEnded(function(){
    stopApp()
  })

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
    globalVariable$parameterValue <- rbind(globalVariable$parameterValue, parameterValue)

    # Number of parallel runs cannot be higher than number of input parameter sets
    globalVariable$ncores <- min(globalVariable$ncores, nrow(parameterValue))

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
               globalVariable$firstRun,
               globalVariable$readOutputScript)

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
      globalVariable$simData <- temp$simData
      globalVariable$objValueCali <- temp$objValueCali
      globalVariable$objValueValid <- temp$objValueValid

    # If this is not the first run, then combine result with the existing data
    } else {
      globalVariable$simData <- bindList(globalVariable$simData, temp$simData)
      globalVariable$objValueCali <- c(globalVariable$objValueCali, temp$objValueCali)
      globalVariable$objValueValid <- c(globalVariable$objValueValid, temp$objValueValid)
    }

   # Next run, no need to copy unchanged SWAT input files
   globalVariable$copyUnchangeFiles <- FALSE

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
  observeEvent(input$loadProject, {

    # Get full link of the selected file
    shinyjs::disable("loadProject")
    spsComps::shinyCatch(RSWATProjectFile <- file.choose(), blocking_level = "none")
    shinyjs::enable("loadProject")

    spsComps::shinyCatch(
      if(substr(RSWATProjectFile, nchar(RSWATProjectFile) - 15,
                nchar(RSWATProjectFile)) == "RSWATproject.rds"){

        # Read data in this file
        globalVariable <- readRDS(RSWATProjectFile)

        # Now the load project is true (because the RSWATproject.rds was given)
        globalVariable$loadProject <- TRUE

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
        output$tableSWATParam <- renderDT(globalVariable$SWATParam)

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
        output$tableOutputExtractionDisplayOnly <- renderDT(
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
        output$printObservedDataFile <- renderText(globalVariable$observedDataFile)

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
        globalVariable$nCaliParam <- nrow(globalVariable$paraSelection)

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

        # update output extraction reminder text
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
  # Tab 1. Parameter sampling
  #-----------------------------------------------------------------------------
  generalSettingServer("general_setting_id")

  #-----------------------------------------------------------------------------
  # Tab 2. Parameter sampling
  #-----------------------------------------------------------------------------
  paramSamplingServer("param_sampling_id")

  #-----------------------------------------------------------------------------
  # Tab 3. Run SWAT
  #-----------------------------------------------------------------------------
  runSWATServer("run_swat_id")

  #-----------------------------------------------------------------------------
  # Tab 4. Evaluate output
  #-----------------------------------------------------------------------------
  objFunctionServer("obj_function_id")
  sensAnalysisServer("sens_analysis_id")
  paraOptUncerServer("para_opt_uncer_id")
  # ****************************************************************************
  # 5. R-SWAT Education
  # ****************************************************************************
  swatEduServer("swat_edu_id")
  #-----------------------------------------------------------------------------
}
