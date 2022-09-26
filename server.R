
# maximum upload file 500 MB
options(shiny.maxRequestSize = 500*1024^2)

# Creating server
server <- function(input, output, session) {

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

  #-----------------------------------------------------------------------------
  # Global function for running SWAT
  #-----------------------------------------------------------------------------
  SWAT <- function(parameterValue){
    print(parameterValue)
    # Make sure that parameterValue is data frame or matrix
    if (is.matrix(parameterValue) |
        is.data.frame(parameterValue)){

      # Numbering the parameter set number and add to the 1st column
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

    # If this is not the first run, then combine result to the existing data
    } else {
      globalVariable$simData <<- bindList(globalVariable$simData, temp$simData)
      globalVariable$objValueCali <<- c(globalVariable$objValueCali, temp$objValueCali)
      globalVariable$objValueValid <<- c(globalVariable$objValueValid, temp$objValueValid)
    }

   # Next run, don't need to copy unchanged SWAT input files
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
      shinyCatch(
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
    shinyCatch(RSWATProjectFile <- file.choose(), blocking_level = "none")
    shinyjs::enable("loadProject")

    shinyCatch( 
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
                        label = "1. Working folder",
                        value = globalVariable$workingFolder)
        
        # Update TxtInOut folder
        updateTextInput(session, "TxtInOutFolder",
                        label = "2. TxtInOut folder",
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
          output$tableParaSelection <- renderExcel(
            excelTable(data = globalVariable$paraSelection,
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
          output$tableParaSelection <- renderExcel(
            excelTable(data = globalVariable$paraSelection,
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
                                      'Cali_(from_hydroPSO_package)',
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
          output$tableOutputExtraction <- renderExcel(
            excelTable(data = globalVariable$outputExtraction,
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
          output$tableOutputExtraction <- renderExcel(
            excelTable(data = globalVariable$outputExtraction,
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
                          max = detectCores())
        
        # Update objective function
        updateSelectInput(session,
                          "objFunction",
                          label = "1. Select objective function",
                          choices = c('NSE', 'KGE', 'R2', 'RMSE', 'aBIAS',
                                      'userObjFunction'),
                          selected = globalVariable$objFunction)
        
        # Update get observed data files
        output$printObservedDataFile <<- renderText(globalVariable$observedDataFile)
        
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
    
    # Check if the TxtInOut matches the SWAT project
    if (globalVariable$TxtInOutSWAT & globalVariable$SWATPlusProject){
      output$checkTxtInOutFolder <- renderText(
        "ERROR: This should be TxtInOut of SWAT "
        ) 
    } else if (globalVariable$TxtInOutSWAT & globalVariable$SWATProject){
      output$checkTxtInOutFolder <- renderText(" ") 
    } else {
      
    }

    # Check if the TxtInOut matches the SWAT+ project    
    if (globalVariable$TxtInOutSWATPlus & globalVariable$SWATProject){
      output$checkTxtInOutFolder <- renderText(
        "ERROR: This should be TxtInOut of SWAT+ "
        ) 
    } else if (globalVariable$TxtInOutSWATPlus & globalVariable$SWATPlusProject){
      output$checkTxtInOutFolder <- renderText(" ") 
    } else {
      
    }

    # ****************************************************************************
    # Select SWAT parameters to calibration and/or sensitivity: Default setting
    # ****************************************************************************
    if (!globalVariable$loadProject){
      
      # Update template for parameter and output extraction according to the SWAT project
      if (globalVariable$SWATProject){
        
        # Example of parameter selection for SWAT project
        output$tableParaSelection <-
          renderExcel(excelTable(data = dataParaSelectionSWAT,
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
          renderExcel(excelTable(data = dataOutputExtractionSWAT,
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
          renderExcel(excelTable(data = dataParaSelectionSWATPlus,
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
          renderExcel(excelTable(data = dataOutputExtractionSWATPlus,
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
      
      # check if the TxtInOut is correct
      if (globalVariable$TxtInOutSWAT & globalVariable$SWATPlusProject){
        output$checkTxtInOutFolder <- renderText(
          "ERROR: This should be TxtInOut of SWAT "
          ) 
      }

      if (globalVariable$TxtInOutSWATPlus & globalVariable$SWATProject){
        output$checkTxtInOutFolder <- renderText(
          "ERROR: This should be TxtInOut of SWAT+ "
          ) 
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
  # Get executable SWAT file
  # ****************************************************************************
  observe({

    req(input$getSWATexe)
    
    # Get full path to SWAT exe file
    shinyjs::disable("getSWATexe")
    shinyCatch(globalVariable$SWATexeFile <<- file.choose(), 
               blocking_level = "none")
    shinyjs::enable("getSWATexe")
    
    shinyCatch(
      if (grepl(".exe", globalVariable$SWATexeFile, fixed = TRUE)){
        output$printSWATexe <- renderText(globalVariable$SWATexeFile)      
      } else {
        output$printSWATexe <- renderText("Error: The selected file must have '.exe' extention")
      },
      blocking_level = "error")

    
  })

  # ****************************************************************************
  # Files with list of all SWAT parameters (get file) + display content of file
  # ****************************************************************************
  observe({
    
    req(input$getSWATParamFile)
    
    # Get full path to SWAT exe file
    shinyjs::disable("getSWATParamFile")
    shinyCatch(globalVariable$SWATParamFile <<- file.choose(), 
               blocking_level = "none")
    shinyjs::enable("getSWATParamFile")
    
    shinyCatch(
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
  # Check input 'Select SWAT parameters for calibration'
  # ****************************************************************************
  observeEvent(input$checkParameterTableButton, {
    
    shinyCatch(
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
    paraSelection <-  excel_to_R(input$tableParaSelection)
    
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

  })

  # ****************************************************************************
  # Parameter sampling: Default setting
  # ****************************************************************************
  output$tableParaSampling <- renderExcel(excelTable(data = dataParaSampling,
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
                          paste("optim_sa(fun = SWAT, start = c(runif(nParam,", 
                          "min = minCol, max = maxCol)), lower = minCol,",
                          "upper = maxCol, trace = TRUE, control = list(t0 = 10,",
                          "nlimit = 5,t_min = 0.1, dyn_rf = FALSE,rf = 1,r = 0.7)))"))
      outputText <- helpTextCali
      
    # From hydroPSO package
    } else if (input$samplingApproach == 'Cali_(from_hydroPSO_package)'){
      
      updateTextAreaInput(session, "inputInfo", 
                          paste("3. Additional infomation about the selected",
                          "sensitivity/calibration approach"),
                          paste("hydroPSO(fn = SWAT, lower=minCol, upper=maxCol,",
                          "control=list(maxit=1))"))
      outputText <- helpTextCali
      
    } else if (input$samplingApproach == 'Cali_(from_nloptr_package)'){
      
      updateTextAreaInput(session, "inputInfo", 
                          paste("3. Additional infomation about the selected",
                          "sensitivity/calibration approach"),
                          paste("bobyqa(runif(nParam, min = minCol, max = maxCol),",
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
                          paste("sensCaliObject <-  morris(model = SWAT, factors = ",
                          "nParam, binf = minCol, bsup = maxCol, r = 4, design = ",
                          "list(type = 'oat', levels = 5, grid.jump = 3)) \n",
                          "print(sensCaliObject)[]", sep = ""))
      outputText <- helpTextSensi
    }
    output$displayInputInfo <- renderText(outputText)
  })

  # ****************************************************************************
  # Parameter sampling: get input information
  # ****************************************************************************
  observe({
    req(input$inputInfo)

    # Check if user need to input R command
    if (input$samplingApproach == 'Sensi_(from_sensitivity_package)' |
        input$samplingApproach == 'Cali_(from_optimization_package)' |
        input$samplingApproach == 'Cali_(from_hydroPSO_package)' |
        input$samplingApproach == 'Cali_(from_nloptr_package)' |
        input$samplingApproach == 'Sensi_(from_userDefined_package)' |
        input$samplingApproach == 'Cali_(from_userDefined_package)' ){

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

  #-----------------------------------------------------------------------------
  # Tab 3. Run SWAT
  #-----------------------------------------------------------------------------

  # ****************************************************************************
  # Output extraction: Default setting
  # ****************************************************************************
  if(!globalVariable$loadProject){
    if (globalVariable$SWATProject){
      output$tableOutputExtraction <- 
        renderExcel(excelTable(data = dataOutputExtractionSWAT,
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
        renderExcel(excelTable(data = dataOutputExtractionSWATPlus,
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
    outputExtraction <- excel_to_R(input$tableOutputExtraction)

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
    shinyCatch(OutputVar <- getNumberOutputVar(outputExtraction), 
               blocking_level = "error")
    globalVariable$nOutputVar <<- OutputVar$nOutputVar

    # Check if this option is activated
    globalVariable$userReadSwatOutput <<- OutputVar$userReadSwatOutput

    # Display table of observed file names needed for calibration/optimization
    output$tableOutputExtractionDisplayOnly <- renderDataTable(
      printVariableNameObservedFiles(outputExtraction)
    )

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
      myDate <- getFileCioInfo(trimws(input$TxtInOutFolder))
      
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
  # Get user input number of cores
  # ****************************************************************************
  observe({
    req(input$ncores)
    # Assign the number of selected cores to the global variables
    globalVariable$ncores <<- input$ncores
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
        shinyCatch(
          globalVariable$parameterValue <<- lhsRange(globalVariable$ncores,
                                                     getParamRange(globalVariable$paraSelection)), 
                   blocking_level = "error"
          )
      }

      
      # Load all files that are going to be updated
      shinyCatch(
        globalVariable$caliParam <<- loadParamChangeFileContent(globalVariable$HRUinfo,
                                                                globalVariable$paraSelection,
                                                                globalVariable$SWATParam,
                                                                globalVariable$TxtInOutFolder), 
        blocking_level = "error"
      )
        
      
      # Save global variables
      shinyCatch(
        saveRDS(globalVariable, file = paste(globalVariable$workingFolder, '/', 
                                             'RSWATproject.rds', sep ='')), 
        blocking_level = "error"
      )
      
      # Copy unchanged file for the first simulation
      copyUnchangeFiles <- TRUE
      firstRun <- TRUE

      # Run SWAT for all iteration ---------------------------------------------
      if ((globalVariable$samplingApproach == 'Sensi_Cali_(uniform_Latin_Hypercube_Sampling)') |
          (globalVariable$samplingApproach == 'Cali_(Generalized_Likelihood_Uncertainty_Estimation)') |
        (globalVariable$samplingApproach == 'Read_User_Parameter_File')){

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
        shinyCatch(
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
          shinyCatch(
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
          shinyCatch(
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
            best$simData <- temp$simData
          } else {
            idBest <- which(temp$objValueCali == max(temp$objValueCali))
            if (length(idBest) > 1) {idBest = idBest[1]}

            # Take the better parameter value and objective function values
            best$parameterValue <- best$parameterValue[idBest, ]
            best$objValueCali <- temp$objValueCali[idBest]
          }

          # Loop over number of iteration
          for (i in 1:nIters){

            if (i == 1){
              # Save iteration result
              saveIterationResult$parameterValue <- globalVariable$parameterValue
              saveIterationResult$objValueCali <- temp$objValueCali
              saveIterationResult$perCriteriaCali <- temp$perCriteriaCali
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
            shinyCatch(
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
            shinyCatch(
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
            saveIterationResult$perCriteriaCali <- bindList(saveIterationResult$perCriteriaCali,temp$perCriteriaCali)
            saveIterationResult$simData <- bindList(saveIterationResult$simData,temp$simData)
          }

          if (parallelMode == 1) {
            # Take the better parameter set/data if exists
            for (j in 1:nrow(newPar)){
              if(temp$objValueCali[j] > best$objValueCali[j]){
                best$objValueCali[j] <- temp$objValueCali[j]
              }
            }
          } else {
            idBest <- which(temp$objValueCali == max(temp$objValueCali))
            if (length(idBest) > 1) {idBest = idBest[1]}

            if(temp$objValueCali[idBest] > best$objValueCali){
              best$objValueCali <- temp$objValueCali[idBest]
            }
          }

          # print out intermediate results
          print(c(i, round(best$objValueCali, digits = 3)))

          # Assign back to global variables
          globalVariable$parameterValue <<- saveIterationResult$parameterValue
          globalVariable$objValueCali <<- saveIterationResult$objValueCali
          globalVariable$perCriteriaCali <<- saveIterationResult$perCriteriaCali
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

      } else if (globalVariable$samplingApproach == 'Sensi_(from_sensitivity_package)' |
                 globalVariable$samplingApproach == 'Sensi_(from_userDefined_package)' |
                 globalVariable$samplingApproach == 'Cali_(from_optimization_package)' |
                 globalVariable$samplingApproach == 'Cali_(from_hydroPSO_package)' |
                 globalVariable$samplingApproach == 'Cali_(from_nloptr_package)' |
                 globalVariable$samplingApproach == 'Cali_(from_userDefined_package)') {

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
      shinyCatch(
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

      # Collumn setting of the parameter value table
      columnsParameterValue <- data.frame(
        title = c("Simulation Nr.", globalVariable$paraSelection$Parameter),
        source = rep(NA, ncol(globalVariable$parameterValue)),
        width = rep(300, ncol(globalVariable$parameterValue)),
        type = rep('numeric', ncol(globalVariable$parameterValue))
        )

      # Fill the parameter table with content (parameter values)
      output$tableDisplayParameterSet <- renderExcel(
        excelTable(data = globalVariable$parameterValue,
                   columns = columnsParameterValue,
                   editable = FALSE,
                   allowInsertRow = FALSE,
                   allowInsertColumn = FALSE,
                   allowDeleteColumn = FALSE,
                   allowDeleteRow = FALSE,
                   rowDrag = FALSE,
                   columnResize = FALSE,
                   wordWrap = FALSE)
        )
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
    if (input$objFunction == 'NSE' | input$objFunction == 'KGE' |
        input$objFunction == 'R2'){
      updateSelectInput(session, 'minOrmax',
                        label = 'Minimize or maximize the objective function?',
                        selected = 'Maximize')
    } else if (input$objFunction == 'aBIAS' | input$objFunction == 'RMSE'){
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
    shinyCatch(
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
    output$tableObsVarDisplay <- renderDataTable(
      mergeDataFrameDiffRow(globalVariable$observedData)
      )
  })

  # ****************************************************************************
  # Calculate objective function values
  # ****************************************************************************
  observeEvent(input$calObjFunction, {

    # Check if all simulations are finished
    if (globalVariable$checkSimComplete){

      # check which parameter sampling approach is selected
      if (globalVariable$samplingApproach == 'Cali_(Dynamically_Dimensioned_Search)' |
          globalVariable$samplingApproach == 'Cali_(from_userDefined_package)'  |
          globalVariable$samplingApproach == 'Cali_(from_optimization_package)' |
          globalVariable$samplingApproach == 'Cali_(from_hydroPSO_package)' |
          globalVariable$samplingApproach == 'Cali_(from_nloptr_package)' |
          globalVariable$samplingApproach == 'Sensi_(from_sensitivity_package)' |
          globalVariable$samplingApproach == 'Sensi_(from_userDefined_package)' ){

        # Display message
        showModal(modalDialog(
          title = "Important message",
          "The objective function was/will be calculated after each model run (Step 3)",
          easyClose = TRUE,
          size = "l"
        ))

      # If the below approaches are selected, then calculate objective function
      } else if (globalVariable$samplingApproach == 'Sensi_Cali_(uniform_Latin_Hypercube_Sampling)'|
                 globalVariable$samplingApproach == 'Cali_(Generalized_Likelihood_Uncertainty_Estimation)'|
                 globalVariable$samplingApproach == 'Read_User_Parameter_File'){

        withProgress(message = 'Calculating objective function...', {

          # Calculate objective function
          output$printCalObjFunction <- NULL

          # Objective function in the intermediate step
          shinyCatch(
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

    # Plot parameter values and objective function
    if(!is.null(globalVariable$parameterValue) & !is.null(globalVariable$objValueCali)){
      output$plotObjFunction <- renderPlotly(plotObjFuncParaValue(globalVariable))
    } else {
      output$plotObjFunction <- NULL
    }
  })

  # ****************************************************************************
  # Calculate objective function: Display objective function values
  # ****************************************************************************
  observe({
    req(input$checkDisplayObjFunction)
    # Get parameter values

    if(!is.null(globalVariable$parameterValue) & !is.null(globalVariable$objValueCali)){
      tableParaObj <- globalVariable$parameterValue

      # Replace the first row as it is the simulation number with the obj function value
      tableParaObj <- cbind(tableParaObj[,1], globalVariable$objValueCali, 
                            globalVariable$objValueValid, tableParaObj[,-c(1)])
      tableParaObj <- as.data.frame(tableParaObj)

      colnames(tableParaObj) <- c("SimNr", "objCalibration", "objValidation", globalVariable$paraSelection$Parameter)

      # round up to 3 decimal digits
      is.num <- sapply(tableParaObj, is.numeric)
      tableParaObj[is.num] <- lapply(tableParaObj[is.num], round, 3)

      # Visual setting for the output table
      columnsCalObjFunction <- data.frame(title = colnames(tableParaObj),
                                          source = rep(NA, ncol(tableParaObj)),
                                          width = rep(300, ncol(tableParaObj)),
                                          type = rep('text', ncol(tableParaObj)))

      # Fill output tables with parameter and objective function values
      output$tableCalObjFunction <- renderExcel(excelTable(data = tableParaObj,
                                                           columns = columnsCalObjFunction,
                                                           editable = FALSE,
                                                           allowInsertRow = FALSE,
                                                           allowInsertColumn = FALSE,
                                                           allowDeleteColumn = FALSE,
                                                           allowDeleteRow = FALSE,
                                                           rowDrag = FALSE,
                                                           columnResize = FALSE,
                                                           wordWrap = FALSE))
    } else {
      output$tableCalObjFunction <- NULL
    }
  })

  # 4.2. Sensitivity Analysis
  # ****************************************************************************
  # Display parameter sensitivity ranking: need to break this to smaller code
  # ****************************************************************************
  observeEvent(input$calSensitivity, {

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
          shinyCatch(
            tableSensitivity <- summary(lm(
              formula = objFunction ~ ., tableSensitivity))[4]$coefficients[,3:4], 
            blocking_level = "error"
          )
          
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
          output$tableSensitivity <- renderDataTable(globalVariable$tableSensitivity)

          # Set 1 second delay for the progress bar
          Sys.sleep(1)
          incProgress(1/2)
        })

      # Check if users do sensi. analysis with R packages
      } else if (globalVariable$samplingApproach == 'Sensi_(from_sensitivity_package)'|
                 globalVariable$samplingApproach == 'Sensi_(from_userDefined_package)'){

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
    }
  })


  # 4.3. Optimization/Uncertainty
  # ****************************************************************************
  # Input behavioral threshold: Check if the input behavioral threshold is valid
  # ****************************************************************************
  observe({

    # By default the input behavioral threshold value is not valid
    globalVariable$isBehThresholdValid <<- FALSE

    # Check if the user-defined behavioral threshold is in a valid range
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
    }
  })

  # ****************************************************************************
  # Input variable number to plot: Calculate values of all tables for display
  # ****************************************************************************
  observe({
    req(input$checkPlotVariableNumber)

    # Check if there are parameter values and the selected behavioral threshold is valid
    if(!is.null(globalVariable$parameterValue) & globalVariable$isBehThresholdValid){

      # Find behavioral simulations - 95PPU
      shinyCatch(
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
      output$PlotVariableNumber <- renderPlotly(
        ggplotly(globalVariable$PlotVariableNumber +
                   theme(text = element_text(size=10)))
        )

      # Set format for the 95PPU table
      columnsTableBehaSim <- data.frame(title = c('Date','Lower 95PPU', 'Median',
                                                  'Upper 95PPU', 'Best Simulation'),
                                        source = rep(NA, 5),
                                        width = rep(300, 5),
                                        type = rep('numeric', 5))

      # Fill the 95PPU table with values
      output$tableBehaSim <- renderExcel(excelTable(
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
      output$tableBehaParam <- renderExcel(excelTable(
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
  # Save plot simulated result
  # ****************************************************************************
  observe({
    req(input$savePlotVariableNumber)

    # Display window for saving plot
    showModal(
      modalDialog(
        textInput("savePlotVariableNumberFileName", "File name (must have .pdf)",
                  placeholder = 'RSWATPlot.pdf'
        ),

        # User defined setting for figure width
        numericInput("savePlotVariableNumberWidth", "Width in cm", 10,
                     min = 1, max = 100),

        # User defined setting for figure height
        numericInput("savePlotVariableNumberHeight", "Height in cm", 10,
                     min = 1, max = 100),

        # Display instruction
        span('Press OK to save plot as .pdf file in the working folder'),

        # Set displayed texts for the the modal and action button
        footer = tagList(
          modalButton("Cancel"),
          actionButton("savePlotVariableNumberok", "OK")
        )
      )
    )

  })

  # Saving figure only when user press the OK button
  observeEvent(input$savePlotVariableNumberok, {

    # Set working directory, which is the working folder
    setwd(globalVariable$workingFolder)

    # Now save the plot as pdf file
    ggsave(input$savePlotVariableNumberFileName,
           plot = globalVariable$PlotVariableNumber,
           device = "pdf",
           width = input$savePlotVariableNumberWidth,
           height = input$savePlotVariableNumberHeight,
           units = c("cm"))

    # print message
    showModal(modalDialog(
      title = " ",
      "Plot was saved as .pdf file in the working folder",
      easyClose = TRUE,
      size = "l"
    ))
  })


  #-----------------------------------------------------------------------------
  # Tab 5. Visualization
  #-----------------------------------------------------------------------------
  # 5.1. Visualize watout file
  # ****************************************************************************
  # Update selected column watout.dat file type
  # ****************************************************************************
  observe({
    req(input$watoutFile)

    # Read data and header of the watout file
    globalVariable$visualWatoutData <<- readWatout(input$watoutFile$datapath)
    globalVariable$visualWatoutHeader <<- readWatoutHeader(input$watoutFile$datapath)

    # Display column header
    updateSelectizeInput(session, "selectColWatout",
                         choices = globalVariable$visualWatoutHeader,
                         selected = globalVariable$visualWatoutHeader[2]
    )
  })

  # ****************************************************************************
  # Update selected column observed
  # ****************************************************************************
  observe({
    req(input$observedFile)

    # Read observed data
    globalVariable$visualObserveData <<- read.table(input$observedFile$datapath,
                                                    header = TRUE, sep = "")

    # Display header of observed data
    updateSelectizeInput(session, "selectColObs",
                         choices = colnames(globalVariable$visualObserveData),
                         selected = globalVariable$visualObserveData[2]
    )
  })

  # ****************************************************************************
  # Plot watout file
  # ****************************************************************************
  observe({
    req(input$watoutFile)

    # Plot selected data
    if(is.data.frame(globalVariable$visualWatoutData)){

      # Check if observed data file is missing
      if (!is.null(input$observedFile$datapath)){

        # Search for data in the selected column
        if (input$selectColObs %in% colnames(globalVariable$visualObserveData)){

          # Plot the selected data
          pltWatout <- plotWatout(globalVariable$visualWatoutData,
                                  globalVariable$visualWatoutHeader,
                                  input$selectColWatout,
                                  globalVariable$visualObserveData,
                                  input$selectColObs)
          output$plotWatout <- renderPlotly(pltWatout$plot)
        }

      } else {

        # Only plot simulated data if observed data is not provided
        if (input$selectColWatout %in% globalVariable$visualWatoutHeader){
          pltWatout <- plotWatout(globalVariable$visualWatoutData,
                                  globalVariable$visualWatoutHeader,
                                  input$selectColWatout,
                                  NULL,
                                  NULL)
          output$plotWatout <- renderPlotly(pltWatout$plot)
        }

      }
    }

  })

  # 5.2. Visualize output.hru
  # ****************************************************************************
  # Get HRU shape file file
  # ****************************************************************************
  observe({

    # Get a list of available volumes
    volumes <- getVolumes()

    # Create a connection to the server side filesystem
    shinyFileChoose(input, "getHruShp",
                    roots = volumes,
                    filetypes=c('', 'shp'),
                    session = session)

    # Full link to the selected file
    hruShpFile <- parseFilePaths(volumes, input$getHruShp)

    if(length(hruShpFile$datapath) == 1){

      # HRU file path
      hruShpFile <- as.character(hruShpFile$datapath)

      # Print out selected file
      output$printHruShp <- renderText(hruShpFile)

      # Check if file exist
      if (file.exists(hruShpFile)){

        # Read hru shape file
        displayOutput$hruShp <<- readOGR(hruShpFile)
      } else {

        # If hru shape file does not exit, return Null
        displayOutput$hruShp <<- NULL
      }
    }
  })

  # ****************************************************************************
  # Get TxtInOut directory
  # ****************************************************************************
  # Update select date rnage
  observe({

    # Require input of the TxtInOut folder
    req(input$hruTxtInOutFolder)

    # Read file.cio
    fileCio <- trimws(paste(input$hruTxtInOutFolder, '/file.cio', sep=''))

    if (file.exists(fileCio)){

      # Get information from the file.cio
      displayOutput$hruFileCioInfo <<- getFileCioInfo(input$hruTxtInOutFolder)

      # Update date for plot based on file.cio
      updateDateInput(session, "hruPlotDate",
                      min = displayOutput$hruFileCioInfo$startEval,
                      max = displayOutput$hruFileCioInfo$endSim,
                      value = displayOutput$hruFileCioInfo$startEval[1])

      # Update date for plot based on file.cio
      updateDateRangeInput(session, "hruInputDateRange",
                           start = displayOutput$hruFileCioInfo$startEval,
                           end = displayOutput$hruFileCioInfo$endSim,
                           min = displayOutput$hruFileCioInfo$startEval,
                           max = displayOutput$hruFileCioInfo$endSim)

      # Update date for plot based on file.cio
      updateDateInput(session, "hruPlotMonth",
                      min = displayOutput$hruFileCioInfo$startEval,
                      max = displayOutput$hruFileCioInfo$endSim,
                      value = displayOutput$hruFileCioInfo$startEval[1])

      # Update date for plot based on file.cio
      updateDateRangeInput(session, "hruInputMonthRange",
                           start = displayOutput$hruFileCioInfo$startEval,
                           end = displayOutput$hruFileCioInfo$endSim,
                           min = displayOutput$hruFileCioInfo$startEval,
                           max = displayOutput$hruFileCioInfo$endSim)

      # Update date for plot based on file.cio
      updateDateInput(session, "hruPlotYear",
                      min = displayOutput$hruFileCioInfo$startEval,
                      max = displayOutput$hruFileCioInfo$endSim,
                      value = displayOutput$hruFileCioInfo$startEval[1])

      # Update date for plot based on file.cio
      updateDateRangeInput(session, "hruInputYearRange",
                           start = displayOutput$hruFileCioInfo$startEval,
                           end = displayOutput$hruFileCioInfo$endSim,
                           min = displayOutput$hruFileCioInfo$startEval,
                           max = displayOutput$hruFileCioInfo$endSim)

    }

    # Get full link to the output.hru file
    fileHru <- trimws(paste(input$hruTxtInOutFolder, '/output.hru', sep=''))

    # Check if fileHRU exists
    if(file.exists(fileHru)){

      # Read header of output.hru
      ouputHruHeader <- readOutputHruHeader(fileHru)

      # Remove column with these names out of plot data
      temp <- c('LULC', 'HRU', 'GIS', 'SUB', 'MGT', 'MO', 'DA', 'YR')
      ouputHruHeader <- ouputHruHeader[-na.omit(match(temp,ouputHruHeader))]

      # Update selected column for plot
      updateSelectizeInput(session, "hruSelectCol",
                           choices = ouputHruHeader,
                           selected = ouputHruHeader[1])

    } else {

      # If file does not exist, return null
      updateSelectizeInput(session, "hruSelectCol",
                           choices = NULL)
    }

  })

  # ****************************************************************************
  # get HRU data
  # ****************************************************************************
  observe({

    req(input$hruTxtInOutFolder)

    # Get full path of the file output.hru
    fileHru <- trimws(paste(input$hruTxtInOutFolder, '/output.hru', sep=''))

    if(file.exists(fileHru)){

      # Read HRU data
      temp <- read.table(fileHru, header = FALSE, sep = "", skip = 9)

      # Get header of output.hru
      colnames(temp) <- readOutputHruHeader(fileHru)

      # Assign header to its data frame
      displayOutput$hruData <<- temp

    } else {

      # If file does not exist, return null
      displayOutput$hruData <<- NULL
    }
  })

  # ****************************************************************************
  # Plot hru
  # ****************************************************************************
  observe({

    # Required input to activate these commands
    req(input$hruTxtInOutFolder,
        input$hruSelectCol,
        input$hruTempAgg)

    # Check which temporal aggregation daily - monthly - or yearly
    # Get input date/date range and assigned them to global variables
    if (input$hruTempAgg == 'Daily'){
      displayOutput$hruInputDateRange <<- input$hruInputDateRange
      displayOutput$hruPlotDate <<- input$hruPlotDate
    } else if (input$hruTempAgg == 'Monthly'){
      displayOutput$hruInputDateRange <<- input$hruInputMonthRange
      displayOutput$hruPlotDate <<- input$hruPlotMonth
    } else {
      displayOutput$hruInputDateRange <<- input$hruInputYearRange
      displayOutput$hruPlotDate <<- input$hruPlotYear
    }

    # Check if there is data for plot
    if (!is.null(displayOutput$hruData)){

      # Get subset of the data for plot
      hruPlotData <- subsetOutputHru(displayOutput$hruData,
                                     displayOutput$hruInputDateRange[1],
                                     displayOutput$hruInputDateRange[2],
                                     input$hruSelectCol,
                                     input$hruTempAgg)

      # Check if there is no hru shape file
      if(!is.null(displayOutput$hruShp)){

        # Display output text (by default)
        outputText <- as.character(displayOutput$hruPlotDate)

        # Modify output text depending on the selected temporal aggregation
        if (input$hruTempAgg == "Monthly"){
          outputText <- substr(outputText,1,7)
        } else if (input$hruTempAgg == "Yearly"){
          outputText <- substr(outputText,1,4)
        }

        # Now display output text
        outputText <- paste("You selected Variable: ",
                            input$hruSelectCol,
                            ", Timestep: ",
                            input$hruTempAgg,
                            ", Time: ",
                            outputText,
                            sep = "")

        # Set title for the plot
        output$hruPlotTitle <- renderText(outputText)

        # Set min-max range for color of the plot
        displayOutput$plotHruValues <<- hruShpValue(hruPlotData,
                                                    displayOutput$hruPlotDate)

        minVal <- min(displayOutput$plotHruValues)
        maxVal <- max(displayOutput$plotHruValues)

        # Update the min-max range in the slider input
        updateSliderInput(session, "hruPlotRange", "7. Set value range for plot",
                          min = minVal, max = maxVal,
                          value = c(minVal, maxVal),
                          step = (maxVal - minVal)/100)

        # Now plot the result
        output$hruPlotHRU <- renderPlot(ggplotHruPolygon(displayOutput$hruShp,
                                                         displayOutput$plotHruValues))
        displayOutput$plotHru <<- TRUE


      # In other cases, don't display any plot
      } else {
        output$hruPlotHRU <- NULL
        displayOutput$plotHru <<- FALSE
      }

    } else {
      output$hruPlotHRU <- NULL
      displayOutput$plotHru <<- FALSE
    }


  })

  # ****************************************************************************
  # Update plot hru
  # ****************************************************************************
  observe({

    req(input$hruPlotRange)

    if(displayOutput$plotHru){
      # Update plot range
      plotValues <- displayOutput$plotHruValues
      plotValues <- replace(plotValues, plotValues <  input$hruPlotRange[1],
                            input$hruPlotRange[1])
      plotValues <- replace(plotValues, plotValues > input$hruPlotRange[2],
                            input$hruPlotRange[2])

      # Now display the hru plot
      output$hruPlotHRU <- renderPlot(ggplotHruPolygon(displayOutput$hruShp,
                                                       plotValues))
    } else {
      output$hruPlotHRU <- NULL
    }

  })

  # 6.1. Visualize output.rch
  # ****************************************************************************
  # Read file.cio information
  # ****************************************************************************
  observe({
    req(input$rchFileCio)

    # Get full path to the .cio file
    fileName <- input$rchFileCio$datapath

    # Check if this is the .cio file
    if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) == ".cio") {

      # Get information about simulation time in this file
      displayOutput$rchFileCioInfo <<- getFileCioInfo(input$rchFileCio$datapath)

    # Otherwise set output is null - no information about the simulation time
    } else {
      displayOutput$rchFileCioInfo <<- NULL
    }

  })

  # ****************************************************************************
  # Read output.rch  file
  # ****************************************************************************
  observe({
    req(input$outputRchFile)

    # Get full path to the output.rch file
    fileName <- input$outputRchFile$datapath

    # Chek if this is really .rch file
    if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) == ".rch") {

      # Load data in this file
      displayOutput$outputRchFile <<- readOutputRch(fileName)

      # Update/Give the reach number that user can select
      updateSelectizeInput(session, "rchSelectedReach",
                           choices = c(1:max(displayOutput$outputRchFile[,1])))

      # Update/Give the list of variables that user can select
      updateSelectizeInput(session, "rchSelectedVariable",
                           choices = colnames(displayOutput$outputRchFile))
    } else {
      displayOutput$outputRchFile <<- NULL
    }
  })


  # ****************************************************************************
  # Read observed data  file
  # ****************************************************************************
  observe({
    req(input$rchObservedFile)

    # Read observed data
    displayOutput$rchObsData <<- read.table(input$rchObservedFile$datapath,
                                           header = TRUE, sep = "")

    # Assign date for observed data
    displayOutput$rchObsData[,1] <<- as.Date(displayOutput$rchObsData[,1],
                                            "%Y-%m-%d")

    # Update variable name in the observed data
    updateSelectizeInput(session, "rchObsVariable",
                         choices = colnames(displayOutput$rchObsData)
                         [2:ncol(displayOutput$rchObsData)])


  })

  # ****************************************************************************
  # Subset observed data file
  # ****************************************************************************
  observe({
    req(input$rchObsVariable)

    # check if there are observed data
    if (!is.null(input$rchObsVariable) &
        exists("rchObsData", where = displayOutput)){

      # Get subset of observed data
      colNr <- match(input$rchObsVariable, colnames(displayOutput$rchObsData))
      displayOutput$rchSubsetObsData <<- displayOutput$rchObsData[,c(1, colNr)]
    }

  })

  # ****************************************************************************
  # Plot result
  # ****************************************************************************
  observe({

    req(input$rchTempAgg)
    req(input$rchSelectedVariable)
    req(input$rchSelectedReach)

    # Check if observed are also given
    if (isTruthy(input$rchObsVariable) & isTruthy(input$rchObservedFile)){
      if (exists("rchSubsetObsData", where = displayOutput) &
          exists("outputRchFile", where = displayOutput)&
          exists("rchFileCioInfo", where = displayOutput)){

        # Get subset of data in the .rch file
        outputRchSubset <- subsetOutputRch(displayOutput$outputRchFile,
                                           input$rchTempAgg,
                                           input$rchSelectedVariable,
                                           c(displayOutput$rchFileCioInfo$startEval,
                                             displayOutput$rchFileCioInfo$endSim),
                                           input$rchSelectedReach)

        # Plot the result
        plt <- plotOutputRchSubset(outputRchSubset, displayOutput$rchSubsetObsData)

        # Display plot
        output$rchPlotRch <- renderPlotly(plt)

      } else {
        output$rchPlotRch <- NULL
      }
    } else {
      if (exists("outputRchFile", where = displayOutput)){

        outputRchSubset <- subsetOutputRch(displayOutput$outputRchFile,
                                           input$rchTempAgg,
                                           input$rchSelectedVariable,
                                           c(displayOutput$rchFileCioInfo$startEval,
                                             displayOutput$rchFileCioInfo$endSim),
                                           input$rchSelectedReach)
        temp <- NULL
        plt <- plotOutputRchSubset(outputRchSubset, temp)


        output$rchPlotRch <- renderPlotly(plt)

      } else {
        output$rchPlotRch <- NULL
      }
    }


  })

  # 7.1. Visualize output.sub
  # ****************************************************************************
  # Read subbasin shape file
  # ****************************************************************************
  observe({

    # Get all available volumes
    volumes <- getVolumes()

    # Display select file window
    shinyFileChoose(input, "getSubShape",
                    roots = volumes,
                    filetypes=c('', 'shp'),
                    session = session)

    # Get full path to the selected file
    subShapeFile <- parseFilePaths(volumes, input$getSubShape)

    # Read subbasin shape file
    if(length(subShapeFile$datapath) == 1){
      subShapeFile <- as.character(subShapeFile$datapath)
      output$printSubShape <- renderText(subShapeFile)

      if (file.exists(subShapeFile)){

        displayOutput$subShape <<- readOGR(subShapeFile)
      } else {
        displayOutput$subShape <<- NULL
      }
    }
  })

  # ****************************************************************************
  # Get TxtInOut directory
  # ****************************************************************************
  # Update select date range
  observe({

    req(input$subTxtInOutFolder)

    # Get full path to the file.cio file
    fileCio <- trimws(paste(input$subTxtInOutFolder, '/file.cio', sep=''))

    # check if file.cio exists
    if (file.exists(fileCio)){

      # Get simulation date from file.cio
      displayOutput$subFileCioInfo <<- getFileCioInfo(input$subTxtInOutFolder)

      # Update the selected date
      updateDateInput(session, "subPlotDate",
                      min = displayOutput$subFileCioInfo$startEval,
                      max = displayOutput$subFileCioInfo$endSim,
                      value = displayOutput$subFileCioInfo$startEval[1])

      # Update the selected date range
      updateDateRangeInput(session, "subInputDateRange",
                           start = displayOutput$subFileCioInfo$startEval,
                           end = displayOutput$subFileCioInfo$endSim,
                           min = displayOutput$subFileCioInfo$startEval,
                           max = displayOutput$subFileCioInfo$endSim)


      # Update the selected month
      updateDateInput(session, "subPlotMonth",
                      min = displayOutput$subFileCioInfo$startEval,
                      max = displayOutput$subFileCioInfo$endSim,
                      value = displayOutput$subFileCioInfo$startEval[1])

      # Update the selected month range
      updateDateRangeInput(session, "subInputMonthRange",
                           start = displayOutput$subFileCioInfo$startEval,
                           end = displayOutput$subFileCioInfo$endSim,
                           min = displayOutput$subFileCioInfo$startEval,
                           max = displayOutput$subFileCioInfo$endSim)

      # Update the selected year
      updateDateInput(session, "subPlotYear",
                      min = displayOutput$subFileCioInfo$startEval,
                      max = displayOutput$subFileCioInfo$endSim,
                      value = displayOutput$subFileCioInfo$startEval[1])

      # Update the selected year range
      updateDateRangeInput(session, "subInputYearRange",
                           start = displayOutput$subFileCioInfo$startEval,
                           end = displayOutput$subFileCioInfo$endSim,
                           min = displayOutput$subFileCioInfo$startEval,
                           max = displayOutput$subFileCioInfo$endSim)

    }

    # Get output.sub data and update select column
    fileSub <- trimws(paste(input$subTxtInOutFolder, '/output.sub', sep=''))

    # check if output.sub file exists
    if(file.exists(fileSub)){

        # Read data in the output.sub file
        displayOutput$subData <<- readOutputRch(fileSub)

        # Update variables name for selection
        updateSelectizeInput(session, "subSelectCol",
                             choices = colnames(displayOutput$subData)[-c(1)])
    } else {
      updateSelectizeInput(session, "subSelectCol",
                           choices = NULL)
      displayOutput$subData <<- NULL
    }

  })

  # ****************************************************************************
  # Plot sub
  # ****************************************************************************
  observe({

    req(input$subTxtInOutFolder,
        input$subSelectCol,
        input$subTempAgg)

    # Check with temporal aggregation is given
    if (input$subTempAgg == 'Daily'){
      displayOutput$subInputDateRange <<- input$subInputDateRange
      displayOutput$subPlotDate <<- input$subPlotDate
    } else if (input$subTempAgg == 'Monthly'){
      displayOutput$subInputDateRange <<- input$subInputMonthRange
      displayOutput$subPlotDate <<- input$subPlotMonth
    } else {
      displayOutput$subInputDateRange <<- input$subInputYearRange
      displayOutput$subPlotDate <<- input$subPlotYear
    }

    # Get subset of data based on user selection criteria
    if (!is.null(displayOutput$subData)){
      subPlotData <- subsetOutputHru(displayOutput$subData,
                                     displayOutput$subInputDateRange[1],
                                     displayOutput$subInputDateRange[2],
                                     input$subSelectCol,
                                     input$subTempAgg)

      # Check if subbasin shape file exist
      if(!is.null(displayOutput$subShape)){
         rowNr <- match(displayOutput$subPlotDate, subPlotData[,1])
         if (input$subTempAgg == 'Monthly'){
           rowNr <- which(subPlotData[,1] == substr(displayOutput$subPlotDate, 1, 7))
         } else if (input$subTempAgg == 'Yearly'){
           rowNr <- which(subPlotData[,1] == substr(displayOutput$subPlotDate, 1, 4))
         } else {
           rowNr <- which(subPlotData[,1] == displayOutput$subPlotDate)
         }

        # display text
        outputText <- as.character(displayOutput$subPlotDate)
        if (input$subTempAgg == "Monthly"){
          outputText <- substr(outputText,1,7)
        } else if (input$subTempAgg == "Yearly"){
          outputText <- substr(outputText,1,4)
        }
        outputText <- paste("You selected Variable: ",
                            input$subSelectCol,
                            ", Timestep: ",
                            input$subTempAgg,
                            ", Time: ",
                            outputText,
                            sep = "")

        # Display title for the subbasin plot
        output$subPlotTitle <- renderText(outputText)

        # Data for subbasin plot
        displayOutput$plotSubValues <<- as.numeric(subPlotData[rowNr, 2:ncol(subPlotData)])

        # Get range for plot
        minVal <- min(displayOutput$plotSubValues)
        maxVal <- max(displayOutput$plotSubValues)

        # Update the allowable range for plot
        updateSliderInput(session, "subPlotRange", "7. Set value range for plot",
                          min = minVal, max = maxVal,
                          value = c(minVal, maxVal),
                          step = (maxVal - minVal)/100)

        # Display plot
        output$subPlotSub <- renderPlot({
          ggplotPolygon(displayOutput$subShape, displayOutput$plotSubValues)
        })

        displayOutput$plotSub <<- TRUE

      } else {
        output$subPlotSub <- NULL
        displayOutput$plotSub <<- FALSE
      }

    } else {
      output$subPlotSub <- NULL
      displayOutput$plotSub <<- FALSE
    }

  })


  # ****************************************************************************
  # Update plot sub
  # ****************************************************************************
  observe({

    req(input$subPlotRange)

    # check if user selects to plot subbasin outputs
    if(displayOutput$plotSub){

      # Update plot range
      plotValues <- displayOutput$plotSubValues
      plotValues <- replace(plotValues, plotValues <  input$subPlotRange[1],
                            input$subPlotRange[1])
      plotValues <- replace(plotValues, plotValues > input$subPlotRange[2],
                            input$subPlotRange[2])

      # Display the plot
      output$subPlotSub <- renderPlot({
        ggplotPolygon(displayOutput$subShape, plotValues)
      })

    # Other cases, don't display anything
    } else {
      output$hruPlotHRU <- NULL
    }

  })

  #-----------------------------------------------------------------------------
}
