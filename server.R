
# maximum upload file 500 MB
options(shiny.maxRequestSize = 500*1024^2)

# Creating server
server <- function(input, output, session) {
 
  #-----------------------------------------------------------------------------  
  # Global variables (use <<- for storing globalVariable inside observe)
  #-----------------------------------------------------------------------------
  globalVariable <- list()
  displayOutput <- list()
  globalVariable$checkSimComplete <- FALSE

    
  #-----------------------------------------------------------------------------
  # Tab 1. General Setting
  #-----------------------------------------------------------------------------
  
  # ****************************************************************************  
  # Get working folder
  # ****************************************************************************
  observe({ 
    globalVariable$workingFolder <<- input$workingFolder
    globalVariable$CurrentSimulationReportFile <<- paste(input$workingFolder, 
                                                         '/Output/CurrentSimulationReport.log', 
                                                         sep ='')
  })
  
  # ****************************************************************************  
  # TxtInOut folder: Display HRU info from TxtInOut folder
  # ****************************************************************************
  
  observe({
    req(input$TxtInOutFolder)
    if (checkDirFileExist(input$TxtInOutFolder, "", ".hru")){
      
      globalVariable$HRUinfo <<- getHruInfo(input$TxtInOutFolder)
      globalVariable$TxtInOutFolder <<- input$TxtInOutFolder  
      
      uniqueSoil <- unique(globalVariable$HRUinfo$soil) 
      uniqueLandUse <- unique(globalVariable$HRUinfo$lu)
      uniqueSlope <- unique(globalVariable$HRUinfo$slope)
      minMaxSubbasin <- range(globalVariable$HRUinfo$sub)
      
      nSoil <- length(uniqueSoil)
      nLU <- length(uniqueLandUse)
      nSlope <- length(uniqueSlope)
      
      nRow <- max(nSoil, nLU, nSlope, 2)
      
      output$tableHRUinfo <- renderDataTable(globalVariable$HRUinfo)
      
      displayOutput$uniqueHruProperties <<- 
        data.frame(minMaxSubbasin = c(minMaxSubbasin,rep(NA, nRow - 2)),
                   Landuse = c(uniqueLandUse,rep(NA, nRow - nLU)),
                   soilName = c(uniqueSoil,rep(NA, nRow - nSoil)),
                   slopeClass = c(uniqueSlope,rep(NA, nRow - nSlope))
        )
      
    } else {
      globalVariable$HRUinfo <<- NULL
      globalVariable$TxtInOutFolder <<- NULL
      output$tableHRUinfo <- NULL
      displayOutput$uniqueHruProperties <<- NULL
    }
  })  
  
  # ****************************************************************************  
  # Files with list of all SWAT parameters (get file) + display content of file
  # ****************************************************************************

  observe({
    volumes <- getVolumes()
    shinyFileChoose(input, "getSWATParamFile", 
                    roots = volumes,  
                    filetypes=c('', 'txt'),
                    session = session)
    
    SWATParamFile <- parseFilePaths(volumes, input$getSWATParamFile)
    
    output$printSWATParamFile <- NULL
    globalVariable$SWATParamFile <<- NULL
    globalVariable$SWATParam <<- NULL
    output$tableSWATParam <- NULL
    
    if (length(SWATParamFile$datapath) == 1){
      if (SWATParamFile$name == "swatParam.txt"){
        globalVariable$SWATParamFile <<- as.character(SWATParamFile$datapath) 
        globalVariable$SWATParam <<- loadSwatParam(globalVariable$SWATParamFile)
        output$printSWATParamFile <- renderText(globalVariable$SWATParamFile)
        output$tableSWATParam <- renderDataTable(globalVariable$SWATParam)
      }
    }
    
  })


  # ****************************************************************************  
  # Get executable SWAT file
  # ****************************************************************************  
  observe({
    volumes <- getVolumes()
    shinyFileChoose(input, "getSWATexe", 
                    roots = volumes, 
                    filetypes=c('', 'exe'),
                    session = session)
    
    SWATexeFile <- parseFilePaths(volumes, input$getSWATexe)
    
    if(length(SWATexeFile$datapath) == 1){
      output$printSWATexe <- renderText(SWATexeFile$datapath)
      globalVariable$SWATexeFile <<- as.character(SWATexeFile$datapath)
    }
  })
  
  #-----------------------------------------------------------------------------
  # Tab 2. Parameter sampling
  #----------------------------------------------------------------------------- 

  # ****************************************************************************  
  # Display help for parameter selection
  # **************************************************************************** 
  
  observe({
    req(input$helpParameterSelection)
    
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
  })
  
  # ****************************************************************************  
  # Select SWAT parameters to calibration and/or sensitivity: Default setting
  # ****************************************************************************  
  output$tableParaSelection <- renderExcel(excelTable(data = dataParaSelection, 
                                                      columns = columnsParaSelection, 
                                                      editable = TRUE,
                                                      allowInsertRow = TRUE,
                                                      allowInsertColumn = TRUE,
                                                      allowDeleteColumn = TRUE,
                                                      allowDeleteRow = TRUE, 
                                                      rowDrag = TRUE,
                                                      columnResize = FALSE, 
                                                      wordWrap = TRUE))  

  # ****************************************************************************  
  # Check input 'Select SWAT parameters for calibration'
  # ****************************************************************************  
  observeEvent(input$checkParameterTableButton, {
    checkParameterTable <- checkSwatParameterName(globalVariable$paraSelection, 
                                                  globalVariable$SWATParam, 
                                                  globalVariable$HRUinfo)
    output$checkParameterTableTxtOuput <- renderText(checkParameterTable$checkMessage)
  })
  

  # ****************************************************************************  
  # Save "SWAT parameters for calibration" to global variable
  # ****************************************************************************  
  observe({
    paraSelection <-  excel_to_R(input$tableParaSelection)
    if(is.null(paraSelection)) paraSelection <- dataParaSelection
    globalVariable$paraSelection  <<- paraSelection
    
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
    paraSampling <- excel_to_R(input$tableParaSampling)
    if(is.null(paraSampling)) paraSampling <- dataParaSampling
    globalVariable$paraSampling  <<- paraSampling
    
    if (globalVariable$paraSampling$samplingApproach == "Sensi_Cali_(LHS)"){
      output$printSelectedParaSensiApproach <- 
        renderText(paste("Mutivariable regression, global approach. ",
                         "Parameters generated by Latin Hypercube Sampling (LHS)",
                         sep = "")
                   )
      
      output$printHelpInputinfo <- renderText("Please input the number of parametersets (iterations) in the InputInfo column, e.g., 10")
      
    } else if (globalVariable$paraSampling$samplingApproach == "Sensi_(Morris)") {
      output$printSelectedParaSensiApproach <- renderText(paste("Partial derivative, ", 
                                                                "global approach. ",
                                                                "See you command 'design = ..:' ",
                                                                "for parameter sampling approach", sep =""))   
      output$printHelpInputinfo <- renderText("Please write morris command in the InputInfo. For example,
      
      morris(model = SWAT, factors = nParameters, binf = minColumn, bsup = maxColumn, r = 4, design = list(type = 'oat', levels = 5, grid.jump = 3))

The first 4 input fields MUST be 'model = SWAT, factors = nParameters, binf = minColumn, bsup = maxColumn', 
other input fields could be modifed, see function morris in the sensitivity package for help. In the first
4 fields, this tool automatically gets information from the above table as input for this function. E.g.,
SWAT means it will take results from the SWAT model, nParameters is the number of parameters in the above
table, minColumn and maxColumn means values in the column Min and Max from the above table, The number of
model runs are (nParameters + 1) * r")
    } else if(globalVariable$paraSampling$samplingApproach == "Cali_(DDS)"){
      output$printHelpInputinfo <- renderText("Please write the number of iterations and parallel approach. For example,
      10, 2
means 10 iterations and the parallel approach is 2, they must be seperated by the comma 
Parallel approach = 1 => DDS is run independently from each core.
Parallel approach = 2 => the best parameterset among all cores at ith iteration is used for the next iteration)
The number mof model runs = number of iterations * number of parallel runs (cores)
Please go to step 4.1 to provide information about the objective function (1) and observed data files (2) 
before '3. Run SWAT'")
    } else {
      output$printSelectedParaSensiApproach <- NULL
    }
    
    
  })
  
  #-----------------------------------------------------------------------------
  # Tab 3. Run SWAT
  #-----------------------------------------------------------------------------  

  # ****************************************************************************  
  # Output extraction: Default setting
  # ****************************************************************************
  output$tableOutputExtraction <- renderExcel(excelTable(data = dataOutputExtraction, 
                                                         columns = columnsOutputExtraction, 
                                                         editable = TRUE,
                                                         allowInsertRow = TRUE,
                                                         allowInsertColumn = FALSE,
                                                         allowDeleteColumn = FALSE,
                                                         allowDeleteRow = TRUE, 
                                                         rowDrag = FALSE,
                                                         columnResize = FALSE,
                                                         wordWrap = TRUE))
  
  # ****************************************************************************  
  # Get user output extraction
  # ****************************************************************************
  observe({
    outputExtraction <- excel_to_R(input$tableOutputExtraction)
    if(is.null(outputExtraction)) outputExtraction <- dataOutputExtraction
    globalVariable$outputExtraction  <<- outputExtraction  
    OutputVar  <- getNumberOutputVar(outputExtraction)
    globalVariable$nOutputVar <<- OutputVar$nOutputVar
    globalVariable$userReadSwatOutput <<- OutputVar$userReadSwatOutput    
    output$tableOutputExtractionDisplayOnly <- renderDataTable(
      printVariableNameObservedFiles(outputExtraction)
    )
  })
  
  
  # ****************************************************************************  
  # Update select input range based on file.cio in the TxtInOutFolder
  # ****************************************************************************
  observe({
    req(input$TxtInOutFolder)
    
    if (checkDirFileExist(input$TxtInOutFolder, "file.cio", "")){
      myDate <- getFileCioInfo(input$TxtInOutFolder)
      globalVariable$fileCioInfo <<- myDate
      
      updateDateRangeInput(session, "dateRangeCali",
                           start = myDate$startEval,
                           end = myDate$endSim,
                           min = myDate$startEval,
                           max = myDate$endSim
      )
    }
  })

  # ****************************************************************************  
  # Get user input range for calibration
  # ****************************************************************************
  observe({ 
    globalVariable$dateRangeCali <<- input$dateRangeCali
  })     
  
  # ****************************************************************************  
  # Get user input number of cores
  # ****************************************************************************
  observe({ 
    globalVariable$ncores <<- input$ncores
  })
 
  # ****************************************************************************  
  # Run SWAT
  # ****************************************************************************
  observeEvent(input$runSWAT, {
    
    # Performe check list otherwise SWATshiny will be turn off when click this
    
    checkList <- TRUE
    checkList <- checkList & !is.null(globalVariable$workingFolder) 
    checkList <- checkList & !is.null(globalVariable$paraSampling)
    checkList <- checkList & !is.null(globalVariable$paraSelection)
    checkList <- checkList & !is.null(globalVariable$HRUinfo) 
    checkList <- checkList & !is.null(globalVariable$SWATParam)
    checkList <- checkList & !is.null(globalVariable$TxtInOutFolder)     
    checkList <- checkList & !is.null(globalVariable$outputExtraction)
    checkList <- checkList & !is.null(globalVariable$paraSampling)
    checkList <- checkList & !is.null(globalVariable$ncores) 
    checkList <- checkList & !is.null(globalVariable$SWATexeFile) 
    checkList <- checkList & !is.null(globalVariable$fileCioInfo)
    checkList <- checkList & !is.null(globalVariable$dateRangeCali)
    
    if (checkList){
      
      # First get or generate parameter set
      if (globalVariable$paraSampling$samplingApproach == "Sensi_Cali_(LHS)") {
        globalVariable$parameterValue <<- lhsRange(as.numeric(globalVariable$paraSampling$InputInfo),
                                                   getParamRange(globalVariable$paraSelection))
      } else if (globalVariable$paraSampling$samplingApproach == "Sensi_(Morris)"){

        # Get and edit the input text (InputInfo) command
        morrisCommand <-  globalVariable$paraSampling$InputInfo
        morrisCommand <- gsub("SWAT", "NULL", morrisCommand)
        morrisCommand <- gsub("minColumn", "globalVariable$paraSelection$Min", morrisCommand)
        morrisCommand <- gsub("maxColumn", "globalVariable$paraSelection$Max", morrisCommand)
        morrisCommand <- gsub("nParameters", "length(globalVariable$paraSelection$Max)", morrisCommand)
        
        # call morris to generate parameters
        morrisObject <<- eval(parse(text = morrisCommand))
        globalVariable$parameterValue <<- cbind(c(1:nrow(morrisObject$X)), morrisObject$X)
        globalVariable$morrisObject <<- morrisObject
        
      } else if (globalVariable$paraSampling$samplingApproach == "Cali_(DDS)"){
        # Generate initial parameter set
        globalVariable$parameterValue <<- lhsRange(globalVariable$ncores,
                                                   getParamRange(globalVariable$paraSelection))        
      } else {
        Print("Other parameter sampling approaches are under development")
      }
      
      # Run the SWAT model
      # Load all files that are going to be updated
      globalVariable$caliParam <<- loadParamChangeFileContent(globalVariable$HRUinfo, 
                                                              globalVariable$paraSelection,
                                                              globalVariable$SWATParam, 
                                                              globalVariable$TxtInOutFolder)
      
      # Save global variables
      saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                           'SWATShinyObject.rds',
                                           sep =''))
      
      
      # Message show all input was saved
      showModal(modalDialog(
        title = "Save current input",
        HTML("All current inputs were save to the file 'SWATShinyObject.rds'.<br> 
      in the working folder. SWAT is running, close this message .<br>
      You can open the text file '.\\Output\\CurrentSimulationReport.log' .<br>
      in the working folder to see the current simulation. If your simulation .<br>
      is interupted you can restart from the last simulations using these two files"),
        easyClose = TRUE,
        size = "l"
      ))
      
      # Copy unchanged file for the first simulation
      copyUnchangeFiles <- TRUE
      firstRun <- TRUE
      
      if (globalVariable$paraSampling$samplingApproach == "Sensi_Cali_(LHS)"){
        # Run SWAT in parallel
        runSWATpar(globalVariable$workingFolder, 
                   globalVariable$TxtInOutFolder, 
                   globalVariable$outputExtraction, 
                   globalVariable$ncores, 
                   globalVariable$SWATexeFile, 
                   globalVariable$parameterValue,
                   globalVariable$caliParam,
                   copyUnchangeFiles,
                   globalVariable$fileCioInfo,
                   globalVariable$dateRangeCali,
                   firstRun)       
      } else {
        #-----------------------------------------------------------------------
        # Run SWAT
        runSWATpar(globalVariable$workingFolder, 
                   globalVariable$TxtInOutFolder, 
                   globalVariable$outputExtraction, 
                   globalVariable$ncores, 
                   globalVariable$SWATexeFile, 
                   globalVariable$parameterValue,
                   globalVariable$caliParam,
                   copyUnchangeFiles,
                   globalVariable$fileCioInfo,
                   globalVariable$dateRangeCali,
                   firstRun)
 
        # Set firstRun = FALSE -> don't need to copy unchanged files again
        firstRun = FALSE
        
        # Run SWAT with first initial parameter set
        nIters <- as.numeric(strsplit(globalVariable$paraSampling$InputInfo, 
                                      split = ",", 
                                      fixed = TRUE)[[1]][1])
        
        # Get parallel mode
        parallelMode <- as.numeric(strsplit(globalVariable$paraSampling$InputInfo, 
                                            split = ",", 
                                            fixed = TRUE)[[1]][2])
        
        # Objective function with initial parameter set
        temp <- calObjFunction(globalVariable$parameterValue,
                               globalVariable$ncores, 
                               globalVariable$nOutputVar,
                               globalVariable$userReadSwatOutput, 
                               globalVariable$observedData, 
                               globalVariable$workingFolder, 
                               globalVariable$objFunction$Index, 
                               globalVariable$dateRangeCali)

        newPar <- globalVariable$parameterValue
        
        # Assign best parameter to the global variables
        if(parallelMode == 1){
          globalVariable$objValue <<- temp$objValue
          globalVariable$perCriteria <<- temp$perCriteria
          globalVariable$simData <<- temp$simData          
        } else {
          idBest <- which(temp$objValue == max(temp$objValue))
          if (length(idBest) > 1) {idBest = idBest[1]}

          # Take the better parameter set/data if exist
          globalVariable$parameterValue <<- globalVariable$parameterValue[idBest, ]
          globalVariable$objValue <<- temp$objValue[idBest]
          for (k in 1:globalVariable$nOutputVar){
            globalVariable$perCriteria[[k]] <<- temp$perCriteria[[k]][[idBest]]
            globalVariable$simData[[k]] <<- temp$simData[[k]][[idBest]]                  
          }
                    
        }
        
        # Loop over number of iteration
        for (i in 1:nIters){
          print(paste("Iteration ", i, " objective function value = ", 
                      globalVariable$objValue, sep =""))

          # Take better parameter + Generate new parameter set with DDS
          if (parallelMode == 1) {
            # Take the better parameter set/data if exist
            for (j in 1:nrow(newPar)){
              if(temp$objValue[j] > globalVariable$objValue[j]){
                globalVariable$parameterValue[j, ] <<- newPar[j, ]
                globalVariable$objValue[j] <<- temp$objValue[j]
                for (k in 1:globalVariable$nOutputVar){
                  globalVariable$perCriteria[[k]][[j]] <<- temp$perCriteria[[k]][[j]]
                  globalVariable$simData[[k]][[j]] <<- temp$simData[[k]][[j]]                  
                }
              }
            }
            
            # Generate new parameter set
            newPar <- data.frame(min = globalVariable$paraSelection$Min,
                                 max = globalVariable$paraSelection$Max)
            parameterValue <- globalVariable$parameterValue[,2:ncol(globalVariable$parameterValue)]
            newPar <- cbind(newPar, t(parameterValue))
            newPar <- dds(newPar, globalVariable$ncores, i, nIters, 0.2, parallelMode) 
            
            print(globalVariable$objValue)
          } else {
            
            idBest <- which(temp$objValue == max(temp$objValue))
            if (length(idBest) > 1) {idBest = idBest[1]}
            
            if(temp$objValue[idBest] > globalVariable$objValue){
              globalVariable$parameterValue <<- newPar[idBest, ]
              globalVariable$objValue <<- temp$objValue[idBest]
              for (k in 1:globalVariable$nOutputVar){
                globalVariable$perCriteria[[k]] <<- temp$perCriteria[[k]][[idBest]]
                globalVariable$simData[[k]] <<- temp$simData[[k]][[idBest]]                  
              }
            }

            # Generate new parameter set
            newPar <- data.frame(min = globalVariable$paraSelection$Min,
                               max = globalVariable$paraSelection$Max,
                               parameterValue = globalVariable$parameterValue[2:length(globalVariable$parameterValue)])
            newPar <- dds(newPar, globalVariable$ncores, i, nIters, 0.2, parallelMode)
            
          }
          
          
          # Run SWAT
          runSWATpar(globalVariable$workingFolder, 
                     globalVariable$TxtInOutFolder, 
                     globalVariable$outputExtraction, 
                     globalVariable$ncores, 
                     globalVariable$SWATexeFile, 
                     newPar,
                     globalVariable$caliParam,
                     FALSE,
                     globalVariable$fileCioInfo,
                     globalVariable$dateRangeCali,
                     firstRun)
 
       
          # Caculate objective function
          temp <- calObjFunction(newPar,
                                 globalVariable$ncores, 
                                 globalVariable$nOutputVar,
                                 globalVariable$userReadSwatOutput, 
                                 globalVariable$observedData, 
                                 globalVariable$workingFolder, 
                                 globalVariable$objFunction$Index, 
                                 globalVariable$dateRangeCali)

        }
        
        
        # Take best parameter set
        idBest <- which(temp$objValue == max(temp$objValue))
        if (length(idBest) > 1) {idBest = idBest[1]}
        
        # Take the better parameter set/data if exist
        
        if(temp$objValue[idBest] > globalVariable$objValue){
          globalVariable$parameterValue <<- newPar[idBest, ]
          globalVariable$objValue <<- temp$objValue[idBest]
          for (k in 1:globalVariable$nOutputVar){
            globalVariable$perCriteria[[k]] <<- temp$perCriteria[[k]][[idBest]]
            globalVariable$simData[[k]] <<- temp$simData[[k]][[idBest]]                  
          }
        }
        
        print("Best objective function value")
        print(globalVariable$objValue)
        
        #-----------------------------------------------------------------------
      }

      
      saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                           'SWATShinyObject.rds',
                                           sep ='')) 
      
      globalVariable$checkSimComplete <<- TRUE
      
    } else {
      # Message show all input was saved
      showModal(modalDialog(
        title = "Input data/information is missing, please check again",
        easyClose = TRUE,
        size = "l"
      ))
    }
  })
  
  # ****************************************************************************  
  # See simulation report - Open file CurrentSimulationReport.log 
  # ****************************************************************************
  observe({
    req(input$checkCurrentSimulation)
    if (file.exists(globalVariable$CurrentSimulationReportFile)){
      
      fileContent <- readLines(globalVariable$CurrentSimulationReportFile, -1,
                               warn = FALSE)  
      
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
    
    if (!is.null(globalVariable$parameterValue)){
      
      columnsParameterValue <- data.frame(
        title = c("Simulation Nr.", globalVariable$paraSelection$Parameter), 
        source = rep(NA, ncol(globalVariable$parameterValue)),
        width = rep(300, ncol(globalVariable$parameterValue)),
        type = rep('numeric', ncol(globalVariable$parameterValue))
        )
      
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
      output$tableDisplayParameterSet <- NULL
    }
  })
  
  #-----------------------------------------------------------------------------
  # Tab 4. Evaluate output
  #-----------------------------------------------------------------------------  
  # 4.1. Objective function
  
  # ****************************************************************************  
  # Objective function: Default setting
  # ****************************************************************************
  output$tableObjFunction <- renderExcel(excelTable(data = dataObjFunction, 
                                                    columns = columnsObjFunction, 
                                                    editable = TRUE,
                                                    allowInsertRow = FALSE,
                                                    allowInsertColumn = FALSE,
                                                    allowDeleteColumn = FALSE,
                                                    allowDeleteRow = FALSE, 
                                                    rowDrag = FALSE,
                                                    columnResize = TRUE,
                                                    wordWrap = TRUE)) 

  # ****************************************************************************  
  # Get user input objective function
  # ****************************************************************************    
  observe({
    objFunction <- excel_to_R(input$tableObjFunction)
    if(is.null(objFunction)) objFunction <- dataObjFunction
    globalVariable$objFunction  <<- objFunction
  })
  
  # ****************************************************************************  
  # Get observed data files
  # ****************************************************************************  
  observe({
    volumes <- getVolumes()
    shinyFileChoose(input, "getObservedDataFile", 
                    roots = volumes,   
                    filetypes=c('', 'txt'),
                    session = session)
    
    observedDataFile <- parseFilePaths(volumes, input$getObservedDataFile)
    if(length(observedDataFile$datapath) > 0){
      output$printObservedDataFile <- renderText(observedDataFile$datapath)
      globalVariable$observedDataFile <<- sortObservedDataFile(as.character(observedDataFile$datapath))
      globalVariable$observedData <<- list()
      for (i in 1:length(globalVariable$observedDataFile)){
        globalVariable$observedData[[i]] <<- read.table(globalVariable$observedDataFile[i], skip = 1, sep = "")
        colnames(globalVariable$observedData[[i]]) <<- c("Date", "Value")
      }
      saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                           'SWATShinyObject.rds',
                                           sep ='')) 
    }
  })
  
  # ****************************************************************************  
  # Display list of input observed data files
  # ****************************************************************************
  observe({
    req(input$checkDisplayObsVar)
    output$tableObsVarDisplay <- renderDataTable(
      mergeDataFrameDiffRow(globalVariable$observedData)
      )
  })
  
  # ****************************************************************************  
  # Calculate objective function values
  # ****************************************************************************
  observeEvent(input$calObjFunction, {
    
    if (globalVariable$checkSimComplete){
      
      showModal(modalDialog(
        title = "Important message",
        "Calculating objective function ...",
        easyClose = TRUE,
        size = "l"
      ))
      
      # Caculate objective function
      temp <- calObjFunction(globalVariable$parameterValue,
                             globalVariable$ncores, 
                             globalVariable$nOutputVar,
                             globalVariable$userReadSwatOutput, 
                             globalVariable$observedData, 
                             globalVariable$workingFolder, 
                             globalVariable$objFunction$Index, 
                             globalVariable$dateRangeCali)

      globalVariable$objValue <<- temp$objValue
      globalVariable$perCriteria <<- temp$perCriteria
      globalVariable$simData <<- temp$simData
      
      # Update numeric input (threshold objective function)
      minObjValue <- min(globalVariable$objValue)
      maxObjValue <- max(globalVariable$objValue)
      
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
      
    } else {
      showModal(modalDialog(
        title = "Important message",
        "Not all simulations were finised ...",
        easyClose = TRUE,
        size = "l"
      ))
    }
    
    saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                         'SWATShinyObject.rds',
                                         sep =''))  

  })

  # ****************************************************************************  
  # Calculate objective function: Display objective function values
  # ****************************************************************************
  observe({
    req(input$checkDisplayObjFunctionPlot)
    # Get parameter values
    
    if(!is.null(globalVariable$parameterValue) & !is.null(globalVariable$objValue)){
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
    
    if(!is.null(globalVariable$parameterValue) & !is.null(globalVariable$objValue)){
      tableParaObj <- globalVariable$parameterValue 
      
      # replace the first row as it is the simulation number with the obj function value
      tableParaObj[,1] <- globalVariable$objValue 
      tableParaObj <- as.data.frame(tableParaObj)
      
      colnames(tableParaObj) <- c("objectiveFunction", globalVariable$paraSelection$Parameter)
      
      # round up to 3 decimal digits
      is.num <- sapply(tableParaObj, is.numeric)
      tableParaObj[is.num] <- lapply(tableParaObj[is.num], round, 3)
      
      columnsCalObjFunction <- data.frame(title = colnames(tableParaObj), 
                                          source = rep(NA, ncol(tableParaObj)),
                                          width = rep(300, ncol(tableParaObj)),
                                          type = rep('text', ncol(tableParaObj)))
      
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
  observe({
    
    req(input$checkDisplayTableSensitivity)
    
    if (globalVariable$checkSimComplete & !is.null(globalVariable$objValue)){
      
      if (globalVariable$paraSampling$samplingApproach == "Sensi_Cali_(LHS)"){
        # Get parameter values
        tableSensitivity <- globalVariable$parameterValue 
        
        # replace the first row as it is the simulation number with the obj function value
        tableSensitivity[,1] <- globalVariable$objValue 
        
        # give this matrix column names 
        colnames(tableSensitivity) <- c("objFunction", globalVariable$paraSelection[,1])
        tableSensitivity <- as.data.frame(tableSensitivity)
        
        # parameter sensitivity using multivariable regression analysi
        tableSensitivity <- summary(lm(formula = objFunction ~ ., tableSensitivity))[4]$coefficients[,3:4]
        
        # remove the first row because it is the intercept
        tableSensitivity <- tableSensitivity[-c(1),]
        
        # assign result to the global variables
        globalVariable$tableSensitivity <<- tableSensitivity
        
        # display output
        tableSensitivity <- as.data.frame(tableSensitivity)
        
        Parameter_Number <- c(1:nrow(tableSensitivity))
        Parameter_Name <- c(rownames(tableSensitivity))
        Absolute_t_Stat <- abs(tableSensitivity[,1]) 
        
        tableSensitivity <- cbind(Parameter_Number, Parameter_Name, 
                                  Absolute_t_Stat, tableSensitivity) 
        
        rownamesTableSensitivity <- rownames(tableSensitivity)
        rownames(tableSensitivity) <- NULL
        
        colnames(tableSensitivity) <- c("Parameter_Number","Parameter_Name",
                                        "Absolute_t_Stat","t_Stat", "p_Value")
        
        columnsTableSensitivity <- data.frame(title = colnames(tableSensitivity), 
                                              source = rep(NA, ncol(tableSensitivity)),
                                              width = rep(300, ncol(tableSensitivity)),
                                              type = rep('numeric', ncol(tableSensitivity)))
        
        output$tableSensitivity <- renderExcel(excelTable(data = tableSensitivity,
                                                          columns = columnsTableSensitivity,
                                                          editable = FALSE,
                                                          allowInsertRow = FALSE,
                                                          allowInsertColumn = FALSE,
                                                          allowDeleteColumn = FALSE,
                                                          allowDeleteRow = FALSE, 
                                                          rowDrag = FALSE,
                                                          columnResize = FALSE,
                                                          wordWrap = FALSE))
        myPlot <- plotSensitivity(tableSensitivity$Absolute_t_Stat, 
                                  tableSensitivity$p_Value,
                                  tableSensitivity$Parameter_Name) +
                 labs(x ="increasing sensitivity <-- Absolute t-Stat", y = "P-value --> increasing sensitivity") 
       
        output$plotlySensitivity <- renderPlotly(ggplotly(myPlot))
        
      } else if (globalVariable$paraSampling$samplingApproach == "Sensi_(Morris)"){
        
        
        globalVariable$morrisObject <<- tell(globalVariable$morrisObject, globalVariable$objValue)
        tableSensitivity <- print(globalVariable$morrisObject)
        tableSensitivity <- tableSensitivity[1:3]
        
        
        # give this matrix column names 
        tableSensitivity <- as.data.frame(tableSensitivity)
        tableSensitivity <- cbind(globalVariable$paraSelection$Parameter,tableSensitivity)
        colnames(tableSensitivity) <- c("Parameter","mu", "mu.star", "sigma")
        rownames(tableSensitivity) <- NULL        
        
        # display output      
        columnsTableSensitivity <- data.frame(title = colnames(tableSensitivity), 
                                              source = rep(NA, ncol(tableSensitivity)),
                                              width = rep(300, ncol(tableSensitivity)),
                                              type = rep('numeric', ncol(tableSensitivity)))
        
        output$tableSensitivity <- renderExcel(excelTable(data = tableSensitivity,
                                                          columns = columnsTableSensitivity,
                                                          editable = FALSE,
                                                          allowInsertRow = FALSE,
                                                          allowInsertColumn = FALSE,
                                                          allowDeleteColumn = FALSE,
                                                          allowDeleteRow = FALSE, 
                                                          rowDrag = FALSE,
                                                          columnResize = FALSE,
                                                          wordWrap = FALSE))
        
        myPlot <- plotSensitivity(tableSensitivity$mu.star, 
                                  tableSensitivity$sigma,
                                  tableSensitivity$Parameter) +
          labs(x ="mu.star --> increasing sensitivity", y = "sigma --> increasing parameter interation") 
        
        output$plotlySensitivity <- renderPlotly(ggplotly(myPlot))
        
      } else {
        
      }
      
      saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                           'SWATShinyObject.rds',
                                           sep ='')) 
      
    }
  })  
  
  # 4.3. Optimization/Uncertainty
  
  # ****************************************************************************  
  # Input behavioral threshold: Check if the input behavioral threshold is valid
  # ****************************************************************************
  observe({
    globalVariable$isBehThresholdValid <<- FALSE
    if(!is.null(input$behThreshold)){
      if(!is.null(globalVariable$objValue)){
        if (input$behThreshold > max(globalVariable$objValue)){
          output$printMaxBehThreshold <- renderText(paste("The selected value is ", 
                                                          "greater than the maximum value ", 
                                                          max(globalVariable$objValue), 
                                                          sep =""))
        } else {
          globalVariable$isBehThresholdValid <<- TRUE
          output$printMaxBehThreshold <- renderText("check threshold value OK")
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
    if(!is.null(globalVariable$parameterValue) & globalVariable$isBehThresholdValid){
      globalVariable$dataPlotVariableNumber <<- behaSimulation(globalVariable$objValue, 
                                                               globalVariable$simData, 
                                                               globalVariable$parameterValue,
                                                               input$behThreshold, 
                                                               input$plotVarNumber,
                                                               globalVariable$objFunction$Index[1],
                                                               globalVariable$observedData)
      
      tempVar <- globalVariable$dataPlotVariableNumber$ppuSimData
      tempVar <- cbind(tempVar, globalVariable$observedData[[input$plotVarNumber]]$Value)
      
      colnames(tempVar) <- c("date", "lower", "median", "upper", "best", "observed")


      output$PlotVariableNumber <- renderPlotly(plotSimulated(tempVar)) 
      
      # Table
      columnsTableBehaSim <- data.frame(title = c('Date','Lower 95PPU', 'Median', 'Upper 95PPU', 'Best Simulation'), 
                                        source = rep(NA, 5),
                                        width = rep(300, 5),
                                        type = rep('numeric', 5))
      
      output$tableBehaSim <- renderExcel(excelTable(data = globalVariable$dataPlotVariableNumber$ppuSimData,
                                                    columns = columnsTableBehaSim,
                                                    editable = FALSE,
                                                    allowInsertRow = FALSE,
                                                    allowInsertColumn = FALSE,
                                                    allowDeleteColumn = FALSE,
                                                    allowDeleteRow = FALSE, 
                                                    rowDrag = FALSE,
                                                    columnResize = FALSE,
                                                    wordWrap = FALSE)) 
      #Table beha parameter range
      columnsTableBehaParam <- data.frame(title = c('parameter', 'lower_95PPU', 'median', 
                                                    'upper_95PPU', 'bestParameter'), 
                                          source = rep(NA, 5),
                                          width = rep(300, 5),
                                          type = rep('numeric', 5))
      
      output$tableBehaParam <- renderExcel(excelTable(data = cbind(globalVariable$paraSelection$Parameter,
                                                                   globalVariable$dataPlotVariableNumber$ppuParaRange),
                                                      columns = columnsTableBehaParam,
                                                      editable = FALSE,
                                                      allowInsertRow = FALSE,
                                                      allowInsertColumn = FALSE,
                                                      allowDeleteColumn = FALSE,
                                                      allowDeleteRow = FALSE, 
                                                      rowDrag = FALSE,
                                                      columnResize = FALSE,
                                                      wordWrap = FALSE))   
      # P and r factor
      output$printPandRFactor <- renderText(
        paste("p-factor = ", globalVariable$dataPlotVariableNumber$prFactor[1],
              " r-factor = ", globalVariable$dataPlotVariableNumber$prFactor[2],
              sep =""))
      saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                           'SWATShinyObject.rds',
                                           sep ='')) 
    }    
    
  })
  
}
  
  # globalVariable <- readRDS(file = 'C:/Users/nguyenta/Documents/DemoSWATshiny/SWATShinyObject.rds') 
  # Warning in if (temp$objValue[idBest] > globalVariable$objValue) { :the condition has length > 1 and only the first element will be used
