
# maximum upload file 500 MB
options(shiny.maxRequestSize = 500*1024^2)

# Creating server
server <- function(input, output, session) {
 
  #-----------------------------------------------------------------------------  
  # Global variables (use <<- for passing values to this parameter)
  #-----------------------------------------------------------------------------
  globalVariable <<- list()
  displayOutput <<- list()
  globalVariable$checkSimComplete <<- FALSE
  globalVariable$checkParameterTable <<- FALSE
  
    #-----------------------------------------------------------------------------
  # Default values for all tables
  #-----------------------------------------------------------------------------
  # Default setting for parameter selection
  
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
  # Default setting for parameter sampling approach 
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
  
  # Default setting for objective function
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
  # Default setting for output extraction
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
  
  #-----------------------------------------------------------------------------
  # Control data display on tab item
  #-----------------------------------------------------------------------------L
  observeEvent(input$checkParameterTableButton, {
    checkParameterTable <- checkSwatParameterName(globalVariable$paraSelection, 
                                               globalVariable$SWATParam, 
                                               globalVariable$HRUinfo)
    
    globalVariable$checkParameterTable <<- checkParameterTable$check
    output$checkParameterTableTxtOuput <- renderText(checkParameterTable$checkMessage)
  })
    
  # Display current simulation
  observe({
    req(input$checkCurrentSimulation)
    currentSim <- read.table(globalVariable$CurrentSimulationReportFile,
                             header = FALSE)
    colnames(currentSim) <- c('Finished simulation', 'Simulation number', 
                              'Out of', 'Total simulations', 'On core', 
                              'Core number', 'Date', 'Time')
    output$tableCurrentSimulation <- renderDataTable(currentSim)
  })
  
  
  
  # Update input date range for calibration
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
  
  #Display
  observe({
    req(input$checkDisplayObsVar)
    output$tableObsVarDisplay <- renderDataTable(mergeDataFrameDiffRow(globalVariable$observedData))
  })
  
  observe({
    
    req(input$checkDisplayTableSensitivity)
    
    if (globalVariable$checkSimComplete){

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
        
        figSensitivity  <- plot_ly(y = tableSensitivity$p_Value,
                                   x = tableSensitivity$Absolute_t_Stat,         
                                   type = "scatter", 
                                   mode   = 'markers', 
                                   name = rownamesTableSensitivity)
        
        ytitle <- list(title = "P-value --> increasing sensitivity")
        xtitle <- list(title = "increasing sensitivity <-- Absolute t-Stat")
        figSensitivity <- figSensitivity  %>% layout(xaxis = xtitle , yaxis = ytitle , showlegend = TRUE)
        
        output$plotlySensitivity <- renderPlotly(figSensitivity)
        
        
        
      }
     
    }
  })
  
  # Display objective function value
  observe({
    req(input$checkDisplayObjFunction)
    # Get parameter values
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
  })
  
  
  observe({
    req(input$checkPlotVariableNumber)
    
    globalVariable$dataPlotVariableNumber <<- behaSimulation(globalVariable$objValue, 
                                             globalVariable$simData, 
                                             globalVariable$parameterValue,
                                             input$behThreshold, 
                                             input$plotVarNumber,
                                             globalVariable$objFunction$Index[1],
                                             globalVariable$observedData)
    
    dataPlot <- cbind(globalVariable$dataPlotVariableNumber$ppuSimData, 
                      Date = as.Date(globalVariable$observedData[[input$plotVarNumber]]$Date, "%Y-%m-%d"),
                      Value = globalVariable$observedData[[input$plotVarNumber]]$Value
                      )
    
    PlotVariableNumber <- plot_ly(dataPlot, x = ~Date, y = ~bestSim, name = 'Simulated (best)', type = 'scatter', mode = 'lines') 
    PlotVariableNumber <- PlotVariableNumber %>% add_trace(y = ~Value, name = 'Observed', mode = 'lines')
    output$PlotVariableNumber <- renderPlotly(PlotVariableNumber)
  })
 
  observe({
    req(input$checkTableBehaSim)

    if(!is.null(globalVariable$dataPlotVariableNumber)){
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
    } else {
      output$tableBehaSim <- NULL
    }
  })
 
  observe({
    req(input$checkTableBehaParam)
    
    if(!is.null(globalVariable$dataPlotVariableNumber)){
      columnsTableBehaParam <- data.frame(title = c('parameterNumber', 'lower_95PPU', 'median', 
                                                    'upper_95PPU', 'bestParameter'), 
                                        source = rep(NA, 5),
                                        width = rep(300, 5),
                                        type = rep('numeric', 5))
      
      output$tableBehaParam <- renderExcel(excelTable(data = globalVariable$dataPlotVariableNumber$ppuParaRange,
                                                    columns = columnsTableBehaParam,
                                                    editable = FALSE,
                                                    allowInsertRow = FALSE,
                                                    allowInsertColumn = FALSE,
                                                    allowDeleteColumn = FALSE,
                                                    allowDeleteRow = FALSE, 
                                                    rowDrag = FALSE,
                                                    columnResize = FALSE,
                                                    wordWrap = FALSE))      
    } else {
      output$tableBehaParam <- NULL
    }
  })

  observe({
    req(input$checkPandRFactor)
    
    if(!is.null(globalVariable$dataPlotVariableNumber)){
      output$printPandRFactor <- renderText(
        paste("p-factor = ", globalVariable$dataPlotVariableNumber$prFactor[1],
              " r-factor = ", globalVariable$dataPlotVariableNumber$prFactor[2],
              sep ="")
      )
    } else {
      output$printPandRFactor <-  NULL
    }
  })  
  
  #-----------------------------------------------------------------------------
  # Update global parameter for each reactive input
  #-----------------------------------------------------------------------------L
  # Get HRU information (HRU number - subbasin - landuse - soil - slop)
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
  
  # Display table with name of HRU, land use, soil type
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
  
  # Read default SWAT parameter file
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
  
  # Get executable SWAT file
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
  
  
  # Get observed data files
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
    }
  })
  
  # Get list of selected parameter for calibration /optimization
  observe({
    paraSelection <-  excel_to_R(input$tableParaSelection)
    if(is.null(paraSelection)) paraSelection <- dataParaSelection
    globalVariable$paraSelection  <<- paraSelection
  })
  
  
  # Get parameter sampling approach
  observe({
    paraSampling <- excel_to_R(input$tableParaSampling)
    if(is.null(paraSampling)) paraSampling <- dataParaSampling
    globalVariable$paraSampling  <<- paraSampling
  })
  
  # Get definition of objective function
  observe({
    objFunction <- excel_to_R(input$tableObjFunction)
    if(is.null(objFunction)) objFunction <- dataObjFunction
    globalVariable$objFunction  <<- objFunction
  })
  

  # Get definition of output
  observe({
    outputExtraction <- excel_to_R(input$tableOutputExtraction)
    if(is.null(outputExtraction)) outputExtraction <- dataOutputExtraction
    globalVariable$outputExtraction  <<- outputExtraction  
    globalVariable$nOutputVar  <<- getNumberOutputVar(outputExtraction)
  })
  
  # Get working folder 
  observe({ 
    globalVariable$workingFolder <<- input$workingFolder
    globalVariable$CurrentSimulationReportFile <<- paste(input$workingFolder, 
                                                         '/Output/CurrentSimulationReport.log', 
                                                         sep ='')
    
  })

  # Get date range for calibration
  observe({ 
    globalVariable$dateRangeCali <<- input$dateRangeCali
  })     
    
  # Get nubmer of cores
  observe({ 
    globalVariable$ncores <<- input$ncores
  })  

  #-----------------------------------------------------------------------------
  # Run SWAT
  #-----------------------------------------------------------------------------
  observeEvent(input$runSWAT, {
    # Save global variables
    if (file.exists(input$workingFolder)) {
      saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                           'SWATShinyObject.rds',
                                           sep ='')
      )
    }
  
    # First get or generate parameter set
    if (globalVariable$paraSampling$samplingApproach == "Sensi_Cali_(LHS)") {
      globalVariable$parameterValue <<- lhsRange(as.numeric(globalVariable$paraSampling$InputInfo),
                                                 getParamRange(globalVariable$paraSelection))
    } else {
      Print("Other parameter sampling approaches are under development")
    }
   
    # Save global variables
    if (file.exists(input$workingFolder)) {
      saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                           'SWATShinyObject.rds',
                                           sep ='')
      )
    }
   
    # Message show all input was saved
    showModal(modalDialog(
      title = "Save current input",
      HTML("All current inputs were save to the file 'SWATShinyObject.rds'.<br> 
      in the working folder. SWAT is running, close this message .<br>
      You can open the text file '.\\Output\\CurrentSimulationReport.log' .<br>
      in the working folder to see the current simulation. If your simulation is 
           interupted you can restart from the last simulations using these two files"),
      easyClose = TRUE,
      size = "l"
    ))
    

    #  Load all files that are going to be updated
    globalVariable$caliParam <<- loadParamChangeFileContent(globalVariable$HRUinfo, 
                                            globalVariable$paraSelection,
                                            globalVariable$SWATParam, 
                                            globalVariable$TxtInOutFolder)
    
    # Copy unchanged file for the first simulation
    copyUnchangeFiles <- TRUE
    saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                         'SWATShinyObject.rds',
                                         sep =''))
 
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
               globalVariable$dateRangeCali)
    
    saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                         'SWATShinyObject.rds',
                                         sep ='')
    )
  })
   
  #-----------------------------------------------------------------------------
  # Calculate objective function value
  #-----------------------------------------------------------------------------
  observeEvent(input$calObjFunction, {
    
    # Initial text for print out
    txtForPrint <- "Calculating objective function..."
    
    # Check if the CurrentSimulationReportFile.log file exist
    check <- file.exists(globalVariable$CurrentSimulationReportFile)
    
    
    if (check){
      if(nrow(read.table(globalVariable$CurrentSimulationReportFile, 
                         header = FALSE)) == nrow(globalVariable$parameterValue)){
        
      } else {
        check = FALSE
        txtForPrint <- paste(txtForPrint, "Error: Not all simulations were finished", sep ="")
      }
    } else {
      
      # If the log file does not exist, print out message
      txtForPrint <- paste(txtForPrint, "Error: Could not find: .\\Output\\", 
                           "CurrentSimulationReportFile.log",
                           sep ="")
    }
    
    
    showModal(modalDialog(
      title = "Important message",
      txtForPrint,
      easyClose = TRUE,
      size = "l"
    ))
    
    
    if (check){
      #--------------------
      # Read input data
      nSim <- as.integer(nrow(globalVariable$parameterValue)/globalVariable$ncores)
      nSim <- rep(nSim, globalVariable$ncores)
      nSim[globalVariable$ncores] <- nSim[globalVariable$ncores] + 
        nrow(globalVariable$parameterValue) - sum(nSim)
        
      ntimestep <- as.numeric(globalVariable$dateRangeCali[2]- 
                                globalVariable$dateRangeCali[1]) + 2
      
      nVariables <- globalVariable$nOutputVar
      
      simData <- list()

      counter <- rep(0, nVariables)
      globalVariable$objValue <<-  rep(0, nrow(globalVariable$parameterValue))
      
      #Loop over number of cores
      for (i in 1:globalVariable$ncores){
        #loop over number of variables
        for (j in 1:nVariables){
          if ((i == 1)) {
            simData[[j]] <- list()
            globalVariable$perCriteria[[j]] <<- list()
            globalVariable$simData[[j]] <<- list()
          }

          #Loop over number of simulation
          fileNameSimData <- paste(globalVariable$workingFolder, "/Output/Core_", 
                                  i, "/Output_Variable_", j, ".txt", sep = "")
          tempSimData <- read.table(fileNameSimData, header = FALSE, sep = "")
          for (k in 1:nSim[i]){
            sIndex <- (k-1)*ntimestep + 1
            eIndex <- k*ntimestep
            counter[j] <- counter[j] + 1
            
            simData[[j]][[counter[j]]] <- tempSimData[(sIndex + 1):eIndex, 1]
            globalVariable$simData[[j]][[counter[j]]] <<- simData[[j]][[counter[j]]]
            
            globalVariable$perCriteria[[j]][[counter[j]]] <<- perCriteria(globalVariable$observedData[[j]][,2],
                                                             simData[[j]][[counter[j]]])
            if((i == 1) & (j == 1)) {
              perIndex <- match(globalVariable$objFunction$Index[1], 
                    colnames(globalVariable$perCriteria[[j]][[counter[j]]]))
            }
            globalVariable$objValue[counter[j]] <<- globalVariable$objValue[counter[j]] + 
                                           globalVariable$perCriteria[[j]][[counter[j]]][perIndex]         
          }
        }
      }
      # Calculate objective function values
        globalVariable$objValue <<- globalVariable$objValue/nVariables
        
      #--------------------
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
                          max = nVariables,
                          step = 1)
        
    }
    
    globalVariable$checkSimComplete <<- check
    globalVariable$textCheckSimComplete <<- txtForPrint
    saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                         'SWATShinyObject.rds',
                                         sep =''))    
  })
  }

  # globalVariable <- readRDS(file = 'C:/Users/nguyenta/Documents/DemoSWATshiny/SWATShinyObject.rds')  
  #-----------------------------------------------------------------------------
