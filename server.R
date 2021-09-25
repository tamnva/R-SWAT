
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
        renderText(paste("Mutivariable regression, global sensitivity analysis approach. ",
                         "Parameters generated by Latin Hypercube Sampling (LHS)",
                         sep = "")
                   )
      
      output$printHelpInputinfo <- renderText("Please input the number of parametersets (iterations) in the InputInfo column, e.g., 10")
      
    } else if (globalVariable$paraSampling$samplingApproach == "Sensi_(Morris)") {
      output$printSelectedParaSensiApproach <- renderText("You used Morris's elementary effects screening method for global sensitivity analysis - more detail see 'sensitivity' R package")   
      output$printHelpInputinfo <- renderText("Please write morris command in the InputInfo. For example,
      
      morris(model = SWAT, factors = nParameters, binf = minColumn, bsup = maxColumn, r = 4, design = list(type = 'oat', levels = 5, grid.jump = 3))

The first 4 input fields MUST be 'model = SWAT, factors = nParameters, binf = minColumn, bsup = maxColumn', 
other input fields could be modifed, see function morris in the sensitivity package for help. In the first
4 fields, this tool automatically gets information from the above table as input for this function. E.g.,
SWAT means it will take results from the SWAT model, nParameters is the number of parameters in the above
table, minColumn and maxColumn means values in the column Min and Max from the above table, The number of
model runs are (nParameters + 1) * r")
    } else if(globalVariable$paraSampling$samplingApproach == "Cali_(DDS)"){
      output$printSelectedParaSensiApproach <- renderText(paste("You selected: Cali_(DDS) for parameter optimization, ", 
                                                                "not for sensitivtity analysis, skip this step (4.2) and proceed to the next step (4.3)", sep ="")) 
      
      output$printHelpInputinfo <- renderText("Please write the number of iterations and parallel approach. For example,
                          10, 1
means 10 iterations and the parallel approach is 1, they must be seperated by the comma 
Parallel approach = 1 => DDS is run independently from each core.
Parallel approach = 2 => The best parameterset among all cores at ith iteration is used for the next iteration)
The number mof model runs = (number of iterations + 1 ) * number of parallel runs (cores)
Please go to step 4.1 to provide information about the objective function (1) and observed data files (2) 
before '3. Run SWAT'")
    } else if (globalVariable$paraSampling$samplingApproach == "Sensi_(Sobol)") {
      output$printSelectedParaSensiApproach <- renderText("You used Sobol' sensitivity indices (variance-based sensitivity analysis) for global sensitivity analysis - more detail see 'sensitivity' R package")   
      output$printHelpInputinfo <- renderText("Please write sobol command in the InputInfo and the number of iterations (seperated by ;). For example,
        sobol(model = SWAT, X1, X2, order = 1, nboot = 100); 10
The first 3 input fields MUST be 'model = SWAT, X1, X2', 
other input fields could be modifed, see function sobol in the sensitivity package for help. In the first
3 fields, this tool automatically gets information from the above table as input for this function. E.g.,
SWAT means it will take results from the SWAT model, X1, X2 are two sets of random parameter vlaues. 
The number of model runs are (number of parameters + 1) * number of iterations (if order = 1) or
                              much higher (if order = 2)")
    } else if(globalVariable$paraSampling$samplingApproach == "Read_User_Parameter_File"){
      output$printSelectedParaSensiApproach <- renderText(paste("You selected: Read_User_Parameter_File, ", 
      "For parameter sensitivity analysis, please use your external program. ",
      "Or skip this step (4.2) and proceed to the next step (4.3)", sep ="")) 
      
      output$printHelpInputinfo <- renderText("Please paste the link to your parameter file in the InputInfo. For example,
      
              C:/data/examples/readUserParameterFile/myParameterSet.txt
                          
The format (free format, different fields are seperated by space) of this file MUST be as follows (see in the example file myParameterSet.txt)
 - 1st line is the header, next lines is your parameterset values
 - 1st column is for parameter 1 (in the table above), 2nd column is for parameter 2 and so on
 - An empty line at the end of the file

Example (NOTE: In this case, the parameter ranges (min, max) in the table above were not used, but you still need to put some values in that table)

GW_DELAY.gw   CN2.mgt   SOL_K.sol   ALPHA_BF.GW   ESCO.hru   SURLAG.hru  CH_K2.rte    SURLAG.bsn
60.1          0.1       0.12         0.2          0.55       2.5         1.5          4.5
70.1          0.2       0.22         0.12         0.65       3.5         3.5          5.5
                                              ")
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
    globalVariable$outputExtraction <<- outputExtraction  
    OutputVar <- getNumberOutputVar(outputExtraction)
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
        morrisCommand <- globalVariable$paraSampling$InputInfo
        morrisCommand <- gsub("SWAT", "NULL", morrisCommand)
        morrisCommand <- gsub("minColumn", "as.numeric(globalVariable$paraSelection$Min)", morrisCommand)
        morrisCommand <- gsub("maxColumn", "as.numeric(globalVariable$paraSelection$Max)", morrisCommand)
        morrisCommand <- gsub("nParameters", "length(globalVariable$paraSelection$Max)", morrisCommand)
        
        # call morris to generate parameters
        morrisObject <- eval(parse(text = morrisCommand))
        globalVariable$parameterValue <<- cbind(c(1:nrow(morrisObject$X)), morrisObject$X)
        globalVariable$morrisObject <<- morrisObject

      } else if (globalVariable$paraSampling$samplingApproach == "Sensi_(Sobol)"){      

        # Get and edit the input text (InputInfo) command
        sobolCommand <- globalVariable$paraSampling$InputInfo
        sobolCommand <- strsplit(sobolCommand, split = ";")[[1]]
       
        nSobol <- as.numeric(sobolCommand[2])
        sobolCommand <- gsub("SWAT", "NULL", sobolCommand[1])
        
        X1 <- runifSobol(as.numeric(globalVariable$paraSelection$Min), 
                         as.numeric(globalVariable$paraSelection$Max), 
                              nSobol, nrow(globalVariable$paraSelection))
        
        X2 <- runifSobol(as.numeric(globalVariable$paraSelection$Min), 
                         as.numeric(globalVariable$paraSelection$Max), 
                              nSobol, nrow(globalVariable$paraSelection))
        # call sobol to generate parameters
        sobolObject <- eval(parse(text = sobolCommand))
        
        globalVariable$parameterValue <<- cbind(c(1:nrow(sobolObject$X)), as.matrix(sobolObject$X))
        globalVariable$sobolObject <<- sobolObject
        
      } else if (globalVariable$paraSampling$samplingApproach == "Cali_(DDS)"){
        
        # Generate initial parameter set
        globalVariable$parameterValue <<- lhsRange(globalVariable$ncores,
                                                   getParamRange(globalVariable$paraSelection))
      } else if (globalVariable$paraSampling$samplingApproach == "Read_User_Parameter_File"){
        
        # Generate initial parameter set
        parameterValue <- as.matrix(read.table(file = trimws(globalVariable$paraSampling$InputInfo),
                                     header = TRUE, sep =""))
        parameterValue <- cbind(c(1:nrow(parameterValue)),parameterValue)
        colnames(parameterValue) <- NULL
        rownames(parameterValue) <- NULL
        
        globalVariable$parameterValue <<- parameterValue
        
      } else {
        print("Other parameter sampling approaches are under development")
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
        HTML("All current inputs were saved to the file 'SWATShinyObject.rds' in the working folder.<br> 
      SWAT is running, close this message. You can open the text file '.\\Output\\CurrentSimulationReport.log' .<br>
      in the working folder to see the current simulation. Future option (not yet implemented): enable option .<br>
      to restart from the last simulations if your simulation is interupted"),
        easyClose = TRUE,
        size = "l"
      ))
      
      # Copy unchanged file for the first simulation
      copyUnchangeFiles <- TRUE
      firstRun <- TRUE

      # Run SWAT for all iteration ---------------------------------------------  
      if ((globalVariable$paraSampling$samplingApproach == "Sensi_Cali_(LHS)") |
        (globalVariable$paraSampling$samplingApproach == "Sensi_(Morris)") |
        (globalVariable$paraSampling$samplingApproach == "Sensi_(Sobol)") |
        (globalVariable$paraSampling$samplingApproach == "Read_User_Parameter_File")){

        # Check max number of cores
        globalVariable$ncores <<- min(globalVariable$ncores, nrow(globalVariable$parameterValue))

        # run parallel
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
      } else if (globalVariable$paraSampling$samplingApproach == "Cali_(DDS)") {
        
        if(is.null(globalVariable$observedData)){
          
          showModal(modalDialog(
            title = "Not enough information to perform SWAT run",
            HTML("You selected 'Cali_(DDS)', please defined objective function and load observed data (Step 4.1) before running SWAT"),
            easyClose = TRUE,
            size = "l"
          )) 
          
        } else {
          # Save results between iteration
          saveIterationResult <- list()
          
          # Run SWAT with init       
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
          
          # Set the best objective and parameter values
          best <- list()
          if(parallelMode == 1){
            best$objValue <- temp$objValue
            best$perCriteria <- temp$perCriteria
            best$simData <- temp$simData          
          } else {
            idBest <- which(temp$objValue == max(temp$objValue))
            if (length(idBest) > 1) {idBest = idBest[1]}
            
            # Take the better parameter value and objective function values
            best$parameterValue <- best$parameterValue[idBest, ]
            best$objValue <- temp$objValue[idBest]
          }
          
          # Loop over number of iteration
          for (i in 1:nIters){
            
            if (i == 1){
              # Save iteration result
              saveIterationResult$parameterValue <- globalVariable$parameterValue
              saveIterationResult$objValue <- temp$objValue
              saveIterationResult$perCriteria <- temp$perCriteria
              saveIterationResult$simData <- temp$simData
              print("Iteration - Best objective function value (from the respective core if parallel mode = 1)")
            }
            
            print(c(i-1, round(best$objValue, digits = 3)))
            
            # Take better parameter + Generate new parameter set with DDS
            if (parallelMode == 1) {
              # Take the better parameter set/data if exist
              for (j in 1:nrow(newPar)){
                if(temp$objValue[j] > best$objValue[j]){
                  globalVariable$parameterValue[j, ] <<- newPar[j, ]
                  best$objValue[j] <- temp$objValue[j]
                }
              }
              
              # Generate new parameter set
              newPar <- data.frame(min = as.numeric(globalVariable$paraSelection$Min),
                                   max = as.numeric(globalVariable$paraSelection$Max))
              parameterValue <- saveIterationResult$parameterValue[,2:ncol(saveIterationResult$parameterValue)]
              newPar <- cbind(newPar, t(parameterValue))
              newPar <- dds(newPar, globalVariable$ncores, i, nIters, 0.2, parallelMode) 
              
            } else {
              
              idBest <- which(temp$objValue == max(temp$objValue))
              if (length(idBest) > 1) {idBest = idBest[1]}
              
              if(temp$objValue[idBest] > best$objValue){
                globalVariable$parameterValue <<- newPar[idBest, ]
                best$objValue <- temp$objValue[idBest]
              }
              # Generate new parameter set
              newPar <- data.frame(min = as.numeric(globalVariable$paraSelection$Min),
                                   max = as.numeric(globalVariable$paraSelection$Max))
              parameterValue <- saveIterationResult$parameterValue[,2:ncol(saveIterationResult$parameterValue)]
              newPar <- cbind(newPar, t(parameterValue))
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
            
            # Save iteration result
            saveIterationResult$parameterValue <- rbind(saveIterationResult$parameterValue,
                                                        newPar)
            saveIterationResult$objValue <- c(saveIterationResult$objValue,
                                              temp$objValue)
            saveIterationResult$perCriteria <- bindList(saveIterationResult$perCriteria,
                                                        temp$perCriteria)
            
            saveIterationResult$simData <- bindList(saveIterationResult$simData,
                                                    temp$simData)
          }
          
          if (parallelMode == 1) {
            # Take the better parameter set/data if exist
            for (j in 1:nrow(newPar)){
              if(temp$objValue[j] > best$objValue[j]){
                best$objValue[j] <- temp$objValue[j]
              }
            }
          } else {
            idBest <- which(temp$objValue == max(temp$objValue))
            if (length(idBest) > 1) {idBest = idBest[1]}
            
            if(temp$objValue[idBest] > best$objValue){
              best$objValue <- temp$objValue[idBest]
            }
          }
          
          print(c(i, round(best$objValue, digits = 3)))
          
          # Assign back to global variables
          globalVariable$parameterValue <<- saveIterationResult$parameterValue
          globalVariable$objValue <<- saveIterationResult$objValue
          globalVariable$perCriteria <<- saveIterationResult$perCriteria
          globalVariable$simData <<- saveIterationResult$simData
          globalVariable$parameterValue[,1] <<- c(1:nrow(globalVariable$parameterValue))
          
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
        }
        
      } else {
        print("Unknown calibration approach")
      }
      
      # End run SWAT for all iterations ----------------------------------------
      globalVariable$checkSimComplete <<- TRUE
      
      saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                           'SWATShinyObject.rds',
                                           sep ='')) 
      
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

  # ****************************************************************************  
  # 6. Get SWATShinyObject.rds file
  # ****************************************************************************  
  observe({
    volumes <- getVolumes()
    shinyFileChoose(input, "getSWATShinyObject", 
                    roots = volumes, 
                    filetypes=c('', 'rds'),
                    session = session)
    
    SWATShinyObjectFile <- parseFilePaths(volumes, input$getSWATShinyObject)
    
    if(length(SWATShinyObjectFile$datapath) == 1){
      output$printSWATShinyObject <- renderText(SWATShinyObjectFile$datapath)
      globalVariable <<- readRDS(SWATShinyObjectFile$datapath)
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
    
    # Get observed data
    if(length(observedDataFile$datapath) > 0){
      output$printObservedDataFile <- renderText(observedDataFile$datapath)
      globalVariable$observedDataFile <<- sortObservedDataFile(as.character(observedDataFile$datapath))
      globalVariable$observedData <<- list()
      for (i in 1:length(globalVariable$observedDataFile)){
        globalVariable$observedData[[i]] <<- read.table(globalVariable$observedDataFile[i], skip = 1, sep = "")
        colnames(globalVariable$observedData[[i]]) <<- c("Date", "Value")
      }
      
      # Save observed data to globalVariables
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
      
    } else if (globalVariable$paraSampling$samplingApproach == "Cali_(DDS)") {
      showModal(modalDialog(
        title = "Important message",
        "You selected 'Cali_(DDS)' - The objective function is calculated after each model run (Step 3.4)",
        easyClose = TRUE,
        size = "l"
      ))
      
    } else {
      showModal(modalDialog(
        title = "Important message",
        "Not all simulations were finised ...",
        easyClose = TRUE,
        size = "l"
      ))
    }
    
    #Save SWATShinyObject
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
        globalVariable$plotParameterSensitivity <<- plotSensitivity(tableSensitivity$Absolute_t_Stat, 
                                                                   tableSensitivity$p_Value,
                                                                   tableSensitivity$Parameter_Name) +
          labs(x ="increasing sensitivity <-- Absolute t-Stat", y = "P-value --> increasing sensitivity") 
        
        output$plotlySensitivity <- renderPlotly(ggplotly(globalVariable$plotParameterSensitivity))
        
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
        
        globalVariable$plotParameterSensitivity <<- plotSensitivity(tableSensitivity$mu.star, 
                                  tableSensitivity$sigma,
                                  tableSensitivity$Parameter) +
          labs(x ="\u03BC* => increasing sensitivity", y = "\u03C3 => increasing sensitivity") 
        
        output$plotlySensitivity <- renderPlotly(ggplotly(globalVariable$plotParameterSensitivity))
        
        # Sensitivity analysis with Sobol method -------------------------------
        
      } else if(globalVariable$paraSampling$samplingApproach == "Sensi_(Sobol)"){
        
        globalVariable$sobolObject <<- tell(globalVariable$sobolObject, globalVariable$objValue)
        
        tableSensitivity <- print(globalVariable$sobolObject)
        tableSensitivity <- tableSensitivity[1:5]
        
        # Replace X1
        paraName <- row.names(tableSensitivity)
        
        for (i in 1:length(globalVariable$paraSelection$Parameter)){
          paraName <- gsub(paste("X", i, sep = ""), 
                           globalVariable$paraSelection$Parameter[i],
                           paraName)
        }
        
        tableSensitivity <- cbind(paraName,tableSensitivity)
        
        
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
        
        # Plot
        globalVariable$plotParameterSensitivity <<- plotSensiSobol(globalVariable$paraSelection, tableSensitivity)
        output$plotlySensitivity <- renderPlotly(ggplotly(globalVariable$plotParameterSensitivity))
        
      } else if(globalVariable$paraSampling$samplingApproach == "Read_User_Parameter_File"){
        output$plotlySensitivity <- NULL
        output$tableSensitivity <- NULL
      } else {
        
      }
      
    }
  })  
  
  
  # ****************************************************************************  
  # Save plot parameter sensitivity 
  # ****************************************************************************
  observe({
    req(input$savePlotlySensitivity)
    
    showModal(
      modalDialog(
        textInput("savePlotlySensitivityFileName", "File name (must have .pdf)",
                  placeholder = 'SWATshinyPlot.pdf'
        ),
        
        numericInput("savePlotlySensitivityWidth", "Width in cm", 10, min = 1, max = 100),
        
        numericInput("savePlotlySensitivityHeight", "Height in cm", 10, min = 1, max = 100),
        
        span('Press OK to save plot as .pdf file in the working folder'),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("savePlotlySensitivityok", "OK")
        )
      )
    )
 
  })
  
  #
  observeEvent(input$savePlotlySensitivityok, {
    
    # Save plot
    setwd(globalVariable$workingFolder)
    
    ggsave(input$savePlotlySensitivityFileName, 
           plot = globalVariable$plotParameterSensitivity,
           device = "pdf",
           width = input$savePlotlySensitivityWidth,
           height = input$savePlotlySensitivityHeight,
           units = c("cm"))
    
    # print message
    showModal(modalDialog(
      title = " ",
      "Plot was saved as .pdf file in the working folder",
      easyClose = TRUE,
      size = "l"
    ))
    

    
  })
  #---------------------
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
      
      # Save observed data to globalVariables
      saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                           'SWATShinyObject.rds',
                                           sep ='')) 
      

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
      globalVariable$PlotVariableNumber <<- plotSimulated(tempVar)

      output$PlotVariableNumber <- renderPlotly(ggplotly(globalVariable$PlotVariableNumber)) 
      
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
  
  # ****************************************************************************  
  # Save plot simulated result 
  # ****************************************************************************
  observe({
    req(input$savePlotVariableNumber)
    
    showModal(
      modalDialog(
        textInput("savePlotVariableNumberFileName", "File name (must have .pdf)",
                  placeholder = 'SWATshinyPlot.pdf'
        ),
        
        numericInput("savePlotVariableNumberWidth", "Width in cm", 10, min = 1, max = 100),
        
        numericInput("savePlotVariableNumberHeight", "Height in cm", 10, min = 1, max = 100),
        
        span('Press OK to save plot as .pdf file in the working folder'),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("savePlotVariableNumberok", "OK")
        )
      )
    )
    
  })
  
  observeEvent(input$savePlotVariableNumberok, {
    
    # Save plot
    setwd(globalVariable$workingFolder)
    
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
  # 5.1. Visualize watout  
  # ****************************************************************************  
  # Update selected column watout.dat file type
  # ****************************************************************************
  observe({
    req(input$watoutFile)
    # update select input
    globalVariable$visualWatoutData <<- readWatout(input$watoutFile$datapath)
    globalVariable$visualWatoutHeader <<- readWatoutHeader(input$watoutFile$datapath)
    
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
    # update select input
    globalVariable$visualObserveData <<- read.table(input$observedFile$datapath, header = TRUE, sep = "")
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
  
    if(is.data.frame(globalVariable$visualWatoutData)){
      if (!is.null(input$observedFile$datapath)){
        if (input$selectColObs %in% colnames(globalVariable$visualObserveData)){
          pltWatout <- plotWatout(globalVariable$visualWatoutData, globalVariable$visualWatoutHeader, input$selectColWatout, 
                                  globalVariable$visualObserveData, input$selectColObs)
          output$plotWatout <- renderPlotly(pltWatout$plot)
        }

      } else {

        if (input$selectColWatout %in% globalVariable$visualWatoutHeader){
          pltWatout <- plotWatout(globalVariable$visualWatoutData, globalVariable$visualWatoutHeader, input$selectColWatout, 
                                  NULL, NULL)
          output$plotWatout <- renderPlotly(pltWatout$plot)
        }

      }
    }
    
  })

  # 5.1. Visualize output.hru
  
  # ****************************************************************************  
  # Get HRU raster file
  # ****************************************************************************
  observe({
    volumes <- getVolumes()
    shinyFileChoose(input, "getHruRaster", 
                    roots = volumes, 
                    filetypes=c('', 'adf'),
                    session = session)
    
    hruRasterFile <- parseFilePaths(volumes, input$getHruRaster)
    
    if(length(hruRasterFile$datapath) == 1){
      hruRasterFile <- as.character(hruRasterFile$datapath)
      output$printHruRaster <- renderText(hruRasterFile)
      
      if (file.exists(hruRasterFile)){

        displayOutput$hruRaster <<- raster(hruRasterFile)
      } else {
        displayOutput$hruRaster <<- NULL
      }
    }
  })
  
  # ****************************************************************************  
  # Get TxtInOut directory 
  # ****************************************************************************
  observe({
    req(input$hruTempAgg)
  })
  # Update select date rnage
  observe({
    
    req(input$hruTxtInOutFolder)
    
    fileCio <- trimws(paste(input$hruTxtInOutFolder, '/file.cio', sep=''))
    
    if (file.exists(fileCio)){

      displayOutput$hruFileCioInfo <<- getFileCioInfo(input$hruTxtInOutFolder)
      
      updateDateInput(session, "hruPlotDate",
                      min = displayOutput$hruFileCioInfo$startEval,
                      max = displayOutput$hruFileCioInfo$endSim, 
                      value = displayOutput$hruFileCioInfo$startEval[1])  
      
      updateDateRangeInput(session, "hruInputDateRange",
                           start = displayOutput$hruFileCioInfo$startEval,
                           end = displayOutput$hruFileCioInfo$endSim,
                           min = displayOutput$hruFileCioInfo$startEval,
                           max = displayOutput$hruFileCioInfo$endSim)
      
      
      updateDateInput(session, "hruPlotMonth",
                      min = displayOutput$hruFileCioInfo$startEval,
                      max = displayOutput$hruFileCioInfo$endSim, 
                      value = displayOutput$hruFileCioInfo$startEval[1])  
      
      updateDateRangeInput(session, "hruInputMonthRange",
                           start = displayOutput$hruFileCioInfo$startEval,
                           end = displayOutput$hruFileCioInfo$endSim,
                           min = displayOutput$hruFileCioInfo$startEval,
                           max = displayOutput$hruFileCioInfo$endSim)
      
      updateDateInput(session, "hruPlotYear",
                      min = displayOutput$hruFileCioInfo$startEval,
                      max = displayOutput$hruFileCioInfo$endSim, 
                      value = displayOutput$hruFileCioInfo$startEval[1])
      
      updateDateRangeInput(session, "hruInputYearRange",
                           start = displayOutput$hruFileCioInfo$startEval,
                           end = displayOutput$hruFileCioInfo$endSim,
                           min = displayOutput$hruFileCioInfo$startEval,
                           max = displayOutput$hruFileCioInfo$endSim)
      
    }
    
    fileHru <- trimws(paste(input$hruTxtInOutFolder, '/output.hru', sep=''))
  
    if(file.exists(fileHru)){
     
      ouputHruHeader <- readOutputHruHeader(fileHru)

      temp <- c('LULC', 'HRU', 'GIS', 'SUB', 'MGT', 'MO', 'DA', 'YR')

      ouputHruHeader <- ouputHruHeader[-na.omit(match(temp,ouputHruHeader))]

      updateSelectizeInput(session, "hruSelectCol",
                           choices = ouputHruHeader,
                           selected = ouputHruHeader[1])
    } else {
      updateSelectizeInput(session, "hruSelectCol",
                           choices = NULL)      
    }
    
  })
 
  # ****************************************************************************  
  # get HRU data
  # ****************************************************************************
  observe({
    
    req(input$hruTxtInOutFolder)
    
    fileHru <- trimws(paste(input$hruTxtInOutFolder, '/output.hru', sep=''))
    
    if(file.exists(fileHru)){
      # Read HRU data
      
      temp <- read.table(fileHru, header = FALSE, sep = "", skip = 9)
      colnames(temp) <- readOutputHruHeader(fileHru)
      displayOutput$hruData <<- temp
    } else {
      displayOutput$hruData <<- NULL
    }
  })
  
  # ****************************************************************************  
  # Plot hru
  # ****************************************************************************
  observe({
    
    req(input$hruTxtInOutFolder, 
        input$hruSelectCol,
        input$hruTempAgg)
  
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
     
    if (!is.null(displayOutput$hruData)){
      hruPlotData <- subsetOutputHru(displayOutput$hruData, 
                                     displayOutput$hruInputDateRange[1], 
                                     displayOutput$hruInputDateRange[2], 
                                     input$hruSelectCol, 
                                     input$hruTempAgg)

      if(!is.null(displayOutput$hruRaster)){

        hruRaster <- hruRasterValue(displayOutput$hruRaster, 
                                    hruPlotData, 
                                    displayOutput$hruPlotDate)
       
        outputText <- as.character(displayOutput$hruPlotDate)
        if (input$hruTempAgg == "Monthly"){
          outputText <- substr(outputText,1,7)
        } else if (input$hruTempAgg == "Yearly"){
          outputText <- substr(outputText,1,4)
        }
        outputText <- paste("You selected Variable: ", 
                            input$hruSelectCol, 
                            ", Timestep: ",
                            input$hruTempAgg,
                            ", Time: ",
                            outputText,
                            sep = "")
        output$hruPlotTitle <- renderText(outputText)  
        output$hruPlotHRU <- renderPlot({plot(hruRaster)})

      } else {
        output$hruPlotHRU <- NULL
      }
      
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
    
    fileName <- input$rchFileCio$datapath
    if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) == ".cio") {
      displayOutput$rchFileCioInfo <<- getFileCioInfo(input$rchFileCio$datapath)  
    } else {
      displayOutput$rchFileCioInfo <<- NULL
    }
    
  })

  # ****************************************************************************  
  # Read output.rch  file
  # ****************************************************************************
  observe({
    req(input$outputRchFile)
    
    fileName <- input$outputRchFile$datapath
    if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) == ".rch") {
      displayOutput$outputRchFile <<- readOutputRch(fileName)
      
      updateSelectizeInput(session, "rchSelectedReach",
                           choices = c(1:max(displayOutput$outputRchFile[,1])))
      
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
    
    displayOutput$rchObsData <<- read.table(input$rchObservedFile$datapath, 
                                           header = TRUE, sep = "")
    displayOutput$rchObsData[,1] <<- as.Date(displayOutput$rchObsData[,1], 
                                            "%Y-%m-%d")
    
    updateSelectizeInput(session, "rchObsVariable",
                         choices = colnames(displayOutput$rchObsData)
                         [2:ncol(displayOutput$rchObsData)])
    
    
  })
  
  # ****************************************************************************  
  # Subset observed data file
  # **************************************************************************** 
  observe({  
    req(input$rchObsVariable)
    
    if (!is.null(input$rchObsVariable) & 
        exists("rchObsData", where = displayOutput)){
      
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

    if (isTruthy(input$rchObsVariable) & isTruthy(input$rchObservedFile)){
      if (exists("rchSubsetObsData", where = displayOutput) &
          exists("outputRchFile", where = displayOutput)&
          exists("rchFileCioInfo", where = displayOutput)){
        
        outputRchSubset <- subsetOutputRch(displayOutput$outputRchFile,
                                           input$rchTempAgg, 
                                           input$rchSelectedVariable, 
                                           c(displayOutput$rchFileCioInfo$startEval, 
                                             displayOutput$rchFileCioInfo$endSim), 
                                           input$rchSelectedReach)
        
        plt <- plotOutputRchSubset(outputRchSubset, displayOutput$rchSubsetObsData)
        
        
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
  # Read file.cio information
  # ****************************************************************************
  observe({
    req(input$subFileCio)
    
    fileName <- input$subFileCio$datapath
    if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) == ".cio") {
      displayOutput$subFileCioInfo <<- getFileCioInfo(input$subFileCio$datapath)  
    } else {
      displayOutput$subFileCioInfo <<- NULL
    }
  })
  
  # ****************************************************************************  
  # Read output.sub  file
  # ****************************************************************************
  observe({
    req(input$outputSubFile)
    
    fileName <- input$outputSubFile$datapath
    if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) == ".sub") {
      displayOutput$outputSubFile <<- readOutputRch(fileName)
      
      updateSelectizeInput(session, "subSelectedSub",
                           choices = c(1:max(displayOutput$outputSubFile[,1])))
      
      updateSelectizeInput(session, "subSelectedVariable",
                           choices = colnames(displayOutput$outputSubFile))
    } else {
      displayOutput$outputSubFile <<- NULL
    }
  })


  
  # ****************************************************************************  
  # Plot result
  # **************************************************************************** 
  observe({
    
    req(input$subTempAgg)
    req(input$subSelectedVariable)
    req(input$subSelectedSub)

    if (exists("outputSubFile", where = displayOutput)&
        exists("subFileCioInfo", where = displayOutput)){
      
      outputSubSubset <- subsetOutputRch(displayOutput$outputSubFile,
                                         input$subTempAgg, 
                                         input$subSelectedVariable, 
                                         c(displayOutput$subFileCioInfo$startEval, 
                                           displayOutput$subFileCioInfo$endSim), 
                                         input$subSelectedSub)
      
      plt <- plotOutputSubSubset(outputSubSubset)
      
      
      output$subPlotSub <- renderPlotly(plt)
      
    } else {
      output$rchPlotRch <- NULL
    }        
    
  })  
  #-----------------------------------------------------------------------------  
}

# globalVariable <- readRDS(file = 'C:/Users/nguyenta/Documents/DemoSWATshiny/SWATShinyObject.rds') 
# globalVariable <- readRDS(file = 'C:/data/workingFolder/SWATShinyObject.rds') 
# order(x, decreasing = TRUE)