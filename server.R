
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
  # Global function for running SWAT
  #-----------------------------------------------------------------------------
  SWAT <- function(parameterValue){
    if (is.matrix(parameterValue) |
        is.data.frame(parameterValue)){
      nrow <- nrow(parameterValue)
      parameterValue <- cbind(c(1:nrow), parameterValue)

    } else {
      parameterValue <- matrix(c(1,parameterValue), nrow = 1)   

    }
    #Remove row and column names
    rownames(parameterValue) <- NULL
    colnames(parameterValue) <- NULL

    # Save 
    globalVariable$parameterValue <<- rbind(globalVariable$parameterValue, parameterValue)
    ncores <- min(globalVariable$ncores, nrow(parameterValue))
    
    runSWATpar(globalVariable$workingFolder, 
               globalVariable$TxtInOutFolder, 
               globalVariable$outputExtraction, 
               globalVariable$ncores, 
               globalVariable$SWATexeFile, 
               parameterValue,
               globalVariable$caliParam,
               globalVariable$copyUnchangeFiles,
               globalVariable$fileCioInfo,
               globalVariable$dateRangeCali,
               globalVariable$firstRun)
   
    # Objective function with initial parameter set
    temp <- calObjFunction(parameterValue,
                           ncores, 
                           globalVariable$nOutputVar,
                           globalVariable$userReadSwatOutput, 
                           globalVariable$observedData, 
                           globalVariable$workingFolder, 
                           globalVariable$objFunction)
   
   if (is.null(globalVariable$simData)){
     globalVariable$simData <<-temp$simData
     globalVariable$objValue <<- temp$objValue
   } else {
     globalVariable$simData <<- bindList(globalVariable$simData, temp$simData)
     globalVariable$objValue <<- c(globalVariable$objValue, temp$objValue)
   }
   
   # Set first run is false
   globalVariable$copyUnchangeFiles <<- FALSE
   globalVariable$firstRun <<- FALSE
   
   # Minimize or maximize objective function value
   if (globalVariable$minOrmax == "Minimize"){
     output <- temp$objValue
   } else {
     output <- - temp$objValue
   }
   
   return(output)
  }  

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
    req(input$samplingApproach)
    globalVariable$samplingApproach <<- input$samplingApproach
    if (input$samplingApproach == 'Sensi_Cali_(uniform_Latin_Hpercube_Sampling)'){
      updateTextAreaInput(session, "InputInfo", "3. Additional infomation about the selected sensitivity/calibration approach", 
                          "Delete all text here and type the number of iterations (number of parameter sets), for example, 
                                                       10
This approach similar to the SUFI-2 approach")
      
    } else if (input$samplingApproach == 'Cali_(from_optimization_package)'){
      updateTextAreaInput(session, "InputInfo", "3. Additional infomation about the selected sensitivity/calibration approach", 
                          "optim_sa(fun = SWAT, start = c(runif(nParam, min = minCol, max = maxCol)), lower = minCol,upper = maxCol, trace = TRUE, control = list(t0 = 10,nlimit = 5,t_min = 0.1, dyn_rf = FALSE,rf = 1,r = 0.7))"
      )
      
    } else if (input$samplingApproach == 'Cali_(Dynamically_Dimensioned_Search)'){
      updateTextAreaInput(session, "InputInfo", "3. Additional infomation about the selected sensitivity/calibration approach", 
                          "Delte all text here and type the number of iterations and parallel approach, seperated by comma, for example,
                                                      10, 1
10 means the number of interation
1 means the parallel approach (DDS run independently in each core)
2 means intermediate best parameter from all cores is selected and assigned as inital parameter set for next run in all cores")      
    } else if (input$samplingApproach == 'Read_User_Parameter_File'){
      updateTextAreaInput(session, "InputInfo", "3. Additional infomation about the selected sensitivity/calibration approach", 
                          "Delte all text here and type the link to the file, for example,
                                                 C:/data/myParameterFile.txt
The format (free format, different fields are seperated by space) of this file MUST be as follows (see in the example file myParameterSet.txt)
  - 1st line is the header, next lines is your parameterset values
  - 1st column is for parameter 1 (in the table above), 2nd column is for parameter 2 and so on
  - An empty line at the end of the file
Example (NOTE: In this case, the parameter ranges (min, max) in the table above were not used, but you still need to put some values in the min max columns)
GW_DELAY.gw   CN2.mgt   SOL_K.sol   ALPHA_BF.GW   ESCO.hru   SURLAG.hru  CH_K2.rte    SURLAG.bsn
60.1          0.1       0.12         0.2          0.55       2.5         1.5          4.5
70.1          0.2       0.22         0.12         0.65       3.5         3.5          5.5
                                              ")      
    } else {
      updateTextAreaInput(session, "InputInfo", "3. Additional infomation about the selected sensitivity/calibration approachh", 
                          "Delte all text here and type the R command from the 'sensitivity' package' as suggest below:
          When typing functions from the 'sensitivity' package, you might need to run the model, access the min, max of your selected parameters 
          from the above table, the number of selected parameters. You can ACCESS to the model and THESE VARIABLES using the following KEYWORDS
          'SWAT' is the SWAT model/function with inputs are parameters (and all user settings from RSWAT, these were hard coded in the SWAT function) and outputs are vector of objective function values
          'minCol' is a vector of the Min column from the above table
          'maxCol' is a vector of the Max column from the above table
          'nParam' is the number of your selected parameters from the above table

-----------------------------------------------------------------------------------------------------------------------------------------------------  
For sensitivity analysis, you should type only 3 lines of R code (no line break, blank and comment lines are not counted), e.g., with Morris approach
------------------------------------------------------------------------------------------------------------------------------------------------------
# First R command line creates a 'sensitivity object', the variable name MUST be 'sensiObject', for example
sensCaliObject <-  morris(model = SWAT, factors = nParam, binf = minCol, bsup = maxCol, r = 4, design = list(type = 'oat', levels = 5, grid.jump = 3))

# Second R command line tells this tool where the resulted sensitivity table are stored
print(sensCaliObject)[]
                          ")      
    }

  })
  
  # ****************************************************************************  
  # Parameter sampling: get input information
  # ****************************************************************************
  observe({
    #--------------------------------------------------
    req(input$InputInfo)
    
    if (input$samplingApproach == 'Sensi_(from_sensitivity_package)' |
        input$samplingApproach == 'Cali_(from_optimization_package)' |
        input$samplingApproach == 'Sensi_(from_userDefined_package)' |
        input$samplingApproach == 'Cali_(from_userDefined_package)' ){
      globalVariable$sensCaliCommand <<- input$InputInfo
      globalVariable$sensCaliCommand <<- splitRemoveComment(globalVariable$sensCaliCommand)
      
      outputTex <- NULL
      if (!is.null(globalVariable$sensCaliCommand)){
        for (i in 1:length(globalVariable$sensCaliCommand)){
          outputTex <- paste(outputTex, globalVariable$sensCaliCommand[i], "\n", sep ="")
        }            
      }    
      output$displayInputInfo <- renderText(outputTex)
    } else {
      output$displayInputInfo <- NULL
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

    # ****************************************************************************  
    # Parameter sampling: executing input R command in the input text box
    # ****************************************************************************
    
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
      
      # First get or generate parameter set
      if (globalVariable$samplingApproach == 'Cali_(Dynamically_Dimensioned_Search)'){
        
        # Generate initial parameter set
        globalVariable$parameterValue <<- lhsRange(globalVariable$ncores,
                                                   getParamRange(globalVariable$paraSelection))
      } 
      
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
      if ((globalVariable$samplingApproach == 'Sensi_Cali_(uniform_Latin_Hpercube_Sampling)') |
        (globalVariable$samplingApproach == 'Read_User_Parameter_File')){

        # Generate parameter values
        if(input$samplingApproach == 'Sensi_Cali_(uniform_Latin_Hpercube_Sampling)'){
          globalVariable$parameterValue <<- lhsRange(as.numeric(input$InputInfo),
                                                     getParamRange(globalVariable$paraSelection))          
        } else if (globalVariable$samplingApproach == "Read_User_Parameter_File"){
          parameterValue <- as.matrix(read.table(file = trimws(globalVariable$InputInfo),
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
      } else if (globalVariable$samplingApproach == 'Cali_(Dynamically_Dimensioned_Search)') {
        
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
          nIters <- as.numeric(strsplit(globalVariable$InputInfo, 
                                        split = ",", 
                                        fixed = TRUE)[[1]][1])
          
          # Get parallel mode
          parallelMode <- as.numeric(strsplit(globalVariable$InputInfo, 
                                              split = ",", 
                                              fixed = TRUE)[[1]][2])
          
          # Objective function with initial parameter set
          temp <- calObjFunction(globalVariable$parameterValue,
                                 globalVariable$ncores, 
                                 globalVariable$nOutputVar,
                                 globalVariable$userReadSwatOutput, 
                                 globalVariable$observedData, 
                                 globalVariable$workingFolder, 
                                 globalVariable$objFunction)
          
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
                                   globalVariable$objFunction)
            
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
        }
        
      } else if (globalVariable$samplingApproach == 'Sensi_(from_sensitivity_package)' |
                 globalVariable$samplingApproach == 'Sensi_(from_userDefined_package)' |
                 globalVariable$samplingApproach == 'Cali_(from_optimization_package)' |
                 globalVariable$samplingApproach == 'Cali_(from_userDefined_package)') {
        
        if(is.null(globalVariable$observedData)){
          showModal(modalDialog(
            title = "Not enough information to perform SWAT run",
            HTML("Please defined objective function and load observed data (Step 4.1) before running SWAT"),
            easyClose = TRUE,
            size = "l"
          ))
          
        } else {
          # There is no simulated data and obj function values when the model has not been run
          globalVariable$simData <<- NULL
          globalVariable$objValue <<- NULL
          
          # First run is true
          globalVariable$copyUnchangeFiles <<- TRUE
          globalVariable$firstRun <<- TRUE
          
          globalVariable$sensCaliObject <<- eval(parse(text = globalVariable$sensCaliCommand[1]))
          
          # Print output to screen
          # print(globalVariable$sensCaliObject)
          # print(globalVariable$objValue)          
        }

      } else {
        showModal(modalDialog(
          title = "Not enough information to perform SWAT run",
          HTML("Unkown method"),
          easyClose = TRUE,
          size = "l"
        ))       
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
  # ****************************************************************************  
  # 4.1. Get user input Objective function
  # ****************************************************************************    
  observe({
    req(input$objFunction)
    globalVariable$objFunction  <<- input$objFunction
    
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
    globalVariable$minOrmax  <<- input$minOrmax
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
      
      if (globalVariable$samplingApproach == 'Cali_(Dynamically_Dimensioned_Search)' |
          globalVariable$samplingApproach == 'Cali_(from_userDefined_package)'  |
          globalVariable$samplingApproach == 'Cali_(from_optimization_package)' |
          globalVariable$samplingApproach == 'Sensi_(from_sensitivity_package)' |
          globalVariable$samplingApproach == 'Sensi_(from_userDefined_package)' ){
        
        showModal(modalDialog(
          title = "Important message",
          "The objective function was/will be calculated after each model run (Step 3)",
          easyClose = TRUE,
          size = "l"
        ))
        
      } else if (globalVariable$samplingApproach == 'Sensi_Cali_(uniform_Latin_Hpercube_Sampling)'|
                 globalVariable$samplingApproach == 'Read_User_Parameter_File'){

        # Caculate objective function
        temp <- calObjFunction(globalVariable$parameterValue,
                               globalVariable$ncores, 
                               globalVariable$nOutputVar,
                               globalVariable$userReadSwatOutput, 
                               globalVariable$observedData, 
                               globalVariable$workingFolder, 
                               globalVariable$objFunction)
        
        globalVariable$objValue <<- temp$objValue
        globalVariable$perCriteria <<- temp$perCriteria
        globalVariable$simData <<- temp$simData
        
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
  # Update the user interface
  # ****************************************************************************
  observe({
    req(input$updateUI)
    if (!is.null(globalVariable$objValue)){
      # Update numeric input (threshold objective function)
      minObjValue <- min(globalVariable$objValue)
      maxObjValue <- max(globalVariable$objValue)
      
      updateNumericInput(session = session, "behThreshold", 
                         label = "2. Input behavioral threshold", 
                         value = minObjValue,
                         min = minObjValue, 
                         max = maxObjValue, 
                         step = (maxObjValue - minObjValue)/20)
      
      # Update select variable number
      updateSliderInput(session = session,
                        "plotVarNumber", 
                        "3. Input variable number to plot", 
                        value = 1, 
                        min = 1, 
                        max = globalVariable$nOutputVar,
                        step = 1)
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
  observeEvent(input$calSensitivity, {

    
    if (globalVariable$checkSimComplete & !is.null(globalVariable$objValue)){

      # Message show all input was saved
      showModal(modalDialog(
        title = "Sensitivity analysis",
        HTML("Performing sensitivity analysis"),
        easyClose = TRUE,
        size = "l"
      ))
      
      #-------------------------------------      
      if (globalVariable$samplingApproach == 'Sensi_Cali_(uniform_Latin_Hpercube_Sampling)'){

        # Table with parameter and objective funtion values
        tableSensitivity <- globalVariable$parameterValue 
        tableSensitivity[,1] <- globalVariable$objValue 
       
        # Column names 
        colnames(tableSensitivity) <- c("objFunction", globalVariable$paraSelection[,1])
        tableSensitivity <- as.data.frame(tableSensitivity)
        
        # parameter sensitivity using multivariable regression analysi
        tableSensitivity <- summary(lm(formula = objFunction ~ ., tableSensitivity))[4]$coefficients[,3:4]
        
        # remove the first row because it is the intercept
        tableSensitivity <- tableSensitivity[-c(1),]
        
        # assign result to the global variables
        
        tableSensitivity <- as.data.frame(tableSensitivity)
        
        globalVariable$tableSensitivity <<- data.frame(Parameter = rownames(tableSensitivity),
                                                      t_stat = tableSensitivity[,1],
                                                      absolute_t_stat = abs(tableSensitivity[,1]),
                                                      p_value = tableSensitivity[,2])
        
        output$tableSensitivity <- renderDataTable(globalVariable$tableSensitivity)

      } else if (globalVariable$samplingApproach == 'Sensi_(from_sensitivity_package)'|
                 globalVariable$samplingApproach == 'Sensi_(from_userDefined_package)'){
print("ok")
print(globalVariable$sensCaliObject)
        sensCaliObject <- globalVariable$sensCaliObject
        sensiReport <- eval(parse(text = globalVariable$sensCaliCommand[2])) 
        
        print(sensiReport)[]
        
        if ("X1" %in% rownames(sensiReport)) {
          sensiReport <- cbind(parameters = globalVariable$paraSelection[,1],sensiReport)
        }
        
        globalVariable$tableSensitivity <<- sensiReport
        output$tableSensitivity <- renderDataTable(globalVariable$tableSensitivity)
        
      } else if(globalVariable$samplingApproach == "Read_User_Parameter_File"){
        output$tableSensitivity <- NULL
        output$displaySensitivityReport <- renderText("You read parameter sets from external file \n Please use external program for sensitivity analysis in this case ")
      } else {
        
      }
      
    } else {
      # Message show all input was saved
      showModal(modalDialog(
        title = "Sensitivity analysis",
        HTML("Not all simulations are finished"),
        easyClose = TRUE,
        size = "l"
      ))
    }
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
      
      globalVariable$dataPlotVariableNumber <<- behaSimulation(globalVariable$objValue,
                                                               globalVariable$simData,
                                                               globalVariable$parameterValue,
                                                               input$behThreshold,
                                                               input$plotVarNumber,
                                                               globalVariable$objFunction,
                                                               globalVariable$observedData,
                                                               globalVariable$minOrmax)
      
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
  
  # ****************************************************************************  
  # Save all results
  # ****************************************************************************
  observe({
    req(input$saveAllResults)
    # Save observed data to globalVariables
    saveRDS(globalVariable, file = paste(input$workingFolder, '/', 
                                         'SWATShinyObject.rds',
                                         sep ='')) 
    showModal(modalDialog(
      title = "Save results",
      HTML("All results was saved as 'SWATShinyObject.rds' in the working folder"),
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
  # Read subbasin shape file
  # ****************************************************************************
  observe({
    volumes <- getVolumes()
    shinyFileChoose(input, "getSubShape", 
                    roots = volumes, 
                    filetypes=c('', 'shp'),
                    session = session)
    
    subShapeFile <- parseFilePaths(volumes, input$getSubShape)
    
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
    
    fileCio <- trimws(paste(input$subTxtInOutFolder, '/file.cio', sep=''))
    
    if (file.exists(fileCio)){
      
      displayOutput$subFileCioInfo <<- getFileCioInfo(input$subTxtInOutFolder)
      
      updateDateInput(session, "subPlotDate",
                      min = displayOutput$subFileCioInfo$startEval,
                      max = displayOutput$subFileCioInfo$endSim, 
                      value = displayOutput$subFileCioInfo$startEval[1])  
      
      updateDateRangeInput(session, "subInputDateRange",
                           start = displayOutput$subFileCioInfo$startEval,
                           end = displayOutput$subFileCioInfo$endSim,
                           min = displayOutput$subFileCioInfo$startEval,
                           max = displayOutput$subFileCioInfo$endSim)
      
      
      updateDateInput(session, "subPlotMonth",
                      min = displayOutput$subFileCioInfo$startEval,
                      max = displayOutput$subFileCioInfo$endSim, 
                      value = displayOutput$subFileCioInfo$startEval[1])  
      
      updateDateRangeInput(session, "subInputMonthRange",
                           start = displayOutput$subFileCioInfo$startEval,
                           end = displayOutput$subFileCioInfo$endSim,
                           min = displayOutput$subFileCioInfo$startEval,
                           max = displayOutput$subFileCioInfo$endSim)
      
      updateDateInput(session, "subPlotYear",
                      min = displayOutput$subFileCioInfo$startEval,
                      max = displayOutput$subFileCioInfo$endSim, 
                      value = displayOutput$subFileCioInfo$startEval[1])
      
      updateDateRangeInput(session, "subInputYearRange",
                           start = displayOutput$subFileCioInfo$startEval,
                           end = displayOutput$subFileCioInfo$endSim,
                           min = displayOutput$subFileCioInfo$startEval,
                           max = displayOutput$subFileCioInfo$endSim)
      
    }
    
    # Get output.sub data and update select column
    fileSub <- trimws(paste(input$subTxtInOutFolder, '/output.sub', sep=''))
    
    if(file.exists(fileSub)){
        displayOutput$subData <<- readOutputRch(fileSub)

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
    
    if (!is.null(displayOutput$subData)){
      subPlotData <- subsetOutputHru(displayOutput$subData, 
                                     displayOutput$subInputDateRange[1], 
                                     displayOutput$subInputDateRange[2], 
                                     input$subSelectCol, 
                                     input$subTempAgg)
      
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
        output$subPlotTitle <- renderText(outputText) 
        output$subPlotSub <- renderPlot({
          ggplotPolygon(displayOutput$subShape, as.numeric(subPlotData[rowNr, 2:ncol(subPlotData)]))
        })
        
      } else {
        output$subPlotSub <- NULL
      }
      
    } else {
      output$subPlotSub <- NULL
    }
    
    
  })
  

  #-----------------------------------------------------------------------------  
}

# globalVariable <- readRDS(file = 'C:/Users/nguyenta/Documents/DemoSWATshiny/SWATShinyObject.rds') 
# globalVariable <- readRDS(file = 'C:/data/workingFolder/SWATShinyObject.rds') 
# order(x, decreasing = TRUE)