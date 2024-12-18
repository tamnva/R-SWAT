
generalSettingServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    #-----------------------------------------------------------------------------
    # Tab 1. General Setting
    #-----------------------------------------------------------------------------
    # ****************************************************************************
    # Check SWAT or SWAT+ project
    # ****************************************************************************
    observeEvent(input$SWATorSWATplus, {

      # Check SWAT or SWAT+ project
      if (input$SWATorSWATplus == "SWAT"){
        globalVariable$SWATProject <- TRUE
        globalVariable$SWATPlusProject <- FALSE
      } else {
        globalVariable$SWATProject <- FALSE
        globalVariable$SWATPlusProject <- TRUE
      }
      print("okcheck")
      # Initially there is no warning message
      output$checkTxtInOutFolder <- renderText("546546455")
      print("okcheck2")

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
      print("ok1")
      if (!globalVariable$loadProject){

        # Update template for parameter and output extraction according to the SWAT project
        if (globalVariable$SWATProject){
          print("ok2")
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

          globalVariable$paraSelection <- dataParaSelectionSWATPlus
        }
        print("ok3")
      }
      print("ok4")
    })

    # ****************************************************************************
    # Help button select SWAT or SWAT+ project
    # ****************************************************************************
    observeEvent(input$helpSWATorSWATplus, {

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

    observeEvent(input$workingFolder, {
      print("ok5")
      tam$a <- tam$a + 2
      print(tam$a)

      # Save working directory in the global variable
      globalVariable$workingFolder <- trimws(input$workingFolder)

      # Save link to the simulation report log file to the global variable
      globalVariable$CurrentSimulationReportFile <- paste(
        globalVariable$workingFolder,
        '/Output/CurrentSimulationReport.log',
        sep =''
      )
      # Check if working folder exists
      if(!dir.exists(globalVariable$workingFolder)){
        print("ok6")
        # Print out message if working dir does not exist
        output$checkWorkingFolder <- renderText("Input folder does not exist")
      } else {

        # If exists, does not display anything
        output$checkWorkingFolder <- renderText(" ")
      }
      print("ok6.0")
    })

    # ****************************************************************************
    # Help button select working folder
    # ****************************************************************************
    observeEvent(input$helpworkingFolder, {
      print("ok7")
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
    observeEvent(input$TxtInOutFolder, {
      print("ok8")
      # Check if TxtInOut folder exists
      print("ok8a")

      if(!dir.exists(trimws(input$TxtInOutFolder))){
        output$checkTxtInOutFolder <- renderText("Input folder does not exist")
      } else {
        output$checkTxtInOutFolder <- renderText(" ")
      }

      print("ok8b")
      # Check if .hru files exist in this folder
      if (checkDirFileExist(trimws(input$TxtInOutFolder), "", ".cio")){

        # Is this TxtInOut of SWAT or SWAT plus
        globalVariable$TxtInOutSWAT <- checkSWATorSWATplus(trimws(input$TxtInOutFolder))$SWAT
        globalVariable$TxtInOutSWATPlus <- checkSWATorSWATplus(trimws(input$TxtInOutFolder))$SWATPlus

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
        print("ok8c")
        # Save link to TxtInout Folder to the global variable
        globalVariable$TxtInOutFolder <- trimws(input$TxtInOutFolder)

        # If this is a SWAT project
        if (globalVariable$SWATProject & globalVariable$TxtInOutSWAT){

          # Get HRU information (land use, slope, soil, sub)
          spsComps::shinyCatch(
            globalVariable$HRUinfo <- getHruInfo(globalVariable$TxtInOutFolder),
            blocking_level = "error")

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
          displayOutput$uniqueHruProperties <- data.frame(
            minMaxSubbasin = c(minMaxSubbasin,rep(NA, nRow - 2)),
            Landuse = c(uniqueLandUse,rep(NA, nRow - nLU)),
            soilName = c(uniqueSoil,rep(NA, nRow - nSoil)),
            slopeClass = c(uniqueSlope,rep(NA, nRow - nSlope))
          )

          # If this is a SWAT+ project
        } else if (globalVariable$SWATPlusProject & globalVariable$TxtInOutSWATPlus){
          displayOutput$uniqueHruProperties <- NULL
          globalVariable$HRUinfo <- read.table(
            file = paste(globalVariable$TxtInOutFolder,
                         "/hru-data.hru", sep =""),
            header = TRUE, skip = 1, sep = ""
          )
          # Find maximum number of HRU


        } else {
          displayOutput$uniqueHruProperties <- NULL
          globalVariable$HRUinfo <- NULL
        }

        # Display table of HRU information (land use, soil, slope)
        output$tableHRUinfo <- renderDT(globalVariable$HRUinfo)


      } else {

        # If this is not TxtInOut folder, assign global variables to NULL
        globalVariable$HRUinfo <- NULL
        globalVariable$TxtInOutFolder <- NULL
        output$tableHRUinfo <- NULL
        displayOutput$uniqueHruProperties <- NULL
      }
      print("ok9aaaaa")
    })

    # ****************************************************************************
    # Help button select TxtInOut folder
    # ****************************************************************************
    observeEvent(input$helpTxtInOutFolder, {
      print("ok10")
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
    observeEvent(input$getSWATexe, {
      print("ok11")
      # Get full path to SWAT exe file
      shinyjs::disable("getSWATexe")
      spsComps::shinyCatch(globalVariable$SWATexeFile <- file.choose(),
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

    print("ok13-1")
    # ****************************************************************************
    # Help button select executable SWAT
    # ****************************************************************************
    observeEvent(input$helpgetSWATexe, {

      showModal(modalDialog(
        title = "Help: 4. Select executable SWAT",
        "Select the executable SWAT or SWAT+ file, for example, swat_32debug.exe,
      you could download these files here https://swat.tamu.edu/software/",
        easyClose = TRUE
      ))
    })
    print("ok13-1")
    # ****************************************************************************
    # Files with list of all SWAT parameters (get file) + display content of file
    # ****************************************************************************
    observeEvent(input$getSWATParamFile, {
      print("ok12")

      # Get full path to SWAT exe file
      shinyjs::disable("getSWATParamFile")
      spsComps::shinyCatch(globalVariable$SWATParamFile <- file.choose(),
                           blocking_level = "none")
      shinyjs::enable("getSWATParamFile")

      spsComps::shinyCatch(
        if (grepl("swatParam.txt", globalVariable$SWATParamFile, fixed = TRUE) |
            grepl("cal_parms.cal", globalVariable$SWATParamFile, fixed = TRUE)){

          globalVariable$SWATParam <- loadSwatParam(globalVariable$SWATParamFile)
          output$printSWATParamFile <- renderText(globalVariable$SWATParamFile)
          output$tableSWATParam <- renderDT(globalVariable$SWATParam)

        } else {
          output$printSWATParamFile <- renderText(
            paste("Error: The selected file must be either 'swatParam.txt'",
                  "or 'cal_parms.cal'")
          )
        },
        blocking_level = "error")

    })
    print("ok13-1")
    # ****************************************************************************
    # Help button select file SWAT (or SWAT+) parameter file
    # ****************************************************************************
    observeEvent(input$helpSWATparamFile, {
      print("ok13")
      showModal(modalDialog(
        title = "Help: 5. File with list of SWAT or SWAT+ parameters",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "helpSWATparamFile.html"),warn=FALSE)),
        easyClose = TRUE
      ))
    })

  })
}
