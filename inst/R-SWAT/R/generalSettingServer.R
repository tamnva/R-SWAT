
generalSettingServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    #--------------------------------------------------------------------------#
    #                      Check SWAT or SWAT+ project                         #
    #--------------------------------------------------------------------------#
    observe({

      req(input$SWATorSWATplus)

      # Check SWAT or SWAT+ project
      if (input$SWATorSWATplus == "SWAT"){
        globalVariable$SWATProject <- TRUE
        globalVariable$SWATPlusProject <- FALSE
      } else {
        globalVariable$SWATProject <- FALSE
        globalVariable$SWATPlusProject <- TRUE
      }

      out_check <- check_txtinout_message(input$TxtInOutFolder,
                                          globalVariable$SWATPlusProject)
      globalVariable$TxtInOutSWAT <- out_check$txtinout_swat
      globalVariable$TxtInOutSWATPlus <- out_check$txtinout_swat_plus
      output$checkTxtInOutFolder <- renderText(out_check$out_message)

      # ****************************************************************************
      # Select SWAT parameters for calibration and/or sensitivity: Default setting
      # ****************************************************************************

      if (!globalVariable$loadProject){

        # Update template for parameter and output extraction according to the SWAT project
        if (globalVariable$SWATProject){

          # Example of parameter selection for SWAT project
          output$tableParaSelection <-
            excelR::renderExcel(excelR::excelTable(
              data = dataParaSelectionSWAT,
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
            excelR::renderExcel(excelR::excelTable(
              data = dataOutputExtractionSWAT,
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
            excelR::renderExcel(
              excelR::excelTable(data = dataParaSelectionSWATPlus,
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
            excelR::renderExcel(
              excelR::excelTable(data = dataOutputExtractionSWATPlus,
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
          globalVariable$paraSelection <- dataParaSelectionSWATPlus
        }
      }
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

    observe({
      req(input$workingFolder)
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
    observeEvent(input$helpworkingFolder, {
      showModal(modalDialog(
        title = "Help: 2. Working folder",
        "All files created by R-SWAT will be saved in this folder, for example,
      project setting file 'RSWATproject.rds' when you save your project,
      this file also can be used to load project settings from previous setup,
      simulation outputs, TxtInOut folders for parallel runs, etc...",
        easyClose = TRUE
      ))
      globalVariable$workingFolder
    })

    # ****************************************************************************
    # TxtInOut folder: Display HRU info from TxtInOut folder
    # ****************************************************************************
    observe({

      req(input$TxtInOutFolder)
      if(!dir.exists(trimws(input$TxtInOutFolder))){
        output$checkTxtInOutFolder <- renderText("Input folder does not exist")
      } else {
        output$checkTxtInOutFolder <- renderText(" ")
      }


      # Check if .hru files exist in this folder
      if (checkDirFileExist(trimws(input$TxtInOutFolder), "", ".cio")){

        out_check <- check_txtinout_message(input$TxtInOutFolder,
                                            globalVariable$SWATPlusProject)
        globalVariable$TxtInOutSWAT <- out_check$txtinout_swat
        globalVariable$TxtInOutSWATPlus <- out_check$txtinout_swat_plus
        output$checkTxtInOutFolder <- renderText(out_check$out_message)

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
        } else if (globalVariable$SWATPlusProject &
                   globalVariable$TxtInOutSWATPlus){
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
    })

    # ****************************************************************************
    # Help button select TxtInOut folder
    # ****************************************************************************
    observeEvent(input$helpTxtInOutFolder, {
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
      spsComps::shinyCatch(globalVariable$SWATexeFile <- file.choose(),
                           blocking_level = "none")
      shinyjs::enable("getSWATexe")

      spsComps::shinyCatch(
        if (grepl(".exe", globalVariable$SWATexeFile, fixed = TRUE)){
          output$printSWATexe <- renderText(globalVariable$SWATexeFile)
        } else {
          output$printSWATexe <- renderText(
            "Error: The selected file must have '.exe' extention")
        },
        blocking_level = "error")


    })
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
    # ****************************************************************************
    # Files with list of all SWAT parameters (get file) + display content of file
    # ****************************************************************************
    observe({
      req(input$getSWATParamFile)

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
    # ****************************************************************************
    # Help button select file SWAT (or SWAT+) parameter file
    # ****************************************************************************
    observeEvent(input$helpSWATparamFile, {
      showModal(modalDialog(
        title = "Help: 5. File with list of SWAT or SWAT+ parameters",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "helpSWATparamFile.html"),warn=FALSE)),
        easyClose = TRUE
      ))
    })

  })
}
