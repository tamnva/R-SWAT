
swatEduServer <- function(id) {
  moduleServer(id, function(input, output, session) {


    # ****************************************************************************
    # Default list of parameters for manual calibration
    # ****************************************************************************
    observe({
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
    })



    # ****************************************************************************
    # Help button R-SWAT Education select parameter value
    # ****************************************************************************
    observeEvent(input$helpSwatEduSelectParam, {

      showModal(modalDialog(
        title = "1. Selecting parameter values",
        HTML(readLines(file.path(HTMLdir,"HTML",
                                 "selectParameterValue.html"),warn=FALSE)),
        easyClose = TRUE
      ))
    })

    # ****************************************************************************
    # Help button R-SWAT Education run model and model perforamnce
    # ****************************************************************************
    observeEvent(input$helpModelRunPerf, {

      showModal(modalDialog(
        title = "2. Model run",
        HTML(readLines(file.path(HTMLdir,"HTML",
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
          globalVariable$caliParam <- updatedFileContent(globalVariable$HRUinfo,
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
          globalVariable$SwatEduSimData <- list()
          for (i in 1:globalVariable$nOutputVar){
            globalVariable$SwatEduSimData[[i]] <- read.csv(file = paste0(globalVariable$workingFolder,
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

          globalVariable$runManualCaliSuccess <- TRUE

          # Get water balance and nutrient balance
          if (globalVariable$SWATProject){
            output_std <- paste0(globalVariable$workingFolder, "/TxtInOut_1/output.std")
            output_std <- read_output_std(output_std)
            output$swatEduWaterBalance <- renderDT(output_std$waterbalance,
                                                   options = list(scrollX = TRUE, searching = FALSE))
            output$swatEduNutrientBalance <- renderDT(output_std$nutrientblance,
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
    observeEvent(input$showPlotVariables, {

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


  })
}
