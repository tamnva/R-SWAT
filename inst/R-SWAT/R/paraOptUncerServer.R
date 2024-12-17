

paraOptUncerServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # 4.3. Optimization/Uncertainty
    # ****************************************************************************
    # Input behavioral threshold: Check if the input behavioral threshold is valid
    # ****************************************************************************
    observe({
      print("ok40")
      # By default the input behavioral threshold value is not valid
      globalVariable$isBehThresholdValid <- FALSE

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
                globalVariable$isBehThresholdValid <- TRUE
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
      print("ok41")
    })

    # ****************************************************************************
    # Input variable number to plot: Calculate values of all tables for display
    # ****************************************************************************
    observeEvent(input$checkPlotVariableNumber, {
      print("ok42")
      print(!is.null(globalVariable$parameterValue))
      print(globalVariable$isBehThresholdValid)
      spsComps::shinyCatch(
        # Check if there are parameter values and the selected behavioral threshold is valid
        if(!is.null(globalVariable$parameterValue) &
           globalVariable$isBehThresholdValid){

          # Find behavioral simulations - 95PPU
          spsComps::shinyCatch(
            globalVariable$dataPlotVariableNumber <- behaSimulation(
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
          globalVariable$PlotVariableNumber <- plotSimulated(tempVar)

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
          columnsTableBehaParamRange <- data.frame(title = c('parameter', 'lower_95PPU',
                                                             'median','upper_95PPU',
                                                             'bestParameter'),
                                                   source = rep(NA, 5),
                                                   width = rep(300, 5),
                                                   type = rep('numeric', 5))

          # Set format for the behavioral parameter range table
          output$tableBehaParamRange <- excelR::renderExcel(excelR::excelTable(
            data = cbind(globalVariable$paraSelection$Parameter,
                         globalVariable$dataPlotVariableNumber$ppuParaRange),
            columns = columnsTableBehaParamRange,
            editable = FALSE,
            allowInsertRow = FALSE,
            allowInsertColumn = FALSE,
            allowDeleteColumn = FALSE,
            allowDeleteRow = FALSE,
            rowDrag = FALSE,
            columnResize = FALSE,
            wordWrap = FALSE
          ))

          #Table behavioral parameter sets
          numOfParameters <- length(globalVariable$paraSelection$Parameter)
          columnsTableBehaParamSet <- data.frame(title = rep("-", numOfParameters),
                                                 source = rep(NA, numOfParameters),
                                                 width = rep(300, numOfParameters),
                                                 type = rep('text', numOfParameters))

          # Set format for the behavioral parameter range table
          output$tableBehaParamSet <- excelR::renderExcel(excelR::excelTable(
            data = rbind(globalVariable$paraSelection$Parameter,
                         round(globalVariable$dataPlotVariableNumber$behaParameterSet,7)),
            columns = columnsTableBehaParamSet,
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
        },
        blocking_level = "warning"
      )
      print("ok43")
    })

  })
}
