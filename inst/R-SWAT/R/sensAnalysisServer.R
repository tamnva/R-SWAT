
sensAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 4.2. Sensitivity Analysis
    # ****************************************************************************
    # Display parameter sensitivity ranking: need to break this to smaller code
    # ****************************************************************************
    observeEvent(input$calSensitivity, {
      spsComps::shinyCatch(
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
              tableSensitivity <- summary(lm(
                formula = objFunction ~ ., tableSensitivity))[4]$coefficients[,3:4]

              # Remove the first row because it is the intercept
              tableSensitivity <- tableSensitivity[-c(1),]

              # Assign result to the global variables

              tableSensitivity <- as.data.frame(tableSensitivity)

              # Creat data frame with the results from parameter sensi. analysis
              globalVariable$tableSensitivity <- data.frame(Parameter = rownames(tableSensitivity),
                                                            t_stat = tableSensitivity[,1],
                                                            absolute_t_stat = abs(tableSensitivity[,1]),
                                                            p_value = tableSensitivity[,2])

              # Fill output table with values
              output$tableSensitivity <- renderDT(globalVariable$tableSensitivity,
                                                  server = FALSE,
                                                  extensions = c("Buttons"),
                                                  rownames = FALSE,
                                                  options = list(dom = 'Bfrtip',
                                                                 scroller = TRUE,
                                                                 scrollX = TRUE,
                                                                 searching = TRUE,
                                                                 fixedHeader = TRUE,
                                                                 buttons = c('csv', 'excel')))

              # Set 1 second delay for the progress bar
              Sys.sleep(1)
              incProgress(1/2)
            })

            # Check if users do sensi. analysis with R packages
          } else if (globalVariable$samplingApproach %in% c('Sensi_(from_sensitivity_package)',
                                                            'Sensi_(from_userDefined_package)')){

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
              globalVariable$tableSensitivity <- sensiReport
              output$tableSensitivity <- renderDT(globalVariable$tableSensitivity)

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
        },
        blocking_level = "none")
    })


  })
}
