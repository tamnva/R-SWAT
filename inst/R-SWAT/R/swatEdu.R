# ------------------------------------------------------------------------------
# Plot output SWATedu
# ------------------------------------------------------------------------------
plotSwatEdu <- function(globalVariable, varNr){

  Flag <- trimws(globalVariable$observedData[[varNr]]$Flag)
  Flag <- gsub("c", "C", Flag)
  Flag <- gsub("v", "V", Flag)
  Flag <- gsub("C", "simulated_cali", Flag)
  Flag <- gsub("V", "simulated_valid", Flag)
  Flag[which(is.na(Flag))] <- "simulated_unused"

  data <- data.frame(date = globalVariable$observedData[[varNr]]$Date,
                     observed = globalVariable$observedData[[varNr]]$Value,
                     simulated = globalVariable$SwatEduSimData[[varNr]],
                     Flag = Flag)

  myPlot <- ggplot2::ggplot(data) +
    ggplot2::geom_point(ggplot2::aes(x  = date, y = observed, color = "observed"), alpha = 0.65, size = 0.5) +
    ggplot2::geom_line(ggplot2::aes(x  = date, y = simulated, color = Flag), alpha = 0.9, linewidth = 0.5)+
    ggplot2::labs(x  = "", y = paste("variable", varNr), color = "")+
    ggplot2::theme(legend.position="bottom")

  return(myPlot)
}

# ------------------------------------------------------------------------------
# check if input parameter is NULL, NA, or empty
# ------------------------------------------------------------------------------
check_null_na_empty <- function(inputCharacter){
  check <- TRUE
  inputCharacter <- suppressWarnings(as.numeric(inputCharacter))
  if (is.na(inputCharacter)) check <- FALSE
  if (!is.numeric(inputCharacter)) check <- FALSE
  return(check)
}

# ------------------------------------------------------------------------------
# check output water balance
# ------------------------------------------------------------------------------
read_output_std <- function(output_std){
  output_std <- readLines(output_std, -1)
  output_std <- output_std[(length(output_std) - 103):length(output_std)]

  for (i in 1:length(output_std)){
    if(grepl("PRECIP =", output_std[i], fixed=TRUE)){
      output_std <- output_std[(i-1):length(output_std)]
      break
    }
  }

  out <- matrix(rep(NA, 2), ncol = 2)

  for (i in 1:length(output_std)){
    if(grepl(" = ", output_std[i], fixed=TRUE)){
      temp <- strsplit(output_std[i], split = " = ", fixed = TRUE)[[1]]
      out <- rbind(out, trimws(temp))
    }
  }

  out <- out[-c(1),]
  colnames(out) <- c("name", "value")

  output <- list()
  temp <- which(out[,1] == "ORGANIC N")
  output$waterbalance <- out[1:(temp -1),]

  output$nutrientblance <- out[temp:nrow(out),]

  return(output)

}
