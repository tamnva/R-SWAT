
#' Update calibration.cal file for SWAT+
#'
#' @inheritParams runSWATpar
#' @param currentDirectory path which the new calibration.cal will be saved
#'
#' @return No return values, the new calibration.cal file will be save to
#' currentDirectory
#'
#'
#' @export
#'
#'

updateCalibrationFile <- function(paraSelection, parameterValue, currentDirectory){

  # condition type
  conTyp <- c("hsg=", "texture=", "plant=", "landuse=", "region=", "region_lte=")


  fileContent <- c()
  fileContent[1] <- "calibration.cal file is created by R-SWAT"
  fileContent[2] <- as.character(nrow(paraSelection))
  fileContent[3] <- paste0("cal_parm            chg_typ       chg_val    conds ",
                           "soil_lyr1 soil_lyr2       yr1       yr2      day1",
                           "      day2   obj_tot")

  # Change to SWAT+ keywords
  paraSelection[,2] <- gsub("replace", "absval", paraSelection[,2])
  paraSelection[,2] <- gsub("relative", "relchg", paraSelection[,2])
  paraSelection[,2] <- gsub("absolute", "abschg", paraSelection[,2])

  # Remove the extension e.g., .hru in the parameter selection data
  paraName <- strsplit(trimws(paraSelection[,c(1)]), '[.]')
  for (i in 1:length(paraName)){
    paraSelection[i,1] <- paraName[[i]][1]
  }

  # remove spaces in object and conditions
  paraSelection[,5] <- gsub(" ", "", paraSelection[,5], fixed = TRUE)
  paraSelection[,6] <- gsub(" ", "", paraSelection[,6], fixed = TRUE)

  lineCounter <- 3
  for (i in 1:nrow(paraSelection)){

    # If "All" or "all" then select all object and conditions
    if(grepl("All", paraSelection[i,6], fixed = TRUE) |
       grepl("all", paraSelection[i,6], fixed = TRUE)){
      paraSelection[i,6] <- ""
    }

    if(grepl("All", paraSelection[i,5], fixed = TRUE) |
       grepl("all", paraSelection[i,5], fixed = TRUE)){
      paraSelection[i,5] <- ""
    }

    # Parameter value
    val <- parameterValue[i]

    # Soil layer 1 value
    lyr1 <- 0
    if(grepl("lyr1=", paraSelection[i,6], fixed = TRUE)){
      temp <- strsplit(paraSelection[i,6], ";")[[1]]
      for (j in 1:length(temp)){
        if (grepl("lyr1=", temp[j], fixed = TRUE)){
          layerNr <- strsplit(temp[j], "=")[[1]]
          lyr1 <- as.numeric(layerNr[length(layerNr)])
        }
      }
    }


    # Soil layer 2 value
    lyr2 <- 0
    if(grepl("lyr2=", paraSelection[i,6], fixed = TRUE)){
      temp <- strsplit(paraSelection[i,6], ";")[[1]]
      for (j in 1:length(temp)){
        if (grepl("lyr2=", temp[j], fixed = TRUE)){
          layerNr <- strsplit(temp[j], "=")[[1]]
          lyr2 <- as.numeric(layerNr[length(layerNr)])
        }
      }
    }

    year1 <- 0
    day1 <- 0
    if(grepl("year1=", paraSelection[i,6], fixed = TRUE)){
      temp <- strsplit(paraSelection[i,6], ";")[[1]]
      for (j in 1:length(temp)){
        if (grepl("year1=", temp[j], fixed = TRUE)){
          time <- strsplit(temp[j], "=")[[1]]
          year1 <- as.numeric(substr(time[length(time)], 1, 4))
          day1 <- difftime(as.Date(time[length(time)], "%Y%m%d") + 1,
                           as.Date(paste(substr(time[length(time)], 1, 4),
                                         "0101", sep = ""),  "%Y%m%d"), "days")
          day1 <- as.numeric(day1)

        }
      }
    }

    year2 <- 0
    day2 <- 0
    if(grepl("year2=", paraSelection[i,6], fixed = TRUE)){
      temp <- strsplit(paraSelection[i,6], ";")[[1]]
      for (j in 1:length(temp)){
        if (grepl("year2=", temp[j], fixed = TRUE)){
          time <- strsplit(temp[j], "=")[[1]]
          year2 <- as.numeric(substr(time[length(time)], 1, 4))
          day2 <- difftime(as.Date(time[length(time)], "%Y%m%d") + 1,
                           as.Date(paste(substr(time[length(time)], 1, 4),
                                         "0101", sep = ""),  "%Y%m%d"), "days")
          day2 <- as.numeric(day2)

        }
      }
    }

    # object total
    if (paraSelection[i,5] == ""){
      obj_tot <- 0
      obj <-  " "
    } else {
      obj_tot <- length(eval(parse(text = paste("c(",paraSelection[i,5], ")", sep = "" ))))
      obj <- gsub( ":", ",-", paraSelection[i,5])
      obj <- as.numeric(strsplit(obj, ",")[[1]])
    }

    condition <- c()
    conds <- 0
    # condition on hsg, texture, landuse, plant,...
    if(grepl("hsg=", paraSelection[i,6], fixed = TRUE)){
      temp <- strsplit(paraSelection[i,6], ";", fixed = TRUE)[[1]]
      for (j in 1:length(temp)){

        # hsg condition
        for (cond in 1:length(conTyp)){
          if(grepl(conTyp[cond], temp[j], fixed = TRUE)){
            condsplit <- strsplit(gsub(conTyp[cond], "", temp[j], fixed = TRUE), ",")[[1]]
            condsplit <- sort(condsplit)
            conds <- conds + length(condsplit)
            for (k in 1:length(condsplit)){
              condition[length(condition) + 1] <- paste(format(gsub("=", "", conTyp[cond], fixed = TRUE), width = 20, justify = "left"),
                                                        format("=", width = 10, justify = "left"),
                                                        format(0, digits = 5, width = 10, justify = "right"),
                                                        format(condsplit[k], width = 10, justify = "right"),
                                                        sep = "")
            }
          }
        }
      }
    }

    lineCounter <- lineCounter + 1
    fileContent[lineCounter]  <- paste(format(paraSelection[i,1], width = 20, justify = "left"),
                                       format(paraSelection[i,2], width = 10, justify = "left"),
                                       format(round(parameterValue[i], 5), nsmall = 5, width = 10, justify = "right"),
                                       format(conds, width = 10, justify = "right"),
                                       format(lyr1, width = 10, justify = "right"),
                                       format(lyr2, width = 10, justify = "right"),
                                       format(year1, width = 10, justify = "right"),
                                       format(year2, width = 10, justify = "right"),
                                       format(day1, width = 10, justify = "right"),
                                       format(day2, width = 10, justify = "right"),
                                       format(obj_tot, width = 10, justify = "right"),
                                       paste(format(obj, width = 10, justify = "right"), collapse = ""),
                                       sep = "")

    if (conds > 0){
      for (m in 1:conds){
        lineCounter <- lineCounter + 1
        fileContent[lineCounter]  <- condition[m]
      }
    }

  }

  writeLines(fileContent, paste(currentDirectory, "/", "calibration.cal", sep = ""))
}
