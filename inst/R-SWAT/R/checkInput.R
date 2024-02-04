#' Check the existence of files and folders
#'
#' @description
#' Check if a given directory and a given file in that diretory exist
#'
#' @param InputDir Input directory needs to be checked
#' @param fileName Name of a file needs to be checked
#' @param fileExtention Type of the files need to be checked

#' @return A logical variable (TRUE) if both exists
#'
#' @examples
#'
#' checkDirFileExist(InputDir, fileName, fileExtention)
#'
#' @export

checkDirFileExist <- function(InputDir, fileName, fileExtention){

  # Initially check is false
  check <- FALSE

  # First check input dir if it exists
  if (dir.exists(InputDir)) check = TRUE

  # If input dir exists, check files inside
  if (check){

    setwd(InputDir)
    filesExist <- FALSE
    files <- list.files(pattern = paste(fileName, fileExtention, sep =""))
    if (length(files) > 0) {
      check <- TRUE
    } else {
      check <- FALSE
    }
  }
  return(check)
}

#' check checkSwatParameterName
#'
#' @description
#' checkDirFileExist
#'
#' @param InputDir no param is require
#' @param fileName te
#' @param fileExtention asdf

#' @return a bollkne
#'
#' @examples
#' checkSwatParameterName(paraSelection, SWATParam, HRUinfo, SWATProject)
#' @export

# ------------------------------------------------------------------------------
# Check SWAT parameter table for calibration
# ------------------------------------------------------------------------------
checkSwatParameterName <- function(paraSelection, SWATParam, HRUinfo, SWATProject){

  check <- TRUE
  checkMessage <- "Error: "

  if(SWATProject){

    if (is.null(paraSelection)) {
      checkMessage <- paste(checkMessage, "Cannot find the parameter table that ",
                            "needs to be check, please add/modify something ",
                            "in the table. ",
                            sep ="")
      check <- check & FALSE
    }

    if (is.null(SWATParam)) {
      checkMessage <- paste(checkMessage, "Cannot find the SWAT parameter file, ",
                            "please go to General Setting and select the file. ",
                            sep ="")
      check <- check & FALSE
    }

    if (check) {

      for (i in 1:length(paraSelection$Parameter)){

        if (!(paraSelection$Parameter[i] %in%  SWATParam$parameter)){
          checkMessage <- paste(checkMessage, "Cannot find parameter ",
                                paraSelection$Parameter[i],
                                " in the list of SWAT parameter. ",
                                sep = "")
          check <- check & FALSE
        }
      }

      #----------------------
      if (min(as.numeric(paraSelection$Max) - as.numeric(paraSelection$Min)) < 0.0){
        checkMessage <- paste(checkMessage, "Maximum value must be >= Min",
                              sep = "")
        check <- check & FALSE
      }
      #----------------------
      # check subbasin land use, soil, slope combination produce zero?

      if(is.null(HRUinfo)){
        checkMessage <- paste(checkMessage, "Cannot find HRU information (subbasin,",
                              " land use, soil, slope,for checking. Please go back",
                              " to General Setting using input TxtInOut folder. "
                              , sep = "")
        check <- check & FALSE
      } else {

        tempCheck <- checkHRUCombination(paraSelection, SWATParam, HRUinfo)
        if (!tempCheck$check){
          checkMessage <- paste(checkMessage, tempCheck$checkMessage, sep = "")
          check <- check & FALSE
        }

      }
    }

  # SWAT+ project
  } else {
    # Get the selected parameter name
    selectedParam <- paraSelection[,1]

    # Check if the selected parameter in the SWATParam
    check <- check & all(selectedParam %in% paste(SWATParam[,1], ".", SWATParam[,2], sep = ""))

    # Display message if check is not true
    if (!check){
      checkMessage <- paste(checkMessage, " Cannot find the selected parameter",
                            " in the cal_parms.cal file", sep = "")
    }

    # Check if max > min
    if (!all(as.numeric(paraSelection[,4]) >= as.numeric(paraSelection[,3]))){
      checkMessage <- paste(checkMessage, "Maximum value must be >= Min",
                            sep = "")
      check <- check & FALSE
    }
  }


  output <- list()

  if (!check){
    output$checkMessage <- checkMessage
    output$check <- FALSE
  } else {
    output$checkMessage <- "Check successfull! Input is correct"
    output$check <- check
  }

  return(output)
}

# ------------------------------------------------------------------------------
# Check if HRU combination return NULL
# ------------------------------------------------------------------------------
  checkHRUCombination <- function(paraSelection, SWATParam, HRUinfo){

    # List of all file types (according to the spatial resolution)
    hruBasedFile <- c("hru", "gw", "mgt", "chm", "sdr", "sep", "sol")
    subBasedFile <- c("sub", "rte", "swq", "pnd")
    basinBasedFile <- c("wwq", "bsn")

    # Convert paraSelection to list object
    selectCriteria <- list()
    change <- list()
    change$parameterRange <- list()
    change$atLine <-list()
    change$atPosition <- list()
    change$numberFormat <- list()
    change$changeMethod <- list()
    change$applyValue <- list()

    checkMessage <- " "
    check <- TRUE
    allFiles <- c()
    allParam <- c()

    for (i in 1:nrow(paraSelection)){
      para <- trimws(paraSelection[i,1])
      fileType <- strsplit(para, split = '.', fixed = TRUE)[[1]][2]
      changeMethod <- trimws(paraSelection[i,2])

      if (i == 1) {
        change$parameterRange <- matrix(as.numeric(paraSelection[i,3:4]), ncol = 2)
      } else {
        change$parameterRange <- rbind(change$parameterRange,
                                       as.numeric(paraSelection[i,3:4]))
      }

      if(fileType %in% hruBasedFile){
        selectCriteria$sub <- trimws(strsplit(paraSelection[i,5], split=",")[[1]])
        selectCriteria$lu <- trimws(strsplit(paraSelection[i,6], split=",")[[1]])
        selectCriteria$soil <- trimws(strsplit(paraSelection[i,7], split=",")[[1]])
        selectCriteria$slope <- trimws(strsplit(paraSelection[i,8], split=",")[[1]])
      } else if (fileType %in% subBasedFile){
        selectCriteria$sub <- trimws(strsplit(paraSelection[i,5], split=",")[[1]])
      } else {
      }


      #Get list of files
      if(fileType %in% hruBasedFile){
        files <- hruSubset(HRUinfo, selectCriteria)
        files <- gsub("hru", fileType, files)
      } else if (fileType %in% subBasedFile){
        if (selectCriteria$sub == "All") {
          selectCriteria$sub <- c(1:max(HRUinfo$sub))
        } else {
          selectCriteria$sub <- as.numeric(selectCriteria$sub)
        }
        for(j in 1:length(selectCriteria$sub)){
          if (j == 1) {files <- NULL}
          files <- c(files, paste0(sprintf("%05d",
                                           as.integer(selectCriteria$sub[j])),
                                   sprintf("%04d", 0), ".", fileType))
        }
      } else {
        files <- paste("basins.", fileType, sep="")
      }

      if (length(files) == 0){
        checkMessage <- paste(checkMessage, "There is no spatial unit with ",
                              "such and HRU combination:",
                               " Subbasin = ", selectCriteria$sub,
                               " Landuse = ", selectCriteria$lu,
                               " Soil = ", selectCriteria$soil,
                               " Slope = ", selectCriteria$slope,
                               " ",
                               sep = "")
        check <- FALSE
      }

      fileInFiles <- length(which(files %in% allFiles))
      paraInPara <- length(which(para %in% allParam))

      if((fileInFiles > 0) & (paraInPara > 0)){
        checkMessage <- paste(checkMessage, "The same parameter is modified ",
                              "more than 1 times ",
                              para," ",sep = "")
        check <- FALSE
      }

      allFiles <- c(allFiles, files)
      allParam <- c(allParam, para)

    }

    output <- list()
    output$checkMessage <- checkMessage
    output$check <- check

    return(output)
  }

# ------------------------------------------------------------------------------
# Check input observed data file
# ------------------------------------------------------------------------------
  checkObservedData <- function(observedData){

    # Check number of column
    if (ncol(observedData) != 4){
      outMessage <- paste("Number of columns in the observed data",
                          " file should be 4, please see example",
                          " ./data/obs_var_1.txt", sep = "")

    # Check if the data column is numeric
    } else if (!is.numeric(observedData[,3])){
      outMessage <- "3rd column should be data (numbers), not character"

    } else {
      outMessage <- ""
    }

    # Return output message
    return(outMessage)
  }

# ------------------------------------------------------------------------------
# Check TxtInOut folder is SWAT or SWAT plus
# ------------------------------------------------------------------------------
  checkSWATorSWATplus <- function(TxtInOutFolder){

    # List all files in the TxtInOut folder
    files <- list.files(TxtInOutFolder)

    # If there is two files (codes.bsn and parameter.bsn) => this is SWAT+ project
    if(length(files[files %in% c("codes.bsn", "parameters.bsn")]) == 2){
      SWATPlus <- TRUE
      SWAT <- FALSE

    # Else this is SWAT project
    } else {
      SWATPlus <- FALSE
      SWAT <- TRUE
    }

    # Output is list object
    output <- list()
    output$SWATPlus <- SWATPlus
    output$SWAT <- SWAT

    return(output)
  }
