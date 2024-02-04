
#' Load content of the files in the TxtInOut that are need to be updated every
#' model run
#'
#' @param HRUinfo Dataframe which contains HRU information
#' @param paraSelection Dataframe which contains the selected parameters to
#' change and their respective information (e.g., range, spatial level,...).
#'
#' @param SWATParam Dataframe contains parameters of SWAT or SWAT+
#' @param TxtInOutFolder Link to the TxtInOut folder
#'
#' @return A list of all files and their content that will be updated/rewritten
#' every model run.
#'
#' @seealso [getHruInfo()] to read HRU information from a file and return
#' dataframe which contains HRU information

#' @examples
#'
#'\donttest{
#' updatedFileContent(HRUinfo, paraSelection, SWATParam, TxtInOutFolder)
#'}
#'
#' @export
#'
#'

updatedFileContent <- function(HRUinfo, paraSelection, SWATParam, TxtInOutFolder){

  change <- list()

  # If this is SWAT+ project
  if (ncol(HRUinfo) == 6){
    # List of all file types (according to the spatial resolution)
    hruBasedFile <- c("hru", "gw", "mgt", "chm", "sdr", "sep", "sol")
    subBasedFile <- c("sub", "rte", "swq", "pnd")
    resFile <- c("res")
    basinBasedFile <- c("wwq", "bsn")

    # Convert paraSelection to list object
    selectCriteria <- list()
    change$parameterRange <- list()
    change$atLine <-list()
    change$atPosition <- list()
    change$numberFormat <- list()
    change$changeMethod <- list()
    change$applyValue <- list()

    counter <- 0

    for (i in 1:nrow(paraSelection)){
      para <- trimws(paraSelection[i,1])
      temp <-  strsplit(para, split = '.', fixed = TRUE)[[1]]
      fileType <- temp[2]
      changeMethod <- trimws(paraSelection[i,2])

      if (i == 1) {
        change$parameterRange <- matrix(as.numeric(paraSelection[i,3:4]), ncol = 2)
      } else {
        change$parameterRange <- rbind(change$parameterRange,
                                       as.numeric(paraSelection[i,3:4]))
      }

      # Check if there is new files/parameters added by user
      if (length(temp) == 3){
        files <- paste(temp[2], ".", temp[3], sep = "")
      } else {
        if(fileType %in% hruBasedFile){
          selectCriteria$sub <- trimws(strsplit(paraSelection[i,5], split=",")[[1]])
          selectCriteria$lu <- trimws(strsplit(paraSelection[i,6], split=",")[[1]])
          selectCriteria$soil <- trimws(strsplit(paraSelection[i,7], split=",")[[1]])
          selectCriteria$slope <- trimws(strsplit(paraSelection[i,8], split=",")[[1]])
        } else if (fileType %in% subBasedFile){
          selectCriteria$sub <- trimws(strsplit(paraSelection[i,5], split=",")[[1]])
        } else if (fileType %in% resFile){
          selectCriteria$sub <- trimws(strsplit(paraSelection[i,5], split=",")[[1]])
        } else {
        }


        #Get list of files
        if(fileType %in% hruBasedFile){
          files <- hruSubset(HRUinfo, selectCriteria)
          files <- gsub("hru", fileType, files)
        } else if (fileType %in% subBasedFile){
          if ("All" %in% selectCriteria$sub) {
            selectCriteria$sub <- c(1:max(HRUinfo$sub))
          } else {
            selectCriteria$sub <- as.numeric(selectCriteria$sub)
          }
          for(j in 1:length(selectCriteria$sub)){
            if (j == 1) {files <- NULL}
            files <- c(files, paste0(sprintf("%05d", as.integer(
              selectCriteria$sub[j])), sprintf("%04d", 0), ".", fileType))
          }
        } else if (fileType %in% resFile){
          if ("All" %in% selectCriteria$sub) {
            selectCriteria$sub <- substring(list.files(TxtInOutFolder,'.res'), 1,
                                            nchar(list.files(TxtInOutFolder,'.res') ) - 8)
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
      }



      # Find location of the parameters in the file
      index <- which(SWATParam$parameter == para)
      atLine <- SWATParam$lineNumber[[index]]
      atPosition <- c(SWATParam$startPosition[[index]],
                      SWATParam$endPosition[[index]])
      numberFormat <-   paste('%', atPosition[2] - atPosition[1] + 1, '.',
                              SWATParam$precision[[index]], 'f', sep ="")
      absoluteMin <- SWATParam$absoluteMin[[index]]
      absoluteMax <- SWATParam$absoluteMax[[index]]

      if (i == 1){
        nfiles <- length(files)
        change$file <- files
        for (j in 1:nfiles){
          change$atLine[[j]] <- atLine
          change$atPosition[[j]] <- matrix(atPosition,ncol = 2, byrow = TRUE)
          change$numberFormat[[j]] <- numberFormat
          change$changeMethod[[j]] <- changeMethod
          change$applyValue[[j]] <- i
          change$absoluteMin[[j]] <- absoluteMin
          change$absoluteMax[[j]] <- absoluteMax
        }

      } else {

        # Find if old files need to be updated
        intersectFiles <- intersect(change$file,files)
        if (length(intersectFiles) >= 1){
          idx <-  which(change$file %in% intersectFiles)
          for (j in 1:length(idx)){
            change$atLine[[idx[j]]] <- c(change$atLine[[idx[j]]], atLine)
            change$atPosition[[idx[j]]] <- rbind(change$atPosition[[idx[j]]], c(atPosition))
            change$numberFormat[[idx[j]]] <- c(change$numberFormat[[idx[j]]], numberFormat)
            change$changeMethod[[idx[j]]] <- c(change$changeMethod[[idx[j]]], changeMethod)
            change$applyValue[[idx[j]]] <- c(change$applyValue[[idx[j]]], i)
            change$absoluteMin[[idx[j]]] <- c(change$absoluteMin[[idx[j]]], absoluteMin)
            change$absoluteMax[[idx[j]]] <- c(change$absoluteMax[[idx[j]]], absoluteMax)
          }
        }

        change$file <- unique(c(change$file, files))
        nfiles <- length(change$file)

        if (nfiles > counter){
          for (j in (counter+1):nfiles){
            change$atLine[[j]] <- atLine
            change$atPosition[[j]] <- matrix(atPosition, ncol = 2, byrow = TRUE)
            change$numberFormat[[j]] <- numberFormat
            change$changeMethod[[j]] <- changeMethod
            change$applyValue[[j]] <- i
            change$absoluteMin[[j]] <- absoluteMin
            change$absoluteMax[[j]] <- absoluteMax
          }
        }
      }
      counter <- nfiles
    }

    change$fileContent <- readFileContent(TxtInOutFolder, change$file)
    change$paramFlag <- change$applyValue

    # This is SWAT project
  } else {
    change$file <- "calibration.cal"
  }


  return(change)
}
