
#' Get information of all HRU (landuse, soil, slope) in all .hru files of SWAT
#'
#' @param TxtInOut Path to TxtInOut folder of SWAT
#'
#' @return a dataframe of HRU information (see example below)

#' @examples
#'
#'\donttest{
#' # Create a directory and populate with TxtInOut of SWAT
#' extracExampleData(exampleData,"swatTxtInOut", tempdir())
#' TxtInOut <- file.path(tempdir(), "swatTxtInOut")
#'
#' # Now get hru information
#' getHruInfo(TxtInOut)
#'
#'}
#'
#' @export
#'
getHruInfo <- function(TxtInOut){

  # list of .hru files in TxtInOut folder
  lstFiles <-  list.files(path = TxtInOut,
                          pattern = ".hru",
                          full.names = TRUE)

  # remove output.hru from list files
  file <- basename(lstFiles)

  # remove all files that are not hru files
  temp <- c()
  for (i in 1:length(file)){
    if (nchar(file[i]) != 13){
      temp <- c(temp, i)
    } else {
      fname <- substr(file[i],1,9)
      if (is.na(as.numeric(fname))){
        temp <- c(temp, i)
      }
    }
  }


  if(!is.null(temp)){
    file <- file[-temp]
    lstFiles <- lstFiles[-temp]
  }

  # Initialized vector to store HRU information from headers of HRU files
  hru <- c()
  sub <- c()
  soil <- c()
  lu <- c()
  slope <- c()

  # Loop over all HRU files
  for (i in 1:length(lstFiles)){

    temp <- getInfoHruHeader(readLines(lstFiles[i], 1, warn = FALSE))

    # HRU number
    hru <- c(hru, temp$hru)

    # Subbasin number
    sub <- c(sub, temp$sub)

    # Soil name
    soil <- c(soil, temp$soil)

    # Land use class
    lu <- c(lu, temp$lu)

    # Slope class
    slope <- c(slope, temp$slope)
  }

  # Store HRU information in a data frame
  hruInfo <- data.frame("file" = file, "hru" = hru, "sub" = sub,
                        "soil" = soil, "lu" = lu, "slope" = slope)

  # Return result
  return(hruInfo)
}
