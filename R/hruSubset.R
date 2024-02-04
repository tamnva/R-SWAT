#'
#' Get subset of HRU based on certain criteria
#'
#' @param HRUinfo a dataframe which contains HRU information
#' @param selectCriteria a dataframe contains different selection criteria about
#' landuse (lu), soil (soil), slope (slope), and subbasin (sub) number
#'
#'
#' @return a character vector of hru file names
#' @seealso \code{\link{getHruInfo}} for how to get HRU information from TxtInOut
#' @examples
#'
#'\donttest{
#' # Create a folder and extract TxtInOut of SWAT there
#' extracExampleData(exampleData,"swatTxtInOut", tempdir())
#' TxtInOut <- file.path(tempdir(), "swatTxtInOut")
#'
#' # Now get hru information
#' HRUinfo <- getHruInfo(TxtInOut)
#'
#' # The selection criteria is
#' selectCriteria <- data.frame(lu = c("AGRL"), soil = c("Fr333ab4494"),
#'                              sl=c("0-5"), sub =c(1,2))
#'
#' hruSubset(HRUinfo, selectCriteria)
#'}
#'
#' @export
#'
#'
hruSubset <- function(HRUinfo, selectCriteria){

  hru <- HRUinfo

  # Subset with land use selection
  if (is.na(which("All" %in% selectCriteria$lu)[1])) {
    hru <- hru[hru$lu %in% selectCriteria$lu, ]
  }

  # Subset with soil type selection
  if (is.na(which("All" %in% selectCriteria$soil)[1])) {
    hru <- hru[hru$soil %in% selectCriteria$soil, ]
  }

  # Subset with slope class selection
  if (is.na(which("All" %in% selectCriteria$slope)[1])) {
    hru <- hru[hru$slope %in% selectCriteria$slope, ]
  }

  # Subset with subbasin selection
  if (is.na(which("All" %in% selectCriteria$sub)[1])) {
    hru <- hru[hru$sub %in% selectCriteria$sub, ]
  }

  return(hru$file)

}

