#' Extract information from a header of an hru file of SWAT
#'
#' @param hruHeader Content of the first line of a hru file
#'
#' @return A list of hru information
#'
#' @keywords internal

#' @examples
#'
#' hruHeader <- paste0(' .hru file Watershed HRU:2 Subbasin:1 HRU:2 Luse:FRSE ',
#' 'Soil: Fr333ab4494 Slope: 5-9999 9/21/2021 12:00:00 AM ArcSWAT 2012.10_5.21')
#'
#' getInfoHruHeader(hruHeader)
#'
#' @export


getInfoHruHeader <- function(hruHeader){

  # Read information from header of HRU file
  for (i in 1:nchar(hruHeader)){
    if(substr(hruHeader,i,i+2) == "HRU"){
      hruID <- i
    }
    if(substr(hruHeader,i,i+7) == "Subbasin"){
      subID <- i
    }
    if(substr(hruHeader,i,i+3) == "Soil"){
      soilID <- i
    }
    if(substr(hruHeader,i,i+4) == "Slope"){
      slopeID <- i
    }
    if(substr(hruHeader,i,i+3) == "Luse"){
      luseID <- i
    }
  }

  result <- list()

  # get subbasin number
  result$sub <- substr(hruHeader,subID, hruID - 1)
  result$sub <- gsub("Subbasin", " ",result$sub )
  result$sub <- as.numeric(gsub(":", " ",result$sub ))

  # get HRU number
  result$hru <- substr(hruHeader,hruID , luseID - 1)
  result$hru <- gsub("HRU", " ",result$hru )
  result$hru <- as.numeric(gsub(":", " ",result$hru ))

  # get soil name
  result$soil <- substr(hruHeader,soilID, slopeID - 1)
  result$soil <- gsub("Soil", " ",result$soil )
  result$soil <- trimws(gsub(":", " ",result$soil ))

  # get land use name
  result$lu <- substr(hruHeader,luseID, soilID - 1)
  result$lu <- gsub("Luse", " ",result$lu )
  result$lu <- trimws(gsub(":", " ",result$lu ))

  # get slope class
  result$slope <- substr(hruHeader, slopeID, nchar(hruHeader))
  result$slope <- gsub("Slope", " ",result$slope )
  result$slope <- trimws(gsub(":", " ",result$slope ))
  result$slope <- strsplit(result$slope, "\\s+")[[1]][1]

  return(result)
}
