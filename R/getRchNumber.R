#'
#' Get reach/subbasin/reservoir number
#'
#' @param inputText character of the selected reach or subbasins, or resevoir
#' number
#'
#' @return a list object of output
#'
#' @examples
#'
#' getRchNumber(" 1, 3 * 5,7")
#'
#' @export
#'
#'
getRchNumber <- function(inputText){

  rsvRchNumber <- list()

  if(nchar(inputText) > 0){
    inputText <- strsplit(inputText, split = "*", fixed = TRUE)[[1]]
    for (i in 1:length(inputText)){

      rsvRchNumber[[i]] <- as.numeric(strsplit(inputText[i],
                                               split = ",",
                                               fixed = TRUE)[[1]]
      )
      rsvRchNumber[[i]] <-  rsvRchNumber[[i]][!is.na(rsvRchNumber[[i]])]
    }
  } else {
    rsvRchNumber <- NULL
  }

  return(rsvRchNumber)
}
