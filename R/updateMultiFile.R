#' Update multiple TxtInOut files with new parameter values
#'
#' @description
#' Rewrite the TxtInOut with new parameter values
#'
#' @inheritParams runSWATpar
#' @inheritParams getParameterValue
#' @param toDir path to the directory where the updated files will be saved
#' @return no return, results will be saved in todir directory
#' @examples
#'
#'\donttest{
#' updateMultiFile(toDir, caliParam, parameterValue, paraSelection)
#'}
#'
#' @export
#'
#'
updateMultiFile <-  function(toDir, caliParam, parameterValue, paraSelection){

  # If this is SWAT+ project
  if (isTRUE(caliParam$file == "calibration.cal")){
    updateCalibrationFile(paraSelection, parameterValue, toDir)

    # If this is SWAT project
  } else {
    # Loop over list of files
    for (i in 1:length(caliParam$file)){

      updateSingleFile(toDir,
                       caliParam$file[i],
                       caliParam$fileContent[[i]],
                       caliParam$atLine[[i]],
                       caliParam$atPosition[[i]],
                       caliParam$changeMethod[[i]],
                       caliParam$applyValue[[i]],
                       caliParam$numberFormat[[i]],
                       caliParam$absoluteMin[[i]],
                       caliParam$absoluteMax[[i]])
    }
  }
}
