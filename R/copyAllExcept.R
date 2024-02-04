#'
#' Copy all files from one folder to another folder except some files
#'
#' @param fromDir path of the directory contains files being copied
#' @param toDir path of the destination directory where the files are copied to
#' @inheritParams createDirCopyUnchangeFile
#'
#' @return no return, copied files will be saved in toDir
#'
#' @examples
#'
#' \donttest{
#' # Create a directory and populate with TxtInOut of SWAT+
#' extracExampleData(exampleData,"swatPlusTxtInOut", tempdir())
#' fromDir <- file.path(tempdir(), "swatPlusTxtInOut")
#'
#' # Create a directory to copy to this directory
#' dir.create(file.path(tempdir(), "toDir"), showWarnings = FALSE)
#' toDir <- file.path(tempdir(), "toDir")
#'
#' # Except these files
#' exceptFiles <- exampleData$swatPlusFiles[1:73]
#'
#' # Not copy all files from fromDir to toDir except exceptFiles
#' copyAllExcept(fromDir, toDir, exceptFiles)
#' }
#' @export
#'
copyAllExcept <- function(fromDir, toDir, exceptFiles){
  listFiles <- list.files(fromDir, full.names = FALSE)
  listFiles <- listFiles[!listFiles %in% exceptFiles]
  listFiles <- file.path(fromDir, listFiles)
  file.copy(listFiles, toDir)

  # Copy files in subfolders (SWAT+ project)
  listDir <- list.dirs(fromDir, recursive = FALSE)
  if(length(listDir) > 0) {
    for (i in 1:length(listDir)){
      file.copy(listDir[i], toDir, recursive=TRUE)
    }
  }
}
