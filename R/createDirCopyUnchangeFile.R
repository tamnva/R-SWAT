
#' Create TxtInOut directories and copy unchanged files
#'
#' @inheritParams runSWATpar
#' @param TxtInOut path to original TxtInOut folder
#' @param exceptFiles character vector of files that do not need to copy
#' @param swatExe SWAT/SWAT+ executable files
#'
#' @return No return
#' 
#' @examples
#' \donttest{
#' # Create a directory and populate with TxtInOut of SWAT
#' extracExampleData(exampleData,"swatTxtInOut", tempdir())
#' TxtInOut <- file.path(tempdir(), "swatTxtInOut")
#'
#' # Create a directory to copy to this directory
#' dir.create(file.path(tempdir(), "workingDirectory"), showWarnings = FALSE)
#' workingDirectory <- file.path(tempdir(), "workingDirectory")
#'
#' # Except these files
#' exceptFiles <- exampleData$swatFiles[1:280]
#'
#' # Lets assume that this is the SWAT exe file
#' swatExe <- tempfile(pattern = "swatexe", tmpdir = tempdir(), fileext = ".exxxe")
#' file.create(swatExe)
#'
#' createDirCopyUnchangeFile(workingDirectory, 2, TxtInOut, exceptFiles, swatExe, TRUE)
#' }
#'
#' @export
#'
createDirCopyUnchangeFile <- function(workingDirectory, ncores,
                                      TxtInOut, exceptFiles, swatExe,
                                      firstRun){

  # Remove existing TxtInOut_xxx folders
  if (firstRun){
    # List of existing folders in the working directory
    existingDir <- list.dirs(path = workingDirectory,
                             full.names = TRUE, recursive = TRUE)

    for (i in 1:length(existingDir)){

      # Get only the folder names
      temp <- trimws(strsplit(existingDir[i], split="/")[[1]])
      temp <- temp[length(temp)]

      # Delete all TxtInOut folders
      if(substr(temp,1,9) == 'TxtInOut_'){
        unlink(existingDir[i], recursive = TRUE)
      }

    }
  }

  # Create new TxtInOut folders
  for (i in 1:ncores){

    if (firstRun){
      dir <- file.path(workingDirectory, paste0('TxtInOut_', i))
      dir.create(dir, showWarnings = FALSE)
    } else {
      dir <- file.path(workingDirectory,paste0('TxtInOut_', i))
    }

    # Copy all unchanged files
    copyAllExcept(TxtInOut, dir, exceptFiles)

    # Check if exe exist then delete
    temp <- strsplit(swatExe, split="/")[[1]]
    temp <- trimws(temp[length(temp)])
    if (file.exists(file.path(dir, temp))) {
      file.remove(file.path(dir, temp))
    }

    # Copy swat.exe file
    file.copy(swatExe, dir)
  }

  # Create output directory
  if (firstRun){
    dir <- file.path(workingDirectory, 'Output')
    if(file.exists(dir)) {
      unlink(dir, recursive=TRUE,force = TRUE)
      dir.create(dir)
    } else {
      dir.create(dir)
    }
  }
}
