#-------------------------------------------------------------------------------
# Binding list object (with 3 levels of list)
# ------------------------------------------------------------------------------
bindList <- function(toList, inList){

  nOutputVar <- length(lengths(toList))
  lengthToList <- length(lengths(toList[[1]]))
  lengthInList <- length(lengths(inList[[1]]))

  for (i in 1:nOutputVar){
    for (j in 1:lengthInList){
      toList[[i]][[lengthToList + j]] <- inList[[i]][[j]]
    }
  }
  return(toList)
}

#-------------------------------------------------------------------------------
# Write output files
# ------------------------------------------------------------------------------
writeOutputFiles <- function(workingFolder, ncores, nOutputVar, simData){
  for (i in 1:ncores){
    for (j in 1:nOutputVar){
      fileName <- paste(workingFolder, "/Output/Core_", i, "/out_var_",
                        j, ".txt", sep ="")

      # Remove file
      file.remove(fileName)

      # Check which core and which simulation
      sim <- seq(from = i, to = length(simData[[nOutputVar]]), ncores)

      # Write simulation data
      for (k in 1:length(sim)){
        write.table(as.character(sim[k]), fileName, col.names = FALSE,
                    row.names = FALSE, append = TRUE)
        write.table(simData[[j]][[sim[k]]], fileName, col.names = FALSE,
                    row.names = FALSE, append = TRUE)
      }
    }
  }
}

#-------------------------------------------------------------------------------
# Remove output files
# ------------------------------------------------------------------------------
removeOutputFiles <- function(workingFolder, ncores, nOutputVar){
  for (i in 1:ncores){
    for (j in 1:nOutputVar){
      removeFileName <- paste(workingFolder, "/Output/Core_", i,
                              "/out_var_", j, ".txt", sep ="")
      file.remove(removeFileName)
    }
  }
}


#-------------------------------------------------------------------------------
# 1.Draw a sample of points specified prior distribution (e.g., uniform distr.)
#-------------------------------------------------------------------------------
runifSampling <- function(nsample, xmin, xmax){

  # Random generator within [0,1] (matrix format)
  nparam <- length(xmin)

  for (i in 1:nsample){
    if (i == 1){
      output <- matrix(runif(nparam), nrow = 1)
    } else {
      output <- rbind(output, runif(nparam))
    }
  }

  # Parameter range conversion
  for (i in 1:nparam){
    output[,i] <- xmin[i] + (xmax[i] - xmin[i]) * output[,i]
  }

  # Add number of simulation in the first column
  output <- cbind(c(1:nrow(output)), output)

  # Remove row name and column name
  colnames(output) <- NULL
  rownames(output) <- NULL

  return(output)

}


#-------------------------------------------------------------------------------
# Function to split input info to lines based on \n and remove comment
#-------------------------------------------------------------------------------
splitRemoveComment <- function(textCommand){

  if((nchar(textCommand) == 0) | (is.null(textCommand))){
     outText <- "No additional input for parameter sample was found"
  } else {
    textCommand <- gsub("minCol", "as.numeric(globalVariable$paraSelection$Min)", textCommand)
    textCommand <- gsub("maxCol", "as.numeric(globalVariable$paraSelection$Max)", textCommand)
    textCommand <- gsub("nParam", "length(globalVariable$paraSelection$Max)", textCommand)
    textCommand <- gsub("objFuncValue", "globalVariable$objValueCali", textCommand)

    textCommand <- strsplit(textCommand, split = "\n", fixed = TRUE)[[1]]

    outText <- NULL
    count <- 1

    for (i in 1:length(textCommand)){
      temp <- trimws(textCommand[i])

      if ((substr(temp, 1, 1) != "#" &
           nchar(temp) != 0)) {
        outText[count] <- temp
        count <- count + 1
      }
    }

  }
  return(outText)
}

#-------------------------------------------------------------------------------
# Function to split input info to lines based on \n and remove comment
#-------------------------------------------------------------------------------
splitRemoveComment <- function(textCommand){

  if((nchar(textCommand) == 0) | (is.null(textCommand))){
    outText <- "No additional input for parameter sample was found"
  } else {
    textCommand <- gsub("minCol", "as.numeric(globalVariable$paraSelection$Min)", textCommand)
    textCommand <- gsub("maxCol", "as.numeric(globalVariable$paraSelection$Max)", textCommand)
    textCommand <- gsub("nParam", "length(globalVariable$paraSelection$Max)", textCommand)
    textCommand <- gsub("objFuncValue", "globalVariable$objValueCali", textCommand)

    textCommand <- strsplit(textCommand, split = "\n", fixed = TRUE)[[1]]

    outText <- NULL
    count <- 1

    for (i in 1:length(textCommand)){
      temp <- trimws(textCommand[i])

      if ((substr(temp, 1, 1) != "#" &
           nchar(temp) != 0)) {
        outText[count] <- temp
        count <- count + 1
      }
    }

  }
  return(outText)
}


#-------------------------------------------------------------------------------
# Function to find location of min and max
#-------------------------------------------------------------------------------
ilocMinMax <- function(iVector, MinOrMax){
  if (MinOrMax == "Maximize"){
    output <- which(iVector == max(iVector))
  } else {
    output <- which(iVector == min(iVector))
  }
  return(output)
}

#-------------------------------------------------------------------------------
# Check if there is an improvement in the objective function
#-------------------------------------------------------------------------------

isNewSimBetter <- function(newObj, oldObj, MinOrMax){

  output <- FALSE

  if (MinOrMax == "Maximize"){
    if (newObj > oldObj){
      output <- TRUE
    }
  } else {
    if (newObj < oldObj){
      output <- TRUE
    }
  }

  return(output)
}

#-------------------------------------------------------------------------------
# Message check txtinout folder
#-------------------------------------------------------------------------------

check_txtinout_message <- function(txtinout_folder, swat_plus_project){


  out_check <- list()

  # Initial values
  out_check$out_message <- " "
  out_check$txtinout_swat <- TRUE
  out_check$txtinout_swat_plus <- TRUE

  if (checkDirFileExist(trimws(txtinout_folder), "", ".cio")){

    # Is this TxtInOut of SWAT or SWAT plus
    out_check$txtinout_swat <- checkSWATorSWATplus(trimws(txtinout_folder))$SWAT
    out_check$txtinout_swat_plus <- checkSWATorSWATplus(
      trimws(txtinout_folder))$SWATPlus

    # Check if the TxtInOut matches the SWAT project
    if (swat_plus_project){
      if(out_check$txtinout_swat){
        out_check$out_message <- "ERROR: This should be TxtInOut of SWAT+"
      }
    } else {
      if(out_check$txtinout_swat_plus){
        out_check$out_message <- "ERROR: This should be TxtInOut of SWAT"
      }
    }
  }

  return(out_check)
}



