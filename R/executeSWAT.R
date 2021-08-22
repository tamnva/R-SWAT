# ------------------------------------------------------------------------------
# Function to call SWAT in sequential (from parameter set 1 to n)
# ------------------------------------------------------------------------------
  runSWATSequential <- function(coreNumber, 
                                workingDirectory, 
                                swatExe, 
                                caliParam, 
                                subParameterSet, 
                                outputExtraction, 
                                fileCioInfo,
                                fromToDate){
    
    # Set working directory
    setwd(paste(workingDirectory, 
                '/', 
                'TxtInOut', 
                '_', 
                coreNumber, 
                sep = ""))
   
    # Get directory where new TxtInOut files are saved
    toDir <- getwd()
  
    # Number of parameter sets
    if(is.vector(subParameterSet)){
      subParameterSet <- matrix(subParameterSet, nrow = 1)
      }
    nParam <- ncol(subParameterSet)
    
    # Loop over number of parameter sets
    for (i in 1:nrow(subParameterSet)) {
      
      # Assign parameter values to caliParam
      caliParam$applyValue <- getParameterValue(caliParam$paramFlag, 
                                                subParameterSet[i,2:nParam]
                                                )
      # Update TxtInOut folder
      updateMultiFile(toDir, 
                      caliParam
                      )
  
      # Call swat.exe file
      exeFile <- strsplit(swatExe, split="/")[[1]]
      if(!file.exists(exeFile[length(exeFile)])) file.copy(swatExe, toDir)
      
      system(trimws(exeFile[length(exeFile)]))

      # Read and write output
      saveOutput(workingDirectory,
                 coreNumber,                   
                 outputExtraction[,2],   
                 outputExtraction[,1],
                 fromToDate, 
                 outputExtraction[,3], 
                 outputExtraction[,4],
                 fileCioInfo,
                 subParameterSet[i,1])
      
      # Write .log file
      write(paste('Finished_simulation_number ', 
                  i, 
                  ' out_of ', 
                  nrow(subParameterSet), 
                  ' on_core ', 
                  coreNumber, 
                  ' ',
                  Sys.time(),
                  sep =''
                  ),
            file= paste(workingDirectory, 
                        '/Output/CurrentSimulationReport.log', 
                        sep =''
                        ), 
            append=TRUE
            )
    }
  }  
  
# ------------------------------------------------------------------------------   
# Run SWAT parallel on n cores with m parameter sets
# ------------------------------------------------------------------------------
  
  runSWATpar <- function(workingDirectory, 
                         TxtInOutFolder, 
                         outputExtraction, 
                         ncores, 
                         swatExe, 
                         parameterValue,
                         caliParam,
                         copyUnchangeFiles,
                         fileCioInfo,
                         fromToDate
                         ){
    
    
    # Create n directory in R
    if(copyUnchangeFiles){
      createDirCopyUnchangeFile(workingDirectory, ncores, 
                                TxtInOutFolder, caliParam$file, swatExe)
    }
    
    # --------------------------------------------------------------------------
    subParameterSet <- splitParameterValue(ncores, parameterValue)
    
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    foreach(i = 1:ncores, .combine = 'c', .export=c("runSWATSequential",
                                                    "getParameterValue", 
                                                    "updateMultiFile",
                                                    "updateMultiFile",
                                                    "updateSingleFile",
                                                    "saveOutput",
                                                    "userReadSwatOutput")) %dopar% {
                                                      
      runSWATSequential(i, 
                        workingDirectory, 
                        swatExe, 
                        caliParam, 
                        subParameterSet[[i]], 
                        outputExtraction, 
                        fileCioInfo,
                        fromToDate)
                                                             
    }
    parallel::stopCluster(cl)     
  }

# ------------------------------------------------------------------------------
# Create n directory in R
# ------------------------------------------------------------------------------
  
  createDirCopyUnchangeFile <- function(workingDirectory, numberOfCores, 
                                        TxtInOut, exceptFiles, swatExe){
    
    existingDir <- list.dirs(path = workingDirectory, 
                             full.names = TRUE, recursive = TRUE)
    
    for (i in 1:length(existingDir)){
      temp <- trimws(strsplit(existingDir[i], split="/")[[1]])
      temp <- temp[length(temp)]
      
      if(substr(temp,1,9) == 'TxtInOut_'){
        unlink(existingDir[i], recursive = TRUE)
      }
      
    }
    
    # Create new TxtInOut folders
    for (i in 1:numberOfCores){
      
      dir <- paste(workingDirectory, '/', 'TxtInOut', '_', i, sep ='')
      
      dir.create(dir)
      
      # Copy all unchange files
      copyAllExcept(TxtInOut, dir, exceptFiles)
      
      # Check if exe exist then delete
      temp <- strsplit(swatExe, split="/")[[1]]
      temp <- trimws(temp[length(temp)])
      if (file.exists(paste(dir, '/', temp, sep =''))) {
        file.remove(paste(dir, '/', temp, sep =''))
      }
      
      # Copy swat.exe file
      file.copy(swatExe, dir)
    }
    
    # Create output directory
    dir <- paste(workingDirectory, '/Output', sep ='')
    if(file.exists(dir)) {
      unlink(dir, recursive=TRUE,force = TRUE)
      dir.create(dir)
    } else {
      dir.create(dir)
    }
  }
  
  
# ------------------------------------------------------------------------------
# Split parameter values to ncores
# ------------------------------------------------------------------------------
  
  splitParameterValue <- function(numberOfCores, parameterValue){
    subParameterSet <- list()
    numberParameterSet <- nrow(parameterValue)           
    numberSubset <- as.integer(numberParameterSet/numberOfCores)
    
    if (numberOfCores > 1){
      for (i in 1:(numberOfCores)){
        istart <- (i-1) * numberSubset + 1
        iend <- i * numberSubset
        
        if((numberSubset < numberParameterSet/numberOfCores) & (i == numberOfCores)){
          subParameterSet[[i]] <- parameterValue[istart:nrow(parameterValue),]
        } else {
          subParameterSet[[i]] <- parameterValue[istart:iend,]
        }
      }      
    } else {
      subParameterSet[[1]] <- parameterValue[,]
    }
    
    return(subParameterSet)
  }  
  