## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, eval = FALSE---------------------------------------------
#  library(RSWAT)
#  
#  # 1.Create an example TxtInOut of SWAT+ (TODO: remove this command if you already # had the TxtInOut folder of SWAT+)
#  extracExampleData(exampleData, "swatPlusTxtInOut", tempdir())
#  
#  # 2.Path to the TxtInOut folder (TODO: replace with your TxtInOut path)
#  TxtInOutFolder <- file.path(tempdir(), "swatPlusTxtInOut")
#  
#  # 3.This is a SWAT+ project, therefore
#  SWATproject <- FALSE
#  
#  # 4.My working directory (TODO: change to your desired working directory)
#  # dir.create(file.path(tempdir(), "workingFolder"))
#  workingFolder <- file.path(tempdir(), "workingFolder")
#  
#  # 5.Path of the SWAT executable file (TODO: replace with your path)
#  SWATexeFile <- "C:/data/rev60.5.7_64rel.exe"
#  
#  # 6.Path of the SWAT+ parameter file cal_parms.cal (TODO: replace with your path)
#  SWATParamFile <- file.path(TxtInOutFolder, "cal_parms.cal")
#  
#  # 7.The parameters that I previously used for model calibration (TODO: change to your paraSelection)
#  paraSelection <- data.frame(
#     Parameter = c("cn2.hru"   , "canmx.hru"),
#     Change =    c("relative"  , "replace"),
#     Min =       c(-0.25       , 1.0),
#     Max =       c(0.25        , 10.0),
#     Object  =   c("All"       , "All"),
#     Conditions = c("All"      , "All")
#     )
#  
#  # You can also create these above table by the interface of R-SWAT then save
#  # the  project. If you do so, you can extract that table from the RSWATproject.rds
#  # file in the working folder (TODO: Delete if you don't read from RSWATproject.rds)
#  
#  # RSWATproject <- readRDS("C:/data/RSWATproject.rds")
#  # paraSelection <- RSWATproject$paraSelection
#  
#  # 8. How parameters should be generated
#  samplingApproach <- "Read_User_Parameter_File"  # Do not change this line
#  
#  # 9. Output extraction for SWAT+ can be created manually or by the GUI (TODO: change this)
#  outputExtraction <- data.frame(
#     FileType = c("channel_sd_day.txt"),
#     FileName = c("channel_sd_day.txt"),
#     Column = c("48"),
#     Reach = c("1")
#   )
#  
#  # 10. Date range for extraction (TODO: change this)
#  dateRangeCali <- as.Date(c("2003-01-01", "2012-12-31"), format = "%Y-%m-%d")
#  
#  # 11. Number of parallel runs  (TODO: change  this)
#  ncores <- 2
#  
#  # 12.  Generate parameter samples using Latin Hypercube Sampling (TODO: change this)
#  parameterValue <- as.matrix(read.table(file = "C:/data/user_parameter_values.txt",
#                                         header = TRUE, sep =""))
#  
#  # Here is the content of my user_parameter_values.txt"
#  #cn2.hru         canmax.hru
#  #0.10            1.5
#  #-0.20           3.0
#  #-0.10           0.2
#  #0.22            1.0
#  #
#  
#  # Remove the column names and number these parametersets from 1 to n. DONOT change this
#  parameterValue <- cbind(c(1:nrow(parameterValue)),parameterValue)
#  colnames(parameterValue) <- NULL
#  rownames(parameterValue) <- NULL
#  

## ---- message=FALSE, eval = FALSE---------------------------------------------
#  # Get HRU infor (HRU names, landuse, soil, slope, subbasin)
#  HRUinfo <- read.table(file.path(TxtInOutFolder, "hru-data.hru"),
#                        header = TRUE, skip = 1, sep = "")
#  
#  # Read SWAT parameter
#  SWATParam <- loadSwatParam(SWATParamFile)
#  
#  # Get location of parameters in TxtInOut files and load TxtInOut file content
#  caliParam <- updatedFileContent(HRUinfo,paraSelection,SWATParam,
#                                  TxtInOutFolder)
#  
#  # Set first run is true so R-SWAT will delete previous simulation
#  firstRun <- TRUE
#  copyUnchangeFiles <- TRUE
#  
#  # Get content of the file.cio file (about simulation time)
#  fileCioInfo <- getSimTime(TxtInOutFolder)
#  
#  # Now start to run SWAT
#  runSWATpar(workingFolder,TxtInOutFolder,outputExtraction,ncores,SWATexeFile,
#             parameterValue,paraSelection,caliParam,copyUnchangeFiles,fileCioInfo,
#             dateRangeCali,firstRun)
#  

## ---- message=FALSE, eval = FALSE---------------------------------------------
#  
#  # Read all output_var_xx.txt files in the workingFolder/Output/Core_xx
#  # First you need to you how many output time steps for each variables
#  # You can open the out_var_xx.txt and check, below lets assume I only have one
#  # output variable, for more detail see function readAllOutVar
#  # TODO: change numTimesteps
#  numTimesteps <- c(3653)
#  output <- readAllOutVar(workingFolder, numTimesteps)
#  
#  # Now calculate the 95PPU (SUFI2 approach) for each output variables, example for variable 1
#  # TODO: change variable and percentiles as your wish
#  variable <- 1
#  percentiles <- c(0.025, 0.5, 0.975)
#  
#  for (itime in 1:nrow(output[[variable]])){
#    ppu <- as.numeric(quantile(output[[variable]][itime,],percentiles))
#    if (itime == 1){
#      ppu95 <- ppu
#    } else {
#      ppu95 <- rbind(ppu95, ppu)
#    }
#  }
#  
#  # Give it a column names
#  colnames(ppu95) <- c("lower95PPU", "median95PPU", "upper95PPU")
#  row.names(ppu95) <- NULL
#  

