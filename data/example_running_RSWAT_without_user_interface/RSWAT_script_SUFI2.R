# This is a script for running R-SWAT without using a graphical user interface
# Currently only design with SUFI2 approach, other approaches will be done soon

# -----------------------------------------------------------------------------#
#                              Start user input                                #
# -----------------------------------------------------------------------------#

# R-SWAT source code
RSWATsourceFile <- "C:/RSWAT_demo/R-SWAT-master/R"

# 1. Set SWATproject is TRUE if you run SWAT, set to FALSE if you run SWAT+
SWATproject <- TRUE

# 2. Working folder (where all files created by R-SWAT are saved)
workingFolder <- "C:/RSWAT_demo/workingFolder"

# 3. TxtInOut folder
TxtInOutFolder <- "C:/RSWAT_demo/R-SWAT-master/data/example_SWAT/TxtInOut"

# 4. SWAT (or SWAT+) executable file
SWATexeFile <- "C:/SWAT/ArcSWAT/swat_64rel.exe"

# 5. SWAT (or SWAT+) parameter file
SWATParamFile <- "C:/RSWAT_demo/R-SWAT-master/data/example_SWAT/swatParam.txt"

# 6. Select parameter for calibration or sensitivity analysis with SWAT
paraSelection <- data.frame(
  Parameter = c("GW_DELAY.gw", "CN2.mgt"),
  Change =    c("replace"    , "relative"),
  Min =       c(50           , -0.25),
  Max =       c(450          , 0.25),
  Subbasin =  c("All"        , "All"),
  Landuse =   c("All"        , "All"),
  Soil =      c("All"        , "All"),
  Slope =     c("All"        , "All")
)

# 6. Select parameter for calibration or sensitivity analysis with SWAT+
# paraSelection <- data.frame(
#   Parameter = c("cn2.hru"   , "canmx.hru"),
#   Change =    c("relative"  , "replace"),
#   Min =       c(-0.25       , 1.0),
#   Max =       c(0.25        , 10.0),
#   Object  =   c("All"       , "All"),
#   Conditions = c("All"      , "All")
# )


# 7. Parameter calibration/sensitivity analysis approach
samplingApproach <- "Sensi_Cali_(uniform_Latin_Hypercube_Sampling)"

# 8. Additional information about parameter calibratoin sensitivity analysis
sensCaliCommand <- 10

# 9. Output extraction for SWAT
outputExtraction <- data.frame(
  FileType = c("watout.dat"),    # if for two files: = c("watout.dat", "output.rch"),
  FileName = c("watout.dat"),    #                   = c("watout.dat", "output.rch"),
  Column = c("4"),               #                   = c("4"         , "6"),
  Reach = c(" ")                 #                   = c(" "         , "2")
)

# 9. Output extraction for SWAT+
# outputExtraction <- data.frame(
#   FileType = c("channel_sd_day.txt"),
#   FileName = c("channel_sd_day.txt"),
#   Column = c("48"),
#   Reach = c("1")
# )

# 10. Date range for extraction
dateRangeCali <- as.Date(c("2000-01-01", "2007-12-31"), format = "%Y-%m-%d")

# 11. Number of parallel runs
ncores <- 4

# 12. Objective function could be "NSE", "KGE", "RMSE", "R2", "aBIAS" is absolute bias ranging from [0, inf]
objFunction <- "NSE"

# 13. Observed data file(s)
observedDataFile <- c("C:/RSWAT_demo/R-SWAT-master/data/example_SWAT/obs_var_1.txt")


# -----------------------------------------------------------------------------#
#                            End user input                                    #
# -----------------------------------------------------------------------------#
# Load R-SWAT functions
# Require packages
requiredPackages <- c('foreach', 'doParallel', 'lhs', 'sensitivity', 'boot', 
                      'optimization', 'hydroPSO','nloptr','spsComps')

# Check and install only missing packages
install.packages(setdiff(requiredPackages,rownames(installed.packages())), dependencies = TRUE) 

# Load these packages
lapply(requiredPackages, library, character.only = TRUE)

# Load R-SWAT functions
RSWATfiles <- c("executeSWAT.R", "glue.R", "multiRegression.R",
                "objFunction.R", "readSaveOutput.R", "updateTxtInOut.R",
                "userObjFunction.R", "userReadSwatOutput.R", "displayOutput.R")

# Load R-SWAT function
setwd(RSWATsourceFile)
lapply(RSWATfiles, source)

# Generate parameter samples using Latin Hypercube Sampling
parameterValue <- lhsRange(sensCaliCommand, getParamRange(paraSelection))

# Get HRU infor (HRU names, landuse, soil, slope, subbasin)
if (SWATproject){
  HRUinfo <- getHruInfo(TxtInOutFolder)
} else {
  HRUinfo <- read.table( paste(TxtInOutFolder, "/hru-data.hru", sep =""),
                         header = TRUE, skip = 1, sep = "")
}

# Read SWAT parameter
SWATParam <- loadSwatParam(SWATParamFile)

# Get location of parameters in TxtInOut files and load TxtInOut file content
caliParam <- loadParamChangeFileContent(HRUinfo,paraSelection,SWATParam,
                                        TxtInOutFolder)

# Set first run is true so R-SWAT will delete previous simulation
firstRun <- TRUE
copyUnchangeFiles <- TRUE

# Get content of the file.cio file (about simulation time)
fileCioInfo <- getFileCioInfo(TxtInOutFolder)

# Now start to run SWAT
runSWATpar(workingFolder,TxtInOutFolder,outputExtraction,ncores,SWATexeFile,
           parameterValue,paraSelection,caliParam,copyUnchangeFiles,fileCioInfo,
           dateRangeCali,firstRun)

# Number of output variables (from the output extraction data frame)
OutputVar <- getNumberOutputVar(outputExtraction)
nOutputVar <- OutputVar$nOutputVar

# Check if users use their own output extraction function
userReadSwatOutput <- OutputVar$userReadSwatOutput

# Get observed data (first need to sort observed data file)
observedDataFile <- sortObservedDataFile(observedDataFile)

# Now read observed data (as list) from observed files
observedData <- list()
for (i in 1:length(observedDataFile)){
  # Read observed files and save to a dummy variable
  temp <- read.table(observedDataFile[i], skip = 1, sep = "")
  # Get bbserved data from dummy variable
  observedData [[i]] <- data.frame(Date = as.POSIXct(paste(temp[,1], 
                                                           temp[,2], 
                                                           sep = " "), 
                                                     format = "%Y-%m-%d %H:%M", 
                                                     tz = ""),
                                   Value = temp[,3],
                                   Flag = temp[,4])
}

# Calculate objective function (this function goes through the output/TxtInOut_x and reads the simulated data)
obj <- calObjFunction(parameterValue,
                      ncores,
                      nOutputVar,
                      userReadSwatOutput,
                      observedData,
                      workingFolder,
                      objFunction)

# Model performance for calibration (obj$objValueCali) and validation (obj$objValueValid)
objValueCali <- obj$objValueCali
objValueValid <- obj$objValueValid

# Parameter sensitivity using multi-variable regression
# Prepare table with parameter values and objective function
parameterObj <- as.data.frame(cbind(objValueCali, parameterValue[,-c(1)]))
colnames(parameterObj) <- c("objFunction", paraSelection[,1])

# Parameter sensitivity using multivariate regression analysis
ParamSensitivity <- summary(lm(formula = objFunction ~ ., parameterObj))[4]$coefficients[,3:4] 

# The objective function should be maximum "Maximize" or minimum "Minimize"
# e.g., if objective function is NSE, KGE , R2 then it should be "Maximize"
#       if objective function is "RMSE", "aBIAS" then it should be "Minimize"
minOrmax <- "Maxmimize"

# Behavioral threshold
behThreshold <- -0.5

# Which output variable number you want to calculate behavioral simulations
varNumber <- 1

# Calculate 95 PPU
behavioral <- behaSimulation(obj$objValueCali,
                             obj$simData,
                             parameterValue,
                             behThreshold,
                             varNumber,
                             objFunction,
                             observedData,
                             minOrmax,
                             samplingApproach)

# behavioral variable contains
# -----------------------95PPU and best simulations 
#   behavioral$ppuSimData
# -----------------------95PPU of parameters 
#   cbind(paraSelection[,1], behavioral$ppuParaRange)
# -----------------------p and r factor for validation 
#   behavioral$prFactorCali
# -----------------------p and r factor for validation 
#   behavioral$prFactorValid


# Save all data + function 
save.image(paste(workingFolder, "/RSWATproject.RData", sep = ""))

# Load all data function for later work
# load("C:/RSWAT_demo/workingFolder/RSWATproject.RData")
