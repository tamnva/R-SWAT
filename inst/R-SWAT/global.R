# -----------------------------------------------------------------------------#
#                  Default parameter change values for SWAT                    #
# -----------------------------------------------------------------------------#
globalVariable <- reactiveValues()
displayOutput <- list()
displayOutput$plotHru <- FALSE
displayOutput$plotSub <- FALSE
globalVariable$checkSimComplete <- FALSE
globalVariable$loadProject <- FALSE
globalVariable$TxtInOutSWAT <- TRUE
globalVariable$TxtInOutSWATPlus <- FALSE
globalVariable$SWATProject <- TRUE
globalVariable$SWATPlusProject <- FALSE
globalVariable$paraSelection <- dataParaSelectionSWAT
globalVariable$nCaliParam <- 8
globalVariable$runManualCaliSuccess <- FALSE
globalVariable$readOutputScript <- NULL
globalVariable$objFunctionScript <- NULL
globalVariable$minOrmax <- "Maximize"
globalVariable$RSWATversion <- "v4.10"
HTMLdir <- system.file("R-SWAT", package = "RSWAT")

dataParaSelectionSWAT <- data.frame(
  Parameter = c('GW_DELAY.gw', 'CN2.mgt', 'SOL_K.sol', 'ALPHA_BF.gw',
                'ESCO.hru', 'SURLAG.hru', 'CH_K2.rte', 'SURLAG.bsn'),
  Change = c('absolute', 'relative', 'relative', rep('replace',5)),
  Min = c(50,-0.25, -0.25, 0.01, 0.5, 0.1, 0.0, 3.0),
  Max = c(450,0.25, 0.25, 0.4, 0.99, 10.0, 0.5, 6.0),
  Subbasin = c(rep('All', 7), NA),
  Landuse = c(rep('All', 6), NA, NA),
  Soil = c(rep('All', 6), NA, NA),
  Slope = c(rep('All', 6), NA, NA))

columnsParaSelectionSWAT <- data.frame(
  title= colnames(dataParaSelectionSWAT),
  source = I(list(NA , c('absolute', 'relative','replace'),
                  NA, NA, NA, NA, NA, NA)),
  width= rep(300, 8),
  type=c('text', 'dropdown', rep('numeric', 3), rep('text', 3))
)

# ------------------------------------------------------------------------------
# Default parameter change values for SWAT +
# ------------------------------------------------------------------------------
dataParaSelectionSWATPlus <- data.frame(
  Parameter = c('cn2.hru', 'canmx.hru', 'esco.hru', 'k.sol',
                'awc.sol', 'surlag.bsn', 'msk_co1.bsn',
                'msk_co2.bsn', 'chk.rte', 'wd_rto.rte'),
  Change = c('relative', 'replace', 'replace', 'replace',
             'replace', 'replace', 'replace',
             'replace', 'replace','replace'),
  Min = c(-0.25,1, 0, 0, 0.01, 0.05,0, 0, 3.0, 0.5),
  Max = c(0.25,10, 1, 20, 1, 10, 10, 10, 6.0, 20),
  Object = c("1:2,3",rep("All",9)),
  Conditions = c("hgs=A,B; lyr1=1; lyr2=2","landuse = corn, past", rep("All",8))
)


columnsParaSelectionSWATPlus <- data.frame(
  title= colnames(dataParaSelectionSWATPlus),
  source = I(list(NA , c('absolute', 'relative','replace'), NA, NA, NA, NA)),
  width= c(rep(200, 5), 400),
  type=c('text','dropdown', rep('numeric',2), rep('text',2))
)
# ------------------------------------------------------------------------------
# Default output variables SWAT+
# ------------------------------------------------------------------------------
dataOutputExtractionSWATPlus <- data.frame(
  FileType = c(paste0("channel_sd_", c("day", "mon", "yr"), ".txt"),
               paste0("channel_sdmorph_", c("day", "mon", "yr"), ".txt"),
               paste0("lsunit_wb_", c("day", "mon", "yr"), ".txt"),
               paste0("basin_wb_", c("day", "mon", "yr"), ".txt"),
               'userReadSwatOutput'),
  FileName = c(paste0("channel_sd_", c("day", "mon", "yr"), ".txt"),
               paste0("channel_sdmorph_", c("day", "mon", "yr"), ".txt"),
               paste0("lsunit_wb_", c("day", "mon", "yr"), ".txt"),
               paste0("basin_wb_", c("day", "mon", "yr"), ".txt"),
               NA),
  Column = c(rep("48, 53", 12), "1"),
  Reach_Unit = c(rep("1, 2 * 1, 2, 3", 12), NA)
)


columnsOutputExtractionSWATPlus <- data.frame(
  title= colnames(dataOutputExtractionSWATPlus),
  source = I(list(c('channel_sd_day.txt',
                    'channel_sd_mon.txt',
                    'channel_sd_yr.txt',
                    'channel_sdmorph_day.txt',
                    'channel_sdmorph_mon.txt',
                    'channel_sdmorph_yr.txt',
                    'lsunit_wb_day.txt',
                    'lsunit_wb_mon.txt',
                    'lsunit_wb_yr.txt',
                    'basin_wb_day.txt',
                    'basin_wb_mon.txt',
                    'basin_wb_yr.txt',
                    'userReadSwatOutput'),
                  NA,
                  NA,
                  NA)),
  width= c(300,
           300,
           300,
           300),
  type=c('dropdown',
         'text',
         'text',
         'text'))

# ------------------------------------------------------------------------------
# Default output variables
# ------------------------------------------------------------------------------
dataOutputExtractionSWAT <- data.frame(
  FileType = c('watout.dat',
               'output.rch',
               'output.sub',
               'output.hru',
               'userReadSwatOutput'),
  FileName = c('watout.dat',
               'output.rch',
               'output.sub',
               'output.hru',
               NA),
  Column = c("4", "7, 8 ", "8, 9", "3", "2"),
  Reach = c(NA, "1, 2 * 1, 2, 3", "1 * 5, 6", "20", NA)
)


columnsOutputExtractionSWAT <- data.frame(
  title= colnames(dataOutputExtractionSWAT),
  source = I(list(c('watout.dat',
                    'output.rch',
                    'output.sub',
                    'output.hru',
                    'userReadSwatOutput'),
                  NA,
                  NA,
                  NA)),
  width= c(300,
           300,
           300,
           300),
  type=c('dropdown',
         'text',
         'text',
         'text'))
# ------------------------------------------------------------------------------
# Default help text for step 2 Parameter sample
# ------------------------------------------------------------------------------
helpTextCali <- paste(
  "Please write a SINGLE line R command from the selected package",  "\n",
  " ", "\n",
  "Use following keywords to access user settings from R-SWAT, e.g.,", "\n",
  " 'nParam' : the number of parameters (see Table above) ", "\n",
  " 'minCol' : R vector of the lower parameter range (see Table above)", "\n",
  " 'maxCol' : R vector of the upper parameter range (see Table above)", "\n",
  " 'SWAT'   : run SWAT with specific parameter set(s) generated by the", "\n",
  "            function from the selected packages and return objective
  function value(s)", "\n",
  " ", "\n",
  "The function from the selected package MUST minimize the values return by
  SWAT", "\n",
  "regardless of the setup in step 4.1. Objective function => 1. Select
  objective function", "\n",
  " ", "\n",
  "Please do steps 4.1. Objective function => 1. Select objective function and
  2.Get observed data file", "\n",
  "before proceeding to the next steps", "\n",
  sep ="")

# ------------------------------------------------------------------------------
helpTextSensi <- paste(
  "Please write TWO R commands (EACH command MUST be on a SINGLE line) from the
  selected package",  "\n",
  "   The first R command line (MUST start with sensCaliObject) executes SWAT
  runs with parameters from the sensitivity function","\n",
  "   The second R command line shows where the result is stored (Table with
  sensitivity indices) ","\n",
  " ","\n",
  "Use following keywords to access user settings from R-SWAT, e.g.,", "\n",
  "  'nParam' : the number of parameters (see Table above) ", "\n",
  "  'minCol' : R vector of the lower parameter range (see Table above)", "\n",
  "  'maxCol' : R vector of the upper parameter range (see Table above)", "\n",
  "  'SWAT'   : run SWAT with specific parameter set(s) generated by the", "\n",
  "            function from the selected packages and return objective function
  value(s)", "\n",
  " ", "\n",
  "Please do steps 4.1. Objective function => 1. Select objective function and ",
  "\n",
  "  2.Get observed data file BEFORE proceeding to the next steps", "\n",
  sep ="")




