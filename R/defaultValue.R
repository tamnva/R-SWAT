

# ------------------------------------------------------------------------------
# Default parameter change values
# ------------------------------------------------------------------------------
dataParaSelection <- data.frame(Parameter = c('GW_DELAY.gw', 'CN2.mgt', 'SOL_K.sol', 
                                             'ALPHA_BF.gw', 'ESCO.hru', 'SURLAG.hru', 
                                             'CH_K2.rte', 'SURLAG.bsn'),
                               Change = c('absolute', 'relative', 'relative', 
                                          'replace', 'replace', 'replace', 'replace', 'replace'),
                               Min = c(50,-0.25, -0.25, 0.01, 0.5, 0.1, 0.0, 3.0),
                               Max = c(450,0.25, 0.25, 0.4, 0.99, 10.0, 0.5, 6.0),
                               Subbasin = c('All','All', 'All', 'All', '1,3,5', 'All', 'All', NA),
                               Landuse = c('All','All', 'All', 'All', 'FRSE, RNGB', 'All', NA, NA),
                               Soil = c('B59H5401, B63H8301','All', 'All', 'All', 'All', 'All', NA, NA),
                               Slope = c('0-5', 'All', 'All', '0-5, 5-9999', 'All', 'All', NA, NA))

columnsParaSelection <- data.frame(title= colnames(dataParaSelection),
                                  source = I(list(NA ,
                                                  c('absolute', 'relative','replace'),
                                                  NA, 
                                                  NA, 
                                                  NA, 
                                                  NA,
                                                  NA, 
                                                  NA)),
                                  width= c(300, 
                                           300, 
                                           300, 
                                           300, 
                                           300, 
                                           300,
                                           300, 
                                           300),
                                  type=c('text', 
                                         'dropdown', 
                                         'numeric',
                                         'numeric', 
                                         'numeric', 
                                         'text',
                                         'text', 
                                         'text')
                                  )

# ------------------------------------------------------------------------------
# Default output variables
# ------------------------------------------------------------------------------
dataOutputExtraction <- data.frame(FileType = c('watout.dat'),
                                  FileName = c('watout.dat'),
                                  Column = c("4"),
                                  Reach = c(NA))
                               

columnsOutputExtraction <- data.frame(title= colnames(dataOutputExtraction),
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
