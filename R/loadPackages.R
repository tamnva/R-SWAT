#------------------------------------------------------------------------------#
# IMPORTANT: First, make sure that your R version >= 4.0.0. In R studio you can 
#            check by selecting Tools => Global Options => General => R version
#------------------------------------------------------------------------------#

# Required packages (add more if necessary). 
# The required package versions will be automatically satisfied by running line 33 

requiredPackages <- c('foreach',         # version >= 1.5.2
                      'doParallel',      # version >= 1.0.17     
                      'lhs',             # version >= 1.1.4
                      'plotly',          # version >= 4.10.0
                      'ggplot2',         # version >= 3.3.5
                      'rgdal',           # version >= 1.5-28
                      'raster',          # version >= 3.5-15 
                      'shiny',           # version >= 1.7.1
                      'shinydashboard',  # version >= 0.7.2
                      'shinyFiles',      # version >= 0.9.1
                      'tippy',           # version >= 0.1.0
                      'excelR',          # version >= 0.4.0
                      'sensitivity',     # version >= 1.27.0
                      'boot',            # version >= 1.3-24
                      'optimization',    # version >= 1.0-9  
                      'hydroPSO',        # version >= 0.5-1
                      'nloptr'           # version >= 2.0.0
                      )
                     
# Check and install only missing packages
install.packages(setdiff(requiredPackages,rownames(installed.packages())), dependencies = TRUE) 

# Update these packages if it has not been updated
update.packages(requiredPackages, ask = FALSE)
  
# Load dependency packages
invisible(lapply(requiredPackages, library, character.only = TRUE))
