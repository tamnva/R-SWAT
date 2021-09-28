# Dependency packages
dependencyPackages <- c('foreach', 'parallel', 'lhs', 'plotly', 'shinydashboard', 
                        'shinyFiles', 'tippy', 'excelR', 'shiny', 'doParallel',
                        'sensitivity', 'ggplot2', 'boot','rgdal', 'raster')

# Check and install only missing packages
install.packages(setdiff(dependencyPackages,rownames(installed.packages())), 
                 dependencies = TRUE) 
  
# Load dependency packages
invisible(lapply(dependencyPackages, library, character.only = TRUE))
