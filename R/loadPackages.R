# Dependency packages (add more if necessary)
dependencyPackages <- c('foreach', 'doParallel',                                    # Parallelization
                        'lhs', 'plotly', 'ggplot2', 'rgdal', 'raster',              # Visualization
                        'shiny', 'shinydashboard', 'shinyFiles', 'tippy', 'excelR', # User interface
                        'sensitivity',  'boot',                                     # Sensitivity
                        'optimization','hydroPSO','nloptr')                                  # Optimization

# Check and install only missing packages
install.packages(setdiff(dependencyPackages,rownames(installed.packages())), 
                 dependencies = TRUE) 
  
# Load dependency packages
invisible(lapply(dependencyPackages, library, character.only = TRUE))

