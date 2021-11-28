# Dependency packages (add more if necessary)
dependencyPackages <- c('foreach', 'doParallel',                                    # Parallelization
                        'lhs', 'plotly', 'ggplot2', 'rgdal', 'raster',              # Visualization
                        'shiny', 'shinydashboard', 'shinyFiles', 'tippy', 'excelR', # User interface
                        'sensitivity',  'boot',                                     # Sensitivity
                        'optimization','hydroPSO','nloptr')                         # Optimization

# Check and install only missing packages
install.packages(setdiff(dependencyPackages,rownames(installed.packages())), 
                 dependencies = TRUE) 
  
# Load dependency packages
invisible(lapply(dependencyPackages, library, character.only = TRUE))

# IMPORTANT: 
# If you see some errors, e.g., error: there is no package called ‘backports’
#                               error: there is no package called ‘terra’
# then you can download these packages from the following links and install manually
# https://cran.r-project.org/bin/windows/contrib/4.0/backports_1.3.0.zip
# https://cran.r-project.org/bin/windows/contrib/4.0/terra_1.4-22.zip
