
# Dependency packages
  packages <- c('foreach', 
                'parallel', 
                'lhs', 
                'raster', 
                'rgdal', 
                'plotly', 
                'shinydashboard', 
                'shinyFiles', 
                'tippy', 
                'excelR')

# Install dependency packages if necessary
  install.packages(repos = 'http://cran.us.r-project.org',
                   setdiff(packages, 
                           rownames(installed.packages())
                           )
                   ) 

# Load dependency packages
  library(foreach)
  library(lhs)
  library(parallel)
  library(raster)
  library(rgdal)
  library(plotly)
  library(shinydashboard)
  library(shinyFiles)
  library(tippy)
  library(excelR)
