
# Dependency packages
  packages <- c('foreach', 'parallel', 'lhs', 'plotly', 'shinydashboard', 
                'shinyFiles', 'tippy', 'excelR', 'shiny')

# Install dependency packages if necessary
  install.packages(setdiff(packages,rownames(installed.packages())),
                   repos = 'http://cran.us.r-project.org') 

# Load dependency packages
  library(shiny)
  library(foreach)
  library(lhs)
  library(parallel)
  library(plotly)
  library(shinydashboard)
  library(shinyFiles)
  library(tippy)
  library(excelR)
