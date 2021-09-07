# Dependency packages
  packages <- c('foreach', 'parallel', 'lhs', 'plotly', 'shinydashboard', 
                'shinyFiles', 'tippy', 'excelR', 'shiny', 'doParallel',
                'sensitivity', 'ggplot2', 'boot')

# Install dependency packages if necessary
  install.packages(setdiff(packages,rownames(installed.packages())),
                   repos = 'https://CRAN.R-project.org') 

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
  library(doParallel)
  library(sensitivity)
  library(ggplot2)
  library(boot)