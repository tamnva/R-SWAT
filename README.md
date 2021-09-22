## SWATshiny

- This is an interactive web-based app (written in [R](https://cran.r-project.org/bin/windows/base/) language with [shiny](https://shiny.rstudio.com/) package) for parallel parameter sensitivity, calibration, and uncertainty analysis with the Soil and Water Assessment Tool ([SWAT](https://swat.tamu.edu/)).
- The latest version of this app is always in the "master" branch.
- If you would like to contribute to the code, have any suggestions, want to report errors, and have scientific collaboration, please contact [me](https://www.ufz.de/index.php?en=46415). The best way to contact me is via the  [google group](https://groups.google.com/g/swatshiny) specially designed for this app. Tutorial videos can be found in [SWATshiny youtube channel](https://www.youtube.com/channel/UCRK1rKFiNgYbG7qKWxAPtEQ)
- This is the first version, the app will be continued to be developed to serve the SWAT community. If you would like to contribute to the development of this app, please let me know. **Together we can make this app much better as my time and R knowledge for this app are very limited.

## Getting Started!

This app is designed for R beginner to advance users, you can use this app even you don't know anything about R. Just install the **<u>latest</u>** version of R that you can find [here](https://cran.r-project.org/bin/windows/base/). I would also suggest installing [RStudio](https://www.rstudio.com/).  After installing R, open R (or RStudio) and use the following code to download the app to your local machine:

    #-----------------------------------------------------------------------------------------------------------
    # 1. If you run SWATshiny for the first time, install the following packages
    #-----------------------------------------------------------------------------------------------------------
    # Denpendency packages
    dependencyPackages <- c('foreach', 'parallel', 'lhs', 'plotly', 'shinydashboard', 
                            'shinyFiles', 'tippy', 'excelR', 'shiny', 'doParallel',
                            'sensitivity', 'ggplot2', 'boot','rgdal', 'raster')
    
    # Check and install only missing packages
    install.packages(setdiff(dependencyPackages,rownames(installed.packages())), dependencies = TRUE) 
      
    # Load dependency packages
    invisible(lapply(dependencyPackages, library, character.only = TRUE))
    
    #-----------------------------------------------------------------------------------------------------------
    # 2. Then run the following commands (next time you use SWATshiny, just run these commands)
    #-----------------------------------------------------------------------------------------------------------
    # Call shiny package
    library(shiny)
    
    # Automatically download this SWATshiny app (code + data) to your local machine and run it
    runGitHub("SWATshiny", "tamnva")
    
    # If you already manually downloaded it from the github, you can run the app using this command
    # runApp("SWATshiny_folder")

If the app is successfully downloaded to your local machine. If it doesn't work, please see this [documentation](https://groups.google.com/g/swatshiny/c/4geoDwlV5Ok). The following interface (attached screenshot on the end of this document) will appear and you can start using this app. Start with Tab '1. General setting' => '2. Parameter sampling' => and so on. On each tab there are subitems (e.g., in the first figure, start with '1. Working folder' => '2. TxtInOut folder' => and so on)

If you don't know which input is required, simply clicking to "Help?" at the right side of each respective input field. A sample of data for running this app can be downloaded from my [GitHub repository](https://github.com/tamnva/SWATshiny/tree/master/data), including:

 1. _/data/TxtInOut_: A TxtInOut folder and all associated files created by ArcSWAT
 2. _swatParam.txt_: An input file describing all SWAT parameters that you will need for the input field _1. General Setting => 4. Files with list of all SWAT Parameters_
 3. _obs_var_1.txt_: Observed data file (streamflow) at the catchment outlet that you might need to input to evaluate the model performance _4.1. Objective function => 2. Get observed data files_
  4. _data/examples_: you will see examples how to include new parameters in new files (e.g., this could be the case  if you modify SWAT code)

## SWATshiny user interface
<p align="center">
  <img src="data/figures/SWATshiny.gif" width=100% title="hover text">
</p>


#### 

