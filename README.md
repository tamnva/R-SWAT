## R-SWAT <a href="https://github.com/tamnva/R-SWAT/blob/master/data/figures/R-SWAT_logo.svg"><img src="data/figures/R-SWAT_logo.svg" align="right" height="50" /></a>

[![DOI](https://zenodo.org/badge/395115735.svg)](https://zenodo.org/badge/latestdoi/395115735) [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://github.com/tamnva/R-SWAT/blob/master/LICENSE) [![Release](https://img.shields.io/github/release/tamnva/R-SWAT.svg?style=flat-square)](https://github.com/tamnva/R-SWAT/releases)

Please cite R-SWAT as follows:

**Nguyen, T. V.**, Dietrich, J., Dang, D. T., Tran, D. A., Doan, B. V., Sarrazin, F. J., Abbaspour, K., Srinivasan, R. (2022). An interactive graphical interface tool for parameter calibration, sensitivity analysis, uncertainty analysis, and visualization for the Soil and Water Assessment Tool. Environmental Modelling & Software, 156, 105497. https://doi.org/10.1016/j.envsoft.2022.105497

- This is an interactive web-based app (written in [R](https://cran.r-project.org/bin/windows/base/) language with [shiny](https://shiny.rstudio.com/) package) for parallel parameter sensitivity, calibration, and uncertainty analysis with the Soil and Water Assessment Tool ([SWAT](https://swat.tamu.edu/)). The official name of this app is R-SWAT (initially its name was SWATshiny)
- An R script file for running R-SWAT in HPC without using graphical interface is also available [here](https://github.com/tamnva/R-SWAT/blob/master/data/example_running_RSWAT_without_user_interface/RSWAT_script_SUFI2.R).
- The latest version of this app is always in the "master" branch.
- If you would like to contribute to the code, have any suggestions, want to report errors, and have scientific collaboration, please contact [me](https://www.ufz.de/index.php?en=46415). The best way to contact me is via the  [google group](https://groups.google.com/g/R-SWAT) specially designed for this app. Tutorial videos can be found in [R-SWAT youtube channel](https://www.youtube.com/channel/UCRK1rKFiNgYbG7qKWxAPtEQ)
- This is the first version, the app will be continued to be developed to serve the SWAT community. If you would like to contribute to the development of this app, please let me know. Together we can make this app much better as my time and R knowledge for this app are very limited.
- [A full documentation of _R-SWAT_ with examples is in its wiki page](https://github.com/tamnva/R-SWAT/wiki/R-SWAT-User-Manual).

## Quick Start!

This app is designed for R beginner to advance users, you can use this app even you don't know anything about R. 

To run this app, install latest **[R version 4.1.1](https://cran.r-project.org/bin/windows/base/)** or newer version (If you use old version of R this **might NOT** work). Then **[install R studio](https://www.rstudio.com/)**. Then do the following steps:

- **Step 1**: [Click here](https://github.com/tamnva/R-SWAT/archive/refs/heads/master.zip) to download the whole code of this app.
- **Step 2**: Extract the downloaded file => Open RStudio
- **Step 3**: Open the file *./R/loadPackages.R* with R studio and install all packages list in this file (take 5-10 minutes).  
- **Step 4**: Open the file server.R or ui.R in R studio => then click the green button named *Run App*

**Next time** when you use this app just **step 4** is needed **or you can download the latest app and run it using the following commands**

    # Load shiny package
    library(shiny)
    
    # Automatically download this R-SWAT app (code + data) to your local machine and run it
    runGitHub("R-SWAT", "tamnva")
    
    # If you already manually downloaded it from the github, you can run the app using this command
    # runApp("RSWAT_folder")

The following interface (attached screenshot on the end of this document) will appear and you can start using this app. Start with Tab '1. General setting' => '2. Parameter sampling' => and so on. On each tab there are subitems (e.g., in the first figure, start with '1. Working folder' => '2. TxtInOut folder' => and so on)

If you don't know which input is required, simply clicking to "Help?" at the right side of each respective input field. A sample of data for running this app can be downloaded from my [GitHub repository](https://github.com/tamnva/R-SWAT/blob/master/data/obs_var_1.txt), including:

 1. _/data/example_SWAT/TxtInOut_ and _/data/example_SWAT+/TxtInOut_: TxtInOut folders and all associated files created by ArcSWAT and SWAT+ Editor, respectively
 2. _/data/example_SWAT/swatParam.txt_ and _/data/example_SWAT+/cal_parms.cal_ : An input file describing all SWAT and SWAT+ parameters, respectively, that you will need for the input field _1. General Setting => 4. Files with list of all SWAT Parameters_
 3. _/data/example_SWAT/obs_var_1.txt_ and _/data/example_SWAT+/obs_var_1.txt_: Observed data file (streamflow) at the catchment outlet that you might need to input to evaluate the model performance _4.1. Objective function => 2. Get observed data files_
 4. _data/examples_: you will see examples how to include new parameters in new files (e.g., this could be the case  if you modify SWAT code)

## R-SWAT user interface 
(NOTE: in the latest version, the user interface could be slightly difference)
<p align="center">
  <img src="data/figures/RSWAT.gif" width=100% title="hover text">
</p>
