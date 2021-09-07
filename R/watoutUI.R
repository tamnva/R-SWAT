
# Module hruUI function

watoutUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    # Input data -----------------------------------------------------
    fluidRow(             
      column(width = 4,
             fileInput("watoutFile",
                       "1. Select watout.dat file type", 
                       multiple = TRUE
             )
      ),
      
      column(width = 2,
             selectInput("selectColWatout", 
                         "Select data (column) to plot", 
                         choices = NULL, 
                         width = "100%"
             )
      ), 
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   1. Select watout.dat file type for plot
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
    ),
    
    fluidRow(
      
      column(width = 4,
             fileInput("observedFile",
                       "2. Select observed data file", 
                       multiple = TRUE
             )
      ),
      
      column(width = 2,
             selectInput("selectColObs", 
                         "Select data (column) to plot", 
                         choices = NULL, 
                         width = "100%"
             )
      ),
      
      column(width = 1,
             tippy("Help?", tooltip = "<span style='font-size:16px;'>
                   2. Select observed data file: this is optional, if you want to compare with observed, please
                   load the observed data file, the file format must be similar with
                   'obs_var_1' in the .\\data folder, however, it could have more columns 
                   (e.g., first column is date in yyyy-mm-dd format, 2nd column is data 
                   of the variable 1, 3rd column is data of the variable 2, so on..
                   <span>", 
                   allowHTML = TRUE, 
                   trigger = "click",
                   theme = "translucent"),
      ),
    ),
    
    fluidRow(
      box(status = NULL,
          width = 11 ,
          solidHeader = F,
          plotlyOutput("plotWatout",
                       width = "100%",
                       height = "400px")
          )
      ),
    
    #----------------
  )}

