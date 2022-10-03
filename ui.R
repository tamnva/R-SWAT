
# Create UI
ui <- dashboardPage(
  
  # ----------------------------------------------------------------------Header
  dashboardHeader(
    # Name of the app
    title = "R-SWAT (v2.0.1)", 
    
    # Add two buttons in the header for saving and loading project
    tags$li(class = "dropdown", 
          
            # Save project button
            actionButton(inputId = "saveProject",
                         label = "Save project", 
                         icon = icon("save"),
                         style="background-color: #b1fa87; border-color: #b1fa87"),
            
            # Load project button
            actionButton(inputId = "loadProject", 
                         label = "Load project" , 
                         icon = icon("upload"),
                         style="background-color: #fafa87; border-color: #2e6da4")
            )
  ),
  
  # ---------------------------------------------------------------------Sidebar
  dashboardSidebar(

    # Introduction Tab
    sidebarMenu(
      menuItem("About this app", 
               tabName = "intro", 
               icon = icon("book"), 
               selected = TRUE)
    ),
    
    # General setting tab
    sidebarMenu(
      menuItem("1. General setting", 
               tabName = "generalSetting", 
               icon = icon("cog"), 
               selected = FALSE)
    ),
    
    # Parameter sampling Tab
    sidebarMenu(
      menuItem("2. Parameter sampling", 
               tabName = "paramSampling", 
               icon = icon("dice-five"),
               selected = FALSE)
    ),
    
    # Run SWAT Tab
    sidebarMenu(
      menuItem("3. Run SWAT", 
               tabName = "runSWAT", 
               icon = icon("running"),
               selected = FALSE)
    ),
    
    # Evaluate model output tab
    sidebarMenu(
      menuItem("4. Evaluate output", 
               tabName = "evalOutputTab",
               icon = icon("chart-line"),
               startExpanded = FALSE, 
               selected = FALSE,
               menuSubItem("4.1.Objective function", 
                           tabName = "objFunctionTab", 
                           selected = FALSE),
               menuSubItem("4.2. Sensitivity Analysis", 
                           tabName = "sensAnalysisTab", 
                           selected = FALSE),
                menuSubItem("4.3. Optimization/Uncertainty", 
                          tabName = "paraOptUncerTab", 
                          selected = FALSE))
      
    ),
    
    # Visualization Tab
    sidebarMenu(
      menuItem("Visualization", tabName = "visual",icon = icon("eye"),
               startExpanded = FALSE, selected = FALSE,
               menuSubItem("watout.dat", tabName = "watout", selected = FALSE),
               menuSubItem("output.hru", tabName = "hru", selected = FALSE),
               menuSubItem("output.rch", tabName = "rch", selected = FALSE),
               menuSubItem("output.sub", tabName = "sub", selected = FALSE))
    )
    
  ),

  # ------------------------------------------------------------------------Body
  dashboardBody(
    
    # Change menu background color when selected
    tags$head(tags$style(HTML('/* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #E0E0E0;
                            color: #666666;
                            }
                            '))), 
    
    # Link tab names with their codes
    tabItems(

      tabItem(tabName = "intro",
              introductionUI("introductionUI")),
      
      tabItem(tabName = "generalSetting",
              generalSettingUI("generalSettingUI")),
      
      tabItem(tabName = "paramSampling",
              paramSamplingUI("paramSamplingUI")),
      
      tabItem(tabName = "runSWAT",
              runSwatUI("runSwatUI")),
      
      tabItem(tabName = "objFunctionTab",
              objFunctionUI("objFunctionUI")),
      
      tabItem(tabName = "sensAnalysisTab",
              sensAnalysisUI("sensAnalysisUI")),
      
      tabItem(tabName = "paraOptUncerTab",
              paraOptUncerUI("paraOptUncerUI")),
      
      # Visualization
      tabItem(tabName = "watout",
              watoutUI("watoutUI")),
      
      tabItem(tabName = "hru",
              hruUI("hruUI")),
      
      tabItem(tabName = "rch",
              rchUI("rchUI")),

      tabItem(tabName = "sub",
              subUI("subUI"))
    )
  ),
  
  useShinyjs()
 #------------------------------------------------------------------------------ 
)