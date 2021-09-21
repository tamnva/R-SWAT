

# Create UI
ui <- dashboardPage(
  
  # ----------------------------------------------------------------------Header
  dashboardHeader(
    title = "SWATshiny"
  ),
  
  # ---------------------------------------------------------------------Sidebar
  dashboardSidebar(

    sidebarMenu(
      menuItem("About this app", 
               tabName = "intro", 
               icon = icon("book"), 
               selected = TRUE)
    ),
    
    sidebarMenu(
      menuItem("1. General Setting", 
               tabName = "generalSetting", 
               icon = icon("cog"), 
               selected = FALSE)
    ),
    
    sidebarMenu(
      menuItem("2. Parameter sampling", 
               tabName = "paramSampling", 
               icon = icon("dice-five"),
               selected = FALSE)
    ),
    
    sidebarMenu(
      menuItem("3. Run SWAT", 
               tabName = "runSWAT", 
               icon = icon("running"),
               selected = FALSE)
    ),
    
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
    
    # Tab item for General Setting
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
              watoutUI("watoutUI")
      ),
      
      tabItem(tabName = "hru",
              hruUI("hruUI")
      ),
      
      tabItem(tabName = "rch",
              rchUI("rchUI")
      ),

      tabItem(tabName = "sub",
              subUI("subUI")
      )     
    )
  ) 
 #------------------------------------------------------------------------------ 
)