
library(DT)

# Create UI
ui <- shinydashboard::dashboardPage(

  # ----------------------------------------------------------------------Header
  shinydashboard::dashboardHeader(
    # Name of the app
    title = "RSWAT v5.0",

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
                         icon = icon("download"),
                         style="background-color: #fafa87; border-color: #2e6da4")
            )
  ),

  # ---------------------------------------------------------------------Sidebar
  shinydashboard::dashboardSidebar(

    # Introduction Tab
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("About this app",
               tabName = "intro",
               icon = icon("book"),
               selected = TRUE)
    ),

    # General setting tab
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("1. General setting",
               tabName = "generalSetting",
               icon = icon("cog"),
               selected = FALSE)
    ),

    # Parameter sampling Tab
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("2. Parameter sampling",
               tabName = "paramSampling",
               icon = icon("dice-five"),
               selected = FALSE)
    ),

    # Run SWAT Tab
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("3. Run SWAT",
               tabName = "runSWAT",
               icon = icon("running"),
               selected = FALSE)
    ),

    # Evaluate model output tab
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("4. Evaluate output",
               tabName = "evalOutputTab",
               icon = icon("chart-line"),
               startExpanded = FALSE,
               selected = FALSE,
               shinydashboard::menuSubItem("4.1.Objective function",
                           tabName = "objFunctionTab",
                           selected = FALSE),
               shinydashboard::menuSubItem("4.2. Sensitivity Analysis",
                           tabName = "sensAnalysisTab",
                           selected = FALSE),
                shinydashboard::menuSubItem("4.3. Optimization/Uncertainty",
                          tabName = "paraOptUncerTab",
                          selected = FALSE))

    ),

    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Manual calibration",
               tabName = "swatEdu",
               icon = icon("graduation-cap"),
               selected = FALSE)
    )

  ),

  # ------------------------------------------------------------------------Body
  shinydashboard::dashboardBody(

    # Change menu background color when selected
    tags$head(tags$style(HTML(
      '/* active selected tab in the shinydashboard::sidebarMenu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
      background-color: #E0E0E0;color: #666666}'))),

    # Link tab names with their UI
    shinydashboard::tabItems(

      shinydashboard::tabItem(tabName = "intro",
              introductionUI("introductionUI")),

      shinydashboard::tabItem(tabName = "generalSetting",
              generalSettingUI("general_setting_id")),

      shinydashboard::tabItem(tabName = "paramSampling",
              paramSamplingUI("param_sampling_id")),

      shinydashboard::tabItem(tabName = "runSWAT",
              runSwatUI("run_swat_id")),

      shinydashboard::tabItem(tabName = "objFunctionTab",
              objFunctionUI("obj_function_id")),

      shinydashboard::tabItem(tabName = "sensAnalysisTab",
              sensAnalysisUI("sens_analysis_id")),

      shinydashboard::tabItem(tabName = "paraOptUncerTab",
              paraOptUncerUI("para_opt_uncer_id")),

      shinydashboard::tabItem(tabName = "swatEdu",
              swatEduUI("swat_edu_id"))

    )
  ),

  shinyjs::useShinyjs()
 #------------------------------------------------------------------------------
)
