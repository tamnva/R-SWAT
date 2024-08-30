
library(DT)

# Create UI
ui <- shinydashboard::dashboardPage(

  # ----------------------------------------------------------------------Header
  shinydashboard::dashboardHeader(
    # Name of the app
    title = "RSWAT v4.01",

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
    tags$head(tags$style(HTML('/* active selected tab in the shinydashboard::sidebarMenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #E0E0E0;
                            color: #666666;
                            }
                            '))),

    # Link tab names with their codes
    shinydashboard::tabItems(

      shinydashboard::tabItem(tabName = "intro",
              introductionUI("introductionUI")),

      shinydashboard::tabItem(tabName = "generalSetting",
              generalSettingUI("generalSettingUI")),

      shinydashboard::tabItem(tabName = "paramSampling",
              paramSamplingUI("paramSamplingUI")),

      shinydashboard::tabItem(tabName = "runSWAT",
              runSwatUI("runSwatUI")),

      shinydashboard::tabItem(tabName = "objFunctionTab",
              objFunctionUI("objFunctionUI")),

      shinydashboard::tabItem(tabName = "sensAnalysisTab",
              sensAnalysisUI("sensAnalysisUI")),

      shinydashboard::tabItem(tabName = "paraOptUncerTab",
              paraOptUncerUI("paraOptUncerUI")),

      shinydashboard::tabItem(tabName = "swatEdu",
              swatEduUI("swatEdu"))

    )
  ),

  shinyjs::useShinyjs()
 #------------------------------------------------------------------------------
)
