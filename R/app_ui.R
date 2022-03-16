#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      
      dashboardHeader(title = "Patient experience",
                      titleWidth = 300),
      dashboardSidebar(
        width = 300,
        sidebarMenu(
          
          menuItem("Summary", 
                   tabName = "survey-responses",
                   icon = shiny::icon("chart-line"),
                   selected = TRUE),
          
          selectInput("period", "Period",
                      choices = c("Weekly" = "week", 
                                  "Monthly" = "month", 
                                  "Quarterly" = "quarter"),
                      selected = "Monthly"),
          selectInput("select_area", "Select reporting level",
                      choices = c("Trust" = "Division2", 
                                  "Division" = "Directorate2",
                                  "Directorate" = "TeamN")
          ),
          conditionalPanel("input.select_area == ' Division2'", 
                           selectInput("select_division", "Select division",
                                       choices = c("Mental health services" = 0,
                                                   "Community health services" = 2,
                                                   "Forensic services" = 1))
          ),
          uiOutput("directorate_UI"),
          checkboxInput("separate_area", "Break down individual areas"),
          checkboxInput("separate_mode", "Separate mode of response")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "survey-responses",
                  mod_summary_ui("summary_ui_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'feedbackTracker'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

