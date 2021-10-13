#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(3, 
             selectInput(ns("period"),
                         choices = c("Weekly", "Monthly", "Quarterly"),
                         selected = "Monthly")
             ),
      column(9, 
             plotOutput(ns("time_graph")),
             tableOutput(ns("response_table"))
             )
    )
 
  )
}
    
#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$time_graph
 
  })
}
