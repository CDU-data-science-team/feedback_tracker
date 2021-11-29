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
    
    plotOutput(ns("time_graph")),
    DT::DTOutput(ns("response_table"))
  )
}

#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id, trustData, reactive_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$time_graph <- renderPlot({
      
      draw_plot <- count_responses(trustData, 
                      reactive_inputs()$period, 
                      reactive_inputs()$separate_mode) %>% 
        ggplot2::ggplot(ggplot2::aes(x = date_count, y = n)) +
        ggplot2::geom_line() + ggplot2::geom_point() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1)) +
        ggplot2::theme(legend.position = "none")
      
      if(reactive_inputs()$separate_mode){
        
        draw_plot <- draw_plot + 
          ggplot2::facet_wrap(~ type, scales = "free_y", ncol = 1)
      }
      
      draw_plot
    })
    

    
  })
}
