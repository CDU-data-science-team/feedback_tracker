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
    
    navbarPage("",
               tabPanel("Graph",
                        plotOutput(ns("time_graph"), height = "600px")),
               tabPanel(
                 "Table",
                 p("Click in the boxes above the table to filter based 
                   on the values in that column"),
                 DT::DTOutput(ns("response_table"))
               )
    )
  )
}

#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id, filter_data, reactive_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$time_graph <- renderPlot({
      
      cat(str(reactive_inputs()))
      
      draw_plot <- count_responses(data = filter_data(), 
                                   period = reactive_inputs()$period, 
                                   mode = reactive_inputs()$separate_mode,
                                   area = reactive_inputs()$select_area) %>% 
        ggplot2::ggplot(ggplot2::aes(x = date_count, y = n)) +
        ggplot2::geom_line() + ggplot2::geom_point() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1)) +
        ggplot2::theme(legend.position = "none")
      
      if(reactive_inputs()$separate_mode & reactive_inputs()$separate_area){

        return(
          draw_plot +
            ggplot2::facet_grid(ggplot2::vars(area),
                                ggplot2::vars(type),
                                scales = "free_y")
        )
      }

      if(reactive_inputs()$separate_mode){

        return(
          draw_plot +
            ggplot2::facet_wrap(~ type, scales = "free_y", ncol = 1)
        )
      }
      
      if(reactive_inputs()$separate_area){
        
        return(
          draw_plot +
            ggplot2::facet_wrap(~ area, scales = "free_y", ncol = 1)
        )
      }
      
      draw_plot
    })
    
    output$response_table <- DT::renderDT({
      
      table_data <- trustData %>% 
        count_responses(reactive_inputs()$period, 
                        reactive_inputs()$separate_mode, 
                        area = "Division2")
      
    }, rownames = FALSE, 
    filter = "top", 
    extensions = 'Buttons', 
    options = list(dom = 'Blfrtip',
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
    )
  })
}
