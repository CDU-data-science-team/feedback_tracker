#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  # load data
  
  board <- pins::board_rsconnect()
  
  trustData <- pins::pin_read(board, "chrisbeeley/trustData") %>% 
    dplyr::filter(Date > Sys.Date() - 365 * 2) %>% 
    dplyr::mutate(Directorate2 = dplyr::recode(Directorate2,
                                               "Intellectual and developmental disability" =
                                                 "IDD",
                                               "Mental health services for older people" = 
                                                 "MHSOP"))
  
  care_opinion <- pins::pin_read(board, "chrisbeeley/care_opinion") %>% 
    dplyr::filter(Date > Sys.Date() - 365 * 2)
  
  # inputs
  
  reactive_inputs <- reactive(
    list(
      "period" = input$period
    )
  )
  
  mod_summary_server("summary_ui_1", reactive_inputs = reactive_inputs)
}
