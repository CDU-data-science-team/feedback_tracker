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
  
  trustData <- pin_read("SPACED/trustData") %>% 
    mutate(Directorate2 = recode(Directorate2,
                                 "Intellectual and developmental disability" =
                                   "IDD",
                                 "Mental health services for older people" = 
                                   "MHSOP"))
  
  care_opinion <- pin_get("care_opinion", board = "SPACED")
  
  mod_summary_server("summary_ui_1")
}
