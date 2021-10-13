
board <- pins::board_rsconnect()

trustData <- pins::pin_read(board, "chrisbeeley/trustData") %>% 
  dplyr::filter(Date > Sys.Date() - 365 * 2) %>% 
dplyr::mutate(Directorate2 = dplyr::recode(
  Directorate2,
  "Intellectual and developmental disability" =
    "IDD",
  "Mental health services for older people" = 
    "MHSOP"))

care_opinion <- pins::pin_read(board, "chrisbeeley/care_opinion") %>% 
  dplyr::filter(Date > Sys.Date() - 365 * 2)
