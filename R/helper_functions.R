#' Calculate the number of patient feedback received by week/ month/ quarter
#' @description This function takes a (filtered) dataset and returns the numbers
#' of pieces of patient feedback received by week/ month/ year. Optionally 
#' select a split by mode of feedback
#' @param data Dataframe, filtered by date/ area in a reactive function
#' @param period string, either "week", "month", "year", indicating the preferred
#' granularity with respect to time
#' @param mode boolean- return grouped by mode of response or all together
count_responses <- function(data, period, mode){
  
  count_df <- data %>% 
    dplyr::mutate(type = dplyr::case_when(
      addedby %in% c("jisc", "survey.monkey") ~ "Online",
      addedby == "SMS" ~ "SMS",
      TRUE ~ "Other"
    )) %>% 
    dplyr::mutate(date_count = lubridate::floor_date(Date, period)) %>% 
    tidyr::complete(date_count = seq.Date(min(date_count), max(date_count), 
                                          by = period))
  if(mode){
    
    count_df <- count_df %>% 
      dplyr::group_by(type)
  }
  count_df %>% 
    dplyr::group_by(date_count, .add = TRUE) %>% 
    dplyr::summarise(n = dplyr::n(), .drop = FALSE)
}
