#' Calculate the number of patient feedback received by week/ month/ quarter
#' @description This function takes a (filtered) dataset and returns the numbers
#' of pieces of patient feedback received by week/ month/ year. Optionally 
#' select a split by mode of feedback
#' @param data Dataframe, filtered by date/ area in a reactive function
#' @param period string, either "week", "month", "year", indicating the preferred
#' granularity with respect to time
#' @param mode boolean- return grouped by mode of response or all together
count_responses <- function(data, period, mode, area = NULL){
  
  count_df <- data %>% 
    dplyr::mutate(type = dplyr::case_when(
      addedby %in% c("jisc", "survey.monkey") ~ "Online",
      addedby == "SMS" ~ "SMS",
      TRUE ~ "Other"
    )) %>% 
    dplyr::mutate(date_count = lubridate::floor_date(Date, period))
  
  # it's just too complicated. I'm going to have to write all 4 functions
  # separately
  
  # if both
  
  if(mode & !is.null(area)){
    
    return(
      count_df %>% 
        dplyr::group_by(date_count, type, .data[[area]]) %>% 
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(dplyr::across(where(is.character), factor)) %>% 
        tidyr::complete(date_count = seq.Date(min(date_count), max(date_count), 
                                              by = period),
                        tidyr::nesting(type, !!rlang::sym(area)),
                        fill = list(n = 0)) %>% 
        dplyr::filter(complete.cases(.))
    )
  }
  
  # if neither
  
  if(!mode & is.null(area)){
    
    return(
      count_df %>% 
        dplyr::group_by(date_count) %>% 
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(dplyr::across(where(is.character), factor)) %>% 
        tidyr::complete(date_count = seq.Date(min(date_count), max(date_count), 
                                              by = period),
                        fill = list(n = 0)) %>% 
        dplyr::filter(complete.cases(.))
    )
  }
  
  # if mode
  
  if(mode){
    
    return(
      count_df %>% 
        dplyr::group_by(date_count, type) %>% 
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(dplyr::across(where(is.character), factor)) %>% 
        tidyr::complete(date_count = seq.Date(min(date_count), max(date_count), 
                                              by = period),
                        tidyr::nesting(type),
                        fill = list(n = 0)) %>% 
        dplyr::filter(complete.cases(.))
    )
  }
  
  # if area
  
  if(!is.null(area)){
    
    return(
      count_df %>% 
        dplyr::group_by(date_count, .data[[area]]) %>% 
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(dplyr::across(where(is.character), factor)) %>% 
        tidyr::complete(date_count = seq.Date(min(date_count), max(date_count), 
                                              by = period),
                        tidyr::nesting(!!rlang::sym(area)),
                        fill = list(n = 0)) %>% 
        dplyr::filter(complete.cases(.))
    )
  }
}
