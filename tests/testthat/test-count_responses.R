test_that("Counting responses works", {
  
  test_week <- count_responses(trustData, "week", TRUE)
  
  test_week_agg <- count_responses(trustData, "week", FALSE)
  
  expect_gt(nrow(test_week), 100)
  
  expect_lt(nrow(test_week_agg), 100)
  
  test_month <- count_responses(trustData, "month", TRUE)
  
  expect_gt(nrow(test_month), 50)
  
  count_responses(trustData, "month", TRUE)
  
  trustData %>% 
    count_responses(period = "week", mode = TRUE, area = "Division2")
  
  trustData %>% 
    count_responses("week", FALSE, area = "Division2")
  
  trustData %>% 
    count_responses("week", TRUE, area = NA)
  
  trustData %>% 
    count_responses("week", FALSE, area = NA)
  
  trustData %>% 
    dplyr::filter(Division == 2) %>% 
    count_responses("week", FALSE, area = "Directorate2")

})
