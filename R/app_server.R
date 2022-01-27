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
    dplyr::filter(Date > Sys.Date() - 365 * 2)
  
  care_opinion <- pins::pin_read(board, "chrisbeeley/care_opinion") %>% 
    dplyr::filter(Date > Sys.Date() - 365 * 2)
  
  dirTable <- pins::pin_read(board, "chrisbeeley/dirTable")
  
  # handle reactive UI from division selection
  
  output$directorate_UI <- renderUI({
    
    if(input$select_area != "TeamN"){
      return()
    }
    
    # if directorate is empty return all directorates
    
    finalTable = dirTable %>%
      dplyr::filter(Division %in% input$select_division)
    
    # get rid of corporate and unknown
    
    finalTable = finalTable %>%
      dplyr::filter(!DirC %in% c(0, 40))
    
    # finally pull the directorates and names
    
    directorates = finalTable %>%
      dplyr::pull(DirC)
    
    names(directorates) = finalTable %>%
      dplyr::pull(DirT)
    
    selectInput("select_directorate", "Choose directorate(s)",
                directorates, multiple = TRUE)
    
  })
  
  # data
  
  filter_data <- reactive({
    
    if(input$select_area == "Division2"){ # this is Trust level, confusingly
      
      return(trustData)
    }
    
    if(input$select_area == "Directorate2"){ # this is division level, confusingly
      
      if(!isTruthy(input$select_division)){
        
        return(trustData)
      }
      
      to_return <- trustData %>% 
        dplyr::filter(Division %in% input$select_division)
      
      cat(str(to_return))
      
      return(to_return)
    }
    
    if(input$select_area == "TeamN"){ # this is directorate level, confusingly
      
      if(!isTruthy(input$select_directorate)){
        
        return(trustData %>% 
                 dplyr::filter(Division %in% input$select_division))
      }
      
      to_return <- trustData %>%
        dplyr::filter(Directorate %in% input$select_directorate)
      
      return(to_return)
    }
    
  })
  
  # inputs
  
  reactive_inputs <- reactive({
    
    area_select <- ifelse(input$separate_area, input$select_area, NA)
    
    list(
      "period" = input$period,
      "separate_mode" = input$separate_mode,
      "select_area" = area_select,
      "separate_area" = input$separate_area
    )
  })
  
  mod_summary_server("summary_ui_1", filter_data = filter_data, 
                     reactive_inputs = reactive_inputs)
}
