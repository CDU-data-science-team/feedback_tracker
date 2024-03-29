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
  
  output$filter_facetsUI <- renderUI({
    
    if(!input$separate_area){
      
      return()
    }
    
    areas_to_include <- na.omit(unique(filter_data()[, input$select_area]))
    
    if(length(areas_to_include) > 5){
      
      showModal(modalDialog(
        title = "Filter applied to areas",
        "Some areas have been removed so they fit on the graph- 
        you may wish to add them back in by clicking in 'Areas to 
        compare' (click anywhere to dismiss this message", 
        easyClose = TRUE
      ))
      
      filtered_areas <- areas_to_include[1 : 5]
    } else {
      
      filtered_areas <- areas_to_include
    }
    
    selectInput("filter_facets", "Areas to compare",
                choices = areas_to_include, 
                multiple = TRUE,
                selected = filtered_areas)
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
      "separate_mode" = input$separate_mode, # BOOLEAN
      "select_area" = area_select,
      "separate_area" = input$separate_area, # BOOLEAN
      "filter_facets" = input$filter_facets
    )
  })
  
  output$download_report <- downloadHandler({
    
    filename = "CustomReport.docx",
    content = function(file){
      
      set_params = list(
        period = input$period, 
        mode = input$separate_mode,
        area = input$select_area
      )
      
      render("table_download.Rmd", output_format = "word_document",
             output_file = "custom_report.docx", quiet = TRUE, params = params,
             envir = new.env(parent = globalenv()))
      
      # copy docx to 'file'
      file.copy("table_download.docx", file, overwrite = TRUE)
    }
  })
  
  mod_summary_server("summary_ui_1", filter_data = filter_data, 
                     reactive_inputs = reactive_inputs)
}
