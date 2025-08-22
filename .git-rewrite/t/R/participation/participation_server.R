# participation_server.R - Simplified with Data Manager Integration

participationServer <- function(input, output, session, current_theme = NULL) {
  # Get dependencies from userData
  selectedYear <- session$userData$selectedYear
  data_manager <- session$userData$data_manager
  geo_data <- session$userData$geoData
  
  active_theme <- reactive({
    if (is.function(current_theme)) {
      current_theme()
    } else if (!is.null(current_theme)) {
      current_theme
    } else {
      get_section_theme("participacion")
    }
  })
  
  # Load pre-saved plots
  plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("participation", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("participation", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("participation", selectedYear())
  })
  
  # Render outputs using pre-saved plots
  output$interest_pie <- renderPlotly({
    plots()$interest_pie
  })
  
  # Render maps using pre-saved maps
  output$voting_map <- renderLeaflet({
    maps()$voting_map
  })
  
  # Render percentage values for value boxes
  output$social_movement_support <- renderText({
    percentages()$social_movement_support
  })
  
  # Download handler using pre-saved PNG files
  output$download_voting_map <- downloadHandler(
    filename = function() {
      paste("mapa_voto_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_voto", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
}