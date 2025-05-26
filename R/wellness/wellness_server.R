# wellness_server.R - Simplified with Data Manager Integration

wellnessServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("bienestar")
    }
  })
  
  # Load pre-saved plots
  plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("wellness", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("wellness", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("wellness", selectedYear())
  })
  
  # Render outputs using pre-saved plots
  output$economic_situation_pie <- renderPlotly({
    plots()$economic_situation_pie
  })
  
  output$activities_chart <- renderPlotly({
    plots()$activities_chart
  })
  
  # Render maps using pre-saved maps
  output$migration_intention_map <- renderLeaflet({
    maps()$migration_intention_map
  })
  
  output$economic_situation_map <- renderLeaflet({
    maps()$economic_situation_map
  })
  
  # Render percentage values for value boxes
  output$migration_intention_pct <- renderText({
    percentages()$migration_intention_pct
  })
  
  output$economic_situation_pct <- renderText({
    percentages()$economic_situation_pct
  })
  
  output$cultural_participation_pct <- renderText({
    percentages()$cultural_participation_pct
  })
  
  # Download handlers using pre-saved PNG files
  output$download_migration_map <- downloadHandler(
    filename = function() {
      paste("mapa_migracion_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_migracion", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
  
  output$download_economic_map <- downloadHandler(
    filename = function() {
      paste("mapa_economia_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_economia", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
}