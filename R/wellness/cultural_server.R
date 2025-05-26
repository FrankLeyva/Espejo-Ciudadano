# cultural_server.R - Simplified with Data Manager Integration

culturalServer <- function(input, output, session, current_theme = NULL) {
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
    data_manager$get_plots("cultural", selectedYear())
  })
  
  # Load pre-saved maps (if any)
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("cultural", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("cultural", selectedYear())
  })
  
  # Render outputs using pre-saved plots
  output$cultural_activities_plot <- renderPlotly({
    plots()$cultural_activities_plot
  })
  
  output$entertainment_activities_plot <- renderPlotly({
    plots()$entertainment_activities_plot
  })
  
  # Render maps if they exist
  output$cultural_participation_map <- renderLeaflet({
    if (!is.null(maps()$cultural_participation_map)) {
      maps()$cultural_participation_map
    }
  })
  
  # Render percentage values for value boxes
  output$home_activities_pct <- renderText({
    percentages()$home_activities_pct
  })
  
  output$exercise_activities_pct <- renderText({
    percentages()$exercise_activities_pct
  })
  
  output$nature_activities_pct <- renderText({
    percentages()$nature_activities_pct
  })
  
  output$cultural_participation_pct <- renderText({
    percentages()$cultural_participation_pct
  })
  
  output$entertainment_participation_pct <- renderText({
    percentages()$entertainment_participation_pct
  })
  
  # Download handlers (if maps exist)
  output$download_cultural_map <- downloadHandler(
    filename = function() {
      paste("mapa_participacion_cultural_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Note: Cultural section might not have map files, check if they exist
      map_path <- data_manager$get_map_path("mapa_participacion_cultural", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Cultural map file not found:", map_path))
        file.create(file)
      }
    }
  )
}