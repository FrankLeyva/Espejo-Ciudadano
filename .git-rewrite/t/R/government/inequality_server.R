# inequality_server.R - Simplified with Data Manager Integration

inequalityServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("gobierno")
    }
  })
  
  # Load pre-saved plots
  plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("inequality", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("inequality", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("inequality", selectedYear())
  })
  
  # Render outputs using pre-saved plots
  output$inequality_reduction_plot <- renderPlotly({
    plots()$inequality_reduction_plot
  })
  
  # Render maps using pre-saved maps
  output$rights_violation_map <- renderLeaflet({
    maps()$rights_violation_map
  })
  
  output$inequality_perception_map <- renderLeaflet({
    maps()$inequality_perception_map
  })
  
  # Render percentage values for value boxes
  output$rights_violation_pct <- renderText({
    percentages()$rights_violation_pct
  })
  
  output$inequality_perception_mean <- renderText({
    percentages()$inequality_perception_mean
  })
  
  # Download handler for Rights Violation map using pre-saved PNG
  output$download_rights_violation_map <- downloadHandler(
    filename = function() {
      paste("mapa_derechos_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_derechos", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
  
  # Download handler for Inequality Perception map using pre-saved PNG
  output$download_inequality_map <- downloadHandler(
    filename = function() {
      paste("mapa_desigualdad_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_desigualdad", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
}