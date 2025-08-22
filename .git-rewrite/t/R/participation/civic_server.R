# civic_server.R - Simplified with Data Manager Integration

civicServer <- function(input, output, session, current_theme = NULL) {
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
    data_manager$get_plots("civic", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("civic", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("civic", selectedYear())
  })
  
  # Render outputs using pre-saved plots
  output$mechanisms_plot <- renderPlotly({
    plots()$mechanisms_plot
  })
  
  output$requirements_plot <- renderPlotly({
    plots()$requirements_plot
  })
  
  # Render maps using pre-saved maps
  output$interest_map <- renderLeaflet({
    maps()$interest_map
  })
  
  # Download handler using pre-saved PNG files
  output$download_interest_map <- downloadHandler(
    filename = function() {
      paste("mapa_interes_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_interes", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
}