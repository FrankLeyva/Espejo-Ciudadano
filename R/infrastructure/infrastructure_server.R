# infrastructure_server.R - Simplified with Data Manager Integration
# Education and Healthcare moved to wellness

infrastructureServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("infraestructura")
    }
  })
  
  # Load pre-saved plots
  plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("infrastructure", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("infrastructure", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("infrastructure", selectedYear())
  })
  
  # === Public Services and Housing Outputs ===
  # Render Utilities Plot (overview chart)
  output$utilities_plot <- renderPlotly({
    plots()$utilities_plot
  })
  
  # Render Housing Map (overview)
  output$housing_map <- renderLeaflet({
    maps()$housing_map
  })
  
  # Render percentage values for value boxes
  output$utilities_avg_satisfaction <- renderText({
    percentages()$utilities_avg_satisfaction
  })
  
  output$housing_avg_satisfaction <- renderText({
    percentages()$housing_avg_satisfaction
  })
  
  # === Download Handlers ===
  # Housing Map Download Handler  
  output$download_housing_map <- downloadHandler(
    filename = function() {
      paste("mapa_viviendas_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_viviendas", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
   # Render the report statistics plot (if it exists as a pre-saved plot)
  output$report_statistics_plot <- renderUI({
    if (!is.null(plots()$report_statistics_plot)) {
      plots()$report_statistics_plot
    } else {
      div("Estadísticas de reporte no disponibles")
    }
  })
}