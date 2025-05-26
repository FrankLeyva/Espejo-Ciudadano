# urban_server.R - Simplified with Data Manager Integration

urbanServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("movilidad")
    }
  })
  
  # Load pre-saved plots
  plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("urban", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("urban", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("urban", selectedYear())
  })
  
  # Render outputs using pre-saved plots
  output$env_quality_plot <- renderPlotly({
    plots()$env_quality_plot
  })
  
  # Render percentage values for value boxes
  output$public_transport_work <- renderText({
    percentages()$public_transport_work
  })
  
  output$private_vehicle_usage <- renderText({
    percentages()$private_vehicle_usage
  })
  
  output$bus_satisfaction <- renderText({
    percentages()$bus_satisfaction
  })
  
  output$juarez_bus_satisfaction <- renderText({
    percentages()$juarez_bus_satisfaction
  })
}