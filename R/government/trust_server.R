# trust_server.R - Simplified with Data Manager Integration

trustServer <- function(input, output, session, current_theme = NULL) {
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
    data_manager$get_plots("government", selectedYear())
  })
  
  # Load pre-saved maps (if any)
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("government", selectedYear())
  })
  
  # Load pre-calculated percentages (if any)
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("government", selectedYear())
  })
  
  # Render outputs using pre-saved plots
  output$popular_election_institutions_plot <- renderPlotly({
    plots()$popular_election_institutions_plot
  })
  
  output$public_institutions_media_plot <- renderPlotly({
    plots()$public_institutions_media_plot
  })
  
  output$public_safety_institutions_plot <- renderPlotly({
    plots()$public_safety_institutions_plot
  })
  
  # Note: This module doesn't have maps or value boxes in the current implementation
  # but the structure is maintained for consistency and future expansion
}