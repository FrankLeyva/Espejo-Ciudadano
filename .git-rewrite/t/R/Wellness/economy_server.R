# economy_server.R - Simplified with Data Manager Integration

economyServer <- function(input, output, session, current_theme = NULL) {
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
    data_manager$get_plots("economic", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("economic", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("economic", selectedYear())
  })
  
  # Render outputs using pre-saved plots
  output$income_situation_pie <- renderPlotly({
    plots()$income_situation_pie
  })
  
  output$economic_situation_pie <- renderPlotly({
    plots()$economic_situation_pie
  })
  
  output$economic_expectations_plot <- renderPlotly({
    plots()$economic_expectations_plot
  })
  
  # Render maps using pre-saved maps
  output$economic_improvement_map <- renderLeaflet({
    maps()$economic_improvement_map
  })
  
  output$income_situation_map <- renderLeaflet({
    maps()$income_situation_map
  })
  
  # Render percentage values for value boxes
  output$income_sufficiency_text <- renderText({
    percentages()$income_sufficiency_text
  })
  
  output$savings_capability_text <- renderText({
    percentages()$savings_capability_text
  })
  
  output$economic_improvement_pct <- renderText({
    percentages()$economic_improvement_pct
  })
  
  output$income_satisfaction_pct <- renderText({
    percentages()$income_satisfaction_pct
  })
  
  # Download handler for economy map using pre-saved PNG file
  output$download_economy_map <- downloadHandler(
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