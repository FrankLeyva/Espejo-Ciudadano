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
  
  # Load infrastructure plots for education and healthcare
  infrastructure_plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("infrastructure", selectedYear())
  })
  
  # Load infrastructure maps for education and healthcare
  infrastructure_maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("infrastructure", selectedYear())
  })
  
  # Load infrastructure percentages for education and healthcare
  infrastructure_percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("infrastructure", selectedYear())
  })
  
  # === Original Wellness Outputs ===
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
  
  # === Education and Healthcare Outputs (moved from infrastructure) ===
  # Render Education Map (overview of students)
  output$education_plot <- renderLeaflet({
    infrastructure_maps()$education_map
  })
  
  # Render Healthcare Plot (overview chart)
  output$healthcare_plot <- renderPlotly({
    infrastructure_plots()$healthcare_plot
  })
  
  # Render percentage values for education and healthcare
  output$education_students_pct <- renderText({
    infrastructure_percentages()$education_students_pct
  })
  
  output$healthcare_avg_satisfaction <- renderText({
    infrastructure_percentages()$healthcare_avg_satisfaction
  })
  
  # === Original Wellness Download Handlers ===
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
  
  # === Education and Healthcare Download Handlers (moved from infrastructure) ===
  # Education Map Download Handler
  output$download_gen_students_map <- downloadHandler(
    filename = function() {
      paste("mapa_estudiantes_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_estudiantes", selectedYear())
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
}