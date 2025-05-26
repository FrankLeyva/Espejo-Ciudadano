# environment_server.R - Simplified with Data Manager Integration

environmentServer <- function(input, output, session, current_theme = NULL) {
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
    data_manager$get_plots("environment", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("environment", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("environment", selectedYear())
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$env_satisfaction_tabs)
    
    active_tab <- input$env_satisfaction_tabs
    
    tooltip_content <- switch(active_tab,
      "Calidad del Aire" = "<b>ID</b>: PER Q89 <br>
            <b>Pregunta</b>:	¿Qué tan satisfecho está con LA CALIDAD DEL AIRE? <br>
             <b>Escala</b>:  1-10",
      "Arbolado Urbano" = "<b>ID</b>: PER Q90 <br>
            <b>Pregunta</b>:	¿Qué tan satisfecho está con LA CANTIDAD DE ÁRBOLES EN LA CIUDAD? <br>
             <b>Escala</b>:  1-10",
      "Limpieza de Calles" = "<b>ID</b>: PER Q91 <br>
            <b>Pregunta</b>:	¿Qué tan satisfecho está con LA LIMPIEZA EN LAS CALLES (basura en las calles)? <br>
             <b>Escala</b>:  1-10",
      "Calidad del Agua" = "<b>ID</b>: PER Q92 <br>
            <b>Pregunta</b>:	¿Qué tan satisfecho está con LA CALIDAD DEL AGUA? (CONSIDERAR SI ESTÁ SUCIA, CONTAMINADA O TIENE MAL SABOR) <br>
             <b>Escala</b>:  1-10",
      "<b>ID</b>: PER Q89 <br>
            <b>Pregunta</b>:	¿Qué tan satisfecho está con LA CALIDAD DEL AIRE? <br>
             <b>Escala</b>:  1-10"
    )
    
    update_tooltip_content(session, "env_satisfaction_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q89 <br>
            <b>Pregunta</b>:	¿Qué tan satisfecho está con LA CALIDAD DEL AIRE? <br>
             <b>Escala</b>:  1-10"
    
    update_tooltip_content(session, "env_satisfaction_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render outputs using pre-saved plots
  output$env_problems_plot <- renderPlotly({
    plots()$env_problems_plot
  })
  
  output$air_quality_pie <- renderPlotly({
    plots()$air_quality_pie
  })
  
  output$water_quality_pie <- renderPlotly({
    plots()$water_quality_pie
  })
  
  output$tree_coverage_pie <- renderPlotly({
    plots()$tree_coverage_pie
  })
  
  output$cleanliness_pie <- renderPlotly({
    plots()$cleanliness_pie
  })
  
  # Render maps using pre-saved maps
  output$air_quality_map <- renderLeaflet({
    maps()$air_quality_map
  })
  
  output$urban_trees_map <- renderLeaflet({
    maps()$urban_trees_map
  })
  
  output$street_cleanliness_map <- renderLeaflet({
    maps()$street_cleanliness_map
  })
  
  output$water_quality_map <- renderLeaflet({
    maps()$water_quality_map
  })
  
  # Render percentage values for value boxes
  output$air_quality_pct <- renderText({
    percentages()$air_quality_pct
  })
  
  output$water_quality_pct <- renderText({
    percentages()$water_quality_pct
  })
  
  output$tree_coverage_pct <- renderText({
    percentages()$tree_coverage_pct
  })
  
  output$cleanliness_pct <- renderText({
    percentages()$cleanliness_pct
  })
  
  # Download handlers using pre-saved PNG files
  output$download_air_map <- downloadHandler(
    filename = function() {
      paste("mapa_medio_ambiente_Aire_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_medio_ambiente_Aire", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
  
  output$download_water_map <- downloadHandler(
    filename = function() {
      paste("mapa_medio_ambiente_Agua_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_medio_ambiente_Agua", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
  
  output$download_tree_map <- downloadHandler(
    filename = function() {
      paste("mapa_medio_ambiente_Arbolado_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_medio_ambiente_Arbolado", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
  
  output$download_cleanliness_map <- downloadHandler(
    filename = function() {
      paste("mapa_medio_ambiente_Limpieza_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_path <- data_manager$get_map_path("mapa_medio_ambiente_Limpieza", selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
  
  # Combined download handler for environment maps (if you have a single download button that changes based on tabs)
  output$download_environment_map <- downloadHandler(
    filename = function() {
      # Get map type for filename based on active tab
      map_type <- if(input$env_satisfaction_tabs == "Calidad del Aire") { 
        "Aire"
      } else if (input$env_satisfaction_tabs == "Arbolado Urbano") {
        "Arbolado" 
      } else if (input$env_satisfaction_tabs == "Limpieza de Calles") {
        "Limpieza" 
      } else {
        "Agua"
      }
      paste("mapa_medio_ambiente_", map_type, "_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Get the appropriate PNG file based on active tab
      map_filename <- if(input$env_satisfaction_tabs == "Calidad del Aire") {
        "mapa_medio_ambiente_Aire"
      } else if(input$env_satisfaction_tabs == "Arbolado Urbano") {
        "mapa_medio_ambiente_Arbolado"
      } else if(input$env_satisfaction_tabs == "Limpieza de Calles") {
        "mapa_medio_ambiente_Limpieza"
      } else {
        "mapa_medio_ambiente_Agua"
      }
      
      map_path <- data_manager$get_map_path(map_filename, selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
}