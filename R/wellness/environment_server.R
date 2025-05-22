# environment_server.R - Updated with Enhanced Data Management

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
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("environment", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Environmental problems plot
    plot_key <- paste0("env_problems_plot_", survey_id)
    plot_list$env_problems_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        create_env_problems_plot(
          survey_data$responses,
          custom_theme = active_theme()
        ) %>% 
          apply_plotly_theme(
            title = "",
            custom_theme = active_theme()
          )
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "environment", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("environment_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    map_list <- list()
    
    # Air quality map
    air_quality_data <- data_manager$get_processed_data(survey_id, "Q89", "interval")
    map_list$air_quality_map <- create_interval_district_map(
      air_quality_data,
      geo_data(),
      use_gradient = FALSE,
      color_scale = "Greens",
      custom_theme = active_theme()
    )
    
    # Urban trees map
    urban_trees_data <- data_manager$get_processed_data(survey_id, "Q90", "interval")
    map_list$urban_trees_map <- create_interval_district_map(
      urban_trees_data,
      geo_data(),
      use_gradient = FALSE,
      color_scale = "Greens",
      custom_theme = active_theme()
    )
    
    # Street cleanliness map
    street_cleanliness_data <- data_manager$get_processed_data(survey_id, "Q91", "interval")
    map_list$street_cleanliness_map <- create_interval_district_map(
      street_cleanliness_data,
      geo_data(),
      use_gradient = FALSE,
      color_scale = "Greens",
      custom_theme = active_theme()
    )
    
    # Water quality map
    water_quality_data <- data_manager$get_processed_data(survey_id, "Q92", "interval")
    map_list$water_quality_map <- create_interval_district_map(
      water_quality_data,
      geo_data(),
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations (if needed)
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("environment_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    
    # Get processed data for environmental metrics
    air_quality_data <- data_manager$get_processed_data(survey_id, "Q89", "interval")
    urban_trees_data <- data_manager$get_processed_data(survey_id, "Q90", "interval")
    street_cleanliness_data <- data_manager$get_processed_data(survey_id, "Q91", "interval")
    water_quality_data <- data_manager$get_processed_data(survey_id, "Q92", "interval")
    
    calc_list <- list()
    
    # Calculate average satisfaction for each environmental aspect
    if (!is.null(air_quality_data)) {
      calc_list$air_quality_avg <- round(mean(air_quality_data$value_num, na.rm = TRUE), 1)
    } else {
      calc_list$air_quality_avg <- "N/A"
    }
    
    if (!is.null(urban_trees_data)) {
      calc_list$urban_trees_avg <- round(mean(urban_trees_data$value_num, na.rm = TRUE), 1)
    } else {
      calc_list$urban_trees_avg <- "N/A"
    }
    
    if (!is.null(street_cleanliness_data)) {
      calc_list$street_cleanliness_avg <- round(mean(street_cleanliness_data$value_num, na.rm = TRUE), 1)
    } else {
      calc_list$street_cleanliness_avg <- "N/A"
    }
    
    if (!is.null(water_quality_data)) {
      calc_list$water_quality_avg <- round(mean(water_quality_data$value_num, na.rm = TRUE), 1)
    } else {
      calc_list$water_quality_avg <- "N/A"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
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
  
  # Render outputs
  output$env_problems_plot <- renderPlotly({
    plots()$env_problems_plot
  })
  
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
  
  # Download handler for environment maps
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
      paste("mapa_medio_ambiente_", map_type, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      # Create the appropriate map based on active tab
      if(input$env_satisfaction_tabs == "Calidad del Aire") {
        map <- maps()$air_quality_map
        title_text <- "Satisfacción con la Calidad del Aire"
      } else if(input$env_satisfaction_tabs == "Arbolado Urbano") {
        map <- maps()$urban_trees_map
        title_text <- "Satisfacción con el Arbolado Urbano"
      } else if(input$env_satisfaction_tabs == "Limpieza de Calles") {
        map <- maps()$street_cleanliness_map
        title_text <- "Satisfacción con la Limpieza de Calles"
      } else {
        map <- maps()$water_quality_map
        title_text <- "Satisfacción con la Calidad del Agua"
      }
      
      # Add title and footer
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      title_text, 
                      "</div>"),
          position = "topright"
        ) %>%
        addControl(
          html = paste("<div style='background-color:white; padding:8px; border-radius:5px; font-size:12px;'>", 
                      paste("Resultados de la Encuesta de Percepción y Participación Ciudadana y Buen Gobierno", selectedYear()),
                      "</div>"),
          position = "bottomright"
        )
      
      # Save and convert
      htmlwidgets::saveWidget(map, tmp_html, selfcontained = TRUE)
      
      pagedown::chrome_print(
        input = tmp_html,
        output = file,
        options = list(printBackground = TRUE, scale = 2.0),
        format = "png",
        browser = "/usr/bin/google-chrome",
        extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
      )
      
      # Clean up
      if (file.exists(tmp_html)) {
        file.remove(tmp_html)
      }
    }
  )
}