# transportation_server.R - Updated with Enhanced Data Management

transportationServer <- function(input, output, session, current_theme = NULL) {
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
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("transportation", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Bus issues plot
    plot_key <- paste0("bus_issues_plot_", survey_id)
    plot_list$bus_issues_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        create_transport_issues_plot(
          survey_data$responses,
          issue_type = "bus",
          custom_theme = active_theme()
        ) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Juarez Bus issues plot
    plot_key <- paste0("juarez_bus_issues_plot_", survey_id)
    plot_list$juarez_bus_issues_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        create_transport_issues_plot(
          survey_data$responses,
          issue_type = "juarez_bus",
          custom_theme = active_theme()
        ) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "transportation", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("transportation_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    map_list <- list()
    
    # Bus satisfaction map
    bus_satisfaction <- data_manager$get_processed_data(survey_id, "Q75", "interval")
    map_list$bus_satisfaction_map <- create_interval_district_map(
      bus_satisfaction,
      geo_data(),
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
    
    # Juarez Bus satisfaction map
    juarez_bus_satisfaction <- data_manager$get_processed_data(survey_id, "Q78", "interval")
    map_list$juarez_bus_satisfaction_map <- create_interval_district_map(
      juarez_bus_satisfaction,
      geo_data(),
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$transport_tabs)
    
    active_tab <- input$transport_tabs
    
    tooltip_content <- switch(active_tab,
      "Camión/Rutera" = "<b>ID</b>: PER Q75 <br>
            <b>Pregunta</b>:	En una escala del 1 al 10, que tan satisfecho está con la calidad del servicio del camión/rutera? <br>
             <b>Escala</b>:  1-10",
      "Juárez Bus" = "<b>ID</b>: PER Q78 <br>
            <b>Pregunta</b>:	En una escala del 1 al 10, que tan satisfecho está con la calidad del servicio del BravoBus/EcoBus/ViveBus/Juarez Bus)?<br>
             <b>Escala</b>:  1-10",
      "<b>ID</b>: PER Q75 <br>
            <b>Pregunta</b>:	En una escala del 1 al 10, que tan satisfecho está con la calidad del servicio del camión/rutera? <br>
             <b>Escala</b>:  1-10"
    )
    
    update_tooltip_content(session, "trans_satisfaction_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q75 <br>
            <b>Pregunta</b>:	En una escala del 1 al 10, que tan satisfecho está con la calidad del servicio del camión/rutera? <br>
             <b>Escala</b>:  1-10"
    
    update_tooltip_content(session, "trans_satisfaction_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Update tooltip content based on selected tab for service issues
  observe({
    req(input$service_issues_tabs)
    
    active_tab <- input$service_issues_tabs
    
    tooltip_content <- switch(active_tab,
      "Camión/Rutera" = "<b>ID</b>: PER Q76.1: Q76.6 <br>
            <b>Pregunta</b>:		Tiempo de espera de los camiones / Estado de la unidad / Estado de la parada de camión	/ Trato de los choferes	 / 	Conducción de la unidad por parte del chofer / Tarifa <br>
             <b>Escala</b>:  1=Sí; 2=No",
      "Juárez Bus" = "<b>ID</b>: PER PER Q79.1: Q79.6 <br>
            <b>Pregunta</b>:		Tiempo de espera de los camiones / Estado de la unidad / Estado de la parada de camión	/ Trato de los choferes	 / 	Conducción de la unidad por parte del chofer / Tarifa <br>
             <b>Escala</b>:  1=Sí; 2=No",
      "<b>ID</b>: PER Q76.1: Q76.6 <br>
            <b>Pregunta</b>:		Tiempo de espera de los camiones / Estado de la unidad / Estado de la parada de camión	/ Trato de los choferes	 / 	Conducción de la unidad por parte del chofer / Tarifa <br>
             <b>Escala</b>:  1=Sí; 2=No"
    )
    
    update_tooltip_content(session, "trans_dissatisfaction_tooltip", tooltip_content)
  })
  
  # Set initial tooltip for service issues
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q76.1: Q76.6 <br>
            <b>Pregunta</b>:		Tiempo de espera de los camiones / Estado de la unidad / Estado de la parada de camión	/ Trato de los choferes	 / 	Conducción de la unidad por parte del chofer / Tarifa <br>
             <b>Escala</b>:  1=Sí; 2=No"
    
    update_tooltip_content(session, "trans_dissatisfaction_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render outputs
  output$bus_issues_plot <- renderPlotly({
    plots()$bus_issues_plot
  })
  
  output$juarez_bus_issues_plot <- renderPlotly({
    plots()$juarez_bus_issues_plot
  })
  
  output$bus_satisfaction_map <- renderLeaflet({
    maps()$bus_satisfaction_map
  })
  
  output$juarez_bus_satisfaction_map <- renderLeaflet({
    maps()$juarez_bus_satisfaction_map
  })
  
  # Download handler for transport maps
  output$download_transport_map <- downloadHandler(
    filename = function() {
      # Get map type for filename based on active tab
      map_type <- ifelse(input$transport_tabs == "Camión/Rutera", 
                        "Camion", "Juarez_Bus")
      paste("mapa_transporte_", map_type, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      # Select the appropriate map based on active tab
      if(input$transport_tabs == "Camión/Rutera") {
        map <- maps()$bus_satisfaction_map
        title_text <- "Satisfacción con el Servicio de Camión/Rutera"
      } else {
        map <- maps()$juarez_bus_satisfaction_map
        title_text <- "Satisfacción con el Servicio de Juárez Bus"
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