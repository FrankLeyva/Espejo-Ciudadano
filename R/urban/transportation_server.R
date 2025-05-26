# transportation_server.R - Simplified with Data Manager Integration

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
  
  # Load pre-saved plots
  plots <- reactive({
    req(selectedYear())
    data_manager$get_plots("transportation", selectedYear())
  })
  
  # Load pre-saved maps
  maps <- reactive({
    req(selectedYear())
    data_manager$get_maps("transportation", selectedYear())
  })
  
  # Load pre-calculated percentages
  percentages <- reactive({
    req(selectedYear())
    data_manager$get_percentages("transportation", selectedYear())
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
  
  # Render outputs using pre-saved plots
  output$bus_issues_plot <- renderPlotly({
    plots()$bus_issues_plot
  })
  
  output$juarez_bus_issues_plot <- renderPlotly({
    plots()$juarez_bus_issues_plot
  })
  
  # Render maps using pre-saved maps
  output$bus_satisfaction_map <- renderLeaflet({
    maps()$bus_satisfaction_map
  })
  
  output$juarez_bus_satisfaction_map <- renderLeaflet({
    maps()$juarez_bus_satisfaction_map
  })
  
  # Download handler using pre-saved PNG files
  output$download_transport_map <- downloadHandler(
    filename = function() {
      # Get map type for filename based on active tab
      map_type <- ifelse(input$transport_tabs == "Camión/Rutera", 
                        "Camion", "Juarez_Bus")
      paste("mapa_transporte_", map_type, "_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      map_name <- if(input$transport_tabs == "Camión/Rutera"){
        "mapa_transporte_Camion"
      } else {
        "mapa_transporte_Juarez_Bus"
      }
      
      map_path <- data_manager$get_map_path(map_name, selectedYear())
      
      if (file.exists(map_path)) {
        file.copy(map_path, file)
      } else {
        warning(paste("Map file not found:", map_path))
        file.create(file)
      }
    }
  )
}