# healthcare_server.R - Simplified with Data Manager Integration

healthcareServer <- function(input, output, session, current_theme = NULL) {
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
  
  # Update tooltip content based on selected tab
  observe({
    req(input$healthcare_tabs)
    
    active_tab <- input$healthcare_tabs
    
    tooltip_content <- switch(active_tab,
      "Satisfacción General" = "<b>ID</b>: PER Q15 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con los servicios de salud en general? <br>
             <b>Escala</b>: 1-10",
      "Tiempo de Espera" = "<b>ID</b>: PER Q16 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con el tiempo de espera? <br>
             <b>Escala</b>: 1-10",
      "Instalaciones" = "<b>ID</b>: PER Q17 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con las instalaciones? <br>
             <b>Escala</b>: 1-10",
      "Disponibilidad de Medicamentos" = "<b>ID</b>: PER Q18 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con la disponibilidad de medicamentos? <br>
             <b>Escala</b>: 1-10",
      "Calidad del Servicio" = "<b>ID</b>: PER Q19 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con la calidad del servicio? <br>
             <b>Escala</b>: 1-10",
      "Distancia" = "<b>ID</b>: PER Q20 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con la distancia? <br>
             <b>Escala</b>: 1-10",
      "<b>ID</b>: PER Q15 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con los servicios de salud en general? <br>
             <b>Escala</b>: 1-10"
    )
    
    update_tooltip_content(session, "healthcare_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q15 <br>
            <b>Pregunta</b>: ¿Qué tan satisfecho está con los servicios de salud en general? <br>
             <b>Escala</b>: 1-10"
    
    update_tooltip_content(session, "healthcare_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render outputs using pre-saved plots
  output$healthcare_satisfaction_plot <- renderPlotly({
    plots()$healthcare_satisfaction_plot
  })
  
  output$healthcare_aspects_plot <- renderPlotly({
    plots()$healthcare_aspects_plot
  })
  
  # Render maps using pre-saved maps
  output$healthcare_general_map <- renderLeaflet({
    maps()$healthcare_general_map
  })
  
  output$healthcare_tiempo_map <- renderLeaflet({
    maps()$healthcare_tiempo_map
  })
  
  output$healthcare_instalaciones_map <- renderLeaflet({
    maps()$healthcare_instalaciones_map
  })
  
  output$healthcare_medicamentos_map <- renderLeaflet({
    maps()$healthcare_medicamentos_map
  })
  
  output$healthcare_servicio_map <- renderLeaflet({
    maps()$healthcare_servicio_map
  })
  
  output$healthcare_distancia_map <- renderLeaflet({
    maps()$healthcare_distancia_map
  })
  
  # Render percentage values for value boxes
  output$healthcare_general_avg <- renderText({
    percentages()$healthcare_general_avg
  })
  
  output$healthcare_tiempo_avg <- renderText({
    percentages()$healthcare_tiempo_avg
  })
  
  output$healthcare_instalaciones_avg <- renderText({
    percentages()$healthcare_instalaciones_avg
  })
  
  output$healthcare_medicamentos_avg <- renderText({
    percentages()$healthcare_medicamentos_avg
  })
  
  output$healthcare_servicio_avg <- renderText({
    percentages()$healthcare_servicio_avg
  })
  
  output$healthcare_distancia_avg <- renderText({
    percentages()$healthcare_distancia_avg
  })
  
  # Download handler using pre-saved PNG files
  output$download_healthcare_map <- downloadHandler(
    filename = function() {
      map_type <- switch(input$healthcare_tabs,
        "Satisfacción General" = "General",
        "Tiempo de Espera" = "Tiempo",
        "Instalaciones" = "Instalaciones",
        "Disponibilidad de Medicamentos" = "Medicamentos",
        "Calidad del Servicio" = "Servicio",
        "Distancia" = "Distancia",
        "General"
      )
      
      paste("mapa_serv_salud_", map_type, "_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Determine map name based on active tab
      map_name <- switch(input$healthcare_tabs,
        "Satisfacción General" = "mapa_serv_salud_General",
        "Tiempo de Espera" = "mapa_serv_salud_Tiempo",
        "Instalaciones" = "mapa_serv_salud_Instalaciones",
        "Disponibilidad de Medicamentos" = "mapa_serv_salud_Medicamentos",
        "Calidad del Servicio" = "mapa_serv_salud_Servicio",
        "Distancia" = "mapa_serv_salud_Distancia",
        "mapa_serv_salud_General"
      )
      
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