# public_services_server.R - Simplified with Data Manager Integration

publicServicesServer <- function(input, output, session, current_theme = NULL) {
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
  
  # Service mapping for UI elements
  service_mapping <- c(
    "Q29" = "Agua",
    "Q30" = "Drenaje y Alcantarillado", 
    "Q35" = "CFE",
    "Q40" = "Recolección de Basura",
    "Q45" = "Alumbrado Público",
    "Q51" = "Calles y Pavimentación",
    "Q55" = "Semaforización",
    "Q56" = "Áreas verdes y Espacios públicos",
    "Q58" = "Unidades deportivas",
    "Q59" = "Bibliotecas",
    "Q60" = "Centros Comunitarios",
    "Q61" = "Banquetas", 
    "Q62" = "Espacios para personas con discapacidad"
  )
  
  # Service titles for maps and displays
  service_titles <- c(
    "Q29" = "Satisfacción con los servicios de agua",
    "Q30" = "Satisfacción con los servicios de drenaje y alcantarillado",
    "Q35" = "Satisfacción con los servicios de CFE",
    "Q40" = "Satisfacción con la recolección de basura",
    "Q45" = "Satisfacción con el alumbrado público",
    "Q51" = "Satisfacción con calles y pavimentación",
    "Q55" = "Satisfacción con semaforización y señales viales",
    "Q56" = "Satisfacción con áreas verdes y espacios públicos",
    "Q58" = "Satisfacción con unidades deportivas",
    "Q59" = "Satisfacción con bibliotecas",
    "Q60" = "Satisfacción con centros comunitarios",
    "Q61" = "Satisfacción con banquetas",
    "Q62" = "Satisfacción con espacios para personas con discapacidad"
  )
  
  # Update tooltip content based on selected service
  observe({
    req(input$selected_service)
    active_tab <- service_mapping[input$selected_service]
    
    tooltip_content <- switch(active_tab,
      "Agua" = "<b>ID</b>: PER Q29 <br>
            <b>Pregunta</b>: Que tan satisfecho esta con el SERVICIO DEL AGUA? <br>
             <b>Escala</b>: 1-10",
      "Drenaje y Alcantarillado" = "<b>ID</b>: PER Q30 <br>
            <b>Pregunta</b>: que tan satisfecho está con el SERVICIO DE DRENAJE Y ALCANTARILLADO? <br>
             <b>Escala</b>: 1-10",
      "CFE" = "<b>ID</b>: PER Q35 <br>
            <b>Pregunta</b>: Que tan satisfecho esta con el SERVICIO DE Comision Federal de Electricidad (CFE)? <br>
             <b>Escala</b>: 1-10",
      "Recolección de Basura" = "<b>ID</b>: PER Q40 <br>
            <b>Pregunta</b>: Que tan satisfecho esta con el SERVICIO DE RECOLECCION DE BASURA? <br>
             <b>Escala</b>: 1-10",
      "Alumbrado Público" = "<b>ID</b>: PER Q45 <br>
            <b>Pregunta</b>: Que tan satisfecho se siente con la calidad del servicio de alumbrado público en la CIUDAD? <br>
             <b>Escala</b>: 1-10",
      "Calles y Pavimentación" = "<b>ID</b>: PER Q51 <br>
            <b>Pregunta</b>: Que tan satisfecho se siente con la calidad de las calles y la pavimentación? <br>
             <b>Escala</b>: 1-10",
      "Semaforización" = "<b>ID</b>: PER Q55 <br>
            <b>Pregunta</b>: Que tan satisfecho esta con LA SEMAFORIZACION y señales viales? <br>
             <b>Escala</b>: 1-10",
      "Áreas verdes y Espacios públicos" = "<b>ID</b>: PER Q56 <br>
            <b>Pregunta</b>: Que tan satisfecho está con LA DISPONIBILIDAD DE ÁREAS VERDES Y ESPACIOS PÚBLICOS CERCA DE SU CASA? <br>
             <b>Escala</b>: 1-10",
      "Unidades deportivas" = "<b>ID</b>: PER Q58 <br>
            <b>Pregunta</b>: En que estado considera que se encuentran las UNIDADES DEPORTIVAS? <br>
             <b>Escala</b>: 1-10",
      "Bibliotecas" = "<b>ID</b>: PER Q59 <br>
            <b>Pregunta</b>: En qué estado considera que se encuentran las BIBLIOTECAS? <br>
             <b>Escala</b>: 1-10",
      "Centros Comunitarios" = "<b>ID</b>: PER Q60 <br>
            <b>Pregunta</b>: En que estado considera que se encuentran las CENTROS COMUNITARIOS? <br>
             <b>Escala</b>: 1-10",
      "Espacios para personas con discapacidad" = "<b>ID</b>: PER Q62 <br>
            <b>Pregunta</b>: En que estado considera que se encuentran los ESPACIOS PARA PERSONAS CON DISCAPACIDAD? <br>
             <b>Escala</b>: 1-10",
      "Banquetas" = "<b>ID</b>: PER Q61 <br>
            <b>Pregunta</b>: En qué estado considera que se encuentran las BANQUETAS? <br>
             <b>Escala</b>: 1-10",
      "<b>ID</b>: PER Q29 <br>
            <b>Pregunta</b>: Que tan satisfecho esta con el SERVICIO DEL AGUA? <br>
             <b>Escala</b>: 1-10"
    )
    
    update_tooltip_content(session, "utilities_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q29 <br>
            <b>Pregunta</b>: Que tan satisfecho esta con el SERVICIO DEL AGUA? <br>
             <b>Escala</b>: 1-10"
    
    update_tooltip_content(session, "utilities_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Service-specific map output based on selected service
  output$service_map <- renderLeaflet({
    req(input$selected_service)
    
    # Map the service to the pre-saved map
    service_map_name <- switch(input$selected_service,
      "Q29" = "service_Q29_map",
      "Q30" = "service_Q30_map", 
      "Q35" = "service_Q35_map",
      "Q40" = "service_Q40_map",
      "Q45" = "service_Q45_map",
      "Q51" = "service_Q51_map",
      "Q55" = "service_Q55_map",
      "Q56" = "service_Q56_map",
      "Q58" = "service_Q58_map",
      "Q59" = "service_Q59_map",
      "Q60" = "service_Q60_map",
      "Q61" = "service_Q61_map",
      "Q62" = "service_Q62_map",
      "service_Q29_map"  # default
    )
    
    maps()[[service_map_name]]
  })
  
  # Render the report statistics plot (if it exists as a pre-saved plot)
  output$report_statistics_plot <- renderUI({
    if (!is.null(plots()$report_statistics_plot)) {
      plots()$report_statistics_plot
    } else {
      div("Estadísticas de reporte no disponibles")
    }
  })
  
  # Output service title
  output$service_title <- renderText({
    req(input$selected_service)
    
    title <- service_titles[input$selected_service]
    if (is.na(title)) title <- "Evaluación de servicios por distrito"
    
    return(title)
  })
  
  # Render percentage values for value boxes
  output$water_days <- renderText({
    percentages()$water_days
  })
  
  output$power_outages <- renderText({
    percentages()$power_outages
  })
  
  output$trash_pickup <- renderText({
    percentages()$trash_pickup
  })
  
  output$green_areas_equipment <- renderText({
    percentages()$green_areas_equipment
  })
  
  output$green_areas_lighting <- renderText({
    percentages()$green_areas_lighting
  })
  
  output$green_areas_maintenance <- renderText({
    percentages()$green_areas_maintenance
  })
  
  output$green_areas_security <- renderText({
    percentages()$green_areas_security
  })
  
  # Download data handler - simplified to use pre-saved data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("servicios_publicos_datos_", selectedYear(), "_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Use pre-saved data file if available
      data_path <- data_manager$get_data_path("servicios_publicos", selectedYear(), "xlsx")
      
      if (file.exists(data_path)) {
        file.copy(data_path, file)
      } else {
        warning(paste("Data file not found:", data_path))
        file.create(file)
      }
    }
  )
  
  # Download service map handler using pre-saved PNG files
  output$download_service_map <- downloadHandler(
    filename = function() {
      # Get service name for filename using the mapping from the key table
      service_filename_mapping <- c(
        "Q29" = "Agua",
        "Q30" = "Drenaje_y_Alcantarillado",
        "Q35" = "CFE", 
        "Q40" = "Recoleccion_Basura",
        "Q45" = "Alumbrado_Publico",
        "Q51" = "Calles_y_Pavimentacion",
        "Q55" = "Semaforizacion",
        "Q56" = "Areas_Verdes",
        "Q58" = "Unidades_Deportivas",
        "Q59" = "Bibliotecas",
        "Q60" = "Centros_Comunitarios",
        "Q61" = "Banquetas",
        "Q62" = "Espacios_Discapacidad"
      )
      
      service_name <- service_filename_mapping[input$selected_service]
      if (is.na(service_name)) service_name <- "Servicio"
      
      paste("mapa_servicio_", service_name, "_", selectedYear(), "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Determine map name based on selected service using the PNG file naming convention
      service_filename_mapping <- c(
        "Q29" = "mapa_servicio_Agua",
        "Q30" = "mapa_servicio_Drenaje_y_Alcantarillado",
        "Q35" = "mapa_servicio_CFE",
        "Q40" = "mapa_servicio_Recoleccion_Basura", 
        "Q45" = "mapa_servicio_Alumbrado_Publico",
        "Q51" = "mapa_servicio_Calles_y_Pavimentacion",
        "Q55" = "mapa_servicio_Semaforizacion",
        "Q56" = "mapa_servicio_Areas_Verdes",
        "Q58" = "mapa_servicio_Unidades_Deportivas",
        "Q59" = "mapa_servicio_Bibliotecas",
        "Q60" = "mapa_servicio_Centros_Comunitarios",
        "Q61" = "mapa_servicio_Banquetas",
        "Q62" = "mapa_servicio_Espacios_Discapacidad"
      )
      
      map_name <- service_filename_mapping[input$selected_service]
      if (is.na(map_name)) map_name <- "mapa_servicio_Agua"
      
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