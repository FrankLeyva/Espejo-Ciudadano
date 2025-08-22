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
    "Q40" = "Recolección de Basura"

  )
  
  # Service titles for maps and displays
  service_titles <- c(
    "Q29" = "Satisfacción con los servicios de agua",
    "Q30" = "Satisfacción con los servicios de drenaje y alcantarillado",
    "Q35" = "Satisfacción con los servicios de CFE",
    "Q40" = "Satisfacción con la recolección de basura"
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
      "service_Q29_map"  # default
    )
    
    maps()[[service_map_name]]
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
        "Q40" = "Recoleccion_Basura"
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
        "Q40" = "mapa_servicio_Recoleccion_Basura"
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