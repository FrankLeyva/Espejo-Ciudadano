# public_services_server.R - Updated with Enhanced Data Management

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
  
  # Service mapping for UI elements
  service_mapping <- c(
    "Q29" = "Agua",
    "Q30" = "Drenaje y Alcantarillado",
    "Q35" = "Comisión Federal de Electricidad",
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
            <b>Pregunta</b>:	 Que tan satisfecho esta con el SERVICIO DEL AGUA? <br>
             <b>Escala</b>:  1-10",
      "Drenaje y Alcantarillado" = "<b>ID</b>: PER Q30 <br>
            <b>Pregunta</b>:	 que tan satisfecho está con el SERVICIO DE DRENAJE Y ALCANTARILLADO? <br>
             <b>Escala</b>:  1-10",
      "Comisión Federal de Electricidad" = "<b>ID</b>: PER Q35 <br>
            <b>Pregunta</b>:	 Que tan satisfecho esta con el SERVICIO DE Comision Federal de Electricidad (CFE)? <br>
             <b>Escala</b>:  1-10",
      "Recolección de Basura" = "<b>ID</b>: PER Q40 <br>
            <b>Pregunta</b>:	 Que tan satisfecho esta con el SERVICIO DE RECOLECCION DE BASURA? <br>
             <b>Escala</b>:  1-10",
      "Alumbrado Público" = "<b>ID</b>: PER Q45 <br>
            <b>Pregunta</b>:	 Que tan satisfecho se siente con la calidad del servicio de alumbrado público en la CIUDAD? <br>
             <b>Escala</b>:  1-10",
      "Calles y Pavimentación" = "<b>ID</b>: PER Q51 <br>
            <b>Pregunta</b>:	  Que tan satisfecho se siente con la calidad de las calles y la pavimentación? <br>
             <b>Escala</b>:  1-10",
      "Semaforización" = "<b>ID</b>: PER Q55 <br>
            <b>Pregunta</b>:	 Que tan satisfecho esta con LA SEMAFORIZACION y señales viales? <br>
             <b>Escala</b>:  1-10",
      "Áreas verdes y Espacios públicos" = "<b>ID</b>: PER Q56 <br>
            <b>Pregunta</b>:	 Que tan satisfecho está con LA DISPONIBILIDAD DE ÁREAS VERDES Y ESPACIOS PÚBLICOS CERCA DE SU CASA? <br>
             <b>Escala</b>:  1-10",
      "Unidades deportivas" = "<b>ID</b>: PER Q58 <br>
            <b>Pregunta</b>:	  En que estado considera que se encuentran las UNIDADES DEPORTIVAS? <br>
             <b>Escala</b>:  1-10",
      "Bibliotecas" = "<b>ID</b>: PER Q59 <br>
            <b>Pregunta</b>:	 En qué estado considera que se encuentran las BIBLIOTECAS? <br>
             <b>Escala</b>:  1-10",
      "Centros comunitarios" = "<b>ID</b>: PER Q60 <br>
            <b>Pregunta</b>:	  En que estado considera que se encuentran las CENTROS COMUNITARIOS? <br>
             <b>Escala</b>:  1-10",
      "Espacios para personas con discapacidad" = "<b>ID</b>: PER Q62 <br>
            <b>Pregunta</b>:	 En que estado considera que se encuentran los ESPACIOS PARA PERSONAS CON DISCAPACIDAD? <br>
             <b>Escala</b>:  1-10",
      "Banquetas" = "<b>ID</b>: PER Q61 <br>
            <b>Pregunta</b>:	 En qué estado considera que se encuentran las BANQUETAS? <br>
             <b>Escala</b>:  1-10",
      "<b>ID</b>: PER Q29 <br>
            <b>Pregunta</b>:	 Que tan satisfecho esta con el SERVICIO DEL AGUA? <br>
             <b>Escala</b>:  1-10"
    )
    
    update_tooltip_content(session, "utilities_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q89 <br>
            <b>Pregunta</b>:	¿Qué tan satisfecho está con LA CALIDAD DEL AIRE? <br>
             <b>Escala</b>:  1-10"
    
    update_tooltip_content(session, "utilities_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Try to load pre-saved maps first, then create if needed
  maps <- reactive({
    req(selectedYear(), input$selected_service, geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("public_services_maps_", selectedYear())
    
    # We'll just check if the cache exists, but we'll reload specific maps as needed
    # since they depend on the selected_service which can change
    if (!is.null(data_manager$cache[[map_cache_key]]) && 
        !is.null(data_manager$cache[[map_cache_key]][[input$selected_service]])) {
      return(data_manager$cache[[map_cache_key]][[input$selected_service]])
    }
    
    # If we don't have a cached map for this service, create it
    survey_id <- paste0("PER_", selectedYear())
    
    # Get processed data for the selected service
    prepared_data <- data_manager$get_processed_data(
      survey_id = survey_id,
      question_id = input$selected_service,
      data_type = "interval"
    )
    
    # Create the map
    service_map <- create_interval_district_map(
      data = prepared_data,
      geo_data = geo_data(),
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
    
    # Make sure the cache is initialized
    if (is.null(data_manager$cache[[map_cache_key]])) {
      data_manager$cache[[map_cache_key]] <- list()
    }
    
    # Cache this specific map
    data_manager$cache[[map_cache_key]][[input$selected_service]] <- service_map
    
    return(service_map)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("public_services_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    survey_data <- data_manager$get_survey_data(survey_id)
    
    calc_list <- list()
    
    # Water days calculation
    water_data <- as.numeric(survey_data$responses$Q31)
    water_data <- water_data[!is.na(water_data)]
    if (length(water_data) > 0) {
      calc_list$water_days <- sprintf("%.1f días", mean(water_data, na.rm = TRUE))
    } else {
      calc_list$water_days <- "Datos no disponibles"
    }
    
    # Power outages calculation
    outage_data <- survey_data$responses$Q36
    outage_data <- outage_data[!is.na(outage_data)]
    if (length(outage_data) > 0) {
      outage_mapping <- c(
        "1" = "Casi todos los días",
        "2" = "Al menos una vez a la semana",
        "3" = "Al menos una vez al mes",
        "4" = "Solo hubo un apagón",
        "5" = "No hubo apagones"
      )
      freq_table <- table(outage_data)
      most_common <- names(freq_table)[which.max(freq_table)]
      if (most_common %in% names(outage_mapping)) {
        calc_list$power_outages <- outage_mapping[most_common]
      } else {
        calc_list$power_outages <- "Dato más común"
      }
    } else {
      calc_list$power_outages <- "Datos no disponibles"
    }
    
    # Trash pickup calculation
    trash_data <- as.numeric(survey_data$responses$Q41)
    trash_data <- trash_data[!is.na(trash_data)]
    if (length(trash_data) > 0) {
      calc_list$trash_pickup <- sprintf("%.1f días", mean(trash_data, na.rm = TRUE))
    } else {
      calc_list$trash_pickup <- "Datos no disponibles"
    }
    
    # Green areas ratings
    # Equipment
    equipment_values <- as.numeric(survey_data$responses$Q57.1)
    equipment_values <- equipment_values[!is.na(equipment_values)]
    if (length(equipment_values) > 0) {
      calc_list$green_areas_equipment <- sprintf("%.1f / 10", mean(equipment_values, na.rm = TRUE))
    } else {
      calc_list$green_areas_equipment <- "No disponible"
    }
    
    # Lighting
    lighting_values <- as.numeric(survey_data$responses$Q57.2)
    lighting_values <- lighting_values[!is.na(lighting_values)]
    if (length(lighting_values) > 0) {
      calc_list$green_areas_lighting <- sprintf("%.1f / 10", mean(lighting_values, na.rm = TRUE))
    } else {
      calc_list$green_areas_lighting <- "No disponible"
    }
    
    # Maintenance
    maintenance_values <- as.numeric(survey_data$responses$Q57.3)
    maintenance_values <- maintenance_values[!is.na(maintenance_values)]
    if (length(maintenance_values) > 0) {
      calc_list$green_areas_maintenance <- sprintf("%.1f / 10", mean(maintenance_values, na.rm = TRUE))
    } else {
      calc_list$green_areas_maintenance <- "No disponible"
    }
    
    # Security
    security_values <- as.numeric(survey_data$responses$Q57.4)
    security_values <- security_values[!is.na(security_values)]
    if (length(security_values) > 0) {
      calc_list$green_areas_security <- sprintf("%.1f / 10", mean(security_values, na.rm = TRUE))
    } else {
      calc_list$green_areas_security <- "No disponible"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Get survey data from the data manager for displaying question text
  output$question_text <- renderText({
    req(input$selected_service)
    
    survey_id <- paste0("PER_", selectedYear())
    survey_data <- data_manager$get_survey_data(survey_id)
    
    # Get metadata for the question
    question_meta <- survey_data$metadata %>%
      filter(variable == input$selected_service) %>%
      first()
    
    if (!is.null(question_meta) && !is.na(question_meta$label)) {
      return(question_meta$label)
    } else {
      return("Información no disponible")
    }
  })
  
  # Output service title
  output$service_title <- renderText({
    req(input$selected_service)
    
    title <- service_titles[input$selected_service]
    if (is.na(title)) title <- "Evaluación de servicios por distrito"
    
    return(title)
  })
  
  # Render service map
  output$service_map <- renderLeaflet({
    maps()
  })
  
  # Render the report statistics
  output$report_statistics_plot <- renderUI({
    req(selectedYear())
    
    survey_id <- paste0("PER_", selectedYear())
    survey_data <- data_manager$get_survey_data(survey_id)
    
    create_report_statistics(survey_data$responses)
  })
  
  # Output water days
  output$water_days <- renderText({
    calculations()$water_days
  })
  
  # Output power outages
  output$power_outages <- renderText({
    calculations()$power_outages
  })
  
  # Output trash pickup
  output$trash_pickup <- renderText({
    calculations()$trash_pickup
  })
  
  # Output green areas equipment
  output$green_areas_equipment <- renderText({
    calculations()$green_areas_equipment
  })
  
  # Output green areas lighting
  output$green_areas_lighting <- renderText({
    calculations()$green_areas_lighting
  })
  
  # Output green areas maintenance
  output$green_areas_maintenance <- renderText({
    calculations()$green_areas_maintenance
  })
  
  # Output green areas security
  output$green_areas_security <- renderText({
    calculations()$green_areas_security
  })
  
  # Download data handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("servicios_publicos_datos_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Create a workbook
      wb <- createWorkbook()
      
      survey_id <- paste0("PER_", selectedYear())
      survey_data <- data_manager$get_survey_data(survey_id)
      
      # Add a worksheet for each service
      service_questions <- c("Q29", "Q30", "Q35", "Q40", "Q45", "Q51", "Q55", 
                            "Q56", "Q58", "Q59", "Q60", "Q61", "Q62")
      
      service_names <- c(
        "Agua", "Drenaje y Alcantarillado", "CFE", "Recolección de Basura",
        "Alumbrado Público", "Calles y Pavimentación", "Semaforización",
        "Áreas verdes", "Unidades deportivas", "Bibliotecas",
        "Centros comunitarios", "Banquetas", "Espacios para personas con discapacidad"
      )
      
      for (i in 1:length(service_questions)) {
        # Create a dataframe for this service
        q_id <- service_questions[i]
        service_name <- service_names[i]
        
        # Extract data
        service_data <- data.frame(
          Distrito = survey_data$responses$DISTRICT,
          Calificacion = survey_data$responses[[q_id]]
        )
        
        # Add to workbook
        addWorksheet(wb, service_name)
        writeData(wb, service_name, service_data)
      }
      
      # Add summarized statistics
      addWorksheet(wb, "Resumen")
      summary_data <- data.frame(
        Servicio = service_names,
        Promedio = sapply(service_questions, function(q) {
          mean(as.numeric(survey_data$responses[[q]]), na.rm = TRUE)
        }),
        Mediana = sapply(service_questions, function(q) {
          median(as.numeric(survey_data$responses[[q]]), na.rm = TRUE)
        })
      )
      writeData(wb, "Resumen", summary_data)
      
      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Download service map handler
  output$download_service_map <- downloadHandler(
    filename = function() {
      # Get service name for filename
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
      
      paste("mapa_servicio_", service_name, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Temporary file for HTML content
      tmp_html <- tempfile(fileext = ".html")
      
      # Get title for the selected service
      title_text <- service_titles[input$selected_service]
      if (is.na(title_text)) title_text <- "Evaluación de servicios por distrito"
      
      # Get the map
      map <- maps()
      
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
        options = list(
          printBackground = TRUE,
          scale = 2.0
        ),
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