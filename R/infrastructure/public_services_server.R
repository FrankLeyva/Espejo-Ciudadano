# Función del servidor para el Dashboard de Servicios Públicos
publicServicesServer <- function(input, output, session, current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  
  survey_data <- session$userData$perSurveyData
  
  geo_data <- session$userData$geoData
  
  # Use the current theme
  active_theme <- reactive({
    if (is.function(current_theme)) {
      # If current_theme is a reactive function, call it to get the value
      current_theme()
    } else if (!is.null(current_theme)) {
      # If it's a direct value, use it
      current_theme
    } else {
      # Default to infraestructura theme if nothing provided
      get_section_theme("infraestructura")
    }
  })
  

  observe({
    req(input$selected_service)
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
      "Transporte público" = "<b>ID</b>: PER Q35 <br>
            <b>Pregunta</b>:	 Que tan satisfecho esta con el SERVICIO DE Comision Federal de Electricidad (CFE)? <br>
             <b>Escala</b>:  1-10",
      "<b>ID</b>: PER Q29 <br>
            <b>Pregunta</b>:	 Que tan satisfecho esta con el SERVICIO DEL AGUA? <br>
             <b>Escala</b>:  1-10"
    )
    
    update_tooltip_content(session, "utilities_tooltip", tooltip_content)
  })

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q89 <br>
            <b>Pregunta</b>:	¿Qué tan satisfecho está con LA CALIDAD DEL AIRE? <br>
             <b>Escala</b>:  1-10"
    
    update_tooltip_content(session, "utilities_tooltip", initial_tooltip)
  }, once = TRUE)  







  # Mostrar texto de la pregunta basado en el servicio seleccionado
  output$question_text <- renderText({
    req(survey_data(), input$selected_service)
    
    # Obtener metadatos de la pregunta
    question_meta <- survey_data()$metadata %>%
      filter(variable == input$selected_service) %>%
      first()
    
    if (!is.null(question_meta) && !is.na(question_meta$label)) {
      return(question_meta$label)
    } else {
      return("Información no disponible")
    }
  })
  
  # Calcular y mostrar valor para Q31 (días con agua)
  output$water_days <- renderText({
    req(survey_data())
    
    # Calcular promedio de días con agua
    water_data <- as.numeric(survey_data()$responses$Q31)
    water_data <- water_data[!is.na(water_data)]
    
    if (length(water_data) > 0) {
      mean_days <- mean(water_data, na.rm = TRUE)
      # Formatear a 1 decimal
      sprintf("%.1f días", mean_days)
    } else {
      "Datos no disponibles"
    }
  })
  
  # Calcular y mostrar valor para Q36 (cortes de luz)
  output$power_outages <- renderText({
    req(survey_data())
    
    # Obtener valores y eliminar NAs
    outage_data <- survey_data()$responses$Q36
    outage_data <- outage_data[!is.na(outage_data)]
    
    if (length(outage_data) > 0) {
      # Crear mapeo de valores a descripciones
      outage_mapping <- c(
        "1" = "Casi todos los días",
        "2" = "Al menos una vez a la semana",
        "3" = "Al menos una vez al mes",
        "4" = "Solo hubo un apagón",
        "5" = "No hubo apagones"
      )
      
      # Encontrar el valor más común (moda)
      freq_table <- table(outage_data)
      most_common <- names(freq_table)[which.max(freq_table)]
      
      # Devolver la descripción del valor más común
      if (most_common %in% names(outage_mapping)) {
        outage_mapping[most_common]
      } else {
        "Dato más común"
      }
    } else {
      "Datos no disponibles"
    }
  })
  
  # Calcular y mostrar valor para Q41 (frecuencia de recolección de basura)
  output$trash_pickup <- renderText({
    req(survey_data())
    
    # Calcular promedio de días con agua
    trash_data <- as.numeric(survey_data()$responses$Q41)
    trash_data <- trash_data[!is.na(trash_data)]
    
    if (length(trash_data) > 0) {
      mean_days <- mean(trash_data, na.rm = TRUE)
      # Formatear a 1 decimal
      sprintf("%.1f días", mean_days)
    } else {
      "Datos no disponibles"
    }
  })
  
  # Preparar datos para el mapa 
  prepared_data <- reactive({
    req(survey_data(), input$selected_service)
    
    # Utilizar la función del módulo de intervalo para preparar los datos
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = input$selected_service,
      metadata = survey_data()$metadata
    )
  })
  
  output$service_title <- renderText({
    req(input$selected_service)
    
    # Mismo mapeo que usamos en el renderLeaflet
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
    
    title <- service_titles[input$selected_service]
    if (is.na(title)) title <- "Evaluación de servicios por distrito"
    
    return(title)
  })
  
  # Renderizar mapa usando la función del módulo de intervalo
  output$service_map <- renderLeaflet({
    req(prepared_data(), geo_data(), input$selected_service)
    
    # Use the create_interval_district_map function from the interval module
    create_interval_district_map(
      data = prepared_data(),
      geo_data = geo_data(),
      # You can add these UI inputs if needed:
      # selected_responses = NULL,  # This would be populated if you have a UI element for selecting specific responses
      highlight_extremes = TRUE,    # Default to highlighting extreme values
      use_gradient = FALSE,         # Use gradient coloring for better visualization
      color_scale = "Blues",        # Use a blue color scale for the gradient
      custom_theme = active_theme() # Pass the current theme
    )
  })
  
  output$report_statistics_plot <- renderUI({
    req(survey_data())
    
    # Create the responsive report statistics visualization
    create_report_statistics(survey_data()$responses)
  })
  
  # Manejador de descarga
  output$download_data <- downloadHandler(
    filename = function() {
      paste("servicios_publicos_datos_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Crear un libro de trabajo
      wb <- createWorkbook()
      
      # Añadir una hoja de trabajo para cada servicio
      service_questions <- c("Q29", "Q30", "Q35", "Q40", "Q45", "Q51", "Q55", 
                            "Q56", "Q58", "Q59", "Q60", "Q61", "Q62")
      
      service_names <- c(
        "Agua", "Drenaje y Alcantarillado", "CFE", "Recolección de Basura",
        "Alumbrado Público", "Calles y Pavimentación", "Semaforización",
        "Áreas verdes", "Unidades deportivas", "Bibliotecas",
        "Centros comunitarios", "Banquetas", "Espacios para personas con discapacidad"
      )
      
      for (i in 1:length(service_questions)) {
        # Crear un dataframe para este servicio
        q_id <- service_questions[i]
        service_name <- service_names[i]
        
        # Extraer datos
        service_data <- data.frame(
          Distrito = survey_data()$responses$DISTRICT,
          Calificacion = survey_data()$responses[[q_id]]
        )
        
        # Añadir al libro de trabajo
        addWorksheet(wb, service_name)
        writeData(wb, service_name, service_data)
      }
      
      # Añadir estadísticas resumidas
      addWorksheet(wb, "Resumen")
      summary_data <- data.frame(
        Servicio = service_names,
        Promedio = sapply(service_questions, function(q) {
          mean(as.numeric(survey_data()$responses[[q]]), na.rm = TRUE)
        }),
        Mediana = sapply(service_questions, function(q) {
          median(as.numeric(survey_data()$responses[[q]]), na.rm = TRUE)
        })
      )
      writeData(wb, "Resumen", summary_data)
      
      # Guardar el libro de trabajo
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Calculate average rating for green areas equipment (Q57.1)
  output$green_areas_equipment <- renderText({
    req(survey_data())
    
    # Get values for Q57.1 (Equipment)
    equipment_values <- as.numeric(survey_data()$responses$Q57.1)
    equipment_values <- equipment_values[!is.na(equipment_values)]
    
    if (length(equipment_values) > 0) {
      mean_value <- mean(equipment_values, na.rm = TRUE)
      # Format to 1 decimal place with scale of 1-10
      sprintf("%.1f / 10", mean_value)
    } else {
      "No disponible"
    }
  })
  
  # Calculate average rating for green areas lighting (Q57.2)
  output$green_areas_lighting <- renderText({
    req(survey_data())
    
    # Get values for Q57.2 (Lighting)
    lighting_values <- as.numeric(survey_data()$responses$Q57.2)
    lighting_values <- lighting_values[!is.na(lighting_values)]
    
    if (length(lighting_values) > 0) {
      mean_value <- mean(lighting_values, na.rm = TRUE)
      # Format to 1 decimal place with scale of 1-10
      sprintf("%.1f / 10", mean_value)
    } else {
      "No disponible"
    }
  })
  
  # Calculate average rating for green areas maintenance (Q57.3)
  output$green_areas_maintenance <- renderText({
    req(survey_data())
    
    # Get values for Q57.3 (Maintenance)
    maintenance_values <- as.numeric(survey_data()$responses$Q57.3)
    maintenance_values <- maintenance_values[!is.na(maintenance_values)]
    
    if (length(maintenance_values) > 0) {
      mean_value <- mean(maintenance_values, na.rm = TRUE)
      # Format to 1 decimal place with scale of 1-10
      sprintf("%.1f / 10", mean_value)
    } else {
      "No disponible"
    }
  })
  
  # Calculate average rating for green areas security (Q57.4)
  output$green_areas_security <- renderText({
    req(survey_data())
    
    # Get values for Q57.4 (Security)
    security_values <- as.numeric(survey_data()$responses$Q57.4)
    security_values <- security_values[!is.na(security_values)]
    
    if (length(security_values) > 0) {
      mean_value <- mean(security_values, na.rm = TRUE)
      # Format to 1 decimal place with scale of 1-10
      sprintf("%.1f / 10", mean_value)
    } else {
      "No disponible"
    }
  })
  
  # Download handler for the service map
  output$download_service_map <- downloadHandler(
    filename = function() {
      # Get service name for filename based on selected service
      service_mapping <- c(
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
      
      service_name <- service_mapping[input$selected_service]
      if (is.na(service_name)) service_name <- "Servicio"
      
      paste("mapa_servicio_", service_name, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Temporary file for the HTML content
      tmp_html <- tempfile(fileext = ".html")
      
      # Service titles for the map caption
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
      
      # Get title for the selected service
      title_text <- service_titles[input$selected_service]
      if (is.na(title_text)) title_text <- "Evaluación de servicios por distrito"
      
      # Create the map using the same function and data as the displayed map
      map <- create_interval_district_map(
        data = prepared_data(),
        geo_data = geo_data(),
        highlight_extremes = TRUE,
        use_gradient = FALSE,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
      
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