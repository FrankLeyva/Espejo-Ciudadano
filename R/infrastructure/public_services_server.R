# Función del servidor para el Dashboard de Servicios Públicos
publicServicesServer <- function(input, output, session) {
  selectedYear <- session$userData$selectedYear
  
  # Load survey data with dynamic year
  survey_data <- reactive({
    survey_id <- paste0("PER_", selectedYear())
    load_survey_data(survey_id)
  })
  geo_data <- reactive({
    tryCatch({
      sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error cargando datos geográficos:", e$message), type = "error")
      NULL
    })
  })
  
  # Almacenar tema actual
  current_theme <- reactiveVal(theme_config)
  
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
# Mejora para el value box de cortes de luz
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
      "Q55" = "Satisfacción con áreas verdes y espacios públicos",
      "Q56" = "Satisfacción con unidades deportivas",
      "Q58" = "Satisfacción con bibliotecas",
      "Q59" = "Satisfacción con centros comunitarios",
      "Q60" = "Satisfacción con espacios para personas con discapacidad",
      "Q61" = "Satisfacción con parques",
      "Q62" = "Satisfacción con transporte público"
    )
    
    title <- service_titles[input$selected_service]
    if (is.na(title)) title <- "Evaluación de servicios por distrito"
    
    return(title)
  })
  # Renderizar directamente el mapa de distrito sin los controles del módulo
# Renderizar mapa con estética mejorada
output$service_map <- renderLeaflet({
  req(prepared_data(), geo_data(), input$selected_service)
  
  # Get the actual data frames from the reactives
  prepared_data_df <- prepared_data()
  geo_data_df <- geo_data()
  
  # Service title mapping
  service_titles <- c(
    "Q29" = "Satisfacción con los servicios de agua",
    "Q30" = "Satisfacción con los servicios de drenaje y alcantarillado",
    "Q35" = "Satisfacción con los servicios de CFE",
    "Q40" = "Satisfacción con la recolección de basura",
    "Q45" = "Satisfacción con el alumbrado público",
    "Q51" = "Satisfacción con calles y pavimentación",
    "Q55" = "Satisfacción con áreas verdes y espacios públicos",
    "Q56" = "Satisfacción con unidades deportivas",
    "Q58" = "Satisfacción con bibliotecas",
    "Q59" = "Satisfacción con centros comunitarios",
    "Q60" = "Satisfacción con espacios para personas con discapacidad",
    "Q61" = "Satisfacción con parques",
    "Q62" = "Satisfacción con transporte público"
  )
  
  # Calculate district statistics
  district_stats <- prepared_data_df %>%
    group_by(district) %>%
    summarise(
      mean_value = mean(value_num, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  
  # Find highest and lowest districts
  highest_district <- district_stats %>% 
    filter(mean_value == max(mean_value, na.rm = TRUE)) %>% 
    pull(district) %>% 
    as.character()
    
  lowest_district <- district_stats %>% 
    filter(mean_value == min(mean_value, na.rm = TRUE)) %>% 
    pull(district) %>% 
    as.character()
  
  # Convert district to numeric for matching
  district_stats$district_num <- as.numeric(as.character(district_stats$district))
  
  # Calculate centroids for the geographical data
  geo_data_df$centroid <- sf::st_centroid(geo_data_df$geometry)
  centroids <- sf::st_coordinates(geo_data_df$centroid)
  geo_data_df$lng <- centroids[,1]
  geo_data_df$lat <- centroids[,2]
  
  # Calculate overall average
  overall_mean <- round(mean(district_stats$mean_value, na.rm = TRUE), 1)
  
  # District colors - use theme colors if available
  district_colors <- if (!is.null(current_theme()$palettes$district)) {
    current_theme()$palettes$district
  } else {
    c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
     "#8c564b", "#e377c2", "#bcbd22", "#17becf")
  }
  
  map <- leaflet(geo_data_df) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~{
      # Use district number to determine color
      dist_num <- as.numeric(No_Distrit)
      ifelse(
        !is.na(dist_num) & dist_num >= 1 & dist_num <= length(district_colors),
        district_colors[dist_num],
        "#CCCCCC"  # Default gray
      )
    },
    fillOpacity = 0.7,
    weight = 1,
    color = "#666666",
    dashArray = "3",
    highlight = highlightOptions(
      weight = 2,
      color = "#000000",
      dashArray = "",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  )
  
  # Add district labels
  for (i in 1:nrow(geo_data_df)) {
    dist_num <- geo_data_df$No_Distrit[i]
    match_idx <- which(district_stats$district_num == dist_num)
    
    if (length(match_idx) > 0 && match_idx <= nrow(district_stats)) {
      mean_val <- district_stats$mean_value[match_idx]
      
      # Skip if missing data
      if (is.na(mean_val)) next
      
      # Determine label color based on high/low value
      bg_color <- "#FFFFFF"  # Default white
      text_color <- "#000000"  # Default black
      
      if (as.character(dist_num) %in% highest_district) {
        bg_color <- "#4CAF50"  # Green for highest
        text_color <- "#FFFFFF"
      } else if (as.character(dist_num) %in% lowest_district) {
        bg_color <- "#F44336"  # Red for lowest
        text_color <- "#FFFFFF"
      }
      
      # Enhanced label with district number
      label_html <- sprintf(
        '<div style="background-color: %s; color: %s; padding: 5px 10px; border-radius: 4px; font-weight: bold; text-align: center; box-shadow: 0 1px 3px rgba(0,0,0,0.3);">Distrito %s<br>%.2f</div>',
        bg_color, text_color, dist_num, mean_val
      )
      
      # Add label marker
      map <- map %>% 
        addLabelOnlyMarkers(
          lng = geo_data_df$lng[i],
          lat = geo_data_df$lat[i],
          label = lapply(list(label_html), HTML),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE
          )
        )
    }
  }
  
  # Add overall average control
  map %>% addControl(
    html = sprintf(
      '<div style="background-color: #333333; color: white; padding: 8px 12px; border-radius: 4px; font-weight: bold; box-shadow: 0 2px 5px rgba(0,0,0,0.2);">Promedio general: %.1f</div>',
      overall_mean
    ),
    position = "topright"
  )
})
output$report_statistics_plot <- renderPlotly({
  req(survey_data())
  
  # Create the report statistics visualization
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
        "Alumbrado Público", "Calles y Pavimentación", "Áreas verdes",
        "Unidades deportivas", "Bibliotecas", "Centros comunitarios",
        "Espacios para discapacitados", "Parques", "Transporte público"
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
}