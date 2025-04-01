  # Función del servidor para el Dashboard de Servicios de Salud
healthcareServer <- function(input, output, session) {
  # Cargar datos de encuesta de Percepción 2024
  survey_data <- reactive({
    load_survey_data("PER_2024")
  })
  
  # Cargar datos geográficos
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

  # Mapeo de preguntas de satisfacción con servicios de salud
  health_questions <- c(
    "health_services" = "Q19",   # Servicios de salud en general
    "facilities" = "Q20",        # Instalaciones
    "attention_time" = "Q21",    # Tiempo de atención
    "medication" = "Q22",        # Disponibilidad de medicamentos
    "service_quality" = "Q23",   # Calidad del servicio
    "distance" = "Q24"           # Distancia al centro de salud
  )
  
  # Función para preparar datos de satisfacción
  prepare_health_data <- function(question_id) {
    req(survey_data(), question_id)
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = question_id,
      metadata = survey_data()$metadata
    )
  }
  
  # Datos reactivos para cada pregunta de satisfacción
  health_data_list <- lapply(health_questions, function(q_id) {
    reactive({
      prepare_health_data(q_id)
    })
  })
  
  # Función para crear mapas de satisfacción
  create_satisfaction_map <- function(prepared_data, title) {
    req(prepared_data, geo_data())
    
    # Obtener datos y marco geográfico
    prepared_data_df <- prepared_data
    geo_data_df <- geo_data()
    
    # Calcular estadísticas por distrito
    district_stats <- prepared_data_df %>%
      group_by(district) %>%
      summarise(
        mean_value = mean(value_num, na.rm = TRUE),
        n = n(),
        .groups = 'drop'
      )
    
    # Identificar distritos con valores más altos y más bajos
    highest_district <- district_stats %>% 
      filter(mean_value == max(mean_value, na.rm = TRUE)) %>% 
      pull(district) %>% 
      as.character()
      
    lowest_district <- district_stats %>% 
      filter(mean_value == min(mean_value, na.rm = TRUE)) %>% 
      pull(district) %>% 
      as.character()
    
    # Convertir distrito a numérico para hacer coincidir
    district_stats$district_num <- as.numeric(as.character(district_stats$district))
    
    # Calcular centroides para el marco geográfico
    geo_data_df$centroid <- sf::st_centroid(geo_data_df$geometry)
    centroids <- sf::st_coordinates(geo_data_df$centroid)
    geo_data_df$lng <- centroids[,1]
    geo_data_df$lat <- centroids[,2]
    
    # Calcular promedio general
    overall_mean <- round(mean(district_stats$mean_value, na.rm = TRUE), 1)
    
    # Colores de distrito - usar colores del tema si están disponibles
    district_colors <- if (!is.null(current_theme()$palettes$district)) {
      current_theme()$palettes$district
    } else {
      c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
       "#8c564b", "#e377c2", "#bcbd22", "#17becf")
    }
    
    # Create the map
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
          '<div style="background-color: %s; color: %s; padding: 5px 10px; border-radius: 4px; font-weight: bold; text-align: center; box-shadow: 0 1px 3px rgba(0,0,0,0.3);">Distrito %s<br>%.1f</div>',
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
  }
  
  # Procesar y preparar datos de proveedores de salud (Q17.1 a Q17.8)
  healthcare_providers_data <- reactive({
    req(survey_data())
    
    responses <- survey_data()$responses
    if (survey_id == 'PER_2024') {
    providers <- c(
      "IMSS" = "Q17.1",
      "ISSSTE" = "Q17.2",
      "Instituto de Salud de Bienestar" = "Q17.3",
      "Médicos de farmacias/genéricos" = "Q17.4",
      "Servicio médico privado/particulares" = "Q17.5",
      "No tiene servicio médico" = "Q17.6",
      "Otro" = "Q17.7",
      "MEDICHIHUAHUA" = "Q17.8"
    )} else {
      providers <- c(
        "IMSS" = "Q17.1",
        "ISSSTE" = "Q17.2",
        "Instituto de Salud de Bienestar" = "Q17.3",
        "Médicos de farmacias/genéricos" = "Q17.4",
        "Servicio médico privado/particulares" = "Q17.5",
        "No tiene servicio médico" = "Q17.6",
        "Otro" = "Q17.7")
    }
    
    # Calcular porcentajes de "Sí" para cada proveedor
    provider_percentages <- sapply(providers, function(col) {
      vals <- responses[[col]]
      vals <- vals[!is.na(vals)]
      
      # Contar respuestas "Sí" (valor 1)
      yes_count <- sum(vals == "1")
      percentage <- 100 * yes_count / length(vals)
      
      return(percentage)
    })
    
    # Crear dataframe para la visualización
    result_df <- data.frame(
      provider = names(providers),
      percentage = provider_percentages
    )
    
    # Ordenar por porcentaje (descendente)
    result_df <- result_df[order(result_df$percentage, decreasing = TRUE),]
    
    return(result_df)
  })
  
  # Renderizar mapas para cada aspecto de satisfacción con servicios de salud
  map_outputs <- c(
    "health_services_map", "facilities_map", "attention_time_map", 
    "medication_map", "service_quality_map", "distance_map"
  )
  
  for (i in seq_along(map_outputs)) {
    local({
      output_id <- map_outputs[i]
      data_key <- names(health_questions)[i]
      title <- paste("Satisfacción con", gsub("_", " ", data_key))
      
      output[[output_id]] <- renderLeaflet({
        create_satisfaction_map(health_data_list[[data_key]](), title)
      })
    })
  }
  
  # Renderizar promedios generales
  for (aspect in names(health_questions)) {
    local({
      current_aspect <- aspect
      output_id <- paste0(current_aspect, "_avg")
      
      output[[output_id]] <- renderText({
        req(health_data_list[[current_aspect]]())
        avg <- mean(health_data_list[[current_aspect]]()$value_num, na.rm = TRUE)
        sprintf("%.1f / 10", avg)
      })
    })
  }
  
  # Renderizar distritos con mayor satisfacción
  for (aspect in names(health_questions)) {
    local({
      current_aspect <- aspect
      output_id <- paste0(current_aspect, "_best_district")
      
      output[[output_id]] <- renderText({
        req(health_data_list[[current_aspect]]())
        
        district_stats <- health_data_list[[current_aspect]]() %>%
          group_by(district) %>%
          summarise(
            mean_value = mean(value_num, na.rm = TRUE),
            .groups = 'drop'
          )
        
        best_district <- district_stats %>%
          filter(mean_value == max(mean_value, na.rm = TRUE))
        
        sprintf("Distrito %s (%.1f)", best_district$district[1], best_district$mean_value[1])
      })
    })
  }
  
  # Renderizar gráfico de proveedores de servicios de salud
  output$healthcare_providers_chart <- renderPlotly({
    req(healthcare_providers_data())
    
    providers_data <- healthcare_providers_data()
    
    # Colores para el gráfico
    bar_color <- if (!is.null(current_theme()$colors$primary)) {
      current_theme()$colors$primary
    } else {
      "#1f77b4"  # Default blue
    }
    
    # Crear gráfico de barras horizontal
    plot_ly(
      data = providers_data,
      y = ~reorder(provider, percentage),
      x = ~percentage,
      type = "bar",
      orientation = "h",
      marker = list(
        color = bar_color
      ),
      text = ~paste0(round(percentage, 1), "%"),
      textposition = "auto",
      hoverinfo = "text",
      hovertext = ~paste0(provider, ": ", round(percentage, 1), "%")
    ) %>%
      layout(
        title = "Porcentaje de Ciudadanos por Proveedor de Servicios de Salud",
        xaxis = list(
          title = "Porcentaje (%)",
          range = c(0, 100)
        ),
        yaxis = list(
          title = ""
        ),
        margin = list(l = 150)  # Más espacio para etiquetas de proveedores
      )
  })
}