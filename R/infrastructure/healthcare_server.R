# Función del servidor para el Dashboard de Servicios de Salud
healthcareServer <- function(input, output, session, current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  
  # Load survey data with dynamic year
  survey_data <- reactive({
    survey_id <- paste0("PER_", selectedYear())
    load_survey_data(survey_id)
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

  # Mapeo de preguntas de satisfacción con servicios de salud
  health_questions <- c(
    "health_services" = "Q19",   # Servicios de salud en general
    "facilities" = "Q20",        # Instalaciones
    "attention_time" = "Q21",    # Tiempo de atención
    "medication" = "Q22",        # Disponibilidad de medicamentos
    "service_quality" = "Q23",   # Calidad del servicio
    "distance" = "Q24"           # Distancia al centro de salud
  )
  
  # Función para preparar datos de satisfacción usando prepare_interval_data
  health_data_list <- lapply(health_questions, function(q_id) {
    reactive({
      req(survey_data(), q_id)
      prepare_interval_data(
        data = survey_data()$responses,
        question_id = q_id,
        metadata = survey_data()$metadata
      )
    })
  })
  
  # Procesar y preparar datos de proveedores de salud (Q17.1 a Q17.8)
  healthcare_providers_data <- reactive({
    req(survey_data())
    
    responses <- survey_data()$responses
    metadata <- survey_data()$metadata
    
    if (selectedYear() == '2024') {
      providers <- c(
        "IMSS" = "Q17.1",
        "ISSSTE" = "Q17.2",
        "Instituto de Salud de Bienestar" = "Q17.3",
        "Médicos de farmacias/genéricos" = "Q17.4",
        "Servicio médico privado/particulares" = "Q17.5",
        "No tiene servicio médico" = "Q17.6",
        "Otro" = "Q17.7",
        "MEDICHIHUAHUA" = "Q17.8"
      )
    } else {
      providers <- c(
        "IMSS" = "Q17.1",
        "ISSSTE" = "Q17.2",
        "Instituto de Salud de Bienestar" = "Q17.3",
        "Médicos de farmacias/genéricos" = "Q17.4",
        "Servicio médico privado/particulares" = "Q17.5",
        "No tiene servicio médico" = "Q17.6",
        "Otro" = "Q17.7")
    }
    
    # Prepare binary data for each provider
    provider_data_list <- lapply(providers, function(provider_id) {
      prepared_data <- prepare_binary_data(
        data = responses,
        question_id = provider_id,
        metadata = metadata,
        treat_na_as_negative = TRUE
      )
      
      # Calculate percentage of TRUE values
      true_count <- sum(prepared_data$binary_value, na.rm = TRUE)
      total_count <- nrow(prepared_data)
      percentage <- 100 * true_count / total_count
      
      return(list(
        provider_name = names(providers)[providers == provider_id],
        percentage = percentage,
        prepared_data = prepared_data
      ))
    })
    
    # Create dataframe for visualization
    result_df <- data.frame(
      provider = sapply(provider_data_list, function(pd) pd$provider_name),
      percentage = sapply(provider_data_list, function(pd) pd$percentage)
    )
    
    # Sort by percentage (descending)
    result_df <- result_df[order(result_df$percentage, decreasing = TRUE),]
    
    # Store the full provider data list as an attribute
    attr(result_df, "provider_data_list") <- provider_data_list
    
    return(result_df)
  })
  
  # Renderizar mapas para cada aspecto de satisfacción con servicios de salud usando create_interval_district_map
  map_outputs <- c(
    "health_services_map", "facilities_map", "attention_time_map", 
    "medication_map", "service_quality_map", "distance_map"
  )
  
  for (i in seq_along(map_outputs)) {
    local({
      output_id <- map_outputs[i]
      data_key <- names(health_questions)[i]
      data_id <- health_questions[i]
      title <- paste("Satisfacción con", gsub("_", " ", data_key))
      
      output[[output_id]] <- renderLeaflet({
        req(health_data_list[[data_key]](), geo_data())
        create_interval_district_map(
          data = health_data_list[[data_key]](),
          geo_data = geo_data(),
          highlight_extremes = TRUE,
          use_gradient = FALSE,  # Enable gradient coloring
          color_scale = "Blues",  # Use blues color scale
          custom_theme = active_theme()
        )
      })
    })
  }
  
  # Renderizar mapas para proveedores de salud usando create_binary_district_map
  # Solo para los 3 proveedores principales
  output$providers_maps <- renderUI({
    req(healthcare_providers_data(), geo_data())
    providers_data <- healthcare_providers_data()
    provider_data_list <- attr(providers_data, "provider_data_list")
    
    # Use the top 3 providers
    top_providers <- head(providers_data$provider, 3)
    top_provider_data <- provider_data_list[match(top_providers, providers_data$provider)]
    
    map_boxes <- lapply(seq_along(top_providers), function(i) {
      provider_name <- top_providers[i]
      provider_data <- top_provider_data[[i]]$prepared_data
      
      box_id <- paste0("provider_map_", i)
      
      column(
        width = 4,
        div(
          class = "card mb-4",
          div(class = "card-header", h5(provider_name)),
          div(
            class = "card-body",
            leafletOutput(session$ns(box_id), height = "300px")
          )
        )
      )
    })
    
    fluidRow(map_boxes)
  })
  
  # Create the provider maps
  observe({
    req(healthcare_providers_data(), geo_data())
    providers_data <- healthcare_providers_data()
    provider_data_list <- attr(providers_data, "provider_data_list")
    
    # Use the top 3 providers
    top_providers <- head(providers_data$provider, 3)
    top_provider_data <- provider_data_list[match(top_providers, providers_data$provider)]
    
    for (i in seq_along(top_providers)) {
      local({
        idx <- i
        provider_name <- top_providers[idx]
        provider_data <- top_provider_data[[idx]]$prepared_data
        output_id <- paste0("provider_map_", idx)
        
        output[[output_id]] <- renderLeaflet({
          create_binary_district_map(
            data = provider_data,
            geo_data = geo_data(),
            highlight_extremes = TRUE,
            focus_on_true = TRUE,
            custom_theme = active_theme()
          )
        })
      })
    }
  })
  
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
    
    # Get colors from theme
    primary_color <- if (!is.null(active_theme())) {
      active_theme()$colors$primary
    } else {
      "#1f77b4"  # Default blue
    }

    highlight_color <- if (!is.null(active_theme())) {
      active_theme()$colors$accent
    } else {
      "#ff7f0e"  # Default orange
    }

    # Create single color vector for all bars initially
    colors <- rep(primary_color, nrow(providers_data))

    # Handle ties for highlighting top N items
    # First, identify the top 3 unique values
    unique_top_values <- unique(providers_data$percentage)[1:min(3, length(unique(providers_data$percentage)))]

    # Find all rows that have those top values
    top_indices <- which(providers_data$percentage %in% unique_top_values)

    # Highlight all those rows
    colors[top_indices] <- highlight_color
    
    # Crear gráfico de barras horizontal
    plot_ly(
      data = providers_data,
      y = ~reorder(provider, percentage),
      x = ~percentage,
      type = "bar",
      orientation = "h",
      marker = list(
        color = colors
      ),
      text = ~paste0(round(percentage, 1), "%"),
      textposition = "auto",
      hoverinfo = "text",
      hovertext = ~paste0(provider, ": ", round(percentage, 1), "%")
    ) %>%
      layout(
        title = "",
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