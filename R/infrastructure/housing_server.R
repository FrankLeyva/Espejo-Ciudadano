 # Función del servidor para el Dashboard de Vivienda
housingServer <- function(input, output, session) {
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

  # Funciones auxiliares para preparar datos
  prepare_housing_data <- function(question_id) {
    req(survey_data(), question_id)
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = question_id,
      metadata = survey_data()$metadata
    )
  }
  
  # Crear mapa para visualizar datos de satisfacción
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
  
  # Datos reactivos para cada aspecto de vivienda
  materials_data <- reactive({
    prepare_housing_data("Q26")
  })
  
  spaces_data <- reactive({
    prepare_housing_data("Q27")
  })
  
  location_data <- reactive({
    prepare_housing_data("Q28")
  })
  
  # Renderizar mapas para cada aspecto
  output$materials_map <- renderLeaflet({
    create_satisfaction_map(materials_data(), "Satisfacción con la Calidad de los Materiales")
  })
  
  output$spaces_map <- renderLeaflet({
    create_satisfaction_map(spaces_data(), "Satisfacción con el Tamaño y Espacios")
  })
  
  output$location_map <- renderLeaflet({
    create_satisfaction_map(location_data(), "Satisfacción con la Ubicación y Accesibilidad")
  })
  
  # Calcular y mostrar promedios generales
  output$materials_avg <- renderText({
    req(materials_data())
    avg <- mean(materials_data()$value_num, na.rm = TRUE)
    sprintf("%.1f / 10", avg)
  })
  
  output$spaces_avg <- renderText({
    req(spaces_data())
    avg <- mean(spaces_data()$value_num, na.rm = TRUE)
    sprintf("%.1f / 10", avg)
  })
  
  output$location_avg <- renderText({
    req(location_data())
    avg <- mean(location_data()$value_num, na.rm = TRUE)
    sprintf("%.1f / 10", avg)
  })
  
  # Mostrar distritos con mayor satisfacción
  output$materials_best_district <- renderText({
    req(materials_data())
    
    district_stats <- materials_data() %>%
      group_by(district) %>%
      summarise(
        mean_value = mean(value_num, na.rm = TRUE),
        .groups = 'drop'
      )
    
    best_district <- district_stats %>%
      filter(mean_value == max(mean_value, na.rm = TRUE))
    
    sprintf("Distrito %s (%.1f)", best_district$district[1], best_district$mean_value[1])
  })
  
  output$spaces_best_district <- renderText({
    req(spaces_data())
    
    district_stats <- spaces_data() %>%
      group_by(district) %>%
      summarise(
        mean_value = mean(value_num, na.rm = TRUE),
        .groups = 'drop'
      )
    
    best_district <- district_stats %>%
      filter(mean_value == max(mean_value, na.rm = TRUE))
    
    sprintf("Distrito %s (%.1f)", best_district$district[1], best_district$mean_value[1])
  })
  
  output$location_best_district <- renderText({
    req(location_data())
    
    district_stats <- location_data() %>%
      group_by(district) %>%
      summarise(
        mean_value = mean(value_num, na.rm = TRUE),
        .groups = 'drop'
      )
    
    best_district <- district_stats %>%
      filter(mean_value == max(mean_value, na.rm = TRUE))
    
    sprintf("Distrito %s (%.1f)", best_district$district[1], best_district$mean_value[1])
  })
  
  # Gráfico comparativo
  output$comparison_plot <- renderPlotly({
    req(materials_data(), spaces_data(), location_data())
    
    # Preparar datos para cada aspecto por distrito
    materials_by_district <- materials_data() %>%
      group_by(district) %>%
      summarise(mean_value = mean(value_num, na.rm = TRUE), .groups = 'drop') %>%
      mutate(aspect = "Materiales")
    
    spaces_by_district <- spaces_data() %>%
      group_by(district) %>%
      summarise(mean_value = mean(value_num, na.rm = TRUE), .groups = 'drop') %>%
      mutate(aspect = "Espacios")
    
    location_by_district <- location_data() %>%
      group_by(district) %>%
      summarise(mean_value = mean(value_num, na.rm = TRUE), .groups = 'drop') %>%
      mutate(aspect = "Ubicación")
    
    # Combinar datos
    all_data <- bind_rows(materials_by_district, spaces_by_district, location_by_district)
    
    # Colores para cada aspecto
    aspect_colors <- c(
      "Materiales" = "#E76F51",
      "Espacios" = "#2A9D8F", 
      "Ubicación" = "#E9C46A"
    )
    
    # Crear gráfico
    plot_ly(all_data, x = ~district, y = ~mean_value, color = ~aspect,
           colors = aspect_colors, type = "bar") %>%
      layout(
        title = "Comparación de Satisfacción por Distrito y Aspecto",
        xaxis = list(title = "Distrito"),
        yaxis = list(title = "Nivel de Satisfacción (1-10)", range = c(0, 10)),
        barmode = "group",
        legend = list(title = list(text = "Aspecto"))
      )
  })
}