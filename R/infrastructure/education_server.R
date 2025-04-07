# Función del servidor para el Dashboard de Educación
educationServer <- function(input, output, session,current_theme = NULL) {
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

  # Datos sobre hogares con estudiantes (Q6)
  student_data <- reactive({
    req(survey_data())
    
    # Preparar datos binarios para Q6
    prepare_binary_data(
      data = survey_data()$responses,
      question_id = "Q6",
      metadata = survey_data()$metadata
    )
  })
  
  # Datos de satisfacción con educación básica (Q7)
  basic_edu_data <- reactive({
    req(survey_data())
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q7",
      metadata = survey_data()$metadata
    )
  })
  
  # Datos de satisfacción con educación media superior (Q10)
  highschool_edu_data <- reactive({
    req(survey_data())
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q10",
      metadata = survey_data()$metadata
    )
  })
  
  # Datos de satisfacción con educación superior (Q13)
  college_edu_data <- reactive({
    req(survey_data())
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q13",
      metadata = survey_data()$metadata
    )
  })
  
  # Función para crear mapa binario (hogares con estudiantes)
  create_binary_map <- function(prepared_data, title) {
    req(prepared_data, geo_data())
    
    # Obtener datos geográficos
    geo_data_df <- geo_data()
    
    # Calcular porcentajes por distrito
    district_stats <- prepared_data %>%
      group_by(district) %>%
      summarise(
        total_responses = n(),
        positive_count = sum(binary_value, na.rm = TRUE),
        positive_percent = round(100 * mean(binary_value, na.rm = TRUE), 1),
        .groups = 'drop'
      )
    
    # Identificar distritos con porcentajes más altos y más bajos
    highest_district <- district_stats %>% 
      filter(positive_percent == max(positive_percent, na.rm = TRUE)) %>% 
      pull(district) %>% 
      as.character()
      
    lowest_district <- district_stats %>% 
      filter(positive_percent == min(positive_percent, na.rm = TRUE)) %>% 
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
    overall_percent <- round(mean(district_stats$positive_percent, na.rm = TRUE), 1)
    
    # Colores de distrito - usar colores del tema si están disponibles
    district_colors <- if (!is.null(active_theme()$palettes$district)) {
      active_theme()$palettes$district
    } else {
      c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
       "#8c564b", "#e377c2", "#bcbd22", "#17becf")
    }
    
    # Crear mapa
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
    
    # Añadir etiquetas de distrito
    for (i in 1:nrow(geo_data_df)) {
      dist_num <- geo_data_df$No_Distrit[i]
      match_idx <- which(district_stats$district_num == dist_num)
      
      if (length(match_idx) > 0 && match_idx <= nrow(district_stats)) {
        percent_val <- district_stats$positive_percent[match_idx]
        
        # Omitir si datos faltantes
        if (is.na(percent_val)) next
        
        # Determinar color de etiqueta según valor alto/bajo
        bg_color <- "#FFFFFF"  # Default white
        text_color <- "#000000"  # Default black
        
        if (as.character(dist_num) %in% highest_district) {
          bg_color <- "#4CAF50"  # Green for highest
          text_color <- "#FFFFFF"
        } else if (as.character(dist_num) %in% lowest_district) {
          bg_color <- "#F44336"  # Red for lowest
          text_color <- "#FFFFFF"
        }
        
        # Etiqueta mejorada con número de distrito
        label_html <- sprintf(
          '<div style="background-color: %s; color: %s; padding: 5px 10px; border-radius: 4px; font-weight: bold; text-align: center; box-shadow: 0 1px 3px rgba(0,0,0,0.3);">Distrito %s<br>%.1f%%</div>',
          bg_color, text_color, dist_num, percent_val
        )
        
        # Añadir marcador de etiqueta
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
    
    return(map)
  }
  
  # Función para crear mapa de satisfacción (intervalo)
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
    district_colors <- if (!is.null(active_theme()$palettes$district)) {
      active_theme()$palettes$district
    } else {
      c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
       "#8c564b", "#e377c2", "#bcbd22", "#17becf")
    }
    
    # Crear mapa
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
    
    # Añadir etiquetas de distrito
    for (i in 1:nrow(geo_data_df)) {
      dist_num <- geo_data_df$No_Distrit[i]
      match_idx <- which(district_stats$district_num == dist_num)
      
      if (length(match_idx) > 0 && match_idx <= nrow(district_stats)) {
        mean_val <- district_stats$mean_value[match_idx]
        
        # Omitir si datos faltantes
        if (is.na(mean_val)) next
        
        # Determinar color de etiqueta según valor alto/bajo
        bg_color <- "#FFFFFF"  # Default white
        text_color <- "#000000"  # Default black
        
        if (as.character(dist_num) %in% highest_district) {
          bg_color <- "#4CAF50"  # Green for highest
          text_color <- "#FFFFFF"
        } else if (as.character(dist_num) %in% lowest_district) {
          bg_color <- "#F44336"  # Red for lowest
          text_color <- "#FFFFFF"
        }
        
        # Etiqueta mejorada con número de distrito
        label_html <- sprintf(
          '<div style="background-color: %s; color: %s; padding: 5px 10px; border-radius: 4px; font-weight: bold; text-align: center; box-shadow: 0 1px 3px rgba(0,0,0,0.3);">Distrito %s<br>%.1f</div>',
          bg_color, text_color, dist_num, mean_val
        )
        
        # Añadir marcador de etiqueta
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
    
    return(map)
  }
  
  # Renderizar mapa de hogares con estudiantes
  output$students_map <- renderLeaflet({
    create_binary_map(student_data(), "Hogares con Estudiantes")
  })
  
  # Renderizar mapas de satisfacción con niveles educativos
  output$basic_education_map <- renderLeaflet({
    create_satisfaction_map(basic_edu_data(), "Satisfacción con Educación Básica")
  })
  
  output$highschool_education_map <- renderLeaflet({
    create_satisfaction_map(highschool_edu_data(), "Satisfacción con Educación Media Superior")
  })
  
  output$college_education_map <- renderLeaflet({
    create_satisfaction_map(college_edu_data(), "Satisfacción con Educación Superior")
  })
  
  # Promedio general de hogares con estudiantes
  output$students_avg <- renderText({
    req(student_data())
    avg_percent <- mean(student_data()$binary_value, na.rm = TRUE) * 100
    sprintf("%.1f%%", avg_percent)
  })
  
  # Promedios generales de satisfacción
  output$basic_education_avg <- renderText({
    req(basic_edu_data())
    avg <- mean(basic_edu_data()$value_num, na.rm = TRUE)
    sprintf("%.1f / 10", avg)
  })
  
  output$highschool_education_avg <- renderText({
    req(highschool_edu_data())
    avg <- mean(highschool_edu_data()$value_num, na.rm = TRUE)
    sprintf("%.1f / 10", avg)
  })
  
  output$college_education_avg <- renderText({
    req(college_edu_data())
    avg <- mean(college_edu_data()$value_num, na.rm = TRUE)
    sprintf("%.1f / 10", avg)
  })
  
  # Gráfico comparativo de satisfacción con niveles educativos
  output$education_comparison_plot <- renderPlotly({
    req(basic_edu_data(), highschool_edu_data(), college_edu_data())
    
    # Preparar datos para cada nivel educativo por distrito
    basic_by_district <- basic_edu_data() %>%
      group_by(district) %>%
      summarise(mean_value = mean(value_num, na.rm = TRUE), .groups = 'drop') %>%
      mutate(level = "Educación Básica")
    
    highschool_by_district <- highschool_edu_data() %>%
      group_by(district) %>%
      summarise(mean_value = mean(value_num, na.rm = TRUE), .groups = 'drop') %>%
      mutate(level = "Educación Media Superior")
    
    college_by_district <- college_edu_data() %>%
      group_by(district) %>%
      summarise(mean_value = mean(value_num, na.rm = TRUE), .groups = 'drop') %>%
      mutate(level = "Educación Superior")
    
    # Combinar datos
    all_data <- bind_rows(basic_by_district, highschool_by_district, college_by_district)
    
    # Colores para cada nivel educativo
    level_colors <- c(
      "Educación Básica" = "#2A9D8F",
      "Educación Media Superior" = "#E9C46A", 
      "Educación Superior" = "#E76F51"
    )
    
    # Crear gráfico
    plot_ly(all_data, x = ~district, y = ~mean_value, color = ~level,
           colors = level_colors, type = "bar") %>%
      layout(
        title = "Comparación de Satisfacción con Niveles Educativos por Distrito",
        xaxis = list(title = "Distrito"),
        yaxis = list(title = "Nivel de Satisfacción (1-10)", range = c(0, 10)),
        barmode = "group",
        legend = list(title = list(text = "Nivel Educativo"))
      )
  })
}