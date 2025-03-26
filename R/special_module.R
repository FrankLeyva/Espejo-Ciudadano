create_report_statistics <- function(survey_data) {
  # Define services and questions
  services <- c(
    "Agua potable, drenaje o alcantarillado",
    "Alumbrado público",
    "Baches, calles o pavimentación",
    "Energía eléctrica",
    "Recolección de basura"
  )
  
  # Questions in order matching the services
  report_questions <- c("Q32", "Q48", "Q52", "Q37", "Q42")
  addressed_questions <- c("Q33", "Q49", "Q53", "Q38", "Q43")
  
  # Define colors for services
  service_colors <- c(
    "#F4A261",  # Orange for Agua potable
    "#6969B3",  # Purple for Alumbrado público
    "#2A9D8F",  # Teal for Baches
    "#E86486",  # Pink for Energía eléctrica
    "#DAA520" #Yellow for Recolección de basura
  )
  
  # Initialize data frames to store results
  report_percentages <- numeric(length(services))
  addressed_percentages <- numeric(length(services))
  
  # Calculate percentages for each service
  for (i in 1:length(services)) {
    # Calculate report percentage
    report_q <- report_questions[i]
    report_values <- survey_data[[report_q]]
    report_values <- report_values[!is.na(report_values)]
    
    # Consider values of 1, TRUE, Yes, or "true" as positive responses
    made_report <- report_values == "1" | 
                  report_values == "TRUE" | 
                  report_values == "Yes" |
                  report_values == "true"
    
    report_percentages[i] <- 100 * mean(made_report, na.rm = TRUE)
    
    # Get addressed percentage (only for those who made reports)
    addressed_q <- addressed_questions[i]
    addressed_values <- survey_data[[addressed_q]]
    
    # Get indices of people who made reports
    made_report_indices <- which(made_report)
    
    # Calculate addressed percentage only for those who made reports
    if (length(made_report_indices) > 0) {
      addressed_filtered <- addressed_values[made_report_indices]
      addressed_filtered <- addressed_filtered[!is.na(addressed_filtered)]
      
      if (length(addressed_filtered) > 0) {
        addressed_percentages[i] <- 100 * mean(
          addressed_filtered == "1" | 
          addressed_filtered == "TRUE" | 
          addressed_filtered == "Yes" |
          addressed_filtered == "true", 
          na.rm = TRUE
        )
      } else {
        addressed_percentages[i] <- 0
      }
    } else {
      addressed_percentages[i] <- 0
    }
  }
  
  # Create data frame for plotting
  plot_data <- data.frame(
    service = factor(services, levels = services),  # Keep order consistent
    report = report_percentages,
    addressed = addressed_percentages,
    color = service_colors
  )
  
  # Create a clean basic plot
  p <- plot_ly() %>%
    add_bars(
      data = plot_data,
      y = ~service,
      x = ~report,
      orientation = "h",
      marker = list(color = ~color),
      name = "Reporte",
      text = ~paste0(round(report, 1), "%"),
      textposition = "inside",
      insidetextfont = list(color = "white", size = 14),
      hoverinfo = "text",
      hovertext = ~paste0(service, ": ", round(report, 1), "%")
    )
  
  # Add annotations for addressed percentages with boxes and labels
  annotations <- list()
  
  # Create annotation index counter
  annot_idx <- 1
  
  for (i in 1:nrow(plot_data)) {
    # Position for boxes and annotations
    x_pos <- plot_data$report[i] + 0.5
    y_pos <- i-1  # plotly uses 0-based indices for categorical axes
    
    # Add "% atendido" label above box
    annotations[[annot_idx]] <- list(
      x = x_pos,
      y = y_pos - 0.3,  # Position slightly above
      text = "% atendido",
      showarrow = FALSE,
      font = list(size = 10, color = "#555555"),
      xanchor = "center",
      yanchor = "bottom"
    )
    annot_idx <- annot_idx + 1
    
    # Add percentage in box
    annotations[[annot_idx]] <- list(
      x = x_pos,
      y = y_pos,
      text = paste0("<b>", round(plot_data$addressed[i], 1), "%</b>"),
      showarrow = FALSE,
      font = list(size = 14, color = "#333333"),
      bgcolor = "#F9F9F9",
      bordercolor = "#DDDDDD",
      borderwidth = 2,
      borderpad = 5,
      xanchor = "center"
    )
    annot_idx <- annot_idx + 1
  }
  
  # Clean up the layout
  p <- p %>% layout(
    title = list(
      text = "INTERPUSO ALGÚN REPORTE RELACIONADO AL SERVICIO...",
      font = list(size = 16)
    ),
    xaxis = list(
      title = "",
      range = c(0, 15),  # Changed to 15 as requested
      showgrid = FALSE,
      zeroline = TRUE,
      showticklabels = TRUE,
      tickvals = c(0, 15),
      ticktext = c("0", "15")
    ),
    yaxis = list(
      title = "",
      autorange = "reversed"
    ),
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = -0.2
    ),
    margin = list(l = 180, r = 180, t = 80, b = 80),
    annotations = annotations
  )
  
  return(p)
}

# Special functions for the overview dashboard

# Function to calculate average satisfaction for public services
calculate_service_averages <- function(survey_data) {
  # Define service questions and their labels
  service_questions <- c(
    "Q29" = "Agua",
    "Q30" = "Drenaje y alcantarillado",
    "Q35" = "CFE",
    "Q40" = "Recolección de basura",
    "Q45" = "Alumbrado público",
    "Q51" = "Calles y pavimentación",
    "Q55" = "Áreas verdes",
    "Q56" = "Unidades deportivas",
    "Q58" = "Bibliotecas",
    "Q59" = "Centros comunitarios",
    "Q60" = "Espacios para discapacitados",
    "Q61" = "Parques",
    "Q62" = "Transporte público"
  )
  
  # Calculate averages for each service
  service_averages <- sapply(names(service_questions), function(q_id) {
    values <- as.numeric(survey_data[[q_id]])
    values <- values[!is.na(values)]
    mean(values, na.rm = TRUE)
  })
  
  # Create dataframe for plotting
  result_df <- data.frame(
    service = service_questions,
    average = service_averages
  )
  
  # Sort by average (descending)
  result_df <- result_df[order(result_df$average, decreasing = TRUE),]
  
  return(result_df)
}

# Function to create service averages barplot
create_service_averages_plot <- function(survey_data, custom_theme = NULL) {
  # Get service averages
  service_data <- calculate_service_averages(survey_data)
  
  # Get colors from theme
  bar_color <- if (!is.null(custom_theme) && !is.null(custom_theme$colors$primary)) {
    custom_theme$colors$primary
  } else {
    "#1f77b4"  # Default blue
  }
  
  # Create barplot
  plot_ly(
    data = service_data,
    y = ~reorder(service, average),
    x = ~average,
    type = "bar",
    orientation = "h",
    marker = list(color = bar_color),
    text = ~paste0(round(average, 1)),
    textposition = "auto",
    hoverinfo = "text",
    hovertext = ~paste0(service, ": ", round(average, 1), "/10")
  ) %>%
    layout(
      title = list(
        text = "Satisfacción Promedio con Servicios Públicos",
        font = if (!is.null(custom_theme) && !is.null(custom_theme$typography)) {
          list(
            family = custom_theme$typography$font_family,
            size = custom_theme$typography$sizes$title
          )
        } else {
          list(family = "Arial", size = 16)
        }
      ),
      xaxis = list(
        title = "Satisfacción Promedio (1-10)",
        range = c(0, 10)
      ),
      yaxis = list(
        title = ""
      ),
      margin = list(l = 150)  # More space for service labels
    )
}

# Function to calculate percentage of homes with students
calculate_student_presence <- function(survey_data) {
  # Get district column
  districts <- unique(survey_data$DISTRICT)
  districts <- districts[!is.na(districts)]
  
  # Function to check if any education level is present (1 in any Q6.x column)
  has_student <- function(row) {
    education_cols <- grep("^Q6\\.", names(survey_data), value = TRUE)
    any(sapply(education_cols, function(col) row[[col]] == "1"), na.rm = TRUE)
  }
  
  # Calculate percentages by district
  district_percentages <- sapply(districts, function(d) {
    district_rows <- survey_data[survey_data$DISTRICT == d, ]
    # Calculate percentage of homes with students
    student_count <- sum(apply(district_rows, 1, has_student), na.rm = TRUE)
    percentage <- 100 * student_count / nrow(district_rows)
    return(percentage)
  })
  
  # Create dataframe for plotting
  result_df <- data.frame(
    district = districts,
    percentage = district_percentages
  )
  
  return(result_df)
}

create_education_overview <- function(survey_data, custom_theme = NULL) {
  #  # Cargar datos de encuesta de Percepción 2024
  survey_data <- reactive({
    load_survey_data("PER_2024_V2")
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
  
    req(student_data(), geo_data())
    
    # Obtener datos geográficos
    geo_data_df <- geo_data()
    
    # Calcular porcentajes por distrito
    district_stats <- student_data() %>%
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
    district_colors <- if (!is.null(current_theme()$palettes$district)) {
      current_theme()$palettes$district
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
create_healthcare_overview <- function(survey_data, custom_theme = NULL) {
  # Prepare data for Q19 - histogram of general satisfaction with health services
  healthcare_data <- survey_data
  
  # Convert Q19 to numeric
  healthcare_data$Q19 <- as.numeric(as.character(healthcare_data$Q19))
  
  # Remove NA values
  healthcare_data <- healthcare_data[!is.na(healthcare_data$Q19), ]
  
  # Get colors from theme
  bar_color <- if (!is.null(custom_theme)) {
    custom_theme$colors$primary
  } else {
    theme_config$colors$primary
  }
  
  line_color <- if (!is.null(custom_theme)) {
    custom_theme$colors$neutral
  } else {
    theme_config$colors$neutral
  }
  
  # Create histogram
  plot_ly(
    data = healthcare_data,
    x = ~Q19,
    type = "histogram",
    nbinsx = 10,
    marker = list(
      color = bar_color,
      line = list(
        color = line_color,
        width = 1
      )
    )
  ) %>%
    apply_plotly_theme(
      title = "Distribución de satisfacción con servicios de salud",
      xlab = "Nivel de satisfacción (1-10)",
      ylab = "Frecuencia",
      custom_theme = custom_theme
    )
}

create_utilities_overview <- function(survey_data, custom_theme = NULL) {
  # Prepare data for utility services satisfaction averages
  utility_questions <- c(
    "Q29", "Q30", "Q35", "Q40", "Q45", "Q51", "Q55", 
    "Q56", "Q58", "Q59", "Q60", "Q61", "Q62"
  )
  
  utility_labels <- c(
    "Agua", "Drenaje", "CFE", "Recolección de basura",
    "Alumbrado", "Calles", "Áreas verdes", "Unidades deportivas", 
    "Bibliotecas", "Centros comunitarios", "Espacios p/discapacidad", 
    "Parques", "Transporte público"
  )
  
  # Calculate mean for each service
  service_means <- sapply(utility_questions, function(q) {
    values <- as.numeric(as.character(survey_data[[q]]))
    mean(values, na.rm = TRUE)
  })
  
  # Create data frame for plotting
  plot_data <- data.frame(
    service = factor(utility_labels, levels = utility_labels[order(service_means, decreasing = TRUE)]),
    mean_satisfaction = service_means[order(service_means, decreasing = TRUE)]
  )
  
  # Get color from theme
  bar_color <- if (!is.null(custom_theme)) {
    custom_theme$colors$primary
  } else {
    theme_config$colors$primary
  }
  
  # Create horizontal bar chart
  plot_ly(
    data = plot_data,
    y = ~service,
    x = ~mean_satisfaction,
    type = "bar",
    orientation = 'h',
    marker = list(
      color = bar_color
    ),
    text = ~paste0(service, ": ", round(mean_satisfaction, 2)),
    hoverinfo = "text"
  ) %>%
    apply_plotly_theme(
      title = "Satisfacción promedio con servicios públicos",
      xlab = "Satisfacción promedio (1-10)",
      ylab = "",
      custom_theme = custom_theme
    ) %>%
    layout(
      xaxis = list(range = c(0, 10))  # Set x-axis from 0 to 10
    )
}

create_housing_overview <- function(survey_data, geo_data, custom_theme = NULL) {
  # Prepare data for Q25 - housing satisfaction map
  housing_data <- survey_data
  
  # Convert Q25 to numeric
  housing_data$Q25 <- as.numeric(as.character(housing_data$Q25))
  
  # Calculate district averages
  district_stats <- housing_data %>%
    filter(!is.na(Q25), !is.na(DISTRICT)) %>%
    group_by(DISTRICT) %>%
    summarise(
      mean_value = mean(Q25, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  
  # Convert district to numeric for matching
  district_stats$district_num <- as.numeric(as.character(district_stats$DISTRICT))
  
  # Find highest and lowest districts
  highest_district <- district_stats %>% 
    filter(mean_value == max(mean_value, na.rm = TRUE)) %>% 
    pull(district_num)
    
  lowest_district <- district_stats %>% 
    filter(mean_value == min(mean_value, na.rm = TRUE)) %>% 
    pull(district_num)
  
  # Calculate centroids for label placement
  geo_data$centroid <- sf::st_centroid(geo_data$geometry)
  centroids <- sf::st_coordinates(geo_data$centroid)
  geo_data$lng <- centroids[,1]
  geo_data$lat <- centroids[,2]
  
  # Get district colors from theme or use default
  district_colors <- if (!is.null(custom_theme)) {
    custom_theme$palettes$district
  } else {
    c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
     "#8c564b", "#e377c2", "#bcbd22", "#17becf")
  }
  
  # Create map
  map <- leaflet(geo_data) %>%
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
  for (i in 1:nrow(geo_data)) {
    dist_num <- geo_data$No_Distrit[i]
    match_idx <- which(district_stats$district_num == dist_num)
    
    if (length(match_idx) > 0 && match_idx <= nrow(district_stats)) {
      mean_val <- district_stats$mean_value[match_idx]
      
      # Skip if missing data
      if (is.na(mean_val)) next
      
      # Determine label color based on high/low value
      bg_color <- "#FFFFFF"  # Default white
      text_color <- "#000000"  # Default black
      
      if (!is.na(dist_num) && dist_num %in% highest_district) {
        bg_color <- "#4CAF50"  # Green for highest
        text_color <- "#FFFFFF"
      } else if (!is.na(dist_num) && dist_num %in% lowest_district) {
        bg_color <- "#F44336"  # Red for lowest
        text_color <- "#FFFFFF"
      }
      
      # Create label with district number
      label_html <- sprintf(
        '<div style="background-color: %s; color: %s; padding: 5px; border-radius: 3px; font-weight: bold; text-align: center;">Distrito %s<br>%.2f</div>',
        bg_color, text_color, dist_num, mean_val
      )
      
      # Add label marker
      map <- map %>% 
        addLabelOnlyMarkers(
          lng = geo_data$lng[i],
          lat = geo_data$lat[i],
          label = lapply(list(label_html), HTML),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE
          )
        )
    }
  }
  
  # Calculate overall average
  overall_mean <- round(mean(district_stats$mean_value, na.rm = TRUE), 1)
  
  # Add overall average control
  map %>% addControl(
    html = sprintf(
      '<div style="background-color: #333333; color: white; padding: 5px; border-radius: 3px;"><strong>Satisfacción vivienda promedio: %.1f</strong></div>',
      overall_mean
    ),
    position = "topright"
  )
}
