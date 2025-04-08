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
# Functions for government dashboard visualizations

# Function for Card 1: Officials Knowledge Pie Charts
create_officials_knowledge_pie <- function(data, question_id, official_type, custom_theme = NULL) {
  # Extract the data for the specified question
  values <- data[[question_id]]
  values <- values[!is.na(values)]
  
  # Count responses
  if(length(values) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Handle the reversed mapping for Q7
  if(question_id == "Q7") {
    # For Q7: 1 = No conoce, 2 = Sí conoce
    knows_yes <- sum(values == "2", na.rm = TRUE)
    knows_no <- sum(values == "1", na.rm = TRUE)
  } else {
    # For Q5 and Q9: 1 = Sí conoce, 2 = No conoce
    knows_yes <- sum(values == "1", na.rm = TRUE)
    knows_no <- sum(values == "2", na.rm = TRUE)
  }
  
  total <- knows_yes + knows_no
  
  # Calculate percentages
  perc_yes <- round(100 * knows_yes / total, 1)
  perc_no <- round(100 * knows_no / total, 1)
  
  # Prepare data for pie chart
  pie_data <- data.frame(
    Response = c("Sí conoce", "No conoce"),
    Count = c(knows_yes, knows_no),
    Percentage = c(perc_yes, perc_no)
  )
  
  # Get colors from custom theme if provided
  pie_colors <- if (!is.null(custom_theme)) {
    c(custom_theme$colors$primary, custom_theme$colors$secondary)
  } else {
    c("#1f77b4", "#E74C3C") # Default colors
  }
  
  # Create pie chart
  plot_ly(
    labels = ~pie_data$Response,
    values = ~pie_data$Count,
    type = "pie",
    textinfo = "label+percent",
    hoverinfo = "text",
    text = ~paste0(pie_data$Response, ": ", pie_data$Count, " (", pie_data$Percentage, "%)"),
    marker = list(
      colors = pie_colors
    )
  ) %>%
    layout(
      title = list(
        text = paste("¿Conoce el nombre de su", official_type, "?"),
        font = if (!is.null(custom_theme)) {
          list(
            family = custom_theme$typography$font_family,
            size = custom_theme$typography$sizes$title,
            color = custom_theme$colors$text
          )
        } else {
          list(
            family = "Arial",
            size = 16,
            color = "#2C3E50"
          )
        }
      ),
      showlegend = TRUE
    )
}

# Function for Card 2: Inequality Perception Pie Chart
create_inequality_perception_pie <- function(data, custom_theme = NULL) {
  # Extract Q87 data for perception of inequality
  values <- data[["Q87"]]
  values <- values[!is.na(values)]
  
  if(length(values) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Create frequency table
  inequality_table <- table(values)
  
  # Create mapping for the 5-point scale
  inequality_labels <- c(
    "1" = "Muy alta",
    "2" = "Alta",
    "3" = "Media",
    "4" = "Baja",
    "5" = "Muy baja"
  )
  
  # Prepare data for pie chart
  pie_data <- data.frame(
    Response = sapply(names(inequality_table), function(x) inequality_labels[x]),
    Count = as.numeric(inequality_table),
    stringsAsFactors = FALSE
  ) 
pie_data <- pie_data %>%
  drop_na()    
  # Calculate percentages
  pie_data$Percentage <- round(100 * pie_data$Count / sum(pie_data$Count), 1)
  
  # Get colors from custom theme if provided
  colorscale <- if (!is.null(custom_theme)) {
    colorRampPalette(c(custom_theme$colors$primary, custom_theme$colors$highlight))(nrow(pie_data))
  } else {
    colorRampPalette(c("#1f77b4", "#3498DB"))(nrow(pie_data))
  }
  
  # Create pie chart
  plot_ly(
    labels = ~pie_data$Response,
    values = ~pie_data$Count,
    type = "pie",
    textinfo = "label+percent",
    hoverinfo = "text",
    text = ~paste0(pie_data$Response, ": ", pie_data$Count, " (", pie_data$Percentage, "%)"),
    marker = list(
      colors = colorscale
    )
  ) %>%
    layout(
      title = list(
        text = "¿Cómo considera la desigualdad en Ciudad Juárez?",
        font = if (!is.null(custom_theme)) {
          list(
            family = custom_theme$typography$font_family,
            size = custom_theme$typography$sizes$title,
            color = custom_theme$colors$text
          )
        } else {
          list(
            family = "Arial",
            size = 16,
            color = "#2C3E50"
          )
        }
      ),
      showlegend = TRUE
    )
}

# Function for Card 3: Government Expectations Bar Plot
create_government_expectations_plot <- function(data, custom_theme = NULL) {
  # Extract data for government expectations (Q18, Q19, Q20)
  federal_exp <- as.numeric(data[["Q19"]])
  state_exp <- as.numeric(data[["Q20"]])
  city_exp <- as.numeric(data[["Q21"]])
  
  # Calculate means and remove NAs
  federal_mean <- mean(federal_exp, na.rm = TRUE)
  state_mean <- mean(state_exp, na.rm = TRUE)
  city_mean <- mean(city_exp, na.rm = TRUE)
  
  # Prepare data for bar chart
  exp_data <- data.frame(
    Government = c("Federal", "Estatal", "Municipal"),
    Expectation = c(federal_mean, state_mean, city_mean)
  )
  
  # Round values for display
  exp_data$Expectation_rounded <- round(exp_data$Expectation, 1)
  
  # Get colors from custom theme if provided
  bar_colors <- if (!is.null(custom_theme)) {
    colorRampPalette(c(custom_theme$colors$primary, custom_theme$colors$highlight))(3)
  } else {
    c("#1f77b4", "#ff7f0e", "#2ca02c")
  }
  
  # Create bar chart
  plot_ly(
    data = exp_data,
    x = ~Government,
    y = ~Expectation,
    type = "bar",
    marker = list(
      color = bar_colors
    ),
    text = ~paste0("Expectativa: ", Expectation_rounded, "/10"),
    hoverinfo = "text"
  ) %>%
    apply_plotly_theme(
      title = "Expectativas Ciudadanas por Nivel de Gobierno (Escala 1-10)",
      xlab = "Nivel de Gobierno",
      ylab = "Expectativa Promedio",
      custom_theme = custom_theme
    ) %>%
    layout(
      yaxis = list(range = c(0, 10)) # Scale from 0 to 10
    )
}
create_important_problems_plot <- function(data, custom_theme = NULL) {
  # Extract data for important problems (Q81 and Q82)
  problem1 <- data[["Q81"]]
  problem2 <- data[["Q82"]]
  
  # Combine both responses
  all_problems <- c(problem1, problem2)
  all_problems <- all_problems[!is.na(all_problems)]
  
  if(length(all_problems) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No hay datos suficientes para visualizar"))
  }
  
  # Create frequency table
  problems_table <- table(all_problems)
  
  # Create correct mapping for problem categories based on provided info
  problem_labels <- c(
    "1" = "Servicios básicos",
    "2" = "Drogadicción",
    "3" = "Transporte público",
    "4" = "Falta de hospitales/clínicas de salud",
    "5" = "Inseguridad/Violencia",
    "6" = "Pobreza",
    "7" = "Corrupción",
    "8" = "Falta de valores",
    "9" = "Desempleo",
    "10" = "Economía/Crisis",
    "11" = "Impunidad",
    "12" = "Participación ciudadana",
    "13" = "Alumbrado público",
    "14" = "Covid 19",
    "15" = "Calles y pavimentación",
    "16" = "Drenaje pluvial", 
    "17" = "Infraestructura",
    "18" = "Problemas medio ambientales(agua, aire, basura)",
    "19" = "Otro (especificar cuál)"
  )
  
  # Create data frame for all categories with data
  bar_data <- data.frame(
    Category = names(problems_table),
    Count = as.numeric(problems_table),
    stringsAsFactors = FALSE
  )
  
  # Add problem labels
  bar_data$Problem <- sapply(bar_data$Category, function(x) {
    if(x %in% names(problem_labels)) problem_labels[x] else paste("Categoría", x)
  })
  
  # Calculate percentages
  total_responses <- sum(bar_data$Count)
  bar_data$Percentage <- round(100 * bar_data$Count / total_responses, 2)
  
  # Sort by count in descending order
  bar_data <- bar_data[order(-bar_data$Count), ]
  
  # Get colors from custom theme
  primary_color <- if (!is.null(custom_theme)) custom_theme$colors$primary else "#1f77b4"  # Default blue
  highlight_color <- if (!is.null(custom_theme)) custom_theme$colors$highlight else "#28a745"  # Default green
  
  # Create single color vector for all bars
  colors <- rep(primary_color, nrow(bar_data))
  
  # Highlight top three with the same highlight color
  if(nrow(bar_data) >= 3) {
    colors[1:3] <- highlight_color
  } else {
    colors[1:nrow(bar_data)] <- highlight_color
  }
  
  # Create horizontal bar chart
  plot_ly(
    data = bar_data,
    y = ~Problem,
    x = ~Count,
    type = "bar",
    orientation = 'h',
    marker = list(
      color = colors
    ),
    hoverinfo = "text",
    text = ~paste0(Percentage, "%")
  ) %>%
    layout(
      title = "Distribución de Problemas Importantes",
      xaxis = list(
        title = "Frecuencia",
        showgrid = TRUE,
        gridcolor = "#E1E1E1"
      ),
      yaxis = list(
        title = "",
        automargin = TRUE,
        categoryorder = 'total ascending'
      ),
      margin = list(l = 160, r = 20, t = 40, b = 30),
      font = list(
        family = "Arial",
        size = 12
      )
    ) %>%
    config(displayModeBar = FALSE)
}
# Function to create bicycle distribution pie chart
create_bicycle_distribution <- function(survey_data, custom_theme = NULL) {
  # Extract bicycle counts from Q67
  bicycle_counts <- as.numeric(survey_data$Q67)
  bicycle_counts <- bicycle_counts[!is.na(bicycle_counts)]
  
  # Categorize the counts
  categories <- c("Ninguno", "1 Bicicleta", "2 Bicicletas", "3 Bicicletas", "4 o más bicicletas")
  
  # Calculate counts for each category
  category_counts <- c(
    sum(bicycle_counts < 1, na.rm = TRUE),
    sum(bicycle_counts == 1, na.rm = TRUE),
    sum(bicycle_counts == 2, na.rm = TRUE),
    sum(bicycle_counts == 3, na.rm = TRUE),
    sum(bicycle_counts > 3, na.rm = TRUE)
  )
  
  # Calculate percentages
  percentages <- round(100 * category_counts / sum(category_counts), 1)
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Category = categories,
    Count = category_counts,
    Percentage = percentages
  )
  
  # Get colors from custom theme if provided
  pie_colors <- if (!is.null(custom_theme)) {
    colorRampPalette(c(custom_theme$colors$primary, custom_theme$colors$highlight))(length(categories))
  } else {
    colorRampPalette(c("#1f77b4", "#3498DB"))(length(categories))
  }
  
  # Create pie chart
  plot_ly(
    labels = ~plot_data$Category,
    values = ~plot_data$Count,
    type = "pie",
    textinfo = "label+percent",
    hoverinfo = "text",
    text = ~paste0(plot_data$Category, ": ", plot_data$Count, " hogares (", plot_data$Percentage, "%)"),
    marker = list(
      colors = pie_colors
    )
  ) %>%
    layout(
      title = list(
        text = "Distribución de Bicicletas por Hogar",
        font = if (!is.null(custom_theme)) {
          list(
            family = custom_theme$typography$font_family,
            size = custom_theme$typography$sizes$title,
            color = custom_theme$colors$text
          )
        } else {
          list(
            family = "Arial",
            size = 16,
            color = "#2C3E50"
          )
        }
      ),
      legend = list(
        orientation = "h",
        y = -0.1
      )
    )
}

# Function to create vehicle distribution pie chart
create_vehicle_distribution <- function(survey_data, custom_theme = NULL) {
  # Extract vehicle counts from Q66
  vehicle_counts <- as.numeric(survey_data$Q66)
  vehicle_counts <- vehicle_counts[!is.na(vehicle_counts)]
  
  # Categorize the counts
  categories <- c("Ninguno", "1 Vehículo", "2 Vehículos", "3 Vehículos", "4 o más vehículos")
  
  # Calculate counts for each category
  category_counts <- c(
    sum(vehicle_counts < 1, na.rm = TRUE),
    sum(vehicle_counts == 1, na.rm = TRUE),
    sum(vehicle_counts == 2, na.rm = TRUE),
    sum(vehicle_counts == 3, na.rm = TRUE),
    sum(vehicle_counts > 3, na.rm = TRUE)
  )
  
  # Calculate percentages
  percentages <- round(100 * category_counts / sum(category_counts), 1)
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Category = categories,
    Count = category_counts,
    Percentage = percentages
  )
  
  # Get colors from custom theme if provided
  pie_colors <- if (!is.null(custom_theme)) {
    colorRampPalette(c(custom_theme$colors$secondary, custom_theme$colors$highlight))(length(categories))
  } else {
    colorRampPalette(c("#E74C3C", "#F39C12"))(length(categories))
  }
  
  # Create pie chart
  plot_ly(
    labels = ~plot_data$Category,
    values = ~plot_data$Count,
    type = "pie",
    textinfo = "label+percent",
    hoverinfo = "text",
    text = ~paste0(plot_data$Category, ": ", plot_data$Count, " hogares (", plot_data$Percentage, "%)"),
    marker = list(
      colors = pie_colors
    )
  ) %>%
    layout(
      title = list(
        text = "Distribución de Vehículos Motorizados por Hogar",
        font = if (!is.null(custom_theme)) {
          list(
            family = custom_theme$typography$font_family,
            size = custom_theme$typography$sizes$title,
            color = custom_theme$colors$text
          )
        } else {
          list(
            family = "Arial",
            size = 16,
            color = "#2C3E50"
          )
        }
      ),
      legend = list(
        orientation = "h",
        y = -0.1
      )
    )
}
# Function to create transportation issues bar plot
create_transport_issues_plot <- function(survey_data, issue_type = "bus", custom_theme = NULL) {
  # Define questions and labels based on issue type
  if (issue_type == "bus") {
    # Bus transportation issues (Q76.1 to Q76.7)
    question_ids <- paste0("Q76.", 1:7)
    title <- "Insatisfacción con Aspectos del Servicio de Camión/Rutera"
  } else {
    # Juarez Bus transportation issues (Q79.1 to Q79.7)
    question_ids <- paste0("Q79.", 1:7)
    title <- "Insatisfacción con Aspectos del Servicio de Juárez Bus"
  }
  
  # Define issue labels
  issue_labels <- c(
    "Tiempo de espera", 
    "Estado de la unidad", 
    "Estado de la parada", 
    "Trato de los choferes",
    "Conducción de la unidad", 
    "Tarifa", 
    "Otro"
  )
  
  # Calculate percentages for each issue
  issue_percentages <- numeric(length(question_ids))
  for (i in 1:length(question_ids)) {
    # Extract binary values (1 = Yes, meaning dissatisfied)
    values <- survey_data[[question_ids[i]]]
    
    # Calculate percentage of "Yes" responses
    issue_percentages[i] <- 100 * sum(values == "1", na.rm = TRUE) / length(values)
  }
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Issue = issue_labels,
    Percentage = round(issue_percentages, 1)
  )
  
  # Sort by percentage for better visualization
  plot_data <- plot_data[order(-plot_data$Percentage), ]
  
  # Get colors from custom theme if provided
  bar_color <- if (!is.null(custom_theme)) {
    custom_theme$colors$primary
  } else {
    "#1f77b4"  # Default blue
  }
  
  # Create horizontal bar chart
  plot_ly(
    data = plot_data,
    y = ~Issue,
    x = ~Percentage,
    type = "bar",
    orientation = "h",
    marker = list(
      color = bar_color
    ),
    text = ~paste0(Percentage, "%"),
    textposition = "auto",
    hoverinfo = "text",
    hovertext = ~paste0(Issue, ": ", Percentage, "%")
  ) %>%
    apply_plotly_theme(
      title = title,
      xlab = "Porcentaje",
      ylab = "",
      custom_theme = custom_theme
    ) %>%
    layout(
      xaxis = list(
        range = c(0, 10)
      ),
      yaxis = list(
        categoryorder = "total ascending"
      )
    )
}

# Function to create transportation comparison plot
create_transportation_comparison <- function(survey_data, custom_theme = NULL) {
  # Extract satisfaction ratings for both transportation types
  bus_ratings <- as.numeric(survey_data$Q75)
  bus_ratings <- bus_ratings[!is.na(bus_ratings)]
  
  juarez_bus_ratings <- as.numeric(survey_data$Q79)
  juarez_bus_ratings <- juarez_bus_ratings[!is.na(juarez_bus_ratings)]
  
  # Calculate average ratings
  bus_avg <- mean(bus_ratings, na.rm = TRUE)
  juarez_bus_avg <- mean(juarez_bus_ratings, na.rm = TRUE)
  
  # Calculate median ratings
  bus_median <- median(bus_ratings, na.rm = TRUE)
  juarez_bus_median <- median(juarez_bus_ratings, na.rm = TRUE)
  
  # Get colors from custom theme if provided
  colors <- if (!is.null(custom_theme)) {
    c(custom_theme$colors$primary, custom_theme$colors$secondary)
  } else {
    c("#1f77b4", "#ff7f0e")
  }
  
  # Create comparison plot
  plot_data <- data.frame(
    TransportType = c("Camión/Rutera", "Juárez Bus"),
    Average = c(bus_avg, juarez_bus_avg),
    Median = c(bus_median, juarez_bus_median)
  )
  
  # Melt the data for easier plotting
  plot_data_melted <- reshape2::melt(
    plot_data, 
    id.vars = "TransportType", 
    variable.name = "Metric",
    value.name = "Rating"
  )
  
  # Label mapping for Spanish
  metric_labels <- c("Average" = "Promedio", "Median" = "Mediana")
  plot_data_melted$Metric <- factor(plot_data_melted$Metric, 
                                   levels = c("Average", "Median"),
                                   labels = c("Promedio", "Mediana"))
  
  # Create grouped bar chart
  plot_ly(
    data = plot_data_melted,
    x = ~TransportType,
    y = ~Rating,
    color = ~Metric,
    type = "bar",
    colors = colors,
    text = ~paste0(round(Rating, 1)),
    textposition = "auto",
    hoverinfo = "text",
    hovertext = ~paste0(TransportType, " - ", Metric, ": ", round(Rating, 1))
  ) %>%
    apply_plotly_theme(
      title = "Comparación de Satisfacción por Tipo de Transporte",
      xlab = "Tipo de Transporte",
      ylab = "Calificación (1-10)",
      custom_theme = custom_theme
    ) %>%
    layout(
      yaxis = list(
        range = c(0, 10)
      ),
      barmode = "group"
    )
}
# Function to create environmental problems plot
create_env_problems_plot <- function(survey_data, custom_theme = NULL) {
  # Extract environmental problems data from Q97
  env_problems <- survey_data$Q97
  env_problems <- env_problems[!is.na(env_problems)]

  # Define problem categories
  problem_categories <- c(
    "1" = "Neumáticos/llantas tiradas",
    "2" = "Calles sucias/Basura en las calles",
    "3" = "Parques sucios/descuidados",
    "4" = "Falta de recolección de residuos",
    "5" = "Basureros clandestinos/Casas-terrenos",
    "6" = "Terrenos baldíos",
    "7" = "Otro"
  )

  # Create frequency table
  problem_table <- table(env_problems)

  # Create data frame for plotting
  plot_data <- data.frame(
    Problem = sapply(names(problem_table), function(x) problem_categories[x]),
    Count = as.numeric(problem_table),
    stringsAsFactors = FALSE
  )

  # Calculate percentages
  plot_data$Percentage <- round(100 * plot_data$Count / sum(plot_data$Count), 1)

  # Sort by count descending
  plot_data <- plot_data[order(-plot_data$Count), ]

  # Get color from custom theme if provided
  bar_color <- if (!is.null(custom_theme)) {
    custom_theme$colors$primary
  } else {
    "
#20c997"  # Default teal green
  }

  # Create bar chart
  plot_ly(
    data = plot_data,
    x = ~Problem,
    y = ~Percentage,
    type = "bar",
    marker = list(
      color = bar_color
    ),
    text = ~paste0(Percentage, "%"),
    textposition = "auto",
    hoverinfo = "text",
    hovertext = ~paste0(Problem, ": ", Count, " respuestas (", Percentage, "%)")
  ) %>%
    apply_plotly_theme(
      title = "Mayor Problema Ambiental en la Colonia",
      xlab = "",
      ylab = "Porcentaje",
      custom_theme = custom_theme
    ) %>%
    layout(
      xaxis = list(
        tickangle = 45,
          categoryorder = 'total ascending'
      )
    )
}

# Function to create environmental comparison plot
create_env_comparison_plot <- function(survey_data, custom_theme = NULL) {
  # Define the environmental aspect questions
  env_questions <- c("Q89", "Q90", "Q91", "Q92")
  env_labels <- c("Calidad del Aire", "Arbolado Urbano", "Limpieza de Calles", "Calidad del Agua")
  
  # Calculate averages for each aspect
  env_means <- sapply(env_questions, function(q) {
    values <- as.numeric(survey_data[[q]])
    mean(values, na.rm = TRUE)
  })
  
  # Calculate medians for each aspect
  env_medians <- sapply(env_questions, function(q) {
    values <- as.numeric(survey_data[[q]])
    median(values, na.rm = TRUE)
  })
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Aspect = env_labels,
    Average = env_means,
    Median = env_medians
  )
  
  # Melt the data for easier plotting
  plot_data_melted <- reshape2::melt(
    plot_data, 
    id.vars = "Aspect", 
    variable.name = "Metric",
    value.name = "Rating"
  )
  
  # Label mapping for Spanish
  metric_labels <- c("Average" = "Promedio", "Median" = "Mediana")
  plot_data_melted$Metric <- factor(plot_data_melted$Metric, 
                                   levels = c("Average", "Median"),
                                   labels = c("Promedio", "Mediana"))
  
  # Get colors from custom theme if provided
  colors <- if (!is.null(custom_theme)) {
    c(custom_theme$colors$primary, custom_theme$colors$secondary)
  } else {
    c("#20c997", "#28a745")  # Green tones
  }
  
  # Create grouped bar chart
  plot_ly(
    data = plot_data_melted,
    x = ~Aspect,
    y = ~Rating,
    color = ~Metric,
    type = "bar",
    colors = colors,
    text = ~paste0(round(Rating, 1)),
    textposition = "auto",
    hoverinfo = "text",
    hovertext = ~paste0(Aspect, " - ", Metric, ": ", round(Rating, 1))
  ) %>%
    apply_plotly_theme(
      title = "Comparación de Satisfacción con Aspectos Ambientales",
      xlab = "Aspecto Ambiental",
      ylab = "Calificación (1-10)",
      custom_theme = custom_theme
    ) %>%
    layout(
      yaxis = list(
        range = c(0, 10)
      ),
      xaxis = list(
        categoryorder = 'total ascending'
      ),
      barmode = "group"
    )
}


# Function to create environmental quality bar plot
create_env_quality_plot <- function(survey_data, custom_theme = NULL) {
  # Define the environmental aspect questions
  env_questions <- c("Q89", "Q90", "Q91", "Q92")
  env_labels <- c("Calidad del Aire", "Arbolado Urbano", "Limpieza de Calles", "Calidad del Agua")
  
  # Calculate averages for each aspect
  env_means <- sapply(env_questions, function(q) {
    values <- as.numeric(survey_data[[q]])
    mean(values, na.rm = TRUE)
  })
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Aspect = factor(env_labels, levels = env_labels),
    Average = env_means
  )
  
  # Get colors from custom theme
  bar_colors <- if (!is.null(custom_theme)) {
    colorRampPalette(c(custom_theme$colors$primary, custom_theme$colors$highlight))(length(env_questions))
  } else {
    c("#3498db", "#2ecc71", "#f1c40f", "#9b59b6")
  }
  
  # Create bar chart
  plot_ly(
    data = plot_data,
    x = ~Aspect,
    y = ~Average,
    type = "bar",
    marker = list(
      color = bar_colors
    ),
    text = ~paste0(round(Average, 1)),
    textposition = "auto",
    hoverinfo = "text",
    hovertext = ~paste0(Aspect, ": ", round(Average, 1), "/10")
  ) %>%
    apply_plotly_theme(
      title = "Satisfacción con Aspectos Ambientales",
      xlab = "",
      ylab = "Calificación Promedio (1-10)",
      custom_theme = custom_theme
    ) %>%
    layout(
      yaxis = list(
        range = c(0, 10)
      )
    )
}
create_transport_modes_plot <- function(survey_data, mode_type = "work", custom_theme = NULL) {
  # Define questions and labels based on mode type
  if (mode_type == "work") {
    # Work transportation questions (Q72.1 to Q72.11)
    question_ids <- paste0("Q72.", 1:11)
    title <- "Modos de Transporte para ir al Trabajo"
    mode_labels <- c(
      "Caminando", "Bicicleta", "Autobús escolar", "Autobús especial", 
      "Taxi", "Uber/Didi/InDriver", "Motocicleta", "Vehículo propio",
      "Camión/Rutera/Autobús", "Juarez Bus", "Otro medio"
    )
  } else {
    # General transportation questions (Q73.1 to Q73.11)
    question_ids <- paste0("Q73.", 1:11)
    title <- "Modos de Transporte General"
    mode_labels <- c(
      "Caminando", "Bicicleta", "Autobús escolar", "Autobús especial", 
      "Taxi", "Uber/Didi/InDriver", "Motocicleta", "Vehículo propio",
      "Camión/Rutera/Autobús", "Juarez Bus", "Otro medio"
    )
  }
  
  # Calculate percentages for each mode
  mode_percentages <- numeric(length(question_ids))
  for (i in 1:length(question_ids)) {
    # Extract binary values (1 = Yes)
    values <- survey_data[[question_ids[i]]]
    
    # Calculate percentage of "Yes" responses
    mode_percentages[i] <- 100 * sum(values == "1", na.rm = TRUE) / length(values)
  }
  
  # Create data frame for plotting
  plot_data <- data.frame(
    Mode = mode_labels,
    Percentage = round(mode_percentages, 1)
  )
  
  # Sort by percentage for better visualization
  plot_data <- plot_data[order(-plot_data$Percentage), ]
  
  # Get colors from custom theme if provided
  bar_color <- if (!is.null(custom_theme)) {
    custom_theme$colors$primary
  } else {
    "#1f77b4"  # Default blue
  }
  
  # Create horizontal bar chart
  plot_ly(
    data = plot_data,
    y = ~Mode,
    x = ~Percentage,
    type = "bar",
    orientation = "h",
    marker = list(
      color = bar_color
    ),
    text = ~paste0(Percentage, "%"),
    textposition = "auto",
    hoverinfo = "text",
    hovertext = ~paste0(Mode, ": ", Percentage, "%")
  ) %>%
    apply_plotly_theme(
      title = title,
      xlab = "Porcentaje",
      ylab = "",
      custom_theme = custom_theme
    ) %>%
    layout(
      xaxis = list(
        range = c(0, 100)
      ),
      yaxis = list(
        categoryorder = "total ascending"
      )
    )
}