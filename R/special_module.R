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
      hoverinfo = "none",
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
      text = "",
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
    showlegend = FALSE,
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = -0.2
    ),
    margin = list(l = 180, r = 180, t = 80, b = 80),
    annotations = annotations
  )
  p <- p %>% config(
    modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", 
                               "zoomIn2d", "zoomOut2d", "autoScale2d", 
                               "hoverClosestCartesian", "hoverCompareCartesian","hoverClosestPie"),
    modeBarButtonsToAdd = c("resetScale2d", "toImage"),
    displaylogo=FALSE,
    locale = "es",
    responsive = TRUE
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

create_education_overview <- function(survey_data, geo_data, custom_theme = NULL) {
  # Check if we have data
  if (is.null(survey_data) || nrow(survey_data) == 0 || is.null(geo_data)) {
    return(leaflet() %>% 
             addTiles() %>%
             addControl("No hay datos suficientes para visualizar", position = "topright"))
  }
  
  # Prepare data about households with students (Q6)
  # First check if the Q6 structure exists
  if (!"Q6" %in% names(survey_data)) {
    # Try to find Q6.x columns (checkbox question style)
    q6_cols <- grep("^Q6\\.", names(survey_data), value = TRUE)
    
    if (length(q6_cols) > 0) {
      # Create a binary indicator for households with at least one student
      survey_data$has_student <- apply(survey_data[, q6_cols], 1, function(row) {
        any(row == "1" | row == "Selected", na.rm = TRUE)
      })
    } else {
      return(leaflet() %>% 
               addTiles() %>%
               addControl("Datos de educación no disponibles en esta encuesta", position = "topright"))
    }
  } else {
    # Simple Q6 column exists
    survey_data$has_student <- survey_data$Q6 == "1" | survey_data$Q6 == "Sí" | 
                            survey_data$Q6 == "Si" | survey_data$Q6 == "TRUE" | 
                            survey_data$Q6 == "true" | survey_data$Q6 == "Yes"
  }
  
  # Calculate percentages by district
  district_stats <- survey_data %>%
    group_by(DISTRICT) %>%
    summarise(
      total_responses = n(),
      positive_count = sum(has_student, na.rm = TRUE),
      positive_percent = round(100 * mean(has_student, na.rm = TRUE), 1),
      .groups = 'drop'
    )
  
  # Identify districts with highest and lowest percentages
  highest_district <- district_stats %>% 
    filter(!is.na(positive_percent)) %>%
    filter(positive_percent == max(positive_percent, na.rm = TRUE)) %>% 
    pull(DISTRICT) %>% 
    as.character()
    
  lowest_district <- district_stats %>% 
    filter(!is.na(positive_percent)) %>%
    filter(positive_percent == min(positive_percent, na.rm = TRUE)) %>% 
    pull(DISTRICT) %>% 
    as.character()
  
  # Convert district to numeric for proper matching
  district_stats$district_num <- as.numeric(as.character(district_stats$DISTRICT))
  
  # Calculate centroids for label placement
  geo_data$centroid <- sf::st_centroid(geo_data$geometry)
  centroids <- sf::st_coordinates(geo_data$centroid)
  geo_data$lng <- centroids[,1]
  geo_data$lat <- centroids[,2]
  
  # Calculate area and bounding boxes for better label placement
  geo_data$area <- as.numeric(sf::st_area(geo_data$geometry))
  
  # Create a sophisticated position adjustment system based on district relationships
  calculate_label_positions <- function(geo_data) {
    n <- nrow(geo_data)
    positions <- data.frame(
      district = geo_data$No_Distrit,
      lng = geo_data$lng,
      lat = geo_data$lat,
      offset_x = rep(0, n),
      offset_y = rep(0, n)
    )
    
    # Create a matrix of distances between districts
    distances <- matrix(0, nrow = n, ncol = n)
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        dist <- sqrt((positions$lng[i] - positions$lng[j])^2 + 
                    (positions$lat[i] - positions$lat[j])^2)
        distances[i,j] <- distances[j,i] <- dist
      }
    }
    
    # Define threshold for nearby districts
    distance_threshold <- mean(distances[distances > 0]) * 0.6
    
    # Identify clusters of nearby districts
    nearby <- distances < distance_threshold & distances > 0
    
    # Apply specific offsets for problematic pairs
    for (i in 1:n) {
      nearby_districts <- which(nearby[i,])
      
      if (length(nearby_districts) > 0) {
        # Number the district has nearby
        num_neighbors <- length(nearby_districts)
        
        # Specific handling for district 9 (move it south)
        if (positions$district[i] == 9) {
          positions$offset_y[i] <- -0.020  # Move south
          positions$offset_x[i] <- -0.005  # Slight west adjustment
        }
        # Keep district 7 in place but move slightly west
        else if (positions$district[i] == 7) {
          positions$offset_x[i] <- -0.008  # Move west
        }
        # Move district 8 slightly east
        else if (positions$district[i] == 8) {
          positions$offset_x[i] <- 0.010   # Move east
        }
        # Default adjustments based on number of neighbors
        else if (num_neighbors >= 2) {
          # Districts with many neighbors need more spread
          angle <- (i %% 8) * (2 * pi / 8)  # Distribute in 8 directions
          positions$offset_x[i] <- 0.010 * cos(angle)
          positions$offset_y[i] <- 0.010 * sin(angle)
        }
        else if (num_neighbors == 1) {
          # For districts with one neighbor, move away from that neighbor
          neighbor_idx <- nearby_districts[1]
          dx <- positions$lng[i] - positions$lng[neighbor_idx]
          dy <- positions$lat[i] - positions$lat[neighbor_idx]
          
          # Normalize and apply small offset
          dist <- sqrt(dx^2 + dy^2)
          if (dist > 0) {
            positions$offset_x[i] <- 0.006 * (dx / dist)
            positions$offset_y[i] <- 0.006 * (dy / dist)
          }
        }
      }
    }
    
    return(positions)
  }
  
  # Calculate optimal positions
  label_positions <- calculate_label_positions(geo_data)
  
  # Calculate overall average percentage
  overall_percent <- round(mean(district_stats$positive_percent, na.rm = TRUE), 1)
  
  # Get district colors from custom theme if provided
  district_colors <- if (!is.null(custom_theme)) {
    custom_theme$palettes$district
  } else {
    c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
     "#8c564b", "#e377c2", "#bcbd22", "#17becf")
  }
  
  # Get diverging palette from custom theme if provided for highlighting
  diverging_palette <- if (!is.null(custom_theme) && !is.null(custom_theme$palettes$diverging)) {
    custom_theme$palettes$diverging
  } else {
    c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c")
  }
  
  # Extract the high and low colors from the diverging palette
  highest_color <- diverging_palette[length(diverging_palette)]  # Get last color (highest)
  lowest_color <- diverging_palette[1]    # Red for lowest
  
  # PRE-CALCULATE all values needed for the map
  geo_data$fill_color <- "#CCCCCC"  # Default gray
  geo_data$hover_label <- ""
  geo_data$label_text <- ""
  geo_data$label_bg <- "#FFFFFF"
  geo_data$label_color <- "#000000"
  geo_data$is_extreme <- FALSE
  geo_data$extreme_type <- ""  # Will be "highest" or "lowest"
  geo_data$offset_x <- label_positions$offset_x[match(geo_data$No_Distrit, label_positions$district)]
  geo_data$offset_y <- label_positions$offset_y[match(geo_data$No_Distrit, label_positions$district)]
  
  # Apply district colors
  for (i in 1:nrow(geo_data)) {
    dist_num <- geo_data$No_Distrit[i]
    # Convert to 1-based index for district palette (if districts are 2-10)
    district_index <- as.numeric(dist_num) - 1
    
    if (!is.na(district_index) && district_index >= 1 && district_index <= length(district_colors)) {
      geo_data$fill_color[i] <- district_colors[district_index]
    }
  }
  
  # Create hover and popup labels
  for (i in 1:nrow(geo_data)) {
    dist_num <- geo_data$No_Distrit[i]
    match_idx <- which(district_stats$district_num == dist_num)
    
    if (length(match_idx) > 0) {
      percent_val <- district_stats$positive_percent[match_idx]
      
      geo_data$hover_label[i] <- sprintf(
        "Distrito: %s<br>Porcentaje de hogares con estudiantes: %s%%<br>Total: %d/%d",
        dist_num,
        percent_val,
        district_stats$positive_count[match_idx],
        district_stats$total_responses[match_idx]
      )
      
      # Only add label if percent is not NA
      if (!is.na(percent_val)) {
        geo_data$label_text[i] <- sprintf("Distrito %s<br>%s%%", dist_num, percent_val)
        
        # Set background/text color based on if this is highest/lowest district
        if (as.character(dist_num) %in% highest_district) {
          geo_data$is_extreme[i] <- TRUE
          geo_data$extreme_type[i] <- "highest"
          geo_data$label_bg[i] <- highest_color
          geo_data$label_color[i] <- "#FFFFFF"  # White text for readability
        } else if (as.character(dist_num) %in% lowest_district) {
          geo_data$is_extreme[i] <- TRUE
          geo_data$extreme_type[i] <- "lowest"
          geo_data$label_bg[i] <- lowest_color
          geo_data$label_color[i] <- "#FFFFFF"  # White text for readability
        }
      }
    } else {
      geo_data$hover_label[i] <- sprintf("Distrito: %s<br>Sin datos", dist_num)
    }
  }
  
  # Create base map
  map <- leaflet(geo_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(
      fillColor = ~fill_color,
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
      ),
      label = ~lapply(hover_label, HTML)
    )
  
  # Add district labels with enhanced styling and adjusted positions
  for (i in 1:nrow(geo_data)) {
    if (geo_data$label_text[i] != "") {
      # Create label HTML with consistent styling but enhanced for extremes
      if (geo_data$is_extreme[i]) {
        extreme_text <- ifelse(geo_data$extreme_type[i] == "highest", 
                              "Mayor porcentaje", 
                              "Menor porcentaje")
        
        label_html <- sprintf(
          '<div style="background-color: %s; color: %s; padding: 3px 8px; border-radius: 3px; font-weight: bold; text-align: center;">%s<br>Distrito %s: %s</div>',
          geo_data$label_bg[i], geo_data$label_color[i], extreme_text, geo_data$No_Distrit[i], geo_data$label_text[i]
        )
      } else {
        # Standard label - consistent format for all
        label_html <- sprintf(
          '<div style="background-color: white; color: black; padding: 3px 8px; border-radius: 3px; font-weight: bold; text-align: center;">Distrito %s:<br>%s%%</div>',
          geo_data$No_Distrit[i], district_stats$positive_percent[match(geo_data$No_Distrit[i], district_stats$district_num)]
        )
      }
      
      # Add label with calculated offset position
      map <- map %>% addLabelOnlyMarkers(
        lng = geo_data$lng[i] + geo_data$offset_x[i],
        lat = geo_data$lat[i] + geo_data$offset_y[i],
        label = lapply(list(label_html), HTML),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE
        )
      )
    }
  }
  
  # Add overall average label
  map <- map %>% addControl(
    html = sprintf(
      '<div style="background-color: #333333; color: white; padding: 5px; border-radius: 3px;"><strong>Porcentaje general: %s%%</strong></div>',
      overall_percent
    ),
    position = "topright"
  )

  
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
    custom_theme$palettes$sequential
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
      title = "",
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
  
# Get colors from theme
primary_color <- if (!is.null(custom_theme)) {
  custom_theme$colors$primary
} else {
  "#1f77b4"  # Default blue
}

highlight_color <- if (!is.null(custom_theme)) {
  custom_theme$colors$accent
} else {
  "#ff7f0e"  # Default orange
}

# Create single color vector for all bars initially
colors <- rep(primary_color, nrow(plot_data))

# Handle ties for highlighting top N items
# First, identify the top 3 unique values
unique_top_values <- unique(plot_data$mean_satisfaction)[1:min(3, length(unique(plot_data$mean_satisfaction)))]

# Find all rows that have those top values
top_indices <- which(plot_data$mean_satisfaction %in% unique_top_values)

# Highlight all those rows
colors[top_indices] <- highlight_color
  
  # Create horizontal bar chart
  plot_ly(
    data = plot_data,
    y = ~service,
    x = ~mean_satisfaction,
    type = "bar",
    orientation = 'h',
    marker = list(
      color = colors
    ),
    text = ~paste0(service, ": ", round(mean_satisfaction, 2)),
    hoverinfo = "text"
  ) %>%
    apply_plotly_theme(
      title = "",
      xlab = "Satisfacción promedio (1-10)",
      ylab = "",
      custom_theme = custom_theme
    ) %>%
    layout(
      xaxis = list(range = c(0, 10)),
      yaxis = list(
        categoryorder = "total ascending"
      )  
    )
}

create_housing_overview <- function(survey_data, geo_data, custom_theme = NULL) {
  # Check if we have data
  if (is.null(survey_data) || nrow(survey_data) == 0 || is.null(geo_data)) {
    return(leaflet() %>% 
             addProviderTiles(providers$CartoDB.Positron) %>%
             addControl("No hay datos suficientes para visualizar", position = "topright"))
  }
  
  # Check if Q25 exists in the dataset
  if (!"Q25" %in% names(survey_data)) {
    return(leaflet() %>% 
             addProviderTiles(providers$CartoDB.Positron) %>%
             addControl("Datos de satisfacción de vivienda no disponibles en esta encuesta", position = "topright"))
  }
  
  # Prepare data for Q25 - housing satisfaction map
  housing_data <- survey_data
  
  # Convert Q25 to numeric
  housing_data$Q25 <- suppressWarnings(as.numeric(as.character(housing_data$Q25)))
  
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
  
  # Make sure district_num column exists and is valid
  if (nrow(district_stats) == 0 || all(is.na(district_stats$district_num))) {
    return(leaflet() %>% 
             addProviderTiles(providers$CartoDB.Positron) %>%
             addControl("No se pudieron calcular estadísticas de vivienda por distrito", position = "topright"))
  }
  
  # Find highest and lowest districts
  highest_district <- district_stats %>% 
    filter(!is.na(mean_value)) %>%
    filter(mean_value == max(mean_value, na.rm = TRUE)) %>% 
    pull(district_num)
    
  lowest_district <- district_stats %>% 
    filter(!is.na(mean_value)) %>%
    filter(mean_value == min(mean_value, na.rm = TRUE)) %>% 
    pull(district_num)
  
  # Calculate centroids for label placement
  geo_data$centroid <- sf::st_centroid(geo_data$geometry)
  centroids <- sf::st_coordinates(geo_data$centroid)
  geo_data$lng <- centroids[,1]
  geo_data$lat <- centroids[,2]
  
  # Create a sophisticated position adjustment system based on district relationships
  calculate_label_positions <- function(geo_data) {
    n <- nrow(geo_data)
    positions <- data.frame(
      district = geo_data$No_Distrit,
      lng = geo_data$lng,
      lat = geo_data$lat,
      offset_x = rep(0, n),
      offset_y = rep(0, n)
    )
    
    # Create a matrix of distances between districts
    distances <- matrix(0, nrow = n, ncol = n)
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        dist <- sqrt((positions$lng[i] - positions$lng[j])^2 + 
                    (positions$lat[i] - positions$lat[j])^2)
        distances[i,j] <- distances[j,i] <- dist
      }
    }
    
    # Define threshold for nearby districts
    distance_threshold <- mean(distances[distances > 0]) * 0.6
    
    # Identify clusters of nearby districts
    nearby <- distances < distance_threshold & distances > 0
    
    # Apply specific offsets for problematic pairs
    for (i in 1:n) {
      nearby_districts <- which(nearby[i,])
      
      if (length(nearby_districts) > 0) {
        # Number the district has nearby
        num_neighbors <- length(nearby_districts)
        
        # Specific handling for district 9 (move it south)
        if (positions$district[i] == 9) {
          positions$offset_y[i] <- -0.020  # Move south
          positions$offset_x[i] <- -0.005  # Slight west adjustment
        }
        # Keep district 7 in place but move slightly west
        else if (positions$district[i] == 7) {
          positions$offset_x[i] <- -0.008  # Move west
        }
        # Move district 8 slightly east
        else if (positions$district[i] == 8) {
          positions$offset_x[i] <- 0.010   # Move east
        }
        # Default adjustments based on number of neighbors
        else if (num_neighbors >= 2) {
          # Districts with many neighbors need more spread
          angle <- (i %% 8) * (2 * pi / 8)  # Distribute in 8 directions
          positions$offset_x[i] <- 0.010 * cos(angle)
          positions$offset_y[i] <- 0.010 * sin(angle)
        }
        else if (num_neighbors == 1) {
          # For districts with one neighbor, move away from that neighbor
          neighbor_idx <- nearby_districts[1]
          dx <- positions$lng[i] - positions$lng[neighbor_idx]
          dy <- positions$lat[i] - positions$lat[neighbor_idx]
          
          # Normalize and apply small offset
          dist <- sqrt(dx^2 + dy^2)
          if (dist > 0) {
            positions$offset_x[i] <- 0.006 * (dx / dist)
            positions$offset_y[i] <- 0.006 * (dy / dist)
          }
        }
      }
    }
    
    return(positions)
  }
  
  # Calculate optimal positions
  label_positions <- calculate_label_positions(geo_data)
  
  # Get district colors from custom theme if provided
  district_colors <- if (!is.null(custom_theme)) {
    custom_theme$palettes$district
  } else {
    c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
     "#8c564b", "#e377c2", "#bcbd22", "#17becf")
  }
  
  # Get diverging palette from custom theme if provided for highlighting
  diverging_palette <- if (!is.null(custom_theme) && !is.null(custom_theme$palettes$diverging)) {
    custom_theme$palettes$diverging
  } else {
    c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c")
  }
  
 # Extract the high and low colors from the diverging palette
 highest_color <- diverging_palette[length(diverging_palette)]  # Get last color (highest)
 lowest_color <- diverging_palette[1]   
  
  # PRE-CALCULATE all values needed for the map
  geo_data$fill_color <- "#CCCCCC"  # Default gray
  geo_data$hover_label <- ""
  geo_data$label_text <- ""
  geo_data$label_bg <- "#FFFFFF"
  geo_data$label_color <- "#000000"
  geo_data$is_extreme <- FALSE
  geo_data$extreme_type <- ""  # Will be "highest" or "lowest"
  geo_data$offset_x <- label_positions$offset_x[match(geo_data$No_Distrit, label_positions$district)]
  geo_data$offset_y <- label_positions$offset_y[match(geo_data$No_Distrit, label_positions$district)]
  
  # Apply district colors
  for (i in 1:nrow(geo_data)) {
    dist_num <- geo_data$No_Distrit[i]
    # Convert to 1-based index for district palette (if districts are 2-10)
    district_index <- as.numeric(dist_num) - 1
    
    if (!is.na(district_index) && district_index >= 1 && district_index <= length(district_colors)) {
      geo_data$fill_color[i] <- district_colors[district_index]
    }
  }
  
  # Create hover and popup labels
  for (i in 1:nrow(geo_data)) {
    dist_num <- geo_data$No_Distrit[i]
    match_idx <- which(district_stats$district_num == dist_num)
    
    if (length(match_idx) > 0) {
      mean_val <- district_stats$mean_value[match_idx]
      
      geo_data$hover_label[i] <- sprintf(
        "Distrito: %s<br>Satisfacción vivienda promedio: %.2f<br>N: %d",
        dist_num,
        mean_val,
        district_stats$n[match_idx]
      )
      
      # Only add label if mean is not NA
      if (!is.na(mean_val)) {
        geo_data$label_text[i] <- sprintf("%.2f", mean_val)
        
        # Set background/text color based on if this is highest/lowest district
        if (!is.na(dist_num) && dist_num %in% highest_district) {
          geo_data$is_extreme[i] <- TRUE
          geo_data$extreme_type[i] <- "highest"
          geo_data$label_bg[i] <- highest_color
          geo_data$label_color[i] <- "#FFFFFF"  # White text for readability
        } else if (!is.na(dist_num) && dist_num %in% lowest_district) {
          geo_data$is_extreme[i] <- TRUE
          geo_data$extreme_type[i] <- "lowest"
          geo_data$label_bg[i] <- lowest_color
          geo_data$label_color[i] <- "#FFFFFF"  # White text for readability
        }
      }
    } else {
      geo_data$hover_label[i] <- sprintf("Distrito: %s<br>Sin datos", dist_num)
    }
  }
  
  # Create base map
  map <- leaflet(geo_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(
      fillColor = ~fill_color,
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
      ),
      label = ~lapply(hover_label, HTML)
    )
  
  # Add district labels with enhanced styling and adjusted positions
  for (i in 1:nrow(geo_data)) {
    if (!is.na(geo_data$label_text[i]) && geo_data$label_text[i] != "") {
      # Create label HTML with consistent styling but enhanced for extremes
      if (geo_data$is_extreme[i]) {
        extreme_text <- ifelse(geo_data$extreme_type[i] == "highest", 
                              "Mayor satisfacción", 
                              "Menor satisfacción")
        
        label_html <- sprintf(
          '<div style="background-color: %s; color: %s; padding: 3px 8px; border-radius: 3px; font-weight: bold; text-align: center;">%s<br>Distrito %s: %s</div>',
          geo_data$label_bg[i], geo_data$label_color[i], extreme_text, geo_data$No_Distrit[i], geo_data$label_text[i]
        )
      } else {
        # Standard label - consistent format for all
        label_html <- sprintf(
          '<div style="background-color: white; color: black; padding: 3px 8px; border-radius: 3px; font-weight: bold; text-align: center;">Distrito %s:<br>%s</div>',
          geo_data$No_Distrit[i], geo_data$label_text[i]
        )
      }
      
      # Add label with calculated offset position
      map <- map %>% addLabelOnlyMarkers(
        lng = geo_data$lng[i] + geo_data$offset_x[i],
        lat = geo_data$lat[i] + geo_data$offset_y[i],
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
  map <- map %>% addControl(
    html = sprintf(
      '<div style="background-color: #333333; color: white; padding: 5px; border-radius: 3px;"><strong>Satisfacción vivienda promedio: %.1f</strong></div>',
      overall_mean
    ),
    position = "topright"
  )

  
  return(map)
}


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
  if(question_id == "Q7"| question_id == "Q8") {
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
        text = paste(""),
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
      showlegend = FALSE
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
    rev(custom_theme$palettes$sequential)
  } else {
    rev(colorRampPalette(c("#1f77b4", "#3498DB"))(nrow(pie_data)))
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
        text = "",
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
      showlegend = FALSE
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
    custom_theme$palettes$categorical
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
      title = "",
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
    "19" = "Otro"
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
  
  # Get colors from theme
 primary_color <- if (!is.null(custom_theme)) {
  custom_theme$colors$primary
} else {
  "#1f77b4"  # Default blue
}

highlight_color <- if (!is.null(custom_theme)) {
  custom_theme$colors$accent
} else {
  "#ff7f0e"  # Default orange
}

# Create single color vector for all bars initially
colors <- rep(primary_color, nrow(bar_data))

# Handle ties for highlighting top N items
# First, identify the top 3 unique values
unique_top_values <- unique(bar_data$Percentage)[1:min(3, length(unique(bar_data$Percentage)))]

# Find all rows that have those top values
top_indices <- which(bar_data$Percentage %in% unique_top_values)

# Highlight all those rows
colors[top_indices] <- highlight_color
  
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
      title = "",
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
    custom_theme$palettes$sequential
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
    text = ~paste0(plot_data$Count, " hogares"),
    marker = list(
      colors = pie_colors
    )
  ) %>%
    layout(
      title = list(
        text = "",
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
      showlegend = FALSE
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
    custom_theme$palettes$sequential

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
        text = "",
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
      showlegend = FALSE
    )%>%
      config(
        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", 
                                   "zoomIn2d", "zoomOut2d", "autoScale2d", 
                                   "hoverClosestCartesian", "hoverCompareCartesian","hoverClosestPie"),
        modeBarButtonsToAdd = c("resetScale2d", "toImage"),
        displaylogo=FALSE,
        locale = "es",
        responsive = TRUE
    )
}
# Function to create transportation issues bar plot
create_transport_issues_plot <- function(survey_data, issue_type = "bus", custom_theme = NULL) {
  # Define questions and labels based on issue type
  if (issue_type == "bus") {
    # Bus transportation issues (Q76.1 to Q76.7)
    question_ids <- paste0("Q76.", 1:6)
    title <- ""
  } else {
    # Juarez Bus transportation issues (Q79.1 to Q79.7)
    question_ids <- paste0("Q79.", 1:6)
    title <- ""
  }
  
  # Define issue labels
  issue_labels <- c(
    "Tiempo de espera", 
    "Estado de la unidad", 
    "Estado de la parada", 
    "Trato de los choferes",
    "Conducción de la unidad", 
    "Tarifa"
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
  
# Get primary color from theme
primary_color <- if (!is.null(custom_theme)) {
  custom_theme$colors$primary
} else {
  "#1f77b4"  # Default blue
}

# Get highlight color from theme
highlight_color <- if (!is.null(custom_theme)) {
  custom_theme$colors$accent
} else {
  "#ff7f0e"  # Default orange highlight
}

  # Create single color vector for all bars initially
  colors <- rep(primary_color, nrow(plot_data))
  
  # Handle ties for highlighting top N items
  # First, identify the top 3 unique values
  unique_top_values <- unique(plot_data$Percentage)[1:min(3, length(unique(plot_data$Percentage)))]
  
  # Find all rows that have those top values
  top_indices <- which(plot_data$Percentage %in% unique_top_values)
  
  # Highlight all those rows
  colors[top_indices] <- highlight_color
  
  # Create horizontal bar chart
  plot_ly(
    data = plot_data,
    y = ~Issue,
    x = ~Percentage,
    type = "bar",
    orientation = "h",
    marker = list(
      color = colors
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
        range = c(0, 6)
      ),
      yaxis = list(
        categoryorder = "total ascending"
      )
    ) %>%
      config(
        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", 
                                   "zoomIn2d", "zoomOut2d", "autoScale2d", 
                                   "hoverClosestCartesian", "hoverCompareCartesian","hoverClosestPie"),
        modeBarButtonsToAdd = c("resetScale2d", "toImage"),
        displaylogo=FALSE,
        locale = "es",
        responsive = TRUE
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
create_env_problems_plot <- function(survey_data, custom_theme = NULL) {
  # Extract environmental problems data from Q97
  env_problems <- survey_data$Q97
  env_problems <- env_problems[!is.na(env_problems)]
  
  # Define problem categories - excluding "Otro"
  problem_categories <- c(
    "1" = "Neumáticos/llantas tiradas",
    "2" = "Calles sucias/Basura en las calles",
    "3" = "Parques sucios/descuidados",
    "4" = "Falta de recolección de residuos",
    "5" = "Basureros clandestinos/Casas-terrenos",
    "6" = "Terrenos baldíos"
    # "7" = "Otro" - Removed
  )
  
  # Create frequency table
  problem_table <- table(env_problems)
  
  # Create data frame for plotting (exclude "7" for "Otro")
  plot_data <- data.frame(
    Problem = sapply(names(problem_table), function(x) {
      if (x != "7") problem_categories[x] else NA
    }),
    Count = as.numeric(problem_table),
    stringsAsFactors = FALSE
  )
  
  # Remove the row with NA (corresponding to "Otro")
  plot_data <- plot_data[!is.na(plot_data$Problem), ]
  
  # Calculate percentages based on the filtered data
  plot_data$Percentage <- round(100 * plot_data$Count / sum(plot_data$Count), 1)
  
  # Sort by count descending
  plot_data <- plot_data[order(-plot_data$Count), ]
  
  # Get the highest count problem to highlight
  highest_problem <- which.max(plot_data$Count)
  
  # Get colors from custom theme if provided
  primary_color <- if (!is.null(custom_theme)) {
    custom_theme$colors$primary
  } else {
    "#20c997"  # Default teal green
  }
  
  secondary_color <- if (!is.null(custom_theme)) {
    custom_theme$colors$accent
  } else {
    "#fd7e14"  # Default orange
  }
  
  # Create color vector with all primary color except the highest value
  colors <- rep(primary_color, nrow(plot_data))
  colors[highest_problem] <- secondary_color
  
  # Create bar chart
  plot_ly(
    data = plot_data,
    x = ~Problem,
    y = ~Percentage,
    type = "bar",
    marker = list(
      color = colors
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
  bar_colors <- custom_theme$palettes$categorical[c(1,5,3,6)]
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
    title <- ""
    mode_labels <- c(
      "Caminando", "Bicicleta", "Autobús escolar", "Autobús especial", 
      "Taxi", "Uber/Didi/InDriver", "Motocicleta", "Vehículo propio",
      "Camión/Rutera/Autobús", "Juarez Bus", "Otro medio"
    )
  } else {
    # General transportation questions (Q73.1 to Q73.11)
    question_ids <- paste0("Q73.", 1:11)
    title <- ""
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
  
 # Get primary color from theme
 primary_color <- if (!is.null(custom_theme)) {
  custom_theme$colors$primary
} else {
  "#1f77b4"  # Default blue
}

# Get highlight color from theme
highlight_color <- if (!is.null(custom_theme)) {
  custom_theme$colors$accent
} else {
  "#ff7f0e"  # Default orange highlight
}

  # Create single color vector for all bars initially
  colors <- rep(primary_color, nrow(plot_data))
  
  # Handle ties for highlighting top N items
  # First, identify the top 3 unique values
  unique_top_values <- unique(plot_data$Percentage)[1:min(3, length(unique(plot_data$Percentage)))]
  
  # Find all rows that have those top values
  top_indices <- which(plot_data$Percentage %in% unique_top_values)
  
  # Highlight all those rows
  colors[top_indices] <- highlight_color
  
  # Create horizontal bar chart
  plot_ly(
    data = plot_data,
    y = ~Mode,
    x = ~Percentage,
    type = "bar",
    orientation = "h",
    marker = list(
      color = colors
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