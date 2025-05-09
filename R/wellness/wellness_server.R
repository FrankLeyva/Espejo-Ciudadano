# wellness_server.R

wellnessServer <- function(input, output, session, current_theme = NULL) {
  # Get the selected year from userData
  selectedYear <- session$userData$selectedYear
  
  # Load survey data with dynamic year
   survey_data <- session$userData$perSurveyData

  
  # Load geographical data
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
      # Default to bienestar theme if nothing provided
      get_section_theme("bienestar")
    }
  })
  
  # Process Q4 data for economic situation pie chart
  economic_situation_data <- reactive({
    req(survey_data())
    
    prepare_categorical_data(
      data = survey_data()$responses,
      question_id = "Q4",
      metadata = survey_data()$metadata
    )
  })
  
  # Create economic situation pie chart
  output$economic_situation_pie <- renderPlotly({
    req(economic_situation_data())
    
    create_category_pie(
      economic_situation_data(),
      max_categories = 6,
      custom_theme = active_theme(),
      highlight_max = F,
      palette = "sequential"
    ) %>% 
      apply_plotly_theme(
        title = "",
        custom_theme = active_theme()
      )
  })
  
  # Process Q63 data for migration intention map
  migration_intention_data <- reactive({
    req(survey_data())
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q63",
      metadata = survey_data()$metadata
    )
  })
  
  # Create migration intention map
  output$migration_intention_map <- renderLeaflet({
    req(migration_intention_data(), geo_data())
    
    create_interval_district_map(
      migration_intention_data(),
      geo_data(),
      selected_responses = c("1", "2"),  # "Muchas veces" and "Algunas veces"
      highlight_extremes = TRUE,
      use_gradient = F,
      custom_theme = active_theme()
    )
  })
  
# Create activities chart (Q16.1-Q16.15)
output$activities_chart <- renderPlotly({
  req(survey_data())
  
  # Activity questions Q16.1 to Q16.15
  activity_questions <- paste0("Q16.", 1:15)
  
  # Activity labels
  activity_labels <- c(
    "Actividades dentro del hogar",
    "Contacto con naturaleza/parques", 
    "Visita centro comercial", 
    "Visita segundas/bazares", 
    "Visita bar/cantina", 
    "Cine", 
    "Biblioteca", 
    "Concierto/espectáculo musical",
    "Feria/fiesta popular", 
    "Museo/galería", 
    "Teatro/danza", 
    "Zona histórica/monumento", 
    "Conferencia/coloquio", 
    "Evento deportivo",
    "Ejercicio/actividad física"
  )
  
  # Calculate percentages, counts, and totals for each activity
  results <- lapply(activity_questions, function(qid) {
    binary_data <- prepare_binary_data(
      data = survey_data()$responses,
      question_id = qid,
      metadata = survey_data()$metadata
    )
    
    if(nrow(binary_data) > 0) {
      positive_count <- sum(binary_data$binary_value, na.rm = TRUE)
      total_count <- nrow(binary_data)
      percentage <- 100 * positive_count / total_count
      
      return(list(
        percentage = percentage,
        positive_count = positive_count,
        total_count = total_count
      ))
    } else {
      return(list(
        percentage = 0,
        positive_count = 0,
        total_count = 0
      ))
    }
  })
  
  # Extract percentages for ordering
  percentages <- sapply(results, function(x) x$percentage)
  positive_counts <- sapply(results, function(x) x$positive_count)
  total_counts <- sapply(results, function(x) x$total_count)
  
  # Create data frame for plotting
  ordered_indices <- order(percentages, decreasing = TRUE)
  plot_data <- data.frame(
    activity = factor(activity_labels[ordered_indices], levels = activity_labels[ordered_indices]),
    percentage = percentages[ordered_indices],
    positive_count = positive_counts[ordered_indices],
    total_count = total_counts[ordered_indices],
    rank = 1:length(percentages)  # Add rank for coloring
  )
  
  # Create a color vector - highlight top 3, use primary color for others
  # Get colors from the active theme
  primary_color <- active_theme()$colors$primary
  highlight_color <- active_theme()$colors$accent
  
  # If highlight color is not defined, fall back to a secondary color
  if (is.null(highlight_color)) {
    # Try to get another distinctive color from the theme
    if (!is.null(active_theme()$colors$secondary)) {
      highlight_color <- active_theme()$colors$secondary
    } else if (!is.null(active_theme()$colors$success)) {
      highlight_color <- active_theme()$colors$success
    } else {
      # Fall back to a brighter version of primary if nothing else available
      highlight_color <- colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
    }
  }
  
  # Create color vector - highlight top 3
  bar_colors <- ifelse(plot_data$rank <= 3, highlight_color, primary_color)
  
  plot_ly(
    data = plot_data,
    y = ~activity,
    x = ~percentage,
    type = "bar",
    orientation = 'h',
    marker = list(
      color = bar_colors,
      line = list(
        color = active_theme()$colors$neutral,
        width = 1
      )
    ),
    text = ~paste0(round(percentage, 1), "%"),
    textposition = "auto",
    hoverinfo = "text",
    hovertext = ~paste0(positive_count, "/", total_count, " respuestas)")
  ) %>%
    apply_plotly_theme(
      title = "",
      xlab = "Porcentaje de participación (%)",
      ylab = "",
      custom_theme = active_theme()
    ) %>%
    layout(
      xaxis = list(range = c(0, max(percentages) * 1.1)),
      yaxis = list(categoryorder = 'total ascending')
    )
})

output$download_migration_map <- downloadHandler(
  filename = function() {
    paste("mapa_migracion_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # We need to save the map to a temporary file first
    tmp_html <- tempfile(fileext = ".html")
    
    # Get the map
    map <- create_interval_district_map(
      migration_intention_data(),
      geo_data(),
      selected_responses = c("1", "2"),
      highlight_extremes = TRUE,
      use_gradient = F,
      custom_theme = active_theme()
    )
    
    # Add title and footer to the map directly
    map <- map %>%
      addControl(
        html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                    "Frecuencia con que piensa en irse de la ciudad", 
                    "</div>"),
        position = "topright"
      ) %>%
      addControl(
        html = paste("<div style='background-color:white; padding:8px; border-radius:5px; font-size:12px;'>", 
                    paste("Resultados de la Encuesta de Percepción y Participación Ciudadana y Buen Gobierno", selectedYear()),
                    "</div>"),
        position = "bottomright"
      )
    
    # Save the map to HTML
    htmlwidgets::saveWidget(map, tmp_html, selfcontained = TRUE)
    
    # Use pagedown with Chrome headless
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
    
    # Clean up temporary files
    if (file.exists(tmp_html)) {
      file.remove(tmp_html)
    }
  }
)
}
