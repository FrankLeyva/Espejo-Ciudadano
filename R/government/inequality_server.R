# Server function for Inequality Dashboard
inequalityServer <- function(input, output, session,current_theme = NULL) {
  # Get the selected year from userData
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
      # Default to gobierno theme if nothing provided
      get_section_theme("gobierno")
    }
  })
  # Map for Q84 - Rights Violation (using binary module functions)
  output$rights_violation_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare binary data for Q84
    rights_data <- prepare_binary_data(
      data = survey_data()$responses,
      question_id = "Q84",
      metadata = survey_data()$metadata
    )
    
    # Create binary district map
    create_binary_district_map(
      rights_data,
      geo_data(),
      highlight_extremes = TRUE,
      focus_on_true = TRUE,
      custom_theme = active_theme()
    )
  })
  
  # Map for Q87 - Inequality Perception (using interval module functions)
  output$inequality_perception_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data for Q87
    inequality_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q87",
      metadata = survey_data()$metadata
    )
    
    # Create interval district map
    create_interval_district_map(
      inequality_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "RdBu",
      custom_theme = active_theme()
    )
  })
  
  # Bar chart for Q88 - Institutions contributing to reducing inequality
  output$inequality_reduction_plot <- renderPlotly({
    req(survey_data())
    
    # Get Q88 data
    q88_data <- survey_data()$responses$Q88
    q88_data <- q88_data[!is.na(q88_data)]
    
    # Define institutions labels
    institutions <- c(
      "1" = "Gobierno Federal",
      "2" = "Gobierno Municipal",
      "3" = "Gobierno Estatal",
      "4" = "Organizaciones de la sociedad civil",
      "5" = "Empresas",
      "6" = "Iglesias",
      "7" = "Universidades",
      "8" = "Escuelas (primaria, secundaria, preparatoria)",
      "9" = "La misma ciudadanía",
      "10" = "Ninguna",
      "11" = "No sabe/ No contestó"
    )
    
    # Create frequency table
    inst_table <- table(q88_data)
    
    # Convert to data frame
    bar_data <- data.frame(
      Institution = sapply(names(inst_table), function(x) institutions[x]),
      Count = as.numeric(inst_table),
      stringsAsFactors = FALSE
    )
    
    # Check for NA or NULL values in Institution
    bar_data <- bar_data %>% 
      mutate(Institution = ifelse(is.na(Institution), paste("Categoría", names(inst_table)), Institution))
    
    # Calculate percentages
    bar_data$Percentage <- round(100 * bar_data$Count / sum(bar_data$Count), 1)
    
    # Sort by count in descending order
    bar_data <- bar_data[order(-bar_data$Count), ]
    
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
      y = ~reorder(Institution, Count),
      x = ~Count,
      type = "bar",
      orientation = 'h',
      marker = list(
        color = colors
      ),
      text = ~paste0(Percentage, "%"),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "",
        xlab = "Frecuencia",
        ylab = "",
        custom_theme = active_theme()
      )
  })
  output$download_rights_violation_map <- downloadHandler(
    filename = function() {
      paste("mapa_derechos_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # We need to save the map to a temporary file first
      tmp_html <- tempfile(fileext = ".html")
      
      # Prepare binary data for Q84
    rights_data <- prepare_binary_data(
      data = survey_data()$responses,
      question_id = "Q84",
      metadata = survey_data()$metadata
    )
    
    # Create binary district map
    map <- create_binary_district_map(
      rights_data,
      geo_data(),
      highlight_extremes = TRUE,
      focus_on_true = TRUE,
      custom_theme = active_theme()
    )
      
      # Add title and footer to the map directly
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      "Porcentaje de encuestados que reporta haber tenido sus derechos violentados por distrito", 
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
  output$download_inequality_map <- downloadHandler(
    filename = function() {
      paste("mapa_desigualdad_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # We need to save the map to a temporary file first
      tmp_html <- tempfile(fileext = ".html")
      
      # Prepare interval data for Q87
    inequality_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q87",
      metadata = survey_data()$metadata
    )
    
    # Create interval district map
    map <-create_interval_district_map(
      inequality_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "RdBu",
      custom_theme = active_theme()
    )
      
      # Add title and footer to the map directly
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      "Percepcion de la Desigualdad por Distrito, Escala 1-4", 
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