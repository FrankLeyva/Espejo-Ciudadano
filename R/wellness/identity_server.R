# identity_server.R

identityServer <- function(input, output, session,current_theme = NULL) {
  # Get the selected year from userData
  selectedYear <- session$userData$selectedYear
  
  survey_data <- session$userData$perSurveyData
  
  geo_data <- session$userData$geoData
  
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

  output$monuments_bar <- renderPlotly({
    req(survey_data())
    
    # Count frequencies
    monument_counts <- table(survey_data()$responses$Q65)
    monument_mapping <- c(
      "1" = "La X",
      "2" = "El Monumento A Benito Juárez",
      "3" = "La Catedral",
      "4" = "El Parque Central",
      "5" = "El Chamizal",
      "6" = "La Casa De Juan Gabriel",
      "7" = "El Centro",
      "8" = "La Presidencia",
      "9" = "Umbral Del Milenio",
      "10" = "El Museo De La Ex-Aduana",
      "11" = "El Parque Borunda",
      "12" = "El Monumento A Zapata",
      "13" = "La Rodadora",
      "14" = "Letras JRZ",
      "15" = "La Plaza De Toros",
      "16" = "Monumento A Los Indomables",
      "17" = "Gimnasios Públicos",
      "18" = "La Torre Centinela",
      "19" = "El Gardie",
      "20" = "El Cigarro",
      "21" = "Estadio Benito Juárez",
      "22" = "La Iglesia De San Lorenzo",
      "23" = "Mercado Juárez",
      "24" = "Monumento A Tin Tan",
      "25" = "Monumento Al Trabajo",
      "26" = "Otro",
      "27" = "Ninguno"
    )
    
    # Create data frame
    freq_df <- data.frame(
      code = names(monument_counts),
      monument = sapply(names(monument_counts), function(x) monument_mapping[x]),
      count = as.vector(monument_counts)
    ) %>% filter(!monument %in% c("Otro", "Ninguno"))
    freq_df <- data.frame(
      code = names(monument_counts),
      monument = sapply(names(monument_counts), function(x) monument_mapping[x]),
      count = as.vector(monument_counts)
    ) %>% filter(!monument %in% c("Otro", "Ninguno"))
    
    # Calculate total for percentages (excluding "Otro" and "Ninguno")
    total_responses <- sum(freq_df$count)
    
    # Add percentage column
    freq_df$percentage <- round(100 * freq_df$count / total_responses, 1)
    
    # Sort by count
    freq_df <- freq_df[order(-freq_df$count), ]
    # Limit to top 15
    freq_df <- head(freq_df, 15)
    freq_df$rank <- 1:15

  # Create a color vector - highlight top 3, use primary color for others
  # Get colors from the active theme
  primary_color <- active_theme()$colors$primary
  highlight_color <- active_theme()$colors$accent
    # If highlight color is not defined, fall back to a secondary color
  if (is.null(highlight_color)) {
    # Try to get another distinctive color from the theme
    if (!is.null(active_theme()$colors$accent)) {
      highlight_color <- active_theme()$colors$accent
    } else if (!is.null(active_theme()$colors$success)) {
      highlight_color <- active_theme()$colors$success
    } else {
      # Fall back to a brighter version of primary if nothing else available
      highlight_color <- colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
    }
  }
      # Create color vector - highlight top 3
  bar_colors <- ifelse(freq_df$rank <= 3, highlight_color, primary_color)
    # Create horizontal bar chart
    plot_ly(
      data = freq_df,
      y = ~reorder(monument, count),
      x = ~percentage,  # Changed to percentage for the x-axis
      type = "bar",
      orientation = 'h',
      marker = list(
        color = bar_colors,
        line = list(
          color = active_theme()$colors$neutral,
          width = 1
        )
      ),
      text = ~paste0(percentage, "%"),  # Display percentage on bars
      textposition = "auto",
      hoverinfo = "text",
      hovertext = ~paste0(count, " menciones")
    ) %>%
      apply_plotly_theme(
        title = "",
        xlab = "Porcentaje de menciones (%)",  # Updated axis label
        ylab = "",
      ) %>% 
      layout(
        yaxis = list(
          categoryorder = "total ascending"
        ),
        xaxis = list(
          # Add a % sign to the x-axis values
          ticksuffix = "%"
        )
      )
  })

  
  # Process Q80 data for city pride pie chart
  city_pride_data <- reactive({
    req(survey_data())
    
    # Prepare categorical data for Q80
    prepare_categorical_data(
      data = survey_data()$responses,
      question_id = "Q80",
      metadata = survey_data()$metadata
    )
  })
  
  # Create city pride pie chart
  output$city_pride_pie <- renderPlotly({
    req(city_pride_data())
    
    # Use the categorical module's pie chart function, excluding NS/NC (5)
    pride_data <- city_pride_data()
    pride_data <- pride_data[!pride_data$value %in% c("Ns/Nc"), ]
    
    create_category_pie(
      pride_data,
      max_categories = 4,
      custom_theme = active_theme(),
      highlight_max = F,
      palette = 'sequential',
      inverse=T
    ) %>% apply_plotly_theme(title='')
  })
  
  # Process Q64.2 data for neighborhood connection map
  neighborhood_connection_data <- reactive({
    req(survey_data())
    
    # Prepare interval data for Q64.2
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q64.2",
      metadata = survey_data()$metadata
    )
  })
  
  # Process Q64.3 data for neighbors connection map
  neighbors_connection_data <- reactive({
    req(survey_data())
    
    # Prepare interval data for Q64.3
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q64.3",
      metadata = survey_data()$metadata
    )
  })
  
  # Create neighborhood connection map
  output$neighborhood_connection_map <- renderLeaflet({
    req(neighborhood_connection_data(), geo_data())
    
    # Use interval district map with selected options 4 and 5 (bastante or mucho)
    create_interval_district_map(
      neighborhood_connection_data(),
      geo_data(),
      selected_responses = c("4", "5"),
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  # Create neighbors connection map
  output$neighbors_connection_map <- renderLeaflet({
    req(neighbors_connection_data(), geo_data())
    
    # Use interval district map with selected options 4 and 5 (bastante or mucho)
    create_interval_district_map(
      neighbors_connection_data(),
      geo_data(),
      selected_responses = c("4", "5"),
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  # Calculate statistics for value boxes
  
  # Neighborhood connection percentage (Q64.2 values 4-5)
  output$neighborhood_connection_pct <- renderText({
    req(neighborhood_connection_data())
    
    # Filter and calculate percentage
    filtered_data <- neighborhood_connection_data()
    connected_count <- sum(filtered_data$value_num %in% c(4, 5), na.rm = TRUE)
    total_count <- nrow(filtered_data)
    
    # Calculate percentage
    percentage <- round(100 * connected_count / total_count, 1)
    
    # Return formatted text
    paste0(percentage, "%")
  })
  
  # Neighbors connection percentage (Q64.3 values 4-5)
  output$neighbors_connection_pct <- renderText({
    req(neighbors_connection_data())
    
    # Filter and calculate percentage
    filtered_data <- neighbors_connection_data()
    connected_count <- sum(filtered_data$value_num %in% c(4, 5), na.rm = TRUE)
    total_count <- nrow(filtered_data)
    
    # Calculate percentage
    percentage <- round(100 * connected_count / total_count, 1)
    
    # Return formatted text
    paste0(percentage, "%")
  })
  
  # City pride percentage (Q80 values 3-4)
  output$city_pride_pct <- renderText({
    req(city_pride_data())
    
    # Filter and calculate percentage
    filtered_data <- city_pride_data()
    proud_count <- sum(filtered_data$value %in% c("Algo", "Mucho"), na.rm = TRUE)
    total_count <- sum(!filtered_data$value %in% c("Ns/Nc"), na.rm = TRUE)
    
    # Calculate percentage
    percentage <- round(100 * proud_count / total_count, 1)
    
    # Return formatted text
    paste0(percentage, "%")
  })
 
# Observer to track active tab for download
observeEvent(input$neighborhood_tabs, {
  # Store the active tab in a reactive value for the download handler
  tab_value <- input$neighborhood_tabs
})

# Download handler that adapts based on active tab
output$download_connection_map <- downloadHandler(
  filename = function() {
    # Get map type for filename based on active tab
    map_type <- ifelse(input$neighborhood_tabs == "Vínculo con la colonia o fraccionamiento", 
                      "colonia", "vecinos")
    paste("mapa_vinculo_", map_type, "_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # Temporary file for the HTML content
    tmp_html <- tempfile(fileext = ".html")
    
    # Create the appropriate map based on active tab
    if(input$neighborhood_tabs == "Vínculo con la colonia o fraccionamiento") {
      map <- create_interval_district_map(
        neighborhood_connection_data(),
        geo_data(),
        selected_responses = c("4", "5"),
        highlight_extremes = TRUE,
        use_gradient = F,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
      title_text <- "Vínculo con la colonia o fraccionamiento"
    } else {
      map <- create_interval_district_map(
        neighbors_connection_data(),
        geo_data(),
        selected_responses = c("4", "5"),
        highlight_extremes = TRUE,
        use_gradient = F,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
      title_text <- "Vínculo con los vecinos"
    }
    
    # Add title and footer
    map <- map %>%
      addControl(
        html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                    title_text, 
                    "</div>"),
        position = "topright"
      ) %>%
      addControl(
        html = paste("<div style='background-color:white; padding:8px; border-radius:5px; font-size:12px;'>", 
                    paste("Resultados de la Encuesta de Percepción y Participación Ciudadana y Buen Gobierno", selectedYear()),
                    "</div>"),
        position = "bottomright"
      )
    
    # Save and convert
    htmlwidgets::saveWidget(map, tmp_html, selfcontained = TRUE)
    
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
    
    # Clean up
    if (file.exists(tmp_html)) {
      file.remove(tmp_html)
    }
  }
)
}
