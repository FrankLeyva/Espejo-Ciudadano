# Función del servidor para el Dashboard de Participación
participationServer <- function(input, output, session,current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  
  survey_data <- reactive({
    survey_id <- paste0("PAR_", selectedYear())
    load_survey_data(survey_id)
  })
  
  # Load geographical data
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
      # Default to participacion theme if nothing provided
      get_section_theme("participacion")
    }
  })
  
  # Social Movement Support Percentage
  output$social_movement_support <- renderText({
    req(survey_data())
    
    tryCatch({
      # Extract Q134 values
      values <- survey_data()$responses[["Q136"]]
      values <- values[!is.na(values)]
      
      if(length(values) > 0) {
        # Calculate percentage of "Yes" responses (value = 1)
        support_count <- sum(values == "1", na.rm=T)
        support_percent <- 100 * support_count / length(values)
        
        # Format for display
        sprintf("%.1f%% de los encuestados", support_percent)
      } else {
        "Datos no disponibles"
      }
    }, error = function(e) {
      warning(paste("Error calculating social movement support:", e$message))
      "Error al procesar datos"
    })
  })
  
  # Voting Importance Map
  output$voting_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    tryCatch({
      # Prepare data for Q137
      voting_data <- prepare_interval_data(
        data = survey_data()$responses,
        question_id = "Q139",
        metadata = survey_data()$metadata
      )
      
      # Create district map
      create_interval_district_map(
        data = voting_data, 
        geo_data = geo_data(),
        # Select responses for "Importante" and "Poco importante" (1 and 2)
        selected_responses = c("1", "2"),
        highlight_extremes = TRUE,
        use_gradient = F,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
    }, error = function(e) {
      warning(paste("Error creating voting map:", e$message))
      leaflet() %>% 
        addTiles() %>%
        addControl("Error al generar el mapa de importancia del voto", position = "topright")
    })
  })
  
  # Political Interest Pie Chart
  output$interest_pie <- renderPlotly({
    req(survey_data())
    
    tryCatch({
      # Extract Q130 values
      interest_values <- survey_data()$responses[["Q131"]]
      interest_values <- interest_values[!is.na(interest_values)]
      
      if(length(interest_values) == 0) {
        return(plotly_empty() %>% 
                 layout(title = "No hay datos disponibles sobre interés político"))
      }
      
      # Count responses for each level
      interest_counts <- table(interest_values)
      
      # Create labels for interest levels
      interest_labels <- c(
        "1" = "Nada",
        "2" = "Poco",
        "3" = "Regular",
        "4" = "Algo",
        "5" = "Mucho"
      )
      
      # Create data frame for plotting
      plot_data <- data.frame(
        Interest = sapply(names(interest_counts), function(x) interest_labels[x]),
        Count = as.numeric(interest_counts),
        stringsAsFactors = FALSE
      )
      
      # Calculate percentages
      plot_data$Percentage <- round(100 * plot_data$Count / sum(plot_data$Count), 1)
      
      # Create color gradient based on interest level
      colors <- active_theme()$palettes$sequential
      
      # Create pie chart
      plot_ly(
        labels = ~plot_data$Interest,
        values = ~plot_data$Count,
        type = "pie",
        textinfo = "label+percent",
        hoverinfo = "text",
        text = ~paste0(plot_data$Interest, ": ", plot_data$Count, " (", plot_data$Percentage, "%)"),
        marker = list(
          colors = colors,
          line = list(color = '#FFFFFF', width = 1)
        )
      ) %>%
        layout(
          title = "",
          showlegend = FALSE,
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = -0.1
          )
        ) %>% apply_plotly_theme()
    }, error = function(e) {
      warning(paste("Error creating interest pie chart:", e$message))
      return(plotly_empty() %>% 
               layout(title = "Error al generar gráfico de interés político"))
    })
  })


  output$download_voting_map <- downloadHandler(
    filename = function() {
      paste("mapa_voto_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # We need to save the map to a temporary file first
      tmp_html <- tempfile(fileext = ".html")
      voting_data <- prepare_interval_data(
        data = survey_data()$responses,
        question_id = "Q139",
        metadata = survey_data()$metadata
      )
      # Get the map
      map <- create_interval_district_map(
        data = voting_data, 
        geo_data = geo_data(),
        # Select responses for "Importante" and "Poco importante" (1 and 2)
        selected_responses = c("1", "2"),
        highlight_extremes = TRUE,
        use_gradient = F,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
      
      # Add title and footer to the map directly
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      "Porcentaje que considera que votar es importante", 
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
        browser = "C:/Program Files/Google/Chrome/Application/chrome.exe",
        extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
      )
      
      # Clean up temporary files
      if (file.exists(tmp_html)) {
        file.remove(tmp_html)
      }
    }
  )
}