transportationServer <- function(input, output, session,current_theme = NULL) {
  # Load survey data
  # Get the selected year from userData
  selectedYear <- session$userData$selectedYear
  
  # Load survey data with dynamic year
  survey_data <- reactive({
    survey_id <- paste0("PER_", selectedYear())
    load_survey_data(survey_id)
  })
  
  # Load geographical data
  geo_data <- reactive({
    tryCatch({
      sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error loading geo data:", e$message), type = "error")
      NULL
    })
  })
  
  active_theme <- reactive({
    if (is.function(current_theme)) {
      # If current_theme is a reactive function, call it to get the value
      current_theme()
    } else if (!is.null(current_theme)) {
      # If it's a direct value, use it
      current_theme
    } else {
      # Default to movilidad theme if nothing provided
      get_section_theme("movilidad")
    }
  })
  
  # Prepare data for bus satisfaction map (Q75)
  output$bus_satisfaction_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data using existing function
    bus_satisfaction <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q75",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = bus_satisfaction,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  # Prepare data for Juarez Bus satisfaction map (Q79)
  output$juarez_bus_satisfaction_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data using existing function
    juarez_bus_satisfaction <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q78",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = juarez_bus_satisfaction,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  # Create bus issues plot (Q76.1 to Q76.7)
  output$bus_issues_plot <- renderPlotly({
    req(survey_data())
    create_transport_issues_plot(
      survey_data()$responses, 
      issue_type = "bus", 
      custom_theme = active_theme()
    ) %>%  apply_plotly_theme()
  })
  
  # Create Juarez Bus issues plot (Q79.1 to Q79.7)
  output$juarez_bus_issues_plot <- renderPlotly({
    req(survey_data())
    create_transport_issues_plot(
      survey_data()$responses, 
      issue_type = "juarez_bus", 
      custom_theme = active_theme()
    )%>%  apply_plotly_theme()
  })

# Observer to track active tab for download
observeEvent(input$transport_tabs, {
  # Store the active tab in a reactive value for the download handler
  tab_value <- input$transport_tabs
})

# Download handler that adapts based on active tab
output$download_transport_map <- downloadHandler(
  filename = function() {
    # Get map type for filename based on active tab
    map_type <- ifelse(input$transport_tabs == "Camión/Rutera", 
                      "Camion", "Juarez_Bus")
    paste("mapa_transporte_", map_type, "_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # Temporary file for the HTML content
    tmp_html <- tempfile(fileext = ".html")
    
    # Create the appropriate map based on active tab
    if(input$transport_tabs == "Camión/Rutera") {
      bus_satisfaction <- prepare_interval_data(
        data = survey_data()$responses,
        question_id = "Q75",
        metadata = survey_data()$metadata
      )
      
      map <- create_interval_district_map(
        data = bus_satisfaction,
        geo_data = geo_data(),
        use_gradient = F,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
      title_text <- "Satisfacción con el Servicio de Camión/Rutera"
    } else {
      juarez_bus_satisfaction <- prepare_interval_data(
        data = survey_data()$responses,
        question_id = "Q78",
        metadata = survey_data()$metadata
      )
      
      map <- create_interval_district_map(
        data = juarez_bus_satisfaction,
        geo_data = geo_data(),
        use_gradient = F,
        color_scale = "Blues",
        custom_theme = active_theme()
      )
      title_text <- "Satisfacción con el Servicio de Juárez Bus"
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
      browser = "C:/Program Files/Google/Chrome/Application/chrome.exe",
      extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
    )
    
    # Clean up
    if (file.exists(tmp_html)) {
      file.remove(tmp_html)
    }
  }
)





}