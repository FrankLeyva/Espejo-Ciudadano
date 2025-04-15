infrastructureServer <- function(input, output, session,current_theme = NULL) {
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
  
  # Education Plot
  output$education_plot <- renderLeaflet({
    req(survey_data())
    create_education_overview(survey_data()$responses,geo_data(), active_theme())
  })
  
  # Healthcare Plot
  output$healthcare_plot <- renderPlotly({
    req(survey_data())
    create_healthcare_overview(survey_data()$responses, active_theme())
  })
  
  # Utilities Plot
  output$utilities_plot <- renderPlotly({
    req(survey_data())
    create_utilities_overview(survey_data()$responses, active_theme())
  })
  
  # Housing Map
  output$housing_map <- renderLeaflet({
    req(survey_data(), geo_data())
    create_housing_overview(survey_data()$responses, geo_data(), active_theme())
  })
  output$download_students_map <- downloadHandler(
    filename = function() {
      paste("mapa_estudiantes_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # We need to save the map to a temporary file first
      tmp_html <- tempfile(fileext = ".html")
      
    # Create interval district map
    map <-    create_education_overview(survey_data()$responses,geo_data(), active_theme())

      
      # Add title and footer to the map directly
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      "Hogares con al menos un Estudiante por Distrito", 
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


  output$download_housing_map <- downloadHandler(
    filename = function() {
      paste("mapa_viviendas_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # We need to save the map to a temporary file first
      tmp_html <- tempfile(fileext = ".html")
      

    
    # Create interval district map
    map <-    create_housing_overview(survey_data()$responses, geo_data(), active_theme())

      
      # Add title and footer to the map directly
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      "Satisfacción general de la vivienda (1-10) por distrito", 
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