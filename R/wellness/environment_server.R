# environmentServer.R
environmentServer <- function(input, output, session,current_theme = NULL) {
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
      # Default to bienestar theme if nothing provided
      get_section_theme("bienestar")
    }
  })
  
  # Air quality map
  output$air_quality_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    air_quality_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q89",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = air_quality_data,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Greens",
      custom_theme = active_theme()
    )
  })
  
  # Urban trees map
  output$urban_trees_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    urban_trees_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q90",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = urban_trees_data,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Greens",
      custom_theme = active_theme()
    )
  })
  
  # Street cleanliness map
  output$street_cleanliness_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    street_cleanliness_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q91",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = street_cleanliness_data,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Greens",
      custom_theme = active_theme()
    )
  })
  
  # Water quality map
  output$water_quality_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    water_quality_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q92",
      metadata = survey_data()$metadata
    )
    
    create_interval_district_map(
      data = water_quality_data,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  # Environmental problems plot
  output$env_problems_plot <- renderPlotly({
    req(survey_data())
    create_env_problems_plot(
      survey_data()$responses,
      custom_theme = active_theme()
    ) %>% 
      apply_plotly_theme(
        title = ""
      )
  })
  observeEvent(input$env_satisfaction_tabs, {
    # Store the active tab in a reactive value for the download handler
    tab_value <- input$env_satisfaction_tabs
  })
# Download handler that adapts based on active tab
output$download_environment_map <- downloadHandler(
  filename = function() {
    # Get map type for filename based on active tab
    map_type <- if(input$env_satisfaction_tabs == "Calidad del Aire"){ 
                      "Aire"} else if (input$env_satisfaction_tabs == "Arbolado Urbano"){
                        "Arbolado" } else if (input$env_satisfaction_tabs == "Limpieza de Calles"){
                        "Limpieza" }else {
                        "Agua"
                      }
    paste("mapa_medio_ambiente_", map_type, "_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # Temporary file for the HTML content
    tmp_html <- tempfile(fileext = ".html")
    
    # Create the appropriate map based on active tab
    if(input$env_satisfaction_tabs == "Calidad del Aire") {
      air_quality_data <- prepare_interval_data(
        data = survey_data()$responses,
        question_id = "Q89",
        metadata = survey_data()$metadata
      )
      
      map <- create_interval_district_map(
        data = air_quality_data,
        geo_data = geo_data(),
        use_gradient = F,
        color_scale = "Greens",
        custom_theme = active_theme()
      )
      title_text <- "Satisfacción con la Calidad del Aire"
    } else if(input$env_satisfaction_tabs == "Arbolado Urbano") {
      urban_trees_data <- prepare_interval_data(
        data = survey_data()$responses,
        question_id = "Q90",
        metadata = survey_data()$metadata
      )
      
      map <- create_interval_district_map(
        data = urban_trees_data,
        geo_data = geo_data(),
        use_gradient = F,
        custom_theme = active_theme()
      )
      title_text <- "Satisfacción con el Arbolado Urbano"
    } else if(input$env_satisfaction_tabs == "Limpieza de Calles") {
      street_cleanliness_data <- prepare_interval_data(
        data = survey_data()$responses,
        question_id = "Q91",
        metadata = survey_data()$metadata
      )
      
      map <- create_interval_district_map(
        data = street_cleanliness_data,
        geo_data = geo_data(),
        use_gradient = F,
        color_scale = "Greens",
        custom_theme = active_theme()
      )
      title_text <- "Satisfacción con la Limpieza de Calles"
  } else  {
    water_quality_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q92",
      metadata = survey_data()$metadata
    )
    
    map <- create_interval_district_map(
      data = water_quality_data,
      geo_data = geo_data(),
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
    
    title_text <- "Satisfacción con la Calidad del Agua"
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