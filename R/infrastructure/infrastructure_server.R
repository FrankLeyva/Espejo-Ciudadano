# infrastructure_server.R - Updated with Enhanced Data Management

infrastructureServer <- function(input, output, session, current_theme = NULL) {
  # Get dependencies from userData
  selectedYear <- session$userData$selectedYear
  data_manager <- session$userData$data_manager
  geo_data <- session$userData$geoData
  
  active_theme <- reactive({
    if (is.function(current_theme)) {
      current_theme()
    } else if (!is.null(current_theme)) {
      current_theme
    } else {
      get_section_theme("infraestructura")
    }
  })
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("infrastructure", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    plot_list <- list()
    
    # Healthcare Plot
    plot_key <- paste0("healthcare_overview_", survey_id)
    plot_list$healthcare_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        create_healthcare_overview(survey_data$responses, active_theme())
      }
    )
    
    # Utilities Plot
    plot_key <- paste0("utilities_overview_", survey_id)
    plot_list$utilities_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        create_utilities_overview(survey_data$responses, active_theme())
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "infrastructure", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("infrastructure_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    survey_data <- data_manager$get_survey_data(survey_id)
    map_list <- list()
    
    # Education Map
    map_list$education_map <- create_education_overview(
      survey_data$responses, 
      geo_data(), 
      active_theme()
    )
    
    # Housing Map
    map_list$housing_map <- create_housing_overview(
      survey_data$responses, 
      geo_data(), 
      active_theme()
    )
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations if needed
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("infrastructure_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    # Add any calculations for value boxes here
    calc_list <- list()
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Render Education Map
  output$education_plot <- renderLeaflet({
    maps()$education_map
  })
  
  # Render Healthcare Plot
  output$healthcare_plot <- renderPlotly({
    plots()$healthcare_plot
  })
  
  # Render Utilities Plot
  output$utilities_plot <- renderPlotly({
    plots()$utilities_plot
  })
  
  # Render Housing Map
  output$housing_map <- renderLeaflet({
    maps()$housing_map
  })
  
  # Education Map Download Handler
  output$download_gen_students_map <- downloadHandler(
    filename = function() {
      paste("mapa_estudiantes_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create a temporary HTML file
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the map
      map <- maps()$education_map
      
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
        browser = "/usr/bin/google-chrome",
        extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
      )
      
      # Clean up temporary files
      if (file.exists(tmp_html)) {
        file.remove(tmp_html)
      }
    }
  )
  
  # Housing Map Download Handler
  output$download_housing_map <- downloadHandler(
    filename = function() {
      paste("mapa_viviendas_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create a temporary HTML file
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the map
      map <- maps()$housing_map
      
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