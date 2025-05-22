# participation_server.R - Updated with Enhanced Data Management

participationServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("participacion")
    }
  })
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("participation", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PAR_", selectedYear())
    plot_list <- list()
    
    plot_key <- paste0("interest_pie_", survey_id)
plot_list$interest_pie <- data_manager$get_or_create_plot(
  plot_key = plot_key,
  plot_function = function() {
    interest_data <- data_manager$get_processed_data(survey_id, "Q131", "categorical")
    interest_data <- interest_data[!interest_data$value %in% c("Ns/Nc"), ]
    
    # Create pie chart using the standard function
    create_category_pie(
      interest_data,
      max_categories = 5,
      custom_theme = active_theme(),
      highlight_max = FALSE,
      palette = 'sequential'
    ) %>% apply_plotly_theme(title = '', custom_theme = active_theme())
  }
)
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "participation", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("participation_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PAR_", selectedYear())
    map_list <- list()
    
    # Voting Importance Map
    voting_data <- data_manager$get_processed_data(survey_id, "Q139", "interval")
    map_list$voting_map <- create_interval_district_map(
      data = voting_data, 
      geo_data = geo_data(),
      # Select responses for "Importante" and "Poco importante" (1 and 2)
      selected_responses = c("1", "2"),
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("participation_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PAR_", selectedYear())
    
    # Get processed data
    social_movement_data <- data_manager$get_processed_data(survey_id, "Q136", "binary")
    
    calc_list <- list()
    
    # Calculate social movement support percentage
    if (!is.null(social_movement_data)) {
      support_count <- sum(social_movement_data$binary_value, na.rm = TRUE)
      total_count <- nrow(social_movement_data)
      if (total_count > 0) {
        support_percent <- 100 * support_count / total_count
        calc_list$social_movement_support <- sprintf("%.1f%% de los encuestados", support_percent)
      } else {
        calc_list$social_movement_support <- "Datos no disponibles"
      }
    } else {
      calc_list$social_movement_support <- "Datos no disponibles"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Render outputs
  output$interest_pie <- renderPlotly({
    plots()$interest_pie
  })
  
  output$voting_map <- renderLeaflet({
    maps()$voting_map
  })
  
  output$social_movement_support <- renderText({
    calculations()$social_movement_support
  })
  
  # Download handler for voting map
  output$download_voting_map <- downloadHandler(
    filename = function() {
      paste("mapa_voto_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the map
      map <- maps()$voting_map
      
      # Add title and footer to the map
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
      
      # Save and convert
      htmlwidgets::saveWidget(map, tmp_html, selfcontained = TRUE)
      
      pagedown::chrome_print(
        input = tmp_html,
        output = file,
        options = list(printBackground = TRUE, scale = 2.0),
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