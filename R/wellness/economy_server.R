# economy_server.R - Updated with Enhanced Data Management

economyServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("bienestar")
    }
  })
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("economic", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Income situation pie chart
    plot_key <- paste0("income_situation_pie_", survey_id)
    plot_list$income_situation_pie <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        # Get processed data for Q5
        income_situation_data <- data_manager$get_processed_data(survey_id, "Q5", "categorical")
        
        # Create pie chart with the data
        create_category_pie(
          income_situation_data,
          max_categories = 5,  
          custom_theme = active_theme(),
          highlight_max = FALSE,
          palette = 'sequential',
          inverse = TRUE,
          truncate_labels = TRUE
        ) %>% 
          layout(title = "") %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "economy", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("economy_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    map_list <- list()
    
    # Economic improvement map
    economic_improvement_data <- data_manager$get_processed_data(survey_id, "Q4", "interval")
    map_list$economic_improvement_map <- create_interval_district_map(
      economic_improvement_data,
      geo_data(),
      selected_responses = c("4", "5"),  # Values for "Mejorado algo" and "Mejorado mucho"
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
    
    calc_cache_key <- paste0("economy_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    
    # Get processed data for income situation
    income_situation_data <- data_manager$get_processed_data(survey_id, "Q5", "categorical")
    
    calc_list <- list()
    
    # Calculate income sufficiency percentage (options 1 and 2)
    if (!is.null(income_situation_data)) {
      sufficient_count <- sum(income_situation_data$value %in% c(
        "Les alcanza bien y pueden ahorrar", 
        "Les alcanza justo, sin grandes dificultades"
      ))
      total_count <- nrow(income_situation_data)
      calc_list$income_sufficiency_text <- paste0(round(100 * sufficient_count / total_count, 1), "%")
      
      # Calculate savings capability percentage (option 1 only)
      savings_count <- sum(income_situation_data$value == "Les alcanza bien y pueden ahorrar")
      calc_list$savings_capability_text <- paste0(round(100 * savings_count / total_count, 1), "%")
    } else {
      calc_list$income_sufficiency_text <- "N/A"
      calc_list$savings_capability_text <- "N/A"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Render outputs
  output$income_situation_pie <- renderPlotly({
    plots()$income_situation_pie
  })
  
  output$economic_improvement_map <- renderLeaflet({
    maps()$economic_improvement_map
  })
  
  # Render value box text outputs
  output$income_sufficiency_text <- renderText({
    calculations()$income_sufficiency_text
  })
  
  output$savings_capability_text <- renderText({
    calculations()$savings_capability_text
  })
  
  # Download handler for economy map
  output$download_economy_map <- downloadHandler(
    filename = function() {
      paste("mapa_economia_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the map
      map <- maps()$economic_improvement_map
      
      # Add title and footer
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      "Porcentaje de la población que considera que mejoró su economía en el 2024",
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
      
      # Clean up
      if (file.exists(tmp_html)) {
        file.remove(tmp_html)
      }
    }
  )
}