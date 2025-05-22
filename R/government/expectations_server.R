# expectations_server.R - Updated with Enhanced Data Management

expectationsServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("gobierno")
    }
  })
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("expectations", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PAR_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Government comparison plots
    # Plot 1: Taking citizens into account
    plot_key <- paste0("gov_comparison_plot1_", survey_id)
    plot_list$gov_comparison_plot1 <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        create_gov_comparison_plot(
          survey_data, 
          "2", 
          "Toma en cuenta a ciudadanos",
          active_theme()
        )
      }
    )
    
    # Plot 2: Fulfilling commitments and goals
    plot_key <- paste0("gov_comparison_plot2_", survey_id)
    plot_list$gov_comparison_plot2 <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        create_gov_comparison_plot(
          survey_data, 
          "3", 
          "Cumple compromisos y metas",
          active_theme()
        )
      }
    )
    
    # Plot 3: Applying the law impartially
    plot_key <- paste0("gov_comparison_plot3_", survey_id)
    plot_list$gov_comparison_plot3 <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        create_gov_comparison_plot(
          survey_data, 
          "4", 
          "Aplica la ley de manera imparcial",
          active_theme()
        )
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "expectations", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("expectations_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PAR_", selectedYear())
    map_list <- list()
    
    # Municipal Expectations Map
    municipal_data <- data_manager$get_processed_data(survey_id, "Q19", "interval")
    map_list$municipal_expectations_map <- create_interval_district_map(
      municipal_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
    
    # State Expectations Map
    state_data <- data_manager$get_processed_data(survey_id, "Q20", "interval")
    map_list$state_expectations_map <- create_interval_district_map(
      state_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Purples",
      custom_theme = active_theme()
    )
    
    # Federal Expectations Map
    federal_data <- data_manager$get_processed_data(survey_id, "Q21", "interval")
    map_list$federal_expectations_map <- create_interval_district_map(
      federal_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Reds",
      custom_theme = active_theme()
    )
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("expectations_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PAR_", selectedYear())
    
    # Get processed data
    municipal_data <- data_manager$get_processed_data(survey_id, "Q19", "interval")
    state_data <- data_manager$get_processed_data(survey_id, "Q20", "interval")
    federal_data <- data_manager$get_processed_data(survey_id, "Q21", "interval")
    
    calc_list <- list()
    
    # Calculate mean expectations for each level of government
    if (!is.null(municipal_data)) {
      calc_list$municipal_expectation_mean <- round(mean(municipal_data$value_num, na.rm = TRUE), 1)
    } else {
      calc_list$municipal_expectation_mean <- "N/A"
    }
    
    if (!is.null(state_data)) {
      calc_list$state_expectation_mean <- round(mean(state_data$value_num, na.rm = TRUE), 1)
    } else {
      calc_list$state_expectation_mean <- "N/A"
    }
    
    if (!is.null(federal_data)) {
      calc_list$federal_expectation_mean <- round(mean(federal_data$value_num, na.rm = TRUE), 1)
    } else {
      calc_list$federal_expectation_mean <- "N/A"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$expectations_tabs)
    
    active_tab <- input$expectations_tabs
    
    tooltip_content <- switch(active_tab,
      "Gobierno Municipal" = "<b>ID</b>: PAR Q19  <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Municipal ?<br>
               <b>Escala</b>: 1-10",
      "Gobierno Estatal" = "<b>ID</b>: PAR Q20  <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Estatal ?<br>
               <b>Escala</b>: 1-10",
      "Gobierno Federal" = "<b>ID</b>: PAR Q21  <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Federal ?<br>
               <b>Escala</b>: 1-10",
      "<b>ID</b>: PAR Q19  <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Municipal ?<br>
               <b>Escala</b>: 1-10"
    )
    
    update_tooltip_content(session, "expectations_tooltip", tooltip_content)
  })

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q19  <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Municipal ?<br>
               <b>Escala</b>: 1-10"
    
    update_tooltip_content(session, "expectations_tooltip", initial_tooltip)
  }, once = TRUE)  
  
  observe({
    req(input$perception_tabs)
    
    active_tab <- input$perception_tabs
    
    tooltip_content <- switch(active_tab,
      "Toma en cuenta a ciudadanos" = "<b>ID</b>: PAR Q15.2 Q16.2 Q17.2  <br>
              <b>Pregunta</b>: Tomó en cuenta a los ciudadanos (Municipal/Estatal/Federal) <br>
               <b>Escala</b>: 1=Nunca (nunca lo hace); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Cumple compromisos y metas" = "<b>ID</b>: PAR Q15.3 Q16.3 Q17.3  <br>
              <b>Pregunta</b>: 	Cumplio con sus compromisos y promesas (Municipal/Estatal/Federal) <br>
               <b>Escala</b>: 1=Nunca (nunca lo hace); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Aplica la ley de manera imparcial" = "<b>ID</b>: PAR Q15.4 Q16.4 Q17.4  <br>
              <b>Pregunta</b>: Aplico imparcialmente las leyes (Municipal/Estatal/Federal) <br>
               <b>Escala</b>: 1=Nunca (nunca lo hace); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "<b>ID</b>: PAR Q15.2 Q16.2 Q17.2  <br>
              <b>Pregunta</b>: Tomó en cuenta a los ciudadanos (Municipal/Estatal/Federal) <br>
               <b>Escala</b>: 1=Nunca (nunca lo hace); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    )
    
    update_tooltip_content(session, "perception_tooltip", tooltip_content)
  })

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q15.2 Q16.2 Q17.2  <br>
              <b>Pregunta</b>: Tomó en cuenta a los ciudadanos (Municipal/Estatal/Federal) <br>
               <b>Escala</b>: 1=Nunca (nunca lo hace); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    
    update_tooltip_content(session, "perception_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Helper function for creating government comparison plots
  create_gov_comparison_plot <- function(data, question_index, question_label, active_theme) {
    # Define the questions we want to compare (using index to get corresponding questions)
    question_ids <- list(
      municipal = paste0("Q16.", question_index),
      state = paste0("Q17.", question_index),
      federal = paste0("Q18.", question_index)
    )
    
    # Define the answer scale
    answer_scale <- c(
      "1" = "Nunca",
      "2" = "Poco",
      "3" = "Algo", 
      "4" = "Mucho"
      # Removed NS/NC (5) since we'll filter it out
    )
    
    # Initialize data frame for frequencies
    freq_data <- data.frame(
      Answer = character(),
      Government = character(),
      Count = integer(),
      Percentage = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Calculate frequencies for each government level
    for (gov_level in names(question_ids)) {
      gov_name <- switch(gov_level,
                         "municipal" = "Municipal",
                         "state" = "Estatal",
                         "federal" = "Federal")
      
      q_id <- question_ids[[gov_level]]
      
      # Get values and convert to numeric
      values <- as.numeric(data$responses[[q_id]])
      
      # Remove NS/NC (value 5)
      values <- values[!is.na(values) & values != 5]
      
      # Calculate frequencies
      value_table <- table(values)
      value_counts <- as.data.frame(value_table)
      colnames(value_counts) <- c("Value", "Count")
      value_counts$Value <- as.character(value_counts$Value)
      
      # Add percentage
      value_counts$Percentage <- 100 * value_counts$Count / sum(value_counts$Count)
      
      # Add answer labels and government level
      value_counts$Answer <- answer_scale[value_counts$Value]
      value_counts$Government <- gov_name
      
      # Add to main data frame
      freq_data <- rbind(
        freq_data,
        data.frame(
          Answer = value_counts$Answer,
          Value = as.numeric(value_counts$Value),
          Government = value_counts$Government,
          Count = value_counts$Count,
          Percentage = value_counts$Percentage,
          stringsAsFactors = FALSE
        )
      )
    }
    
    # Ensure the answers are ordered correctly
    freq_data$Answer <- factor(freq_data$Answer, 
                              levels = answer_scale, 
                              ordered = TRUE)
    
    # Get colors from theme
    gov_colors <- active_theme$palettes$categorical
    
    # Create grouped bar chart
    plot_ly(
      data = freq_data,
      x = ~Answer,
      y = ~Percentage,
      color = ~Government,
      colors = gov_colors,
      type = "bar",
      # Add text that will appear on hover
      hoverinfo = "text",
      hovertext = ~paste0(
        Government, " - ", Answer, "<br>",
        "Frecuencia: ", Count, "<br>",
        "Porcentaje: ", round(Percentage, 1), "%"
      ),
      # Add text labels to be displayed on the bars
      text = ~paste0(round(Percentage, 0), "%"),
      textposition = "inside",
      insidetextanchor = "middle",
      textfont = list(
        color = "white",
        size = 12
      )
    ) %>%
      apply_plotly_theme(
        title = paste("Percepción:", question_label),
        xlab = "Respuesta",
        ylab = "Porcentaje (%)",
        custom_theme = active_theme
      ) %>%
      layout(
        barmode = "group",
        yaxis = list(range = c(0, 60)),  # Scale from 0 to 100%
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = 1.1
        ),
        margin = list(t = 100) # Add margin at the top for the legend
      )
  }
  
  # Render outputs - Maps
  output$municipal_expectations_map <- renderLeaflet({
    maps()$municipal_expectations_map
  })
  
  output$state_expectations_map <- renderLeaflet({
    maps()$state_expectations_map
  })
  
  output$federal_expectations_map <- renderLeaflet({
    maps()$federal_expectations_map
  })
  
  # Render outputs - Comparison plots
  output$gov_comparison_plot1 <- renderPlotly({
    plots()$gov_comparison_plot1
  })
  
  output$gov_comparison_plot2 <- renderPlotly({
    plots()$gov_comparison_plot2
  })
  
  output$gov_comparison_plot3 <- renderPlotly({
    plots()$gov_comparison_plot3
  })
  
  # Render outputs - Calculation values
  output$municipal_expectation_mean <- renderText({
    calculations()$municipal_expectation_mean
  })
  
  output$state_expectation_mean <- renderText({
    calculations()$state_expectation_mean
  })
  
  output$federal_expectation_mean <- renderText({
    calculations()$federal_expectation_mean
  })
  
  # Download handler for maps
  output$download_expectations_map <- downloadHandler(
    filename = function() {
      # Get map type for filename based on active tab
      map_type <- if(input$expectations_tabs == "Gobierno Municipal"){ 
                        "Municipal"} else if (input$expectations_tabs == "Gobierno Estatal"){
                          "Estatal" } else {
                          "Federal"
                        }
      paste("mapa_expectativas_gobierno_", map_type, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Temporary file for the HTML content
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the appropriate map based on active tab
      if(input$expectations_tabs == "Gobierno Municipal") {
        map <- maps()$municipal_expectations_map
        title_text <- "Calificación de Expectativas Ciudadanas del Gobierno Municipal por Distrito"
      } else if(input$expectations_tabs == "Gobierno Estatal") {
        map <- maps()$state_expectations_map
        title_text <- "Calificación de Expectativas Ciudadanas del Gobierno Estatal por Distrito"
      } else {
        map <- maps()$federal_expectations_map
        title_text <- "Calificación de Expectativas Ciudadanas del Gobierno Federal por Distrito"
      }
      
      # Add title and footer to the map
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
      
      # Save and convert to PNG
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
      
      # Clean up temporary files
      if (file.exists(tmp_html)) {
        file.remove(tmp_html)
      }
    }
  )
}