expectationsServer <- function(input, output, session,current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  
  survey_data <- session$userData$parSurveyData
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
  # Municipal Expectations Map (Q18)
  output$municipal_expectations_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data for Q18
    municipal_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q19",
      metadata = survey_data()$metadata
    )
    
    # Create interval district map
    create_interval_district_map(
      municipal_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  # State Expectations Map (Q19)
  output$state_expectations_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data for Q19
    state_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q20",
      metadata = survey_data()$metadata
    )
    
    # Create interval district map
    create_interval_district_map(
      state_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Purples",
      custom_theme = active_theme()
    )
  })
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
  # Federal Expectations Map (Q20)
  output$federal_expectations_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data for Q20
    federal_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q21",
      metadata = survey_data()$metadata
    )
    
    # Create interval district map
    create_interval_district_map(
      federal_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Reds",
      custom_theme = active_theme()
    )
  })
  
  output$gov_comparison_plot1 <- renderPlotly({
    req(survey_data())
    create_gov_comparison_plot(
      survey_data(), 
      "2", 
      "Toma en cuenta a ciudadanos",
      active_theme()
    )
  })
  
  output$gov_comparison_plot2 <- renderPlotly({
    req(survey_data())
    create_gov_comparison_plot(
      survey_data(), 
      "3", 
      "Cumple compromisos y metas",
      active_theme()
    )
  })
  
  output$gov_comparison_plot3 <- renderPlotly({
    req(survey_data())
    create_gov_comparison_plot(
      survey_data(), 
      "4", 
      "Aplica la ley de manera imparcial",
      active_theme()
    )
  })
observeEvent(input$expectations_tabs, {
  # Store the active tab in a reactive value for the download handler
  tab_value <- input$expectations_tabs
})
# Download handler that adapts based on active tab
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
  
  # Create the appropriate map based on active tab
  if(input$expectations_tabs == "Gobierno Municipal") {
    municipal_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q19",
      metadata = survey_data()$metadata
    )
    
    # Create interval district map
    map <- create_interval_district_map(
      municipal_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
    title_text <- "Calificación de Expectativas Ciudadanas del Gobierno Municipal por Distrito"
  } else if(input$expectations_tabs == "Gobierno Estatal") {
    state_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q20",
      metadata = survey_data()$metadata
    )
    
    # Create interval district map
    map <- create_interval_district_map(
      state_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Purples",
      custom_theme = active_theme()
    )
    title_text <- "Calificación de Expectativas Ciudadanas del Gobierno Estatal por Distrito"
  }  else  {
    federal_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q21",
      metadata = survey_data()$metadata
    )
    
    # Create interval district map
    map <- create_interval_district_map(
      federal_data,
      geo_data(),
      selected_responses = NULL,  # Show mean values
      highlight_extremes = TRUE,
      use_gradient = F,
      color_scale = "Reds",
      custom_theme = active_theme()
    )
  
    title_text <- "Calificación de Expectativas Ciudadanas del Gobierno Federal por Distrito"
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