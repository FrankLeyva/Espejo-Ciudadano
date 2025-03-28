expectationsServer <- function(input, output, session) {
  # Load survey data
  survey_data <- reactive({
    load_survey_data("PAR_2024") # Using PAR survey
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
  current_theme <- reactiveVal(theme_config)
  
  # Municipal Expectations Map (Q18)
  output$municipal_expectations_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data for Q18
    municipal_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q18",
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
      custom_theme = current_theme()
    )
  })
  
  # State Expectations Map (Q19)
  output$state_expectations_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data for Q19
    state_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q19",
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
      custom_theme = current_theme()
    )
  })
  
  # Federal Expectations Map (Q20)
  output$federal_expectations_map <- renderLeaflet({
    req(survey_data(), geo_data())
    
    # Prepare interval data for Q20
    federal_data <- prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q20",
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
      custom_theme = current_theme()
    )
  })
  
  # Government Comparison Bar Chart
  output$government_comparison_plot <- renderPlotly({
    req(survey_data())
    
    # Define the questions we want to compare
    question_ids <- list(
      municipal = c("Q15.2", "Q15.3", "Q15.4"),
      state = c("Q16.2", "Q16.3", "Q16.4"),
      federal = c("Q17.2", "Q17.3", "Q17.4")
    )
    
    # Labels for the questions
    question_labels <- c(
      "Toma en cuenta a ciudadanos",
      "Cumple compromisos y metas",
      "Aplica la ley de manera imparcial"
    )
    
    # Define the answer scale
    answer_scale <- c(
      "1" = "Nunca",
      "2" = "Poco",
      "3" = "Algo", 
      "4" = "Mucho",
      "5" = "NS/NC"
    )
    
    # Initialize data frame to store results
    comparison_data <- data.frame(
      Question = character(),
      Government = character(),
      Mean = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Calculate means for each question and government level
    for (gov_level in names(question_ids)) {
      gov_name <- switch(gov_level,
                         "municipal" = "Municipal",
                         "state" = "Estatal",
                         "federal" = "Federal")
      
      for (i in 1:length(question_ids[[gov_level]])) {
        q_id <- question_ids[[gov_level]][i]
        q_label <- question_labels[i]
        
        # Get values and convert to numeric
        values <- as.numeric(survey_data()$responses[[q_id]])
        
        # Remove NS/NC (value 5)
        values <- values[!is.na(values) & values != 5]
        
        # Calculate mean
        mean_val <- mean(values, na.rm = TRUE)
        
        # Add to data frame
        comparison_data <- rbind(
          comparison_data,
          data.frame(
            Question = q_label,
            Government = gov_name,
            Mean = mean_val,
            stringsAsFactors = FALSE
          )
        )
      }
    }
    
    # Get colors from theme
    gov_colors <- c(
      "Municipal" = "#1f77b4",  # Blue
      "Estatal" = "#9467bd",    # Purple
      "Federal" = "#d62728"     # Red
    )
    
    # Create grouped bar chart
    plot_ly(
      data = comparison_data,
      x = ~Question,
      y = ~Mean,
      color = ~Government,
      colors = gov_colors,
      type = "bar",
      text = ~paste0(Government, "<br>", round(Mean, 2)),
      hoverinfo = "text"
    ) %>%
      apply_plotly_theme(
        title = "Percepción de los tres niveles de gobierno",
        xlab = "",
        ylab = "Promedio (1-4)",
        custom_theme = current_theme()
      ) %>%
      layout(
        barmode = "group",
        yaxis = list(range = c(1, 4)),  # Scale from 1 to 4
        legend = list(orientation = "h", y = -0.15)
      )
  })
}