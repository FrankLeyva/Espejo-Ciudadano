expectationsServer <- function(input, output, session,current_theme = NULL) {
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
  
# Government Comparison Bar Chart
output$government_comparison_plot <- renderPlotly({
  req(survey_data())
  
  # Define the questions we want to compare
  question_ids <- list(
    municipal = c("Q16.2", "Q16.3", "Q16.4"),
    state = c("Q17.2", "Q17.3", "Q17.4"),
    federal = c("Q18.2", "Q18.3", "Q18.4")
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
    Mode = character(),
    ModeValue = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Helper function to calculate mode
  find_mode <- function(x) {
    # Remove NA values
    x <- x[!is.na(x)]
    if(length(x) == 0) return(NA)
    
    # Calculate frequencies
    freq_table <- table(x)
    # Find the value with highest frequency
    mode_val <- as.numeric(names(freq_table)[which.max(freq_table)])
    return(mode_val)
  }
  
  # Calculate means and modes for each question and government level
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
      
      # Calculate mode
      mode_val <- find_mode(values)
      # Get mode label
      mode_label <- answer_scale[as.character(mode_val)]
      
      # Add to data frame
      comparison_data <- rbind(
        comparison_data,
        data.frame(
          Question = q_label,
          Government = gov_name,
          Mean = mean_val,
          Mode = mode_label,
          ModeValue = mode_val,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  # Get colors from theme
  gov_colors <- active_theme()$palettes$categorical
  
  # Create grouped bar chart
  plot_ly(
    data = comparison_data,
    x = ~Question,
    y = ~Mean,
    color = ~Government,
    colors = gov_colors,
    type = "bar",
    # Add text that will appear on hover
    hoverinfo = "text",
    hovertext = ~paste0(
      Government, "<br>",
      "Respuesta más común: ", Mode
    ),
    # Add text labels to be displayed on the bars
    text = ~paste0(round(Mean, 2)),
    textposition = "outside", # Options: "inside", "outside", "auto", "none"
    insidetextanchor = "middle",
    textfont = list(
      color = "black",
      size = 12
    )
  ) %>%
    apply_plotly_theme(
      title = "Percepción de los tres niveles de gobierno",
      xlab = "",
      ylab = "Promedio (1-4)",
      custom_theme = active_theme()
    ) %>%
    layout(
      barmode = "group",
      yaxis = list(range = c(1, 4)),  # Scale from 1 to 4
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = 1.1
      ),
      margin = list(t = 100) # Add margin at the top for the legend
    )
})
}