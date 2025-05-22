# trust_server.R - Updated with Enhanced Data Management

trustServer <- function(input, output, session, current_theme = NULL) {
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
    saved_plots <- data_manager$load_saved_plots("trust", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PAR_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Institution labels 
    institution_labels <- list(
      # Popular election
      "1" = "Diputados",
      "17" = "Presidente Municipal (Cruz Pérez Cuellar)",
      "18" = "Regidores",
      "19" = "Síndico/a",
      "20" = "Funcionarios del Gob. Municipal",
      "21" = "Gobernador/a (Maru Campos)",
      "22" = "Funcionarios del Gob. Del Estado",
      "23" = "Presidente de República (Andrés Manuel López Obrador)",
      "24" = "Funcionarios del Gob. Federal",
      
      # Public institutions and media
      "2" = "Prensa local (periódicos)",
      "3" = "Televisoras locales",
      "4" = "Iglesias",
      "5" = "Maestros",
      "6" = "Partidos políticos",
      "7" = "Organizaciones ciudadanas (OSCs)",
      "8" = "Universidades",
      "9" = "Empresarios",
      
      # Public safety
      "10" = "Policía municipal",
      "11" = "Tránsito municipal",
      "12" = "Policía Ministerial",
      "13" = "Policía Estatal",
      "14" = "Ejército",
      "15" = "Guardia Nacional",
      "16" = "Jueces o magistrados"
    )
    
    # Group definitions
    popular_election_ids <- c("1", "17", "18", "19", "20", "21", "22", "23", "24")
    public_institutions_media_ids <- c("2", "3", "4", "5", "6", "7", "8", "9")
    public_safety_ids <- c("10", "11", "12", "13", "14", "15", "16")
    
    # Helper function to calculate trust percentages
    calculate_trust_percentage <- function(data, question_ids, labels = NULL) {
      # Initialize vectors to store results
      question_labels <- c()
      trust_percentages <- c()
      count_responses <- c()
      
      # Process each question
      for (i in 1:length(question_ids)) {
        q_id <- question_ids[i]
        full_q_id <- paste0("Q22.", q_id)
        
        # Get data for this question
        values <- data[[full_q_id]]
        values <- values[!is.na(values)]
        
        # Remove NS/NC responses (code 5)
        values <- values[values != "5"]
        
        if (length(values) > 0) {
          # Count trust responses (codes 3 and 4)
          trust_count <- sum(values == "3" | values == "4", na.rm = TRUE)
          
          # Calculate percentage
          percentage <- 100 * trust_count / length(values)
          
          # Get label if provided, otherwise use question ID
          label <- if (!is.null(labels) && length(labels) >= i) labels[i] else paste0("Pregunta ", q_id)
          
          # Add to results
          question_labels <- c(question_labels, label)
          trust_percentages <- c(trust_percentages, percentage)
          count_responses <- c(count_responses, length(values))
        }
      }
      
      # Create data frame with results
      if (length(question_labels) > 0) {
        results_df <- data.frame(
          Institution = question_labels,
          Percentage = trust_percentages,
          Responses = count_responses
        )
        
        # Sort by percentage in descending order
        results_df <- results_df[order(-results_df$Percentage), ]
        return(results_df)
      } else {
        return(NULL)
      }
    }
    
    # Plot 1: Institutions of popular election
    plot_key <- paste0("popular_election_institutions_", survey_id)
    plot_list$popular_election_institutions <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        # Get labels for this group
        labels <- sapply(popular_election_ids, function(id) institution_labels[[id]])
        
        # Calculate trust percentages
        trust_data <- calculate_trust_percentage(
          data = survey_data$responses,
          question_ids = popular_election_ids,
          labels = labels
        )
        
        # Create bar chart
        create_trust_bar_chart(
          data = trust_data,
          title = "",
          custom_theme = active_theme()
        )
      }
    )
    
    # Plot 2: Public institutions and media
    plot_key <- paste0("public_institutions_media_", survey_id)
    plot_list$public_institutions_media <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        # Get labels for this group
        labels <- sapply(public_institutions_media_ids, function(id) institution_labels[[id]])
        
        # Calculate trust percentages
        trust_data <- calculate_trust_percentage(
          data = survey_data$responses,
          question_ids = public_institutions_media_ids,
          labels = labels
        )
        
        # Create bar chart
        create_trust_bar_chart(
          data = trust_data,
          title = "",
          custom_theme = active_theme()
        )
      }
    )
    
    # Plot 3: Public safety institutions
    plot_key <- paste0("public_safety_institutions_", survey_id)
    plot_list$public_safety_institutions <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        
        # Get labels for this group
        labels <- sapply(public_safety_ids, function(id) institution_labels[[id]])
        
        # Calculate trust percentages
        trust_data <- calculate_trust_percentage(
          data = survey_data$responses,
          question_ids = public_safety_ids,
          labels = labels
        )
        
        # Create bar chart
        create_trust_bar_chart(
          data = trust_data,
          title = "",
          custom_theme = active_theme()
        )
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "trust", selectedYear())
    
    return(plot_list)
  })
  
  # Helper function to create trust bar chart
  create_trust_bar_chart <- function(data, title, custom_theme = NULL) {
    if (is.null(data) || nrow(data) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    # Get colors from theme
    primary_color <- if (!is.null(active_theme())) {
      active_theme()$colors$primary
    } else {
      "#1f77b4"  # Default blue
    }
    
    highlight_color <- if (!is.null(active_theme())) {
      active_theme()$colors$accent
    } else {
      "#ff7f0e"  # Default orange
    }
    
    # Create single color vector for all bars initially
    colors <- rep(primary_color, nrow(data))
    
    # Handle ties for highlighting top N items
    # First, identify the top 3 unique values
    unique_top_values <- unique(data$Percentage)[1:min(3, length(unique(data$Percentage)))]
    
    # Find all rows that have those top values
    top_indices <- which(data$Percentage %in% unique_top_values)
    
    # Highlight all those rows
    colors[top_indices] <- highlight_color
    
    # Create horizontal bar chart
    plot_ly(
      data = data,
      y = ~Institution,
      x = ~Percentage,
      type = "bar",
      orientation = "h",
      marker = list(color = colors),
      text = ~paste0(round(Percentage, 1), "%"),
      textposition = "auto",
      hoverinfo = "text",
      hovertext = ~paste0(Institution, ": ", round(Percentage, 1), "% (n=", Responses, ")")
    ) %>%
    layout(
      title = list(
        text = title,
        font = if (!is.null(custom_theme)) {
          list(
            family = custom_theme$typography$font_family,
            size = custom_theme$typography$sizes$title
          )
        } else {
          list(family = "Arial", size = 16)
        }
      ),
      xaxis = list(
        title = "Porcentaje de confianza",
        range = c(0, 100)
      ),
      yaxis = list(
        title = "",
        categoryorder = 'total ascending'
      ),
      margin = list(l = 150, r = 30, t = 50, b = 50)  # More space for labels
    ) %>% 
      apply_plotly_theme(custom_theme = custom_theme)
  }
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("trust_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    # Currently, this module doesn't have any value box calculations
    # but we keep the structure for consistency and future use
    calc_list <- list()
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Render outputs using the cached plots
  output$popular_election_institutions_plot <- renderPlotly({
    plots()$popular_election_institutions
  })
  
  output$public_institutions_media_plot <- renderPlotly({
    plots()$public_institutions_media
  })
  
  output$public_safety_institutions_plot <- renderPlotly({
    plots()$public_safety_institutions
  })
}