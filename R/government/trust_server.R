trustServer <- function(input, output, session,current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  
  survey_data <- reactive({
    survey_id <- paste0("PAR_", selectedYear())
    load_survey_data(survey_id)
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
  
  # Helper function to create trust bar chart
  create_trust_bar_chart <- function(data, title, custom_theme = NULL) {
    if (is.null(data) || nrow(data) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    # Get primary color from theme
    primary_color <- if (!is.null(custom_theme)) {
      custom_theme$colors$primary
    } else {
      "#1f77b4"  # Default blue
    }
    
    # Create horizontal bar chart
    plot_ly(
      data = data,
      y = ~Institution,
      x = ~Percentage,
      type = "bar",
      orientation = "h",
      marker = list(color = primary_color),
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
    )
  }
  
  # Institution labels from the list provided
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
  
  # Plot 1: Institutions of popular election
  output$popular_election_institutions_plot <- renderPlotly({
    req(survey_data())
    
    # Get labels for this group
    labels <- sapply(popular_election_ids, function(id) institution_labels[[id]])
    
    # Calculate trust percentages
    trust_data <- calculate_trust_percentage(
      data = survey_data()$responses,
      question_ids = popular_election_ids,
      labels = labels
    )
    
    # Create bar chart
    create_trust_bar_chart(
      data = trust_data,
      title = "Nivel de confianza en instituciones de elección popular",
      custom_theme = current_theme()
    )
  })
  
  # Plot 2: Public institutions and media
  output$public_institutions_media_plot <- renderPlotly({
    req(survey_data())
    
    # Get labels for this group
    labels <- sapply(public_institutions_media_ids, function(id) institution_labels[[id]])
    
    # Calculate trust percentages
    trust_data <- calculate_trust_percentage(
      data = survey_data()$responses,
      question_ids = public_institutions_media_ids,
      labels = labels
    )
    
    # Create bar chart
    create_trust_bar_chart(
      data = trust_data,
      title = "Nivel de confianza en instituciones públicas y medios de comunicación",
      custom_theme = current_theme()
    )
  })
  
  # Plot 3: Public safety institutions
  output$public_safety_institutions_plot <- renderPlotly({
    req(survey_data())
    
    # Get labels for this group
    labels <- sapply(public_safety_ids, function(id) institution_labels[[id]])
    
    # Calculate trust percentages
    trust_data <- calculate_trust_percentage(
      data = survey_data()$responses,
      question_ids = public_safety_ids,
      labels = labels
    )
    
    # Create bar chart
    create_trust_bar_chart(
      data = trust_data,
      title = "Nivel de confianza en instituciones de seguridad pública",
      custom_theme = current_theme()
    )
  })
}