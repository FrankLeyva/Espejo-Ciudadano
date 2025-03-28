# Función del servidor para el Dashboard de Participación Cívica
civicServer <- function(input, output, session) {
  # Load survey data
  survey_data <- reactive({
    load_survey_data("PAR_2024")
  })
  
  # Load geographical data
  geo_data <- reactive({
    tryCatch({
      sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error cargando datos geográficos:", e$message), type = "error")
      NULL
    })
  })
  
  # Store current theme
  current_theme <- reactiveVal(theme_config)
  
  # Prepare data for political interest map (Q130)
  interest_data <- reactive({
    req(survey_data())
    
    prepare_interval_data(
      data = survey_data()$responses,
      question_id = "Q130",
      metadata = survey_data()$metadata
    )
  })
  
  # Render political interest map
  output$interest_map <- renderLeaflet({
    req(interest_data(), geo_data())
    
    create_interval_district_map(
      data = interest_data(), 
      geo_data = geo_data(),
      # Select responses from "POCO" to "MUCHO" (2, 3, 4, 5)
      selected_responses = c("2", "3", "4", "5"),
      highlight_extremes = TRUE,
      use_gradient = TRUE,
      color_scale = "Blues",
      custom_theme = current_theme()
    )
  })
  
  # Participation mechanisms knowledge plot (Q138.1-Q138.13)
  output$mechanisms_plot <- renderPlotly({
    req(survey_data())
    
    tryCatch({
      # List of mechanisms questions
      mechanism_questions <- paste0("Q138.", 1:13)
      
      # Mechanism labels
      mechanism_labels <- c(
        "El referendum", 
        "El plebiscito", 
        "La iniciativa ciudadana", 
        "La revocación de mandato",
        "Audiencias públicas",
        "Consulta pública",
        "Consejos consultivos",
        "Comités de participación",
        "Planeación participativa",
        "Presupuesto participativo",
        "Cabildo abierto",
        "Contralorías sociales",
        "Mecanismos para niñas, niños y adolescentes"
      )
      
      # Calculate knowledge percentages
      percentages <- numeric(length(mechanism_questions))
      counts <- numeric(length(mechanism_questions))
      
      for (i in 1:length(mechanism_questions)) {
        values <- survey_data()$responses[[mechanism_questions[i]]]
        values <- values[!is.na(values)]
        
        if(length(values) > 0) {
          counts[i] <- sum(values == "1")
          percentages[i] <- 100 * counts[i] / length(values)
        } else {
          counts[i] <- 0
          percentages[i] <- 0
        }
      }
      
      # Create dataframe for plot
      plot_data <- data.frame(
        Mechanism = mechanism_labels,
        Count = counts,
        Percentage = percentages,
        stringsAsFactors = FALSE
      )
      
      # Sort by percentage descending
      plot_data <- plot_data[order(-plot_data$Percentage), ]
      
      # Get colors from theme
      primary_color <- if (!is.null(current_theme()$colors$primary)) {
        current_theme()$colors$primary
      } else {
        "#0d6efd"  # Default blue
      }
      
      highlight_color <- if (!is.null(current_theme()$colors$highlight)) {
        current_theme()$colors$highlight
      } else {
        "#28a745"  # Default green
      }
      
      # Highlight top three mechanisms
      colors <- rep(primary_color, nrow(plot_data))
      if(nrow(plot_data) >= 3) {
        colors[1:3] <- highlight_color
      } else {
        colors[1:nrow(plot_data)] <- highlight_color
      }
      
      # Create horizontal bar chart
      plot_ly(
        data = plot_data,
        y = ~Mechanism,
        x = ~Percentage,
        type = "bar",
        orientation = 'h',
        marker = list(color = colors),
        hoverinfo = "text",
        text = ~paste0(round(Percentage, 1), "%")
      ) %>%
        layout(
          title = "Conocimiento de mecanismos de participación ciudadana",
          xaxis = list(
            title = "Porcentaje de conocimiento",
            showgrid = TRUE,
            gridcolor = "#E1E1E1",
            range = c(0, 100)
          ),
          yaxis = list(
            title = "",
            automargin = TRUE,
            categoryorder = 'total ascending'
          ),
          margin = list(l = 200, r = 20, t = 40, b = 30),
          font = list(
            family = if (!is.null(current_theme()$typography$font_family)) current_theme()$typography$font_family else "Arial",
            size = if (!is.null(current_theme()$typography$sizes$text)) current_theme()$typography$sizes$text else 12
          )
        ) %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      warning(paste("Error creating mechanisms plot:", e$message))
      return(plotly_empty() %>% 
               layout(title = "Error generando visualización de mecanismos"))
    })
  })
  
  # Participation requirements plot (Q131.1-Q131.6)
  output$requirements_plot <- renderPlotly({
    req(survey_data())
    
    tryCatch({
      # List of requirement questions
      requirement_questions <- paste0("Q131.", 1:6)
      
      # Requirement labels
      requirement_labels <- c(
        "Información",
        "Espacios para participar",
        "Tiempo",
        "Dinero",
        "Confianza en las instituciones",
        "Otro"
      )
      
      # Calculate percentages
      percentages <- numeric(length(requirement_questions))
      counts <- numeric(length(requirement_questions))
      
      for (i in 1:length(requirement_questions)) {
        values <- survey_data()$responses[[requirement_questions[i]]]
        values <- values[!is.na(values)]
        
        if(length(values) > 0) {
          counts[i] <- sum(values == "1")
          percentages[i] <- 100 * counts[i] / length(values)
        } else {
          counts[i] <- 0
          percentages[i] <- 0
        }
      }
      
      # Create dataframe for plot
      plot_data <- data.frame(
        Requirement = requirement_labels,
        Count = counts,
        Percentage = percentages,
        stringsAsFactors = FALSE
      )
      
      # Sort by percentage descending
      plot_data <- plot_data[order(-plot_data$Percentage), ]
      
      # Get colors from theme
      primary_color <- if (!is.null(current_theme()$colors$primary)) {
        current_theme()$colors$primary
      } else {
        "#0d6efd"  # Default blue
      }
      
      highlight_color <- if (!is.null(current_theme()$colors$highlight)) {
        current_theme()$colors$highlight
      } else {
        "#28a745"  # Default green
      }
      
      # Highlight top three requirements
      colors <- rep(primary_color, nrow(plot_data))
      if(nrow(plot_data) >= 3) {
        colors[1:3] <- highlight_color
      } else {
        colors[1:nrow(plot_data)] <- highlight_color
      }
      
      # Create horizontal bar chart
      plot_ly(
        data = plot_data,
        y = ~Requirement,
        x = ~Percentage,
        type = "bar",
        orientation = 'h',
        marker = list(color = colors),
        hoverinfo = "text",
        text = ~paste0(round(Percentage, 1), "%")
      ) %>%
        layout(
          title = "Factores necesarios para participar en asuntos públicos",
          xaxis = list(
            title = "Porcentaje",
            showgrid = TRUE,
            gridcolor = "#E1E1E1",
            range = c(0, 100)
          ),
          yaxis = list(
            title = "",
            automargin = TRUE,
            categoryorder = 'total ascending'
          ),
          margin = list(l = 150, r = 20, t = 40, b = 30),
          font = list(
            family = if (!is.null(current_theme()$typography$font_family)) current_theme()$typography$font_family else "Arial",
            size = if (!is.null(current_theme()$typography$sizes$text)) current_theme()$typography$sizes$text else 12
          )
        ) %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      warning(paste("Error creating requirements plot:", e$message))
      return(plotly_empty() %>% 
               layout(title = "Error generando visualización de requisitos"))
    })
  })
}