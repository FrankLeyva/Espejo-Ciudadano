# Función del servidor para el Dashboard de Participación Comunitaria
communityServer <- function(input, output, session,current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  
  survey_data <- session$userData$parSurveyData
  
  
  # Use the current theme
  active_theme <- reactive({
    if (is.function(current_theme)) {
      # If current_theme is a reactive function, call it to get the value
      current_theme()
    } else if (!is.null(current_theme)) {
      # If it's a direct value, use it
      current_theme
    } else {
      # Default to participacion theme if nothing provided
      get_section_theme("participacion")
    }
  })
  
  # Organization participation plot (Q132.1-Q132.11)
  output$organizations_plot <- renderPlotly({
    req(survey_data())
    
    tryCatch({
      # List of organization questions
      organization_questions <- paste0("Q134.", 1:11)
      
      # Organization labels
      organization_labels <- c(
        "Partidos/organizaciones políticas",
        "Sindicatos",
        "Iglesias/asociaciones religiosas",
        "Agrupaciones empresariales",
        "Asociaciones de padres de familia en escuelas",
        "Asociaciones de equipos deportivos",
        "Asociaciones/comités/juntas de vecinos",
        "Clubs sociales/de servicios",
        "Organizaciones civiles/ONGs/Fundaciones",
        "Grupo de autoayuda o superación",
        "Caja popular o cooperativa de crédito"
      )
      
      # Calculate participation percentages
      percentages <- numeric(length(organization_questions))
      counts <- numeric(length(organization_questions))
      
      for (i in 1:length(organization_questions)) {
        values <- survey_data()$responses[[organization_questions[i]]]
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
        Organization = organization_labels,
        Count = counts,
        Percentage = percentages,
        stringsAsFactors = FALSE
      )
      
      # Sort by percentage descending
      plot_data <- plot_data[order(-plot_data$Percentage), ]
      
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
colors <- rep(primary_color, nrow(plot_data))

# Handle ties for highlighting top N items
# First, identify the top 3 unique values
unique_top_values <- unique(plot_data$Percentage)[1:min(3, length(unique(plot_data$Percentage)))]

# Find all rows that have those top values
top_indices <- which(plot_data$Percentage %in% unique_top_values)

# Highlight all those rows
colors[top_indices] <- highlight_color
      
      # Create horizontal bar chart
      plot_ly(
        data = plot_data,
        y = ~Organization,
        x = ~Percentage,
        type = "bar",
        orientation = 'h',
        marker = list(color = colors),
        hoverinfo = "text",
        text = ~paste0(round(Percentage, 1), "%")
      ) %>%
        layout(
          title = "",
          xaxis = list(
            title = "Porcentaje de participación",
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
            family = if (!is.null(active_theme()$typography$font_family)) active_theme()$typography$font_family else "Arial",
            size = if (!is.null(active_theme()$typography$sizes$text)) active_theme()$typography$sizes$text else 12
          )
        ) %>%
        apply_plotly_theme()
    }, error = function(e) {
      warning(paste("Error creating organizations plot:", e$message))
      return(plotly_empty() %>% 
               layout(title = "Error generando visualización de organizaciones"))
    })
  })
  
  # Problem-solving activities plot (Q136.1-Q136.14)
  output$activities_plot <- renderPlotly({
    req(survey_data())
    
    tryCatch({
      # List of activity questions
      activity_questions <- paste0("Q138.", 1:14)
      
      # Activity labels
      activity_labels <- c(
        "Propuestas de planes, programas, leyes o políticas públicas",
        "Vigilar acciones/funciones de servidores/instituciones públicas",
        "Firmar cartas de apoyo/amparos",
        "Quejarse ante las autoridades",
        "Pedir apoyo a una sociedad civil",
        "Asistir a manifestaciones y/o marchas",
        "Colocar mantas, carteles o fotografías",
        "Repartir circulares o manifiestos",
        "Enviar mensajes, opiniones, imágenes por redes sociales",
        "Usar aplicación o herramienta electrónica de denuncia",
        "Resolver problema con vecinos (baches, alumbrado, etc.)",
        "Vigilar la correcta realización de obra pública",
        "Bloquear calles, avenidas, carreteras",
        "Mitin político"
      )
      
      # Calculate participation percentages
      percentages <- numeric(length(activity_questions))
      counts <- numeric(length(activity_questions))
      
      for (i in 1:length(activity_questions)) {
        values <- survey_data()$responses[[activity_questions[i]]]
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
        Activity = activity_labels,
        Count = counts,
        Percentage = percentages,
        stringsAsFactors = FALSE
      )
      
      # Sort by percentage descending
      plot_data <- plot_data[order(-plot_data$Percentage), ]
      
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
colors <- rep(primary_color, nrow(plot_data))

# Handle ties for highlighting top N items
# First, identify the top 3 unique values
unique_top_values <- unique(plot_data$Percentage)[1:min(3, length(unique(plot_data$Percentage)))]

# Find all rows that have those top values
top_indices <- which(plot_data$Percentage %in% unique_top_values)

# Highlight all those rows
colors[top_indices] <- highlight_color
      
      # Create horizontal bar chart
      plot_ly(
        data = plot_data,
        y = ~Activity,
        x = ~Percentage,
        type = "bar",
        orientation = 'h',
        marker = list(color = colors),
        hoverinfo = "text",
        text = ~paste0(round(Percentage, 1), "%")
      ) %>%
        layout(
          title = "Actividades para resolver problemas comunitarios",
          xaxis = list(
            title = "Porcentaje de participación",
            showgrid = TRUE,
            gridcolor = "#E1E1E1",
            range = c(0, 100)
          ),
          yaxis = list(
            title = "",
            automargin = TRUE,
            categoryorder = 'total ascending'
          ),
          margin = list(l = 250, r = 20, t = 40, b = 30),
          font = list(
            family = if (!is.null(active_theme()$typography$font_family)) active_theme()$typography$font_family else "Arial",
            size = if (!is.null(active_theme()$typography$sizes$text)) active_theme()$typography$sizes$text else 12
          )
        ) %>%
          apply_plotly_theme()
    }, error = function(e) {
      warning(paste("Error creating activities plot:", e$message))
      return(plotly_empty() %>% 
               layout(title = "Error generando visualización de actividades"))
    })
  })
}