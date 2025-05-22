# community_server.R - Updated with Enhanced Data Management

communityServer <- function(input, output, session, current_theme = NULL) {
  # Get dependencies from userData
  selectedYear <- session$userData$selectedYear
  data_manager <- session$userData$data_manager
  
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
    saved_plots <- data_manager$load_saved_plots("community", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PAR_", selectedYear())
    plot_list <- list()
    
    # Organization participation plot
    plot_key <- paste0("organizations_plot_", survey_id)
    plot_list$organizations_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
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
        
        # Get the survey data
        survey_data <- data_manager$get_survey_data(survey_id)
        
        # Calculate participation percentages
        percentages <- numeric(length(organization_questions))
        counts <- numeric(length(organization_questions))
        
        for (i in 1:length(organization_questions)) {
          values <- survey_data$responses[[organization_questions[i]]]
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
        primary_color <- active_theme()$colors$primary
        highlight_color <- active_theme()$colors$accent
        
        if (is.null(highlight_color)) {
          highlight_color <- colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
        }
        
        # Create single color vector for all bars initially
        colors <- rep(primary_color, nrow(plot_data))
        
        # Handle ties for highlighting top 3 items
        unique_top_values <- unique(plot_data$Percentage)[1:min(3, length(unique(plot_data$Percentage)))]
        top_indices <- which(plot_data$Percentage %in% unique_top_values)
        colors[top_indices] <- highlight_color
        
        # Create horizontal bar chart
        plot_ly(
          data = plot_data,
          y = ~Organization,
          x = ~Percentage,
          type = "bar",
          orientation = 'h',
          marker = list(
            color = colors,
            line = list(color = active_theme()$colors$neutral, width = 1)
          ),
          hoverinfo = "text",
          text = ~paste0(round(Percentage, 1), "%")
        ) %>%
          apply_plotly_theme(
            title = "",
            xlab = "Porcentaje de participación (%)",
            ylab = "",
            custom_theme = active_theme()
          ) %>%
          layout(
            yaxis = list(categoryorder = 'total ascending'),
            xaxis = list(range = c(0, 100), ticksuffix = "%")
          )
      }
    )
    
    # Problem-solving activities plot
    plot_key <- paste0("activities_plot_", survey_id)
    plot_list$activities_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
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
        
        # Get the survey data
        survey_data <- data_manager$get_survey_data(survey_id)
        
        # Calculate participation percentages
        percentages <- numeric(length(activity_questions))
        counts <- numeric(length(activity_questions))
        
        for (i in 1:length(activity_questions)) {
          values <- survey_data$responses[[activity_questions[i]]]
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
        primary_color <- active_theme()$colors$primary
        highlight_color <- active_theme()$colors$accent
        
        if (is.null(highlight_color)) {
          highlight_color <- colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
        }
        
        # Create single color vector for all bars initially
        colors <- rep(primary_color, nrow(plot_data))
        
        # Handle ties for highlighting top 3 items
        unique_top_values <- unique(plot_data$Percentage)[1:min(3, length(unique(plot_data$Percentage)))]
        top_indices <- which(plot_data$Percentage %in% unique_top_values)
        colors[top_indices] <- highlight_color
        
        # Create horizontal bar chart
        plot_ly(
          data = plot_data,
          y = ~Activity,
          x = ~Percentage,
          type = "bar",
          orientation = 'h',
          marker = list(
            color = colors,
            line = list(color = active_theme()$colors$neutral, width = 1)
          ),
          hoverinfo = "text",
          text = ~paste0(round(Percentage, 1), "%")
        ) %>%
          apply_plotly_theme(
            title = "Actividades para resolver problemas comunitarios",
            xlab = "Porcentaje de participación (%)",
            ylab = "",
            custom_theme = active_theme()
          ) %>%
          layout(
            yaxis = list(categoryorder = 'total ascending'),
            xaxis = list(range = c(0, 100), ticksuffix = "%"),
            margin = list(l = 250, r = 20, t = 40, b = 30)
          )
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "community", selectedYear())
    
    return(plot_list)
  })
  
  # Render outputs
  output$organizations_plot <- renderPlotly({
    plots()$organizations_plot
  })
  
  output$activities_plot <- renderPlotly({
    plots()$activities_plot
  })
}