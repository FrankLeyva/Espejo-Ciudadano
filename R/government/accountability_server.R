# Server function for Accountability Dashboard
accountabilityServer <- function(input, output, session,current_theme = NULL) {
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
      # Default to gobierno theme if nothing provided
      get_section_theme("gobierno")
    }
  })
  
  observe({
    req(input$punishment_tabs)
    
    active_tab <- input$punishment_tabs
    
    tooltip_content <- switch(active_tab,
      "Gobierno Municipal" = "<b>ID</b>: PAR Q123 <br>
            <b>Pregunta</b>:		¿Usted cree que el GOBIERNO MUNICIPAL sanciona a los servidores públicos que son sorprendidos en actos de corrupción? <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Gobierno Estatal" = "<b>ID</b>: PAR Q124 <br>
            <b>Pregunta</b>:	¿Usted cree que el GOBIERNO ESTATAL  sanciona a los servidores públicos que son sorprendidos en actos de corrupción? <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Gobierno Federal" = "<b>ID</b>: PAR Q124 <br>
            <b>Pregunta</b>:	¿Usted cree que el GOBIERNO FEDERAL sanciona a los servidores públicos que son sorprendidos en actos de corrupción? <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "<b>ID</b>: PAR Q123 <br>
            <b>Pregunta</b>:		¿Usted cree que el GOBIERNO MUNICIPAL sanciona a los servidores públicos que son sorprendidos en actos de corrupción? <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    )
    
    update_tooltip_content(session, "punishment_tooltip", tooltip_content)
  })

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q123 <br>
            <b>Pregunta</b>:		¿Usted cree que el GOBIERNO MUNICIPAL sanciona a los servidores públicos que son sorprendidos en actos de corrupción? <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    
    update_tooltip_content(session, "punishment_tooltip", initial_tooltip)
  }, once = TRUE)  	



  observe({
    req(input$corruption_tabs)
    
    active_tab <- input$corruption_tabs
    
    tooltip_content <- switch(active_tab,
      "Gobierno Municipal" = "<b>ID</b>: PAR Q15.1 <br>
            <b>Pregunta</b>:		Llevó a cabo actos de corrupcion <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Gobierno Estatal" = "<b>ID</b>: PAR Q15.2 <br>
            <b>Pregunta</b>:	Llevó a cabo actos de corrupcion <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "Gobierno Federal" = "<b>ID</b>: PAR Q15.3 <br>
            <b>Pregunta</b>:	Llevó a cabo actos de corrupcion <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC",
      "<b>ID</b>: PAR Q15.1 <br>
            <b>Pregunta</b>:		Llevó a cabo actos de corrupcion <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    )
    
    update_tooltip_content(session, "corruption_tooltip", tooltip_content)
  })

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q15.1 <br>
            <b>Pregunta</b>:		Llevó a cabo actos de corrupcion <br>
             <b>Escala</b>:  1=Nunca (nunca lo hacen); 2=Poco (con poca frecuencia); 3=Algo (con algo de frecuencia); 4=Mucho (con mucha frecuencia); 5=NS/NC"
    
    update_tooltip_content(session, "corruption_tooltip", initial_tooltip)
  }, once = TRUE)  	



  # Callout for Q122 - Justice perception
  output$justice_perception <- renderText({
    req(survey_data())
    
    # Get values for Q122
    values <- as.numeric(survey_data()$responses$Q123)
    
    # Remove NS/NC (value 5)
    values <- values[!is.na(values) & values != 5]
    
    # Calculate mean
    mean_val <- mean(values, na.rm = TRUE)
    
    # Map mean value to corresponding label
    label <- case_when(
      mean_val <= 1.5 ~ "Siempre",
      mean_val <= 2.5 ~ "Casi siempre",
      mean_val <= 3.5 ~ "Casi nunca",
      TRUE ~ "Nunca"
    )
    
    return(label)
  })
  
  # Histograms for punishment of corruption (Q123, Q124, Q125)
  
  # Municipal Government - Q123
  output$municipal_punishment_hist <- renderPlotly({
    req(survey_data())
    
    # Get values for Q123
    values <- as.numeric(survey_data()$responses$Q124)
    
    # Remove NS/NC (value 5)
    values <- values[!is.na(values) & values != 5]
    
    # Create data frame for plotting
    df <- data.frame(Value = values)
    
    # Define labels for x-axis
    x_labels <- c("1" = "Nunca", "2" = "Poco", "3" = "Algo", "4" = "Mucho")
    
    # Get colors from theme
    bar_color <- if (!is.null(active_theme()) && !is.null(active_theme()$colors$primary)) {
      active_theme()$palettes$sequential
    } else {
      "#1f77b4"  # Default blue
    }
    
    # Create histogram
    p <- plot_ly(
      x = df$Value,
      type = "histogram",
      histnorm = "count",
      marker = list(
        color = bar_color,
        line = list(color = "white", width = 1)
      ),
      hoverinfo = "y+x"
    ) %>%
      apply_plotly_theme(
        title = "",
        xlab = "",
        ylab = "Frecuencia",
        custom_theme = active_theme()
      ) %>%
      layout(
        xaxis = list(
          tickmode = "array",
          tickvals = c(1, 2, 3, 4),
          ticktext = c("Nunca", "Poco", "Algo", "Mucho")
        ),
        bargap = 0.1
      )
    
    return(p)
  })
  
  # State Government - Q124
  output$state_punishment_hist <- renderPlotly({
    req(survey_data())
    
    # Get values for Q124
    values <- as.numeric(survey_data()$responses$Q125)
    
    # Remove NS/NC (value 5)
    values <- values[!is.na(values) & values != 5]
    
    # Create data frame for plotting
    df <- data.frame(Value = values)
    
    # Get colors from theme
    bar_color <- if (!is.null(active_theme()) && !is.null(active_theme()$colors$secondary)) {
      active_theme()$palettes$sequential
    } else {
      "#9467bd"  # Default purple
    }
    
    # Create histogram
    p <- plot_ly(
      x = df$Value,
      type = "histogram",
      histnorm = "count",
      marker = list(
        color = bar_color,
        line = list(color = "white", width = 1)
      ),
      hoverinfo = "y+x"
    ) %>%
      apply_plotly_theme(
        title = "",
        xlab = "",
        ylab = "Frecuencia",
        custom_theme = active_theme()
      ) %>%
      layout(
        xaxis = list(
          tickmode = "array",
          tickvals = c(1, 2, 3, 4),
          ticktext = c("Nunca", "Poco", "Algo", "Mucho")
        ),
        bargap = 0.1
      )
    
    return(p)
  })
  
  # Federal Government - Q125
  output$federal_punishment_hist <- renderPlotly({
    req(survey_data())
    
    # Get values for Q125
    values <- as.numeric(survey_data()$responses$Q126)
    
    # Remove NS/NC (value 5)
    values <- values[!is.na(values) & values != 5]
    
    # Create data frame for plotting
    df <- data.frame(Value = values)
    
    # Get colors from theme
    bar_color <- if (!is.null(active_theme()) && !is.null(active_theme()$colors$secondary)) {
      active_theme()$palettes$sequential
    } else {
      "#d62728"  # Default red
    }
    
    # Create histogram
    p <- plot_ly(
      x = df$Value,
      type = "histogram",
      histnorm = "count",
      marker = list(
        color = bar_color,
        line = list(color = "white", width = 1)
      ),
      hoverinfo = "y+x"
    ) %>%
      apply_plotly_theme(
        title = "",
        xlab = "",
        ylab = "Frecuencia",
        custom_theme = active_theme()
      ) %>%
      layout(
        xaxis = list(
          tickmode = "array",
          tickvals = c(1, 2, 3, 4),
          ticktext = c("Nunca", "Poco", "Algo", "Mucho")
        ),
        bargap = 0.1
      )
    
    return(p)
  })
  
  # Pie charts for corruption acts (Q15.1, Q16.1, Q17.1)
  
  # Helper function to create pie chart for corruption perception
  create_corruption_pie <- function(values, title, color_palette = NULL) {
    # Remove NS/NC (value 5)
    values <- values[!is.na(values) & values != 5]
    
    # Define labels
    labels <- c("Nunca", "Poco", "Algo", "Mucho")
    
    # Calculate counts and percentages
    freq_table <- table(factor(values, levels = 1:4))
    
    # Create data frame for plotting
    pie_data <- data.frame(
      Label = labels,
      Count = as.numeric(freq_table),
      stringsAsFactors = FALSE
    )
    
    # Calculate percentages
    pie_data$Percentage <- round(100 * pie_data$Count / sum(pie_data$Count), 1)
    
      color_palette <- active_theme()$palettes$sequential
    
    # Create pie chart
    plot_ly(
      labels = ~pie_data$Label,
      values = ~pie_data$Count,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste0(pie_data$Label, ": ", pie_data$Count, " (", pie_data$Percentage, "%)"),
      marker = list(
        colors = color_palette,
        line = list(color = "#FFFFFF", width = 1)
      )
    ) %>%
      layout(
        title = list(
          text = "",
          font = if (!is.null(active_theme())) {
            list(
              family = active_theme()$typography$font_family,
              size = active_theme()$typography$sizes$title,
              color = active_theme()$colors$text
            )
          } else {
            list(
              family = "Arial",
              size = 16,
              color = "#2C3E50"
            )
          }
        ),
        showlegend = FALSE
      )
  }
  
  # Municipal Government - Q15.1
  output$municipal_corruption_pie <- renderPlotly({
    req(survey_data())
    
    # Get values for Q15.1
    values <- as.numeric(survey_data()$responses$Q16.1)
    
    create_corruption_pie(
      values, 
      ""
    ) %>% apply_plotly_theme()
  })
  
  # State Government - Q16.1
  output$state_corruption_pie <- renderPlotly({
    req(survey_data())
    
    # Get values for Q16.1
    values <- as.numeric(survey_data()$responses$Q17.1)
    
    create_corruption_pie(
      values, 
      ""
    )%>% apply_plotly_theme()
  })
  
  # Federal Government - Q17.1
  output$federal_corruption_pie <- renderPlotly({
    req(survey_data())
    
    # Get values for Q17.1
    values <- as.numeric(survey_data()$responses$Q18.1)
    
    create_corruption_pie(
      values, 
      "" 
    )%>% apply_plotly_theme()
  })
}