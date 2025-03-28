# Server function for Accountability Dashboard
accountabilityServer <- function(input, output, session) {
  # Load survey data
  survey_data <- reactive({
    load_survey_data("PAR_2024") # Using PAR survey
  })
  
  # Use the current theme
  current_theme <- reactiveVal(theme_config)
  
  # Callout for Q122 - Justice perception
  output$justice_perception <- renderText({
    req(survey_data())
    
    # Get values for Q122
    values <- as.numeric(survey_data()$responses$Q122)
    
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
    values <- as.numeric(survey_data()$responses$Q123)
    
    # Remove NS/NC (value 5)
    values <- values[!is.na(values) & values != 5]
    
    # Create data frame for plotting
    df <- data.frame(Value = values)
    
    # Define labels for x-axis
    x_labels <- c("1" = "Nunca", "2" = "Poco", "3" = "Algo", "4" = "Mucho")
    
    # Get colors from theme
    bar_color <- if (!is.null(current_theme()) && !is.null(current_theme()$colors$primary)) {
      current_theme()$colors$primary
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
        title = "¿El gobierno municipal castiga a servidores públicos corruptos?",
        xlab = "",
        ylab = "Frecuencia",
        custom_theme = current_theme()
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
    values <- as.numeric(survey_data()$responses$Q124)
    
    # Remove NS/NC (value 5)
    values <- values[!is.na(values) & values != 5]
    
    # Create data frame for plotting
    df <- data.frame(Value = values)
    
    # Get colors from theme
    bar_color <- if (!is.null(current_theme()) && !is.null(current_theme()$colors$secondary)) {
      current_theme()$colors$secondary
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
        title = "¿El gobierno estatal castiga a servidores públicos corruptos?",
        xlab = "",
        ylab = "Frecuencia",
        custom_theme = current_theme()
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
    values <- as.numeric(survey_data()$responses$Q125)
    
    # Remove NS/NC (value 5)
    values <- values[!is.na(values) & values != 5]
    
    # Create data frame for plotting
    df <- data.frame(Value = values)
    
    # Get colors from theme
    bar_color <- if (!is.null(current_theme()) && !is.null(current_theme()$colors$highlight)) {
      current_theme()$colors$highlight
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
        title = "¿El gobierno federal castiga a servidores públicos corruptos?",
        xlab = "",
        ylab = "Frecuencia",
        custom_theme = current_theme()
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
    
    # Get colors for pie chart
    if (is.null(color_palette)) {
      color_palette <- c("#4575b4", "#91bfdb", "#fc8d59", "#d73027")
    }
    
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
          text = title,
          font = if (!is.null(current_theme())) {
            list(
              family = current_theme()$typography$font_family,
              size = current_theme()$typography$sizes$title,
              color = current_theme()$colors$text
            )
          } else {
            list(
              family = "Arial",
              size = 16,
              color = "#2C3E50"
            )
          }
        ),
        showlegend = TRUE
      )
  }
  
  # Municipal Government - Q15.1
  output$municipal_corruption_pie <- renderPlotly({
    req(survey_data())
    
    # Get values for Q15.1
    values <- as.numeric(survey_data()$responses$Q15.1)
    
    create_corruption_pie(
      values, 
      "¿El gobierno municipal ha hecho actos de corrupción?"
    )
  })
  
  # State Government - Q16.1
  output$state_corruption_pie <- renderPlotly({
    req(survey_data())
    
    # Get values for Q16.1
    values <- as.numeric(survey_data()$responses$Q16.1)
    
    create_corruption_pie(
      values, 
      "¿El gobierno estatal ha hecho actos de corrupción?",
      c("#614051", "#8a5c7e", "#b48bad", "#d8c2d7")  # Purple palette
    )
  })
  
  # Federal Government - Q17.1
  output$federal_corruption_pie <- renderPlotly({
    req(survey_data())
    
    # Get values for Q17.1
    values <- as.numeric(survey_data()$responses$Q17.1)
    
    create_corruption_pie(
      values, 
      "¿El gobierno federal ha hecho actos de corrupción?",
      c("#67000d", "#a50f15", "#cb181d", "#ef3b2c")  # Red palette
    )
  })
}