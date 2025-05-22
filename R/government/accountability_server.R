# accountability_server.R - Updated with Enhanced Data Management

accountabilityServer <- function(input, output, session, current_theme = NULL) {
  # Get dependencies from userData
  selectedYear <- session$userData$selectedYear
  data_manager <- session$userData$data_manager
  
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
    saved_plots <- data_manager$load_saved_plots("accountability", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PAR_", selectedYear())
    plot_list <- list()
    
    # Create plot for Municipal Corruption (Q16.1)
    plot_key <- paste0("municipal_corruption_pie_", survey_id)
    plot_list$municipal_corruption_pie <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        corruption_data <- data_manager$get_processed_data(survey_id, "Q16.1", "interval")
        
        # Filter NS/NC (value 5)
        filtered_data <- corruption_data[corruption_data$value_num != 5 & !is.na(corruption_data$value_num), ]
        
        # Define labels
        labels <- c("Nunca", "Poco", "Algo", "Mucho")
        
        # Calculate counts and percentages
        freq_table <- table(factor(filtered_data$value_num, levels = 1:4))
        
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
          ) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Create plot for State Corruption (Q17.1)
    plot_key <- paste0("state_corruption_pie_", survey_id)
    plot_list$state_corruption_pie <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        corruption_data <- data_manager$get_processed_data(survey_id, "Q17.1", "interval")
        
        # Filter NS/NC (value 5)
        filtered_data <- corruption_data[corruption_data$value_num != 5 & !is.na(corruption_data$value_num), ]
        
        # Define labels
        labels <- c("Nunca", "Poco", "Algo", "Mucho")
        
        # Calculate counts and percentages
        freq_table <- table(factor(filtered_data$value_num, levels = 1:4))
        
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
          ) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Create plot for Federal Corruption (Q18.1)
    plot_key <- paste0("federal_corruption_pie_", survey_id)
    plot_list$federal_corruption_pie <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        corruption_data <- data_manager$get_processed_data(survey_id, "Q18.1", "interval")
        
        # Filter NS/NC (value 5)
        filtered_data <- corruption_data[corruption_data$value_num != 5 & !is.na(corruption_data$value_num), ]
        
        # Define labels
        labels <- c("Nunca", "Poco", "Algo", "Mucho")
        
        # Calculate counts and percentages
        freq_table <- table(factor(filtered_data$value_num, levels = 1:4))
        
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
          ) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Municipal Government Punishment (Q124)
    plot_key <- paste0("municipal_punishment_hist_", survey_id)
    plot_list$municipal_punishment_hist <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        # Get processed data
        punishment_data <- data_manager$get_processed_data(survey_id, "Q124", "interval")
        
        # Filter NS/NC (value 5)
        filtered_data <- punishment_data[punishment_data$value_num != 5 & !is.na(punishment_data$value_num), ]
        
        # Create data frame for plotting
        df <- data.frame(Value = filtered_data$value_num)
        
        # Get colors from theme
        bar_color <- active_theme()$palettes$sequential
        
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
      }
    )
    
    # State Government Punishment (Q125)
    plot_key <- paste0("state_punishment_hist_", survey_id)
    plot_list$state_punishment_hist <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        # Get processed data
        punishment_data <- data_manager$get_processed_data(survey_id, "Q125", "interval")
        
        # Filter NS/NC (value 5)
        filtered_data <- punishment_data[punishment_data$value_num != 5 & !is.na(punishment_data$value_num), ]
        
        # Create data frame for plotting
        df <- data.frame(Value = filtered_data$value_num)
        
        # Get colors from theme
        bar_color <- active_theme()$palettes$sequential
        
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
      }
    )
    
    # Federal Government Punishment (Q126)
    plot_key <- paste0("federal_punishment_hist_", survey_id)
    plot_list$federal_punishment_hist <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        # Get processed data
        punishment_data <- data_manager$get_processed_data(survey_id, "Q126", "interval")
        
        # Filter NS/NC (value 5)
        filtered_data <- punishment_data[punishment_data$value_num != 5 & !is.na(punishment_data$value_num), ]
        
        # Create data frame for plotting
        df <- data.frame(Value = filtered_data$value_num)
        
        # Get colors from theme
        bar_color <- active_theme()$palettes$sequential
        
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
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "accountability", selectedYear())
    
    return(plot_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("accountability_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PAR_", selectedYear())
    
    # Get processed data
    justice_data <- data_manager$get_processed_data(survey_id, "Q123", "interval")
    
    calc_list <- list()
    
    # Calculate justice perception
    if (!is.null(justice_data)) {
      # Remove NS/NC (value 5)
      filtered_data <- justice_data[justice_data$value_num != 5 & !is.na(justice_data$value_num), ]
      
      # Calculate mean
      mean_val <- mean(filtered_data$value_num, na.rm = TRUE)
      
      # Map mean value to corresponding label
      label <- case_when(
        mean_val <= 1.5 ~ "Siempre",
        mean_val <= 2.5 ~ "Casi siempre",
        mean_val <= 3.5 ~ "Casi nunca",
        TRUE ~ "Nunca"
      )
      
      calc_list$justice_perception <- label
    } else {
      calc_list$justice_perception <- "N/A"
    }
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Update tooltip content based on selected tab
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

  # Render outputs
  output$justice_perception <- renderText({
    calculations()$justice_perception
  })
  
  output$municipal_punishment_hist <- renderPlotly({
    plots()$municipal_punishment_hist
  })
  
  output$state_punishment_hist <- renderPlotly({
    plots()$state_punishment_hist
  })
  
  output$federal_punishment_hist <- renderPlotly({
    plots()$federal_punishment_hist
  })
  
  output$municipal_corruption_pie <- renderPlotly({
    plots()$municipal_corruption_pie
  })
  
  output$state_corruption_pie <- renderPlotly({
    plots()$state_corruption_pie
  })
  
  output$federal_corruption_pie <- renderPlotly({
    plots()$federal_corruption_pie
  })
}