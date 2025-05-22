# government_server.R - Updated with Enhanced Data Management

governmentServer <- function(input, output, session, current_theme = NULL) {
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
    saved_plots <- data_manager$load_saved_plots("government", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id_per <- paste0("PER_", selectedYear())
    survey_id_par <- paste0("PAR_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Knowledge of Officials Plots
    plot_key <- paste0("officials_knowledge_regidor_", survey_id_par)
    plot_list$officials_knowledge_regidor <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id_par)
        create_officials_knowledge_pie(survey_data$responses, "Q5", "Regidor/a", active_theme()) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    plot_key <- paste0("officials_knowledge_sindico_", survey_id_par)
    plot_list$officials_knowledge_sindico <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id_par)
        create_officials_knowledge_pie(survey_data$responses, "Q7", "Síndico/a", active_theme()) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    plot_key <- paste0("officials_knowledge_dipupadol_", survey_id_par)
    plot_list$officials_knowledge_dipupadol <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id_par)
        create_officials_knowledge_pie(survey_data$responses, "Q8", "Diputado/a Local y/o Estatal", active_theme()) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    plot_key <- paste0("officials_knowledge_diputadof_", survey_id_par)
    plot_list$officials_knowledge_diputadof <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id_par)
        create_officials_knowledge_pie(survey_data$responses, "Q10", "Diputado/a Federal", active_theme()) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Inequality Perception Plot
    plot_key <- paste0("inequality_perception_", survey_id_per)
    plot_list$inequality_perception <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id_per)
        create_inequality_perception_pie(survey_data$responses, active_theme()) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Government Expectations Plot
    plot_key <- paste0("government_expectations_", survey_id_par)
    plot_list$government_expectations <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id_par)
        create_government_expectations_plot(survey_data$responses, active_theme()) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Important Problems Plot
    plot_key <- paste0("important_problems_", survey_id_per)
    plot_list$important_problems <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id_per)
        create_important_problems_plot(survey_data$responses, active_theme()) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "government", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load saved maps first
    map_cache_key <- paste0("government_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    # If no saved maps, we would create them here
    # For now, returning an empty list as the current government module doesn't have maps
    map_list <- list()
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("government_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id_per <- paste0("PER_", selectedYear())
    survey_id_par <- paste0("PAR_", selectedYear())
    
    # Calculate any metrics needed for value boxes
    calc_list <- list()
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$env_satisfaction_tabs)
    
    active_tab <- input$env_satisfaction_tabs
    
    tooltip_content <- switch(active_tab,
      "Regidor/a" = "<b>ID</b>: PAR Q5 <br>
            <b>Pregunta</b>: ¿Conoce o puede mencionar el nombre de los actuales regidores? <br>
             <b>Escala</b>:  1=Sí puede mencionar por lo menos 1 regidor; 2=No conoce ningún regidor",
      "Síndico/a" = "<b>ID</b>: PAR Q7 <br>
            <b>Pregunta</b>:	¿Puede decirme el nombre del síndico o síndica municipal? <br>
             <b>Escala</b>:  1=No conoce el nombre del/la síndico(a); 2=Sí conoce",
      "Diputado/a Local y/o Estatal" = "<b>ID</b>: PAR Q8 <br>
            <b>Pregunta</b>:		Conoce o puede nombrar algun diputado local/ estatal de su distrito (Computada) <br>
             <b>Escala</b>:  1 = No conoce el nombre de algun diputado local; 2 = Si conoce algun diputado local",
      "Diputado/a Federal" = "<b>ID</b>: PAR Q9 <br>
            <b>Pregunta</b>:		¿Puede decirme el nombre del (la) diputado(a) federal de su distrito? NO AYUDAR CON NOMBRES <br>
             <b>Escala</b>:  	1=Sí conoce diputado(a) federal; 2=No conoce diputado(a) federal",
      "<b>ID</b>: PAR Q5 <br>
            <b>Pregunta</b>: ¿Conoce o puede mencionar el nombre de los actuales regidores? <br>
             <b>Escala</b>:  1=Sí puede mencionar por lo menos 1 regidor; 2=No conoce ningún regidor"
    )
    
    update_tooltip_content(session, "knowledge_pub_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q5 <br>
            <b>Pregunta</b>: ¿Conoce o puede mencionar el nombre de los actuales regidores? <br>
             <b>Escala</b>:  1=Sí puede mencionar por lo menos 1 regidor; 2=No conoce ningún regidor"
    
    update_tooltip_content(session, "knowledge_pub_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render outputs using the cached plots
  output$officials_knowledge_regidor_plot <- renderPlotly({
    plots()$officials_knowledge_regidor
  })
  
  output$officials_knowledge_sindico_plot <- renderPlotly({
    plots()$officials_knowledge_sindico
  })
  
  output$officials_knowledge_dipupadol_plot <- renderPlotly({
    plots()$officials_knowledge_dipupadol
  })
  
  output$officials_knowledge_diputadof_plot <- renderPlotly({
    plots()$officials_knowledge_diputadof
  })
  
  output$inequality_perception_plot <- renderPlotly({
    plots()$inequality_perception
  })
  
  output$government_expectations_plot <- renderPlotly({
    plots()$government_expectations
  })
  
  output$important_problems_plot <- renderPlotly({
    plots()$important_problems
  })
  
  # Add download handlers if needed (for maps or other outputs)
}