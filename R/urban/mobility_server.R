# mobility_server.R - Updated with Enhanced Data Management

mobilityServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("movilidad")
    }
  })
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("mobility", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    
    # Create plots using data manager
    plot_list <- list()
    
    # Bicycles pie chart
    plot_key <- paste0("bicycles_pie_", survey_id)
    plot_list$bicycles_pie <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        create_bicycle_distribution(survey_data$responses, active_theme()) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Vehicles pie chart
    plot_key <- paste0("vehicles_pie_", survey_id)
    plot_list$vehicles_pie <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        create_vehicle_distribution(survey_data$responses, active_theme()) %>% 
          apply_plotly_theme(custom_theme = active_theme())
      }
    )
    
    # Work transportation mode plot
    plot_key <- paste0("work_transport_plot_", survey_id)
    plot_list$work_transport_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        create_transport_modes_plot(
          survey_data$responses, 
          mode_type = "work", 
          custom_theme = active_theme()
        )
      }
    )
    
    # General transportation mode plot
    plot_key <- paste0("general_transport_plot_", survey_id)
    plot_list$general_transport_plot <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        survey_data <- data_manager$get_survey_data(survey_id)
        create_transport_modes_plot(
          survey_data$responses, 
          mode_type = "general", 
          custom_theme = active_theme()
        )
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "mobility", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately if needed (not currently used in mobility)
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("mobility_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    # For now, we don't have maps in this module but keeping the structure
    # for consistency and future extensions
    map_list <- list()
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations if needed
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("mobility_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PER_", selectedYear())
    calc_list <- list()
    
    # Add calculations here if needed
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$transport_tabs)
    
    active_tab <- input$transport_tabs
    
    tooltip_content <- switch(active_tab,
      "Transporte al Trabajo" = "<b>ID</b>: PER Q72.1 - Q72.10 <br>
            <b>Pregunta</b>:	Caminando / Bicicleta / Autobús escolar	/ Autobús especial (transporte de personal)	 / Taxi / Uber/Didi/InDriver o cualquier otro servicio por aplicación / Motocicleta / Vehículo propio	/ Camion / Juarez Bus <br>
             <b>Escala</b>:  1=Sí; 2=No	",
      "Transporte General"= "<b>ID</b>: PER Q73.1 - Q73.10 <br>
            <b>Pregunta</b>:	Caminando / Bicicleta / Autobús escolar	/ Autobús especial (transporte de personal)	 / Taxi / Uber/Didi/InDriver o cualquier otro servicio por aplicación / Motocicleta / Vehículo propio	/ Camion / Juarez Bus <br>
             <b>Escala</b>:  1=Sí; 2=No	",
      "<b>ID</b>: PER Q72.1 - Q72.10 <br>
            <b>Pregunta</b>:	Caminando / Bicicleta / Autobús escolar	/ Autobús especial (transporte de personal)	 / Taxi / Uber/Didi/InDriver o cualquier otro servicio por aplicación / Motocicleta / Vehículo propio	/ Camion / Juarez Bus <br>
             <b>Escala</b>:  1=Sí; 2=No	"
    )
    
    update_tooltip_content(session, "transportation_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q89 <br>
            <b>Pregunta</b>:	¿Qué tan satisfecho está con LA CALIDAD DEL AIRE? <br>
             <b>Escala</b>:  1-10"
    
    update_tooltip_content(session, "transportation_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render outputs
  output$bicycles_pie <- renderPlotly({
    plots()$bicycles_pie
  })
  
  output$vehicles_pie <- renderPlotly({
    plots()$vehicles_pie
  })
  
  output$work_transport_plot <- renderPlotly({
    plots()$work_transport_plot
  })
  
  output$general_transport_plot <- renderPlotly({
    plots()$general_transport_plot
  })
}