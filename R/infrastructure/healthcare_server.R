# healthcare_server.R - Complete Rehaul with Enhanced Data Management

healthcareServer <- function(input, output, session, current_theme = NULL) {
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
      get_section_theme("infraestructura")
    }
  })
  
  # Define health satisfaction questions
  health_questions <- list(
    "health_services" = "Q19",   # Servicios de salud en general
    "facilities" = "Q20",        # Instalaciones
    "attention_time" = "Q21",    # Tiempo de atención
    "medication" = "Q22",        # Disponibilidad de medicamentos
    "service_quality" = "Q23",   # Calidad del servicio
    "distance" = "Q24"           # Distancia al centro de salud
  )
  
  # Try to load pre-saved plots first, then create if needed
  plots <- reactive({
    req(selectedYear())
    
    # Try to load saved plots
    saved_plots <- data_manager$load_saved_plots("healthcare", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PER_", selectedYear())
    plot_list <- list()
    
    # Healthcare providers chart
    plot_key <- paste0("healthcare_providers_chart_", survey_id)
    plot_list$healthcare_providers_chart <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        # Determine which providers based on year
        if (selectedYear() == '2024') {
          providers <- list(
            "IMSS" = "Q17.1",
            "ISSSTE" = "Q17.2", 
            "Instituto de Salud de Bienestar" = "Q17.3",
            "Médicos de farmacias/genéricos" = "Q17.4",
            "Servicio médico privado/particulares" = "Q17.5",
            "No tiene servicio médico" = "Q17.6",
            "Otro" = "Q17.7",
            "MEDICHIHUAHUA" = "Q17.8"
          )
        } else {
          providers <- list(
            "IMSS" = "Q17.1",
            "ISSSTE" = "Q17.2",
            "Instituto de Salud de Bienestar" = "Q17.3", 
            "Médicos de farmacias/genéricos" = "Q17.4",
            "Servicio médico privado/particulares" = "Q17.5",
            "No tiene servicio médico" = "Q17.6",
            "Otro" = "Q17.7"
          )
        }
        
        # Process provider data
        provider_results <- list()
        
        for (provider_name in names(providers)) {
          provider_id <- providers[[provider_name]]
          
          tryCatch({
            provider_data <- data_manager$get_processed_data(survey_id, provider_id, "binary")
            
            if (!is.null(provider_data) && nrow(provider_data) > 0) {
              true_count <- sum(provider_data$binary_value, na.rm = TRUE)
              total_count <- nrow(provider_data)
              percentage <- 100 * true_count / total_count
              
              provider_results[[provider_name]] <- data.frame(
                provider = provider_name,
                percentage = percentage,
                stringsAsFactors = FALSE
              )
            }
          }, error = function(e) {
            warning(paste("Error processing provider", provider_name, ":", e$message))
          })
        }
        
        # Combine results
        if (length(provider_results) > 0) {
          provider_df <- do.call(rbind, provider_results)
          
          # Sort by percentage
          provider_df <- provider_df[order(provider_df$percentage, decreasing = TRUE),]
          
          # Get colors from theme
          primary_color <- active_theme()$colors$primary
          highlight_color <- active_theme()$colors$accent
          if (is.null(highlight_color)) {
            highlight_color <- colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
          }
          
          # Create color vector - highlight top 3
          colors <- rep(primary_color, nrow(provider_df))
          if (nrow(provider_df) >= 3) {
            colors[1:3] <- highlight_color
          }
          
          # Create plot
          plot_ly(
            data = provider_df,
            y = ~reorder(provider, percentage),
            x = ~percentage,
            type = "bar",
            orientation = "h",
            marker = list(color = colors),
            text = ~paste0(round(percentage, 1), "%"),
            textposition = "auto",
            hoverinfo = "text",
            hovertext = ~paste0(provider, ": ", round(percentage, 1), "%")
          ) %>%
            apply_plotly_theme(
              title = "",
              xlab = "Porcentaje (%)",
              ylab = "",
              custom_theme = active_theme()
            ) %>%
            layout(
              xaxis = list(range = c(0, max(provider_df$percentage) * 1.1)),
              margin = list(l = 150)
            )
        } else {
          plotly_empty() %>% layout(title = "No hay datos disponibles")
        }
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "healthcare", selectedYear())
    
    return(plot_list)
  })
  
  # Create health data reactives for each question
  health_data <- reactiveValues()
  
  observe({
    req(selectedYear())
    survey_id <- paste0("PER_", selectedYear())
    
    for (aspect in names(health_questions)) {
      question_id <- health_questions[[aspect]]
      
      # Create reactive data for each health aspect
      local({
        current_aspect <- aspect
        current_question <- question_id
        
        health_data[[current_aspect]] <- tryCatch({
          data_manager$get_processed_data(survey_id, current_question, "interval")
        }, error = function(e) {
          warning(paste("Error loading data for", current_aspect, ":", e$message))
          NULL
        })
      })
    }
  })
  
  # Update tooltip content based on selected tab
  observe({
    req(input$healthcare_tabs)
    
    active_tab <- input$healthcare_tabs
    
    tooltip_content <- switch(active_tab,
      "Servicios en General" = "<b>ID</b>: PER Q19 <br>
            <b>Pregunta</b>: Que tan satisfecho/a esta en GENERAL con los servicios de salud que recibe del servicio medico que mas USA? <br>
             <b>Escala</b>: 1-10",
      "Instalaciones" = "<b>ID</b>: PER Q20 <br>
            <b>Pregunta</b>: Que tan satisfecho esta con las INSTALACIONES del servicio medico que usa? <br>
             <b>Escala</b>: 1-10",
      "Tiempo de Atención" = "<b>ID</b>: PER Q21 <br>
            <b>Pregunta</b>: Que tan satisfecho está con el TIEMPO QUE TARDA EN SER ATENDIDO del servicio medico que usa? <br>
             <b>Escala</b>: 1-10",
      "Medicamentos" = "<b>ID</b>: PER Q22 <br>
            <b>Pregunta</b>: Que tan satisfecho está con la DISPONIBILIDAD DE MEDICINAS del servicio medico que usa? <br>
             <b>Escala</b>: 1-10",
      "Calidad de Servicio" = "<b>ID</b>: PER Q23 <br>
            <b>Pregunta</b>: Que tan satisfecho está con la ATENCIÓN RECIBIDA del servicio médico que usa? <br>
             <b>Escala</b>: 1-10",
      "Distancia" = "<b>ID</b>: PER Q24 <br>
            <b>Pregunta</b>: Que tan satisfecho está con la CERCANIA CON LOS CENTROS DE SALUD del servicio médico que usa? <br>
             <b>Escala</b>: 1-10",
      "<b>ID</b>: PER Q19 <br>
            <b>Pregunta</b>: Que tan satisfecho/a esta en GENERAL con los servicios de salud que recibe del servicio medico que mas USA? <br>
             <b>Escala</b>: 1-10"
    )
    
    update_tooltip_content(session, "healthcare_tooltip", tooltip_content)
  })
  
  # Set initial tooltip
  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PER Q19 <br>
            <b>Pregunta</b>: Que tan satisfecho/a esta en GENERAL con los servicios de salud que recibe del servicio medico que mas USA? <br>
             <b>Escala</b>: 1-10"
    
    update_tooltip_content(session, "healthcare_tooltip", initial_tooltip)
  }, once = TRUE)
  
  # Render all health satisfaction maps
  output$health_services_map <- renderLeaflet({
    req(health_data$health_services, geo_data())
    create_interval_district_map(
      health_data$health_services,
      geo_data(),
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  output$facilities_map <- renderLeaflet({
    req(health_data$facilities, geo_data())
    create_interval_district_map(
      health_data$facilities,
      geo_data(),
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  output$attention_time_map <- renderLeaflet({
    req(health_data$attention_time, geo_data())
    create_interval_district_map(
      health_data$attention_time,
      geo_data(),
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  output$medication_map <- renderLeaflet({
    req(health_data$medication, geo_data())
    create_interval_district_map(
      health_data$medication,
      geo_data(),
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  output$service_quality_map <- renderLeaflet({
    req(health_data$service_quality, geo_data())
    create_interval_district_map(
      health_data$service_quality,
      geo_data(),
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  output$distance_map <- renderLeaflet({
    req(health_data$distance, geo_data())
    create_interval_district_map(
      health_data$distance,
      geo_data(),
      highlight_extremes = TRUE,
      use_gradient = FALSE,
      color_scale = "Blues",
      custom_theme = active_theme()
    )
  })
  
  # Healthcare providers chart
  output$healthcare_providers_chart <- renderPlotly({
    plots()$healthcare_providers_chart
  })
  
  # Download handler
  output$download_healthcare_map <- downloadHandler(
    filename = function() {
      map_type <- switch(input$healthcare_tabs,
        "Servicios en General" = "General",
        "Instalaciones" = "Instalaciones", 
        "Tiempo de Atención" = "Tiempo",
        "Medicamentos" = "Medicamentos",
        "Calidad de Servicio" = "Servicio",
        "Distancia" = "Distancia"
      )
      paste("mapa_serv_salud_", map_type, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      # Get the appropriate data and create map based on active tab
      survey_id <- paste0("PER_", selectedYear())
      
      result <- switch(input$healthcare_tabs,
        "Servicios en General" = {
          title_text <- "Satisfacción con los Servicios Médicos en General"
          data <- health_data$health_services
          list(data = data, title = title_text)
        },
        "Instalaciones" = {
          title_text <- "Satisfacción con las Instalaciones de los Servicios Médicos"
          data <- health_data$facilities
          list(data = data, title = title_text)
        },
        "Tiempo de Atención" = {
          title_text <- "Satisfacción con los Tiempos de Atención de los Servicios Médicos"
          data <- health_data$attention_time
          list(data = data, title = title_text)
        },
        "Medicamentos" = {
          title_text <- "Satisfacción con los Medicamentos de los Servicios Médicos"
          data <- health_data$medication
          list(data = data, title = title_text)
        },
        "Calidad de Servicio" = {
          title_text <- "Satisfacción con la Calidad de Servicio de los Servicios Médicos"
          data <- health_data$service_quality
          list(data = data, title = title_text)
        },
        {
          title_text <- "Satisfacción con la Distancia de los Servicios Médicos"
          data <- health_data$distance
          list(data = data, title = title_text)
        }
      )
      
      # Create the map
      if (!is.null(result$data) && nrow(result$data) > 0) {
        map <- create_interval_district_map(
          result$data,
          geo_data(),
          highlight_extremes = TRUE,
          use_gradient = FALSE,
          color_scale = "Blues",
          custom_theme = active_theme()
        )
      } else {
        # Create empty map if no data
        map <- leaflet() %>%
          addTiles() %>%
          addControl(
            html = "<div style='background-color:white; padding:10px; border-radius:5px;'>Sin datos disponibles</div>",
            position = "center"
          )
      }
      
      # Add title and footer
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      result$title, 
                      "</div>"),
          position = "topright"
        ) %>%
        addControl(
          html = paste("<div style='background-color:white; padding:8px; border-radius:5px; font-size:12px;'>", 
                      paste("Resultados de la Encuesta de Percepción y Participación Ciudadana y Buen Gobierno", selectedYear()),
                      "</div>"),
          position = "bottomright"
        )
      
      # Save and convert
      htmlwidgets::saveWidget(map, tmp_html, selfcontained = TRUE)
      
      pagedown::chrome_print(
        input = tmp_html,
        output = file,
        options = list(printBackground = TRUE, scale = 2.0),
        format = "png",
        browser = "/usr/bin/google-chrome",
        extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
      )
      
      # Clean up temporary files
      if (file.exists(tmp_html)) {
        file.remove(tmp_html)
      }
    }
  )
}