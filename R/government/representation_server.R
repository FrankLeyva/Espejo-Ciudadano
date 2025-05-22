# representation_server.R - Updated with Enhanced Data Management

representationServer <- function(input, output, session, current_theme = NULL) {
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
    saved_plots <- data_manager$load_saved_plots("representation", selectedYear())
    
    if (!is.null(saved_plots)) {
      return(saved_plots)
    }
    
    # If no saved plots, create them
    survey_id <- paste0("PAR_", selectedYear())
    plot_list <- list()
    
    # Helper function to create bar chart for specific questions with labels
    create_representative_knowledge_chart <- function(question_prefix, count, labels = NULL, x_range = c(0, 6)) {
      # Initialize vectors for counts and percentages
      ids_vector <- character(0)
      names_vector <- character(0)
      percentages <- numeric(0)
      
      # Process each question
      for (i in 1:count) {
        question_id <- paste0(question_prefix, ".", i)
        
        # Get binary data using data manager
        binary_data <- data_manager$get_processed_data(survey_id, question_id, "binary")
        
        if (!is.null(binary_data) && nrow(binary_data) > 0) {
          # Calculate percentage of positive responses (binary_value = TRUE)
          knows_percentage <- 100 * sum(binary_data$binary_value, na.rm = TRUE) / nrow(binary_data)
          
          # Get label if available, otherwise use placeholder
          name_label <- if (!is.null(labels) && length(labels) >= i) {
            labels[i]
          } else {
            paste0("Representante ", i)
          }
          
          # Add to vectors
          ids_vector <- c(ids_vector, question_id)
          names_vector <- c(names_vector, name_label)
          percentages <- c(percentages, knows_percentage)
        }
      }
      
      # Create data frame
      if (length(names_vector) > 0) {
        results_df <- data.frame(
          ID = ids_vector,
          Representative = names_vector,
          Percentage = percentages
        )
        
        # Sort by percentage in descending order
        results_df <- results_df[order(-results_df$Percentage), ]
        
        # Get colors from theme
        primary_color <- active_theme()$colors$primary
        highlight_color <- active_theme()$colors$accent
        
        if (is.null(highlight_color)) {
          highlight_color <- if (!is.null(active_theme()$colors$secondary)) {
            active_theme()$colors$secondary
          } else {
            colorRampPalette(c(primary_color, "#FFFFFF"))(3)[2]
          }
        }
        
        # Create color vector - highlight top 3
        colors <- rep(primary_color, nrow(results_df))
        unique_top_values <- unique(results_df$Percentage)[1:min(3, length(unique(results_df$Percentage)))]
        top_indices <- which(results_df$Percentage %in% unique_top_values)
        colors[top_indices] <- highlight_color
        
        # Determine optimal height based on number of entries
        optimal_height <- max(400, nrow(results_df) * 40)
        
        # Create bar chart
        plot_ly(
          data = results_df,
          y = ~Representative,
          x = ~Percentage,
          height = optimal_height,
          type = "bar",
          orientation = "h",
          marker = list(color = colors),
          text = ~paste0(round(Percentage, 1), "%"),
          textposition = "auto",
          hoverinfo = "text",
          hovertext = ~paste0(Representative, ": ", round(Percentage, 1), "%")
        ) %>%
          apply_plotly_theme(
            title = "",
            xlab = "Porcentaje que conoce",
            ylab = "",
            custom_theme = active_theme()
          ) %>%
          layout(
            xaxis = list(range = x_range),
            yaxis = list(categoryorder = 'total ascending', automargin = TRUE),
            margin = list(l = 250)
          )
      } else {
        # Return empty plot if no data
        plot_ly() %>%
          layout(title = "No hay datos disponibles")
      }
    }
    
    # Define labels based on selected year
    if (selectedYear() == '2024') {
      regidores_labels <- c(
        "Alejandro Daniel Acosta Aviña",
        "Maria Dolores Adame Alvarado",
        "Alejandro Alberto Jimenez", 
        "Laura Fernanda Avalos Medina",
        "Hector Hugo Avitia Arellanes",
        "Hector Hugo Avitia Corral",
        "Jorge Marcial Bueno Quiroz", 
        "Mayra Karina Castillo Tapia",
        "Luz Clara Cristo Sosa",
        "Antonio Dominguez Alderete",
        "Karla Michaeel Escalante Ramirez",
        "Sandra Garcia Ramos",
        "Pedro Alberto Matus Peña",
        "Jose Mauricio Padilla",
        "Martha Patricia Mendoza Rodriguez",
        "Gloria Rocio Mirazo De la Rosa",
        "Mireya Porras Armendariz",
        "Dina Salgado Sotelo",
        "Sandra Marbel Valenzuela Martinez",
        "Jose Eduardo Valenzuela Martinez"
      )
      
      diputados_locales_labels <- c(
        "Leticia Ortega Máynez (Distrito 02)", 
        "Oscar Daniel Avitia Arellanes (Distrito 03)", 
        "Rosana Díaz Reyes (Distrito 04)", 
        "Edna Xochitl Contreras Herrera (Distrito 05)", 
        "Irlanda Dominique Márquez Nolasco (Distrito 06)", 
        "Elizabeth Guzman Argueta (Distrito 07)", 
        "Edin Cuauhtémoc Estrada Sotelo (Distrito 08)", 
        "Magdalena Rentería Pérez (Distrito 09)", 
        "María Antonieta Pérez Reyes (Distrito 10)"
      )
      
      diputados_federales_labels <- c(
        "Daniel Murguía Lardizabal (Distrito 01)", 
        "Teresita de Jesus Vargas Meráz (Distrito 02)", 
        "Lilia Aguilar Gil (Distrito 03)", 
        "Alejandro Perez Cuellar (Distrito 04)"
      )
    } else {
      regidores_labels <- c(
        "María Dolores Adame Alvarado", "Alma Edith Arredondo Salinas", "Héctor Hugo Avitia Arellanes",
        "Amparo Beltrán Ceballos", "Jorge Marcial Bueno Quiroz", "Mayra Karina Castillo Tapia",
        "Antonio Domínguez Alderete", "Karla Michael Escalante Ramírez", "Ana Carmen Estrada García",
        "Joob Quintin Flores Silva", "Austria Elizabeth Galindo Rodríguez", "Jorge Alberto Gutiérrez Casas",
        "Tania Maldonado Garduño", "Pedro Alberto Matus Peña", "Martha Patricia Mendoza Rodríguez",
        "Vanessa Mora de la O", "Mireya Porras Armendáriz", "Yolanda Cecilia Reyes Castro",
        "Víctor Manuel Talamantes Vázquez", "Enrique Torres Valadez"
      )
      
      diputados_locales_labels <- c(
        "Leticia Ortega Máynez (Distrito 02)", "Oscar Daniel Avitia Arellanes (Distrito 03)", 
        "Rosana Díaz Reyes (Distrito 04)", "Marisela Terrazas Muñoz (Distrito 05)", 
        "Jael Argüelles Díaz (Distrito 06)", "Gustavo de la Rosa Hickerson (Distrito 07)", 
        "Edin Cuauhtémoc Estrada Sotelo (Distrito 08)", "Magdalena Rentería Pérez (Distrito 09)", 
        "María Antonieta Pérez Reyes (Distrito 10)"
      )
      
      diputados_federales_labels <- c(
        "Daniel Murguía Lardizabal (Distrito 01)", "Teresita de Jesús Vargas Meráz (Distrito 02)", 
        "Lilia Aguilar Gil (Distrito 03)", "Daniela Soraya Álvarez Hernández (Distrito 04)"
      )
    }
    
    # Q6.1-6.20: Knowledge of Regidores
    plot_key <- paste0("regidores_knowledge_chart_", survey_id)
    plot_list$regidores_knowledge_chart <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        create_representative_knowledge_chart(
          question_prefix = "Q6", 
          count = 20, 
          labels = regidores_labels,
          x_range = c(0, 0.7)
        )
      }
    )
    
    # Q9.1-9.9: Knowledge of Diputados Locales
    plot_key <- paste0("diputados_locales_knowledge_chart_", survey_id)
    plot_list$diputados_locales_knowledge_chart <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        create_representative_knowledge_chart(
          question_prefix = "Q9", 
          count = 9,
          labels = diputados_locales_labels,
          x_range = c(0, 3)
        )
      }
    )
    
    # Q11.1-11.4: Knowledge of Diputados Federales
    plot_key <- paste0("diputados_federales_knowledge_chart_", survey_id)
    plot_list$diputados_federales_knowledge_chart <- data_manager$get_or_create_plot(
      plot_key = plot_key,
      plot_function = function() {
        create_representative_knowledge_chart(
          question_prefix = "Q11", 
          count = 4, 
          labels = diputados_federales_labels,
          x_range = c(0, 6)
        )
      }
    )
    
    # Save plots for future use
    data_manager$save_plots(plot_list, "representation", selectedYear())
    
    return(plot_list)
  })
  
  # Maps - create separately since they use geo data
  maps <- reactive({
    req(selectedYear(), geo_data())
    
    # Try to load cached maps
    map_cache_key <- paste0("representation_maps_", selectedYear())
    if (!is.null(data_manager$cache[[map_cache_key]])) {
      return(data_manager$cache[[map_cache_key]])
    }
    
    survey_id <- paste0("PAR_", selectedYear())
    map_list <- list()
    
    # Helper function to create district maps for knowledge questions
    create_knowledge_district_map <- function(question_id, custom_theme = NULL) {
      # Get binary data using data manager
      binary_data <- data_manager$get_processed_data(survey_id, question_id, "binary")
      
      if (!is.null(binary_data) && nrow(binary_data) > 0) {
        # Create district map
        create_binary_district_map(
          data = binary_data,
          geo_data = geo_data(),
          highlight_extremes = TRUE,
          focus_on_true = TRUE,  # Focus on "Yes" responses
          custom_theme = custom_theme
        )
      } else {
        # Return a basic leaflet map if no data
        leaflet() %>%
          addTiles() %>%
          setView(lng = -106.4245, lat = 31.6904, zoom = 11)
      }
    }
    
    # Q5: Map of knowledge of Regidor
    map_list$regidor_knowledge_map <- create_knowledge_district_map("Q5", active_theme())
    
    # Q7: Map of knowledge of Síndico
    map_list$sindico_knowledge_map <- create_knowledge_district_map("Q7", active_theme())
    
    # Q8: Map of knowledge of Diputado Local
    map_list$diputadol_knowledge_map <- create_knowledge_district_map("Q8", active_theme())
    
    # Q10: Map of knowledge of Diputado Federal
    map_list$diputadof_knowledge_map <- create_knowledge_district_map("Q10", active_theme())
    
    # Cache maps
    data_manager$cache[[map_cache_key]] <- map_list
    
    return(map_list)
  })
  
  # Value box calculations
  calculations <- reactive({
    req(selectedYear())
    
    calc_cache_key <- paste0("representation_calculations_", selectedYear())
    if (!is.null(data_manager$cache[[calc_cache_key]])) {
      return(data_manager$cache[[calc_cache_key]])
    }
    
    survey_id <- paste0("PAR_", selectedYear())
    calc_list <- list()
    
    # Helper function to calculate average rating
    calculate_avg_rating <- function(question_id) {
      interval_data <- data_manager$get_processed_data(survey_id, question_id, "interval")
      
      if (!is.null(interval_data) && nrow(interval_data) > 0) {
        avg <- mean(interval_data$value_num, na.rm = TRUE)
        return(sprintf("%.1f / 10", avg))
      } else {
        return("No disponible")
      }
    }
    
    # Q12: Regidores representation rating
    calc_list$regidores_rating <- calculate_avg_rating("Q12")
    
    # Q13: Síndico representation rating
    calc_list$sindico_rating <- calculate_avg_rating("Q13")
    
    # Q14: Local deputy representation rating
    calc_list$diputado_local_rating <- calculate_avg_rating("Q14")
    
    # Q15: Federal deputy representation rating
    calc_list$diputado_federal_rating <- calculate_avg_rating("Q15")
    
    # Cache calculations
    data_manager$cache[[calc_cache_key]] <- calc_list
    
    return(calc_list)
  })
  
  # Tooltip observers
  observe({
    req(input$knowledge_tabs)
    
    active_tab <- input$knowledge_tabs
    
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
    
    update_tooltip_content(session, "political_knowledge_tooltip", tooltip_content)
  })

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q5 <br>
        <b>Pregunta</b>: ¿Conoce o puede mencionar el nombre de los actuales regidores? <br>
         <b>Escala</b>:  1=Sí puede mencionar por lo menos 1 regidor; 2=No conoce ningún regidor"
    
    update_tooltip_content(session, "political_knowledge_tooltip", initial_tooltip)
  }, once = TRUE)

  observe({
    req(input$specific_knowledge_tabs)
    
    active_tab <- input$specific_knowledge_tabs
    
    tooltip_content <- switch(active_tab,
      "Regidores" = "<b>ID</b>: PAR Q6.1 - Q6.20 <br>
        <b>Pregunta</b>: Nombres de los regidores en la gráfica <br>
         <b>Escala</b>:  	0=No seleccionado; 1=Seleccionado",
      "Diputados Locales" = "<b>ID</b>: PAR Q8.1 - Q8.10 <br>
        <b>Pregunta</b>:	Nombres de los diputados locales en la gráfica <br>
         <b>Escala</b>:  0=No seleccionado; 1=Seleccionado",
      "Diputados Federales" = "<b>ID</b>: PAR Q10.1 - Q10.4 <br>
        <b>Pregunta</b>:		Nombres de los diputados federales en la gráfica <br>
         <b>Escala</b>:  0=No seleccionado; 1=Seleccionado",
      "<b>ID</b>: PAR Q6.1 - Q6.20 <br>
        <b>Pregunta</b>: Nombres de los regidores en la gráfica <br>
         <b>Escala</b>:  	0=No seleccionado; 1=Seleccionado"
    )
    
    update_tooltip_content(session, "specific_knowledge_tooltip", tooltip_content)
  })

  observeEvent(session$clientData$url_protocol, {
    initial_tooltip <- "<b>ID</b>: PAR Q6.1 - Q6.20 <br>
        <b>Pregunta</b>: Nombres de los regidores en la gráfica <br>
         <b>Escala</b>:  	0=No seleccionado; 1=Seleccionado"
    
    update_tooltip_content(session, "specific_knowledge_tooltip", initial_tooltip)
  }, once = TRUE)

  # Render outputs
  output$regidor_knowledge_map <- renderLeaflet({
    maps()$regidor_knowledge_map
  })
  
  output$sindico_knowledge_map <- renderLeaflet({
    maps()$sindico_knowledge_map
  })
    
  output$diputadol_knowledge_map <- renderLeaflet({
    maps()$diputadol_knowledge_map
  })
  
  output$diputadof_knowledge_map <- renderLeaflet({
    maps()$diputadof_knowledge_map
  })
  
  output$regidores_knowledge_chart <- renderPlotly({
    plots()$regidores_knowledge_chart
  })
  
  output$diputados_locales_knowledge_chart <- renderPlotly({
    plots()$diputados_locales_knowledge_chart
  })
  
  output$diputados_federales_knowledge_chart <- renderPlotly({
    plots()$diputados_federales_knowledge_chart
  })
  
  output$regidores_rating <- renderText({
    calculations()$regidores_rating
  })
  
  output$sindico_rating <- renderText({
    calculations()$sindico_rating
  })
  
  output$diputado_local_rating <- renderText({
    calculations()$diputado_local_rating
  })
  
  output$diputado_federal_rating <- renderText({
    calculations()$diputado_federal_rating
  })

  # Download handler for political knowledge maps
  output$download_political_knowledge_map <- downloadHandler(
    filename = function() {
      map_type <- if(input$knowledge_tabs == "Regidor(a)"){ 
                        "Regidor"} else if (input$knowledge_tabs == "Síndico(a)"){
                          "Sindico" } else if (input$knowledge_tabs == "Diputado(a) Local y/o Estatal"){
                          "Diputado_local_estatal" }else {
                          "Diputado_federal"
                        }
      paste("mapa_conocimiento_representantes_", map_type, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tmp_html <- tempfile(fileext = ".html")
      
      # Create the appropriate map based on active tab
      if(input$knowledge_tabs == "Regidor(a)") {
        map <- maps()$regidor_knowledge_map
        title_text <- "Conocimiento del nombre del Regidor(a) por distrito"
      } else if(input$knowledge_tabs == "Síndico(a)") {
        map <- maps()$sindico_knowledge_map
        title_text <- "Conocimiento del nombre del Síndico(a)"
      } else if(input$knowledge_tabs == "Diputado(a) Local y/o Estatal") {
        map <- maps()$diputadol_knowledge_map
        title_text <- "Conocimiento del nombre del Diputado(a) Local y/o Estatal"
      } else {
        map <- maps()$diputadof_knowledge_map
        title_text <- "Conocimiento del nombre del Diputado(a) Federal"
      }
      
      # Add title and footer
      map <- map %>%
        addControl(
          html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                      title_text, 
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
      
      # Clean up
      if (file.exists(tmp_html)) {
        file.remove(tmp_html)
      }
    }
  )
}