representationServer <- function(input, output, session,current_theme = NULL) {
  selectedYear <- session$userData$selectedYear
  
  survey_data <- reactive({
    survey_id <- paste0("PAR_", selectedYear())
    load_survey_data(survey_id)
  })
  
  # Load geo data
  geo_data <- reactive({
    tryCatch({
      sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error loading geo data:", e$message), type = "error")
      NULL
    })
  })
  
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
  # KNOWLEDGE MAPS
  
  # Helper function to create district maps for knowledge questions
  create_knowledge_district_map <- function(data, question_id, title, geo_data, custom_theme = NULL) {
    # Prepare binary data
    binary_data <- prepare_binary_data(
      data = data,
      question_id = question_id,
      metadata = survey_data()$metadata
    )
    
    # Create district map
    create_binary_district_map(
      data = binary_data,
      geo_data = geo_data,
      highlight_extremes = TRUE,
      focus_on_true = TRUE,  # Focus on "Yes" responses
      custom_theme = custom_theme
    )
  }
  
  # Q5: Map of knowledge of Regidor
  output$regidor_knowledge_map <- renderLeaflet({
    req(survey_data(), geo_data())
    create_knowledge_district_map(
      data = survey_data()$responses,
      question_id = "Q5",
      title = "Conocimiento del nombre del Regidor(a)",
      geo_data = geo_data(),
      custom_theme = active_theme()
    )
  })
  
  # Q7: Map of knowledge of Síndico
  output$sindico_knowledge_map <- renderLeaflet({
    req(survey_data(), geo_data())
    create_knowledge_district_map(
      data = survey_data()$responses,
      question_id = "Q7",
      title = "Conocimiento del nombre del Síndico(a)",
      geo_data = geo_data(),
      custom_theme = active_theme()
    )
  })
    # Q8: Map of knowledge of Diputado Local
    output$diputadol_knowledge_map <- renderLeaflet({
      req(survey_data(), geo_data())
      create_knowledge_district_map(
        data = survey_data()$responses,
        question_id = "Q8",
        title = "Conocimiento del nombre del Diputado(a) Local y/o Estatal",
        geo_data = geo_data(),
        custom_theme = active_theme()
      )
    })
  # Q9: Map of knowledge of Diputado
  output$diputadof_knowledge_map <- renderLeaflet({
    req(survey_data(), geo_data())
    create_knowledge_district_map(
      data = survey_data()$responses,
      question_id = "Q10",
      title = "Conocimiento del nombre del Diputado(a) Federal",
      geo_data = geo_data(),
      custom_theme = active_theme()
    )
  })
  
  # SPECIFIC REPRESENTATIVE KNOWLEDGE BAR CHARTS
  
  # Helper function to create bar chart for specific questions with labels
  # Improved helper function to create bar chart for specific questions with labels
  create_representative_knowledge_chart <- function(data, question_prefix, count, labels = NULL, custom_theme = NULL, x_range = c(0, 6)) {
    # Initialize vectors for counts and percentages
    ids_vector <- character(0)
    names_vector <- character(0)
    percentages <- numeric(0)
    
    # Process each question
    for (i in 1:count) {
      question_id <- paste0(question_prefix, ".", i)
      
      if (question_id %in% names(data)) {
        # Get binary responses (1 = Yes, knows the representative)
        values <- data[[question_id]]
        
        # Calculate percentage of positive responses
        if (length(values) > 0) {
          knows_percentage <- 100 * sum(values == "1", na.rm = TRUE) / length(values)
          
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
      colors <- rep(primary_color, nrow(results_df))
      
      # Handle ties for highlighting top N items
      # First, identify the top 3 unique values
      unique_top_values <- unique(results_df$Percentage)[1:min(3, length(unique(results_df$Percentage)))]
      
      # Find all rows that have those top values
      top_indices <- which(results_df$Percentage %in% unique_top_values)
      
      # Highlight all those rows
      colors[top_indices] <- highlight_color
      
      # Determine optimal height based on number of entries
      # Calculate a reasonable height - 40px per entry with a minimum of 400px
      optimal_height <- max(400, nrow(results_df) * 40)
      
      # Create bar chart
      plot_ly(
        data = results_df,
        y = ~Representative,
        x = ~Percentage,
        height = optimal_height,  # Dynamic height based on data points
        type = "bar",
        orientation = "h",
        marker = list(color = colors),
        text = ~paste0(round(Percentage, 1), "%"),
        textposition = "auto",
        hoverinfo = "text",
        hovertext = ~paste0(Representative, ": ", round(Percentage, 1), "%")
      ) %>%
      layout(
        title = "",
        xaxis = list(
          title = "Porcentaje que conoce",
          range = x_range
        ),
        yaxis = list(
          title = "",
          categoryorder = 'total ascending',
          automargin = TRUE  # Automatically adjust margin to fit labels
        ),
        margin = list(l = 250)  # Increase left margin for longer names
      )
    } else {
      # Return empty plot if no data
      plot_ly() %>%
        layout(title = "No hay datos disponibles")
    }
  }



  
  if (selectedYear() == '2024') {

  # Define actual labels for representatives from metadata
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
    # Note: Q8.10 is "No conoce diputado(a) local/estatal" which is not a name, so excluded
  )
  
  diputados_federales_labels <- c(
    "Daniel Murguía Lardizabal (Distrito 01)", 
    "Teresita de Jesus Vargas Meráz (Distrito 02)", 
    "Lilia Aguilar Gil (Distrito 03)", 
    "Alejandro Perez Cuellar (Distrito 04)"
  )
} else {
   # Define actual labels for representatives from metadata
   regidores_labels <- c("María Dolores Adame Alvarado", "Alma Edith Arredondo Salinas", "Héctor Hugo Avitia Arellanes",
   "Amparo Beltrán Ceballos", "Jorge Marcial Bueno Quiroz", "Mayra Karina Castillo Tapia",
   "Antonio Domínguez Alderete", "Karla Michael Escalante Ramírez", "Ana Carmen Estrada García",
   "Joob Quintin Flores Silva", "Austria Elizabeth Galindo Rodríguez", "Jorge Alberto Gutiérrez Casas",
   "Tania Maldonado Garduño", "Pedro Alberto Matus Peña", "Martha Patricia Mendoza Rodríguez",
   "Vanessa Mora de la O", "Mireya Porras Armendáriz", "Yolanda Cecilia Reyes Castro",
   "Víctor Manuel Talamantes Vázquez", "Enrique Torres Valadez")
 
  
  diputados_locales_labels <-c("Leticia Ortega Máynez (Distrito 02)", "Oscar Daniel Avitia Arellanes (Distrito 03)", 
  "Rosana Díaz Reyes (Distrito 04)", "Marisela Terrazas Muñoz (Distrito 05)", 
  "Jael Argüelles Díaz (Distrito 06)", "Gustavo de la Rosa Hickerson (Distrito 07)", 
  "Edin Cuauhtémoc Estrada Sotelo (Distrito 08)", "Magdalena Rentería Pérez (Distrito 09)", 
  "María Antonieta Pérez Reyes (Distrito 10)")

  
  diputados_federales_labels <- c("Daniel Murguía Lardizabal (Distrito 01)", "Teresita de Jesús Vargas Meráz (Distrito 02)", 
  "Lilia Aguilar Gil (Distrito 03)", "Daniela Soraya Álvarez Hernández (Distrito 04)")

}
  # Q6.1-6.20: Knowledge of Regidores
  output$regidores_knowledge_chart <- renderPlotly({
    req(survey_data())
    create_representative_knowledge_chart(
      data = survey_data()$responses, 
      question_prefix = "Q6", 
      count = 20, 
      labels = regidores_labels,
      custom_theme = active_theme(),
      x_range = c(0, 0.7) 
    )%>%  apply_plotly_theme()
  })
  
  # Q8.1-8.10: Knowledge of Diputados Locales
  output$diputados_locales_knowledge_chart <- renderPlotly({
    req(survey_data())
    create_representative_knowledge_chart(
      data = survey_data()$responses, 
      question_prefix = "Q9", 
      count = 9,  # Only use 9 instead of 10 since Q8.10 is not a name
      labels = diputados_locales_labels,
      custom_theme = active_theme(),
      x_range = c(0, 3)
    )%>%  apply_plotly_theme()
  })
  
  # Q10.1-10.4: Knowledge of Diputados Federales
  output$diputados_federales_knowledge_chart <- renderPlotly({
    req(survey_data())
    create_representative_knowledge_chart(
      data = survey_data()$responses, 
      question_prefix = "Q11", 
      count = 4, 
      labels = diputados_federales_labels,
      custom_theme = active_theme(),
      x_range = c(0, 6)
    ) %>%  apply_plotly_theme()
  })
  
  # REPRESENTATION RATINGS VALUE BOXES
  
  # Helper function to calculate average rating
  calculate_avg_rating <- function(data, question_id) {
    values <- as.numeric(data[[question_id]])
    values <- values[!is.na(values)]
    
    if (length(values) > 0) {
      avg <- mean(values, na.rm = TRUE)
      return(sprintf("%.1f / 10", avg))
    } else {
      return("No disponible")
    }
  }
  
  # Q11: Regidores representation rating
  output$regidores_rating <- renderText({
    req(survey_data())
    calculate_avg_rating(survey_data()$responses, "Q12")
  })
  
  # Q12: Síndico representation rating
  output$sindico_rating <- renderText({
    req(survey_data())
    calculate_avg_rating(survey_data()$responses, "Q13")
  })
  
  # Q13: Local deputy representation rating
  output$diputado_local_rating <- renderText({
    req(survey_data())
    calculate_avg_rating(survey_data()$responses, "Q14")
  })
  
  # Q14: Federal deputy representation rating
  output$diputado_federal_rating <- renderText({
    req(survey_data())
    calculate_avg_rating(survey_data()$responses, "Q15")
  })

  observeEvent(input$knowledge_tabs, {
    # Store the active tab in a reactive value for the download handler
    tab_value <- input$knowledge_tabs
  })
# Download handler that adapts based on active tab
output$download_political_knowledge_map <- downloadHandler(
  filename = function() {
    # Get map type for filename based on active tab
    map_type <- if(input$knowledge_tabs == "Regidor(a)"){ 
                      "Regidor"} else if (input$knowledge_tabs == "Síndico(a)"){
                        "Sindico" } else if (input$knowledge_tabs == "Diputado(a) Local y/o Estatal"){
                        "Diputado_local_estatal" }else {
                        "Diputado_federal"
                      }
    paste("mapa_conocimiento_representantes_", map_type, "_", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # Temporary file for the HTML content
    tmp_html <- tempfile(fileext = ".html")
    
    # Create the appropriate map based on active tab
    if(input$knowledge_tabs == "Regidor(a)") {
      map <-     create_knowledge_district_map(
        data = survey_data()$responses,
        question_id = "Q5",
        title = "Conocimiento del nombre del Regidor(a)",
        geo_data = geo_data(),
        custom_theme = active_theme()
      )
      title_text <- "Conocimiento del nombre del Regidor(a) por distrito"
    } else if(input$knowledge_tabs == "Síndico(a)") {
      map <-     create_knowledge_district_map(
        data = survey_data()$responses,
        question_id = "Q7",
        title = "Conocimiento del nombre del Síndico(a)",
        geo_data = geo_data(),
        custom_theme = active_theme()
      )
      title_text <- "Conocimiento del nombre del Síndico(a)"
    } else if(input$knowledge_tabs == "Diputado(a) Local y/o Estatal") {
      map <-     create_knowledge_district_map(
        data = survey_data()$responses,
        question_id = "Q8",
        title = "Conocimiento del nombre del Diputado(a) Local y/o Estatal",
        geo_data = geo_data(),
        custom_theme = active_theme()
      )
      title_text <- "Conocimiento del nombre del Diputado(a) Local y/o Estatal"
  } else  {
    map <-     create_knowledge_district_map(
      data = survey_data()$responses,
      question_id = "Q10",
      title = "Conocimiento del nombre del Diputado(a) Federal",
      geo_data = geo_data(),
      custom_theme = active_theme()
    )
    
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
      options = list(
        printBackground = TRUE,
        scale = 2.0
      ),
      format = "png",
      browser = "C:/Program Files/Google/Chrome/Application/chrome.exe",
      extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
    )
    
    # Clean up
    if (file.exists(tmp_html)) {
      file.remove(tmp_html)
    }
  }
)



}