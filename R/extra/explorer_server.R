explorerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Use year from user data (this accesses the year selector in the main app)
    selectedYear <- session$userData$selectedYear
    
    # Create a reactive for the current survey ID
    currentSurveyId <- reactive({
      paste0(input$survey_type, "_", input$survey_year)
    })
    
    # Load survey data based on selected type and year
    surveyData <- reactive({
      req(input$survey_type, input$survey_year)
      survey_id <- currentSurveyId()
      
      # Show a loading message
      showNotification("Cargando datos de la encuesta...", type = "message", id = "loading")
      
      # Load the survey data
      tryCatch({
        data <- load_survey_data(survey_id)
        # Remove the loading notification
        removeNotification("loading")
        return(data)
      }, error = function(e) {
        # Handle error
        removeNotification("loading")
        showNotification(
          paste("Error al cargar datos:", e$message), 
          type = "error",
          duration = 10
        )
        return(NULL)
      })
    })
    
    # Load geo data for maps
    geoData <- reactive({
      tryCatch({
        sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
      }, error = function(e) {
        showNotification(
          paste("Error al cargar datos geográficos:", e$message), 
          type = "error",
          duration = 10
        )
        NULL
      })
    })
    
    # Load theme metadata
    allThemes <- reactive({
      tryCatch({
        theme_metadata$load_thematic_classifications()
      }, error = function(e) {
        showNotification(
          paste("Error al cargar metadata de temas:", e$message), 
          type = "error",
          duration = 10
        )
        # Return empty data frame with required columns
        data.frame(
          variable = character(0),
          survey_id = character(0),
          MainTheme = character(0),
          Subtheme = character(0),
          stringsAsFactors = FALSE
        )
      })
    })
    
    # Classify questions by scale type
    questionClassifications <- reactive({
      req(surveyData())
      
      if (is.null(surveyData()$metadata)) {
        showNotification("No se encontraron metadatos de preguntas", type = "warning")
        return(list())
      }
      
      # Classify questions
      tryCatch({
        classify_questions(surveyData()$metadata)
      }, error = function(e) {
        showNotification(
          paste("Error al clasificar preguntas:", e$message), 
          type = "warning",
          duration = 7
        )
        # Return empty classifications
        list(
          razon = character(0),
          intervalo = character(0),
          ordinal = character(0),
          categorico = character(0),
          binaria = character(0),
          nominal = character(0)
        )
      })
    })
    
    # Update theme filters
    observe({
      # Get theme list safely
      themes_list <- tryCatch({
        theme_metadata$get_all_themes()
      }, error = function(e) {
        showNotification("Error al cargar temas", type = "warning")
        character(0)
      })
      
      updateSelectInput(session, "theme_filter", 
                       choices = c("Todos" = "all", themes_list),
                       selected = "all")
    })
    
    # Update subtheme filters based on selected theme
    observe({
      req(input$theme_filter)
      
      if (input$theme_filter != "all") {
        # Get subthemes safely
        subthemes <- tryCatch({
          theme_metadata$get_subthemes_by_theme(input$theme_filter)
        }, error = function(e) {
          showNotification("Error al cargar subtemas", type = "warning")
          character(0)
        })
        
        updateSelectInput(session, "subtheme_filter", 
                         choices = c("Todos" = "all", subthemes),
                         selected = "all")
      } else {
        # If "all themes" is selected, reset subtheme selection
        updateSelectInput(session, "subtheme_filter", 
                         choices = c("Todos" = "all"),
                         selected = "all")
      }
    })
    
    # Filter questions based on search, theme, and subtheme
    filteredQuestions <- reactive({
      req(surveyData(), questionClassifications())
      
      # Get all questions
      all_classifications <- questionClassifications()
      all_questions <- unique(unlist(all_classifications))
      
      # If no questions, return empty list
      if (length(all_questions) == 0) {
        return(list(
          questions = character(0),
          labels = character(0)
        ))
      }
      
      # Create question labels
      question_labels <- sapply(all_questions, function(q) {
        label <- get_question_label(q, surveyData()$metadata)
        if (is.na(label) || label == "") {
          return(q)  # Use question ID if no label
        }
        # Truncate long labels
        if (nchar(label) > 60) {
          return(paste0(q, ": ", substr(label, 1, 57), "..."))
        } else {
          return(paste0(q, ": ", label))
        }
      })
      
      # Apply theme filtering
      filtered_questions <- all_questions
      
      if (input$theme_filter != "all" || input$subtheme_filter != "all") {
        # Get current survey ID
        current_survey_id <- currentSurveyId()
        if (grepl("_V[0-9]+$", current_survey_id)) {
          current_survey_id <- gsub("_V[0-9]+$", "", current_survey_id)
        }
        
        # Filter by theme and subtheme
        relevant_themes <- allThemes() %>% 
          filter(survey_id == current_survey_id)
        
        if (input$theme_filter != "all" && input$subtheme_filter == "all") {
          # Filter by theme only
          theme_filtered <- relevant_themes %>%
            filter(MainTheme == input$theme_filter) %>%
            pull(variable)
          
          filtered_questions <- filtered_questions[filtered_questions %in% theme_filtered]
        } else if (input$theme_filter != "all" && input$subtheme_filter != "all") {
          # Filter by theme and subtheme
          theme_filtered <- relevant_themes %>%
            filter(MainTheme == input$theme_filter, 
                   Subtheme == input$subtheme_filter) %>%
            pull(variable)
          
          filtered_questions <- filtered_questions[filtered_questions %in% theme_filtered]
        }
      }
      
      # Apply search filter if provided
      if (!is.null(input$search_query) && input$search_query != "") {
        search_terms <- tolower(input$search_query)
        # Search in both IDs and labels
        matched_indices <- grep(search_terms, tolower(question_labels), fixed = TRUE)
        
        if (length(matched_indices) > 0) {
          filtered_questions <- filtered_questions[matched_indices]
        } else {
          # If no matches in labels, try question IDs
          matched_ids <- grep(search_terms, tolower(filtered_questions), fixed = TRUE)
          if (length(matched_ids) > 0) {
            filtered_questions <- filtered_questions[matched_ids]
          } else {
            filtered_questions <- character(0)
          }
        }
      }
      
      # Get labels for filtered questions
      filtered_labels <- question_labels[all_questions %in% filtered_questions]
      
      # Return list with questions and labels
      return(list(
        questions = filtered_questions,
        labels = filtered_labels
      ))
    })
    
    # Update question selection dropdown
    observe({
      filtered <- filteredQuestions()
      
      if (length(filtered$questions) == 0) {
        updateSelectInput(
          session,
          "question_select",
          choices = c("No hay preguntas que coincidan con los criterios" = ""),
          selected = ""
        )
      } else {
        # Create choices list
        choices <- setNames(filtered$questions, filtered$labels)
        
        # Sort alphabetically
        choices <- choices[order(names(choices))]
        
        # Update dropdown
        updateSelectInput(
          session,
          "question_select",
          choices = choices,
          selected = NULL
        )
      }
    })
    
    # Determine scale type for the selected question
    selectedScaleType <- reactive({
      req(input$question_select, questionClassifications())
      
      question_id <- input$question_select
      classifications <- questionClassifications()
      
      if (question_id %in% classifications$razon) {
        return("razon")
      } else if (question_id %in% classifications$intervalo) {
        return("intervalo")
      } else if (question_id %in% classifications$ordinal) {
        return("ordinal")
      } else if (question_id %in% classifications$categorico) {
        return("categorico")
      } else if (question_id %in% classifications$binaria) {
        return("binaria")
      } else if (question_id %in% classifications$nominal) {
        return("nominal")
      } else {
        return(NULL)
      }
    })
    
    # Display question text
    output$question_text <- renderUI({
      req(input$question_select, surveyData())
      
      if (input$question_select == "") {
        return(HTML("<em>Seleccione una pregunta para visualizar los resultados</em>"))
      }
      
      # Get question label
      label <- get_question_label(input$question_select, surveyData()$metadata)
      
      # Display question and ID
      HTML(paste0(
        "<strong>ID: </strong>", input$question_select, "<br/>",
        "<strong>Pregunta: </strong>", label, "<br/>",
        "<strong>Tipo: </strong>", 
        switch(selectedScaleType(),
               "razon" = "Razón (numérica continua)",
               "intervalo" = "Intervalo (numérica con rangos)",
               "ordinal" = "Ordinal (categorías ordenadas)",
               "categorico" = "Categórica (categorías sin orden)",
               "binaria" = "Binaria (sí/no)",
               "nominal" = "Nominal (texto abierto)",
               "Desconocido")
      ))
    })
    
    # Generate visualization title
    output$viz_title <- renderText({
      req(input$question_select)
      
      if (input$question_select == "") {
        return("Seleccione una pregunta para visualizar")
      }
      
      "Resultados de la Visualización"
    })
    
    # Display visualization options based on scale type
    output$viz_options <- renderUI({
      req(input$question_select, selectedScaleType())
      
      scale_type <- selectedScaleType()
      
      # Define visualization options based on scale type
      if (scale_type == "razon") {
        selectInput(
          ns("viz_type"),
          "Tipo de Visualización:",
          choices = c(
            "Resumen Estadístico" = "summary",
            "Histograma" = "histogram",
            "Mapa de Distritos" = "district_map",
            "Barras por Distrito" = "district_bars"
          ),
          selected = "summary"
        )
      } else if (scale_type %in% c("intervalo", "ordinal")) {
        selectInput(
          ns("viz_type"),
          "Tipo de Visualización:",
          choices = c(
            "Resumen Estadístico" = "summary",
            "Histograma" = "histogram",
            "Gráfico Circular" = "pie",
            "Mapa de Distritos" = "district_map",
            "Barras por Distrito" = "district_bars"
          ),
          selected = "summary"
        )
      } else if (scale_type == "categorico") {
        selectInput(
          ns("viz_type"),
          "Tipo de Visualización:",
          choices = c(
            "Resumen Estadístico" = "summary",
            "Gráfico de Barras" = "bars",
            "Gráfico Circular" = "pie",
            "Barras Apiladas por Distrito" = "stacked_bars"
          ),
          selected = "summary"
        )
      } else if (scale_type == "binaria") {
        selectInput(
          ns("viz_type"),
          "Tipo de Visualización:",
          choices = c(
            "Resumen Estadístico" = "summary",
            "Gráfico de Barras" = "bars",
            "Gráfico Circular" = "pie",
            "Mapa de Distritos" = "district_map",
            "Barras por Distrito" = "district_bars",
            "Comparación Múltiple" = "multiple_comparison"
          ),
          selected = "summary"
        )
      } else if (scale_type == "nominal") {
        selectInput(
          ns("viz_type"),
          "Tipo de Visualización:",
          choices = c(
            "Resumen Estadístico" = "summary",
            "Frecuencia de Palabras" = "word_freq"
          ),
          selected = "summary"
        )
      } else {
        # Default case
        selectInput(
          ns("viz_type"),
          "Tipo de Visualización:",
          choices = c("Resumen Estadístico" = "summary"),
          selected = "summary"
        )
      }
    })
    
    # Prepare data for the selected question
    preparedData <- reactive({
      req(input$question_select, surveyData(), selectedScaleType())
      
      question_id <- input$question_select
      scale_type <- selectedScaleType()
      
      # Prepare data based on scale type
      if (scale_type == "razon") {
        prepare_razon_data(surveyData()$responses, question_id, surveyData()$metadata)
      } else if (scale_type == "intervalo") {
        prepare_interval_data(surveyData()$responses, question_id, surveyData()$metadata)
      } else if (scale_type == "ordinal") {
        prepare_ordinal_data(surveyData()$responses, question_id, surveyData()$metadata)
      } else if (scale_type == "categorico") {
        prepare_categorical_data(surveyData()$responses, question_id, surveyData()$metadata)
      } else if (scale_type == "binaria") {
        prepare_binary_data(surveyData()$responses, question_id, surveyData()$metadata)
      } else if (scale_type == "nominal") {
        prepare_nominal_data(surveyData()$responses, question_id, surveyData()$metadata)
      } else {
        # Default empty data
        NULL
      }
    })
    
    # Update district filter
    observe({
      req(preparedData())
      
      if (is.null(preparedData()) || nrow(preparedData()) == 0) {
        updateSelectInput(session, "district_filter",
                         choices = character(0),
                         selected = character(0))
        return()
      }
      
      updateSelectInput(session, "district_filter",
                       choices = unique(preparedData()$district),
                       selected = character(0))
    })
    
    # Apply district filter to prepared data
    filteredData <- reactive({
      req(preparedData())
      
      data <- preparedData()
      
      # Apply district filter if selected
      if (length(input$district_filter) > 0) {
        data <- data %>% filter(district %in% input$district_filter)
      }
      
      # Return filtered data
      data
    })
    
    # Show custom visualization options based on selected visualization type
    output$custom_viz_options <- renderUI({
      req(input$viz_type, selectedScaleType())
      
      scale_type <- selectedScaleType()
      viz_type <- input$viz_type
      
      # Create appropriate UI elements based on visualization type
      if (scale_type %in% c("razon", "intervalo", "ordinal") && viz_type == "histogram") {
        # Histogram options
        sliderInput(
          ns("histogram_bins"),
          "Número de Bins:",
          min = 5,
          max = 50,
          value = 30
        )
      } else if (scale_type %in% c("razon", "intervalo", "ordinal", "binaria") && viz_type == "district_map") {
        # Map options
        tagList(
          checkboxInput(
            ns("highlight_extremes"),
            "Resaltar valores extremos",
            value = TRUE
          ),
          checkboxInput(
            ns("use_gradient"),
            "Usar escala de color por valor",
            value = FALSE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("use_gradient")),
            selectInput(
              ns("color_scale"),
              "Paleta de Colores:",
              choices = c(
                "Azules" = "Blues",
                "Rojos" = "Reds",
                "Verdes" = "Greens",
                "Púrpuras" = "Purples",
                "Viridis" = "viridis",
                "Divergente" = "RdBu"
              ),
              selected = "Blues"
            )
          )
        )
      } else if (scale_type %in% c("razon", "intervalo", "ordinal", "binaria") && viz_type == "district_bars") {
        # Bar orientation options
        radioButtons(
          ns("bar_orientation"),
          "Orientación:",
          choices = c(
            "Vertical" = "v",
            "Horizontal" = "h"
          ),
          selected = "v"
        )
      } else if (scale_type == "binaria" && viz_type == "multiple_comparison") {
        # Multiple binary question comparison
        tagList(
          checkboxGroupInput(
            ns("compare_questions"),
            "Preguntas a Comparar:",
            choices = NULL
          ),
          sliderInput(
            ns("top_n"),
            "Mostrar Top N:",
            min = 3,
            max = 20,
            value = 5
          ),
          radioButtons(
            ns("comparison_type"),
            "Tipo de Visualización:",
            choices = c(
              "Barras Horizontales" = "bars",
              "Gráfico Circular" = "pie",
              "Gráfico de Burbujas" = "bubbles"
            ),
            selected = "bars"
          )
        )
      } else if (scale_type == "nominal" && viz_type == "word_freq") {
        # Word frequency options
        tagList(
          sliderInput(
            ns("max_words"),
            "Número Máximo de Palabras:",
            min = 5,
            max = 50,
            value = 20
          ),
          checkboxInput(
            ns("exclude_stopwords"),
            "Excluir Palabras Comunes (Stopwords)",
            value = TRUE
          ),
          sliderInput(
            ns("min_chars"),
            "Longitud Mínima de Palabra:",
            min = 1,
            max = 10,
            value = 3
          )
        )
      } else {
        # No additional options for other visualization types
        div()
      }
    })
    
    # Update binary comparison questions
    observe({
      req(selectedScaleType() == "binaria", input$viz_type == "multiple_comparison")
      
      # Get all binary questions
      binary_questions <- questionClassifications()$binaria
      
      if (length(binary_questions) == 0) {
        updateCheckboxGroupInput(
          session, 
          "compare_questions",
          choices = NULL
        )
        return()
      }
      
      # Get labels for questions
      question_labels <- sapply(binary_questions, function(qid) {
        # Try to get label from metadata
        q_meta <- surveyData()$metadata %>% filter(variable == qid) %>% first()
        if (!is.null(q_meta) && !is.na(q_meta$label)) {
          # Truncate long labels
          label <- q_meta$label
          if (nchar(label) > 50) {
            label <- paste0(substr(label, 1, 47), "...")
          }
          return(paste0(qid, " - ", label))
        } else {
          return(qid)
        }
      })
      
      updateCheckboxGroupInput(
        session, 
        "compare_questions",
        choices = question_labels,
        selected = input$question_select
      )
    })
    
    # Get theme based on section
    sectionTheme <- reactive({
      # Get the current question
      req(input$question_select)
      
      # Default to extras theme
      section_theme <- "extras"
      
      # Try to get theme from metadata
      tryCatch({
        themes_data <- allThemes()
        
        # Get current survey ID
        current_survey_id <- currentSurveyId()
        if (grepl("_V[0-9]+$", current_survey_id)) {
          current_survey_id <- gsub("_V[0-9]+$", "", current_survey_id)
        }
        
        # Find theme info for this question
        question_theme_info <- themes_data %>%
          filter(variable == input$question_select, survey_id == current_survey_id) %>%
          select(MainTheme) %>%
          first()
        
        # Map main themes to section themes
        theme_to_section <- list(
          "Social & Economic Wellbeing" = "bienestar",
          "Public Services" = "infraestructura",
          "Urban Mobility & Environment" = "movilidad",
          "Governance & Civic Engagement" = "gobierno",
          "Community Participation" = "participacion"
        )
        
        if (!is.null(question_theme_info) && !is.na(question_theme_info$MainTheme)) {
          main_theme <- question_theme_info$MainTheme
          
          # Map the main theme to a section theme
          if (main_theme %in% names(theme_to_section)) {
            section_theme <- theme_to_section[[main_theme]]
          }
        }
      }, error = function(e) {
        message("Error determining question theme: ", e$message)
      })
      
      # Get the section theme
      get_section_theme(section_theme)
    })
    
    # Generate the appropriate visualization
    output$visualization <- renderUI({
      req(input$question_select, input$viz_type, selectedScaleType())
      
      scale_type <- selectedScaleType()
      viz_type <- input$viz_type
      
      # Display appropriate visualization based on type and scale
      if (viz_type == "summary") {
        verbatimTextOutput(ns("summary_output"))
      } else if (scale_type == "razon") {
        if (viz_type == "histogram") {
          plotlyOutput(ns("razon_histogram"), height = "500px")
        } else if (viz_type == "district_map") {
          leafletOutput(ns("razon_district_map"), height = "600px")
        } else if (viz_type == "district_bars") {
          plotlyOutput(ns("razon_district_bars"), height = "500px")
        }
      } else if (scale_type == "intervalo" || scale_type == "ordinal") {
        if (viz_type == "histogram") {
          plotlyOutput(ns("interval_histogram"), height = "500px")
        } else if (viz_type == "pie") {
          plotlyOutput(ns("interval_pie"), height = "500px")
        } else if (viz_type == "district_map") {
          leafletOutput(ns("interval_district_map"), height = "600px")
        } else if (viz_type == "district_bars") {
          plotlyOutput(ns("interval_district_bars"), height = "500px")
        }
      } else if (scale_type == "categorico") {
        if (viz_type == "bars") {
          plotlyOutput(ns("categorical_bars"), height = "500px")
        } else if (viz_type == "pie") {
          plotlyOutput(ns("categorical_pie"), height = "500px")
        } else if (viz_type == "stacked_bars") {
          plotlyOutput(ns("categorical_stacked_bars"), height = "500px")
        }
      } else if (scale_type == "binaria") {
        if (viz_type == "bars") {
          plotlyOutput(ns("binary_bars"), height = "500px")
        } else if (viz_type == "pie") {
          plotlyOutput(ns("binary_pie"), height = "500px")
        } else if (viz_type == "district_map") {
          leafletOutput(ns("binary_district_map"), height = "600px")
        } else if (viz_type == "district_bars") {
          plotlyOutput(ns("binary_district_bars"), height = "500px")
        } else if (viz_type == "multiple_comparison") {
          plotlyOutput(ns("binary_comparison"), height = "600px")
        }
      } else if (scale_type == "nominal") {
        if (viz_type == "word_freq") {
          plotlyOutput(ns("nominal_word_freq"), height = "500px")
        }
      }
    })
    
    # Display appropriate download options based on visualization type
    output$download_options <- renderUI({
      req(input$question_select, input$viz_type)
      
      viz_type <- input$viz_type
      scale_type <- selectedScaleType()
      
      if (viz_type == "summary") {
        div(
          class = "download-btn",
          downloadButton(ns("download_summary_csv"), "Descargar CSV"),
          downloadButton(ns("download_summary_excel"), "Descargar Excel")
        )
      } else if (grepl("map$", viz_type)) {
        # For maps
        div(
          class = "download-btn",
          downloadButton(ns("download_map_png"), "Descargar Imagen")
        )
      } else {
        # For plotly charts
        # The download functionality is built into plotly via apply_plotly_theme
        div()
      }
    })
    
    # Download handlers for summary statistics
    output$download_summary_csv <- downloadHandler(
      filename = function() {
        paste0("resumen_", input$question_select, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        if (selectedScaleType() == "razon") {
          # Razon summary
          data <- filteredData()
          summary_stats <- data.frame(
            Statistic = c("Media", "Mediana", "Desviación Estándar", "Mínimo", "Máximo", "N"),
            Value = c(
              round(mean(data$value, na.rm = TRUE), 2),
              median(data$value, na.rm = TRUE),
              round(sd(data$value, na.rm = TRUE), 2),
              min(data$value, na.rm = TRUE),
              max(data$value, na.rm = TRUE),
              nrow(data)
            )
          )
          write.csv(summary_stats, file, row.names = FALSE)
        } else if (selectedScaleType() %in% c("intervalo", "ordinal")) {
          # Interval/Ordinal summary
          data <- filteredData()
          numeric_values <- get_numeric_values(data)
          summary_stats <- data.frame(
            Statistic = c("Media", "Mediana", "Desviación Estándar", "Mínimo", "Máximo", "N"),
            Value = c(
              round(mean(numeric_values, na.rm = TRUE), 2),
              median(numeric_values, na.rm = TRUE),
              round(sd(numeric_values, na.rm = TRUE), 2),
              min(numeric_values, na.rm = TRUE),
              max(numeric_values, na.rm = TRUE),
              length(numeric_values)
            )
          )
          write.csv(summary_stats, file, row.names = FALSE)
        } else if (selectedScaleType() == "categorico") {
          # Categorical summary
          data <- filteredData()
          freq_table <- as.data.frame(table(data$value))
          names(freq_table) <- c("Categoría", "Frecuencia")
          freq_table$Porcentaje <- round(100 * freq_table$Frecuencia / sum(freq_table$Frecuencia), 2)
          write.csv(freq_table, file, row.names = FALSE)
        } else if (selectedScaleType() == "binaria") {
          # Binary summary
          data <- filteredData()
          binary_stats <- data.frame(
            Categoría = c("Sí/Verdadero", "No/Falso", "Total"),
            Conteo = c(
              sum(data$binary_value, na.rm = TRUE),
              sum(!data$binary_value, na.rm = TRUE),
              nrow(data)
            ),
            Porcentaje = c(
              round(100 * mean(data$binary_value, na.rm = TRUE), 2),
              round(100 * (1 - mean(data$binary_value, na.rm = TRUE)), 2),
              100
            )
          )
          write.csv(binary_stats, file, row.names = FALSE)
        } else if (selectedScaleType() == "nominal") {
          # Nominal summary
          data <- filteredData()
          word_freq <- attr(data, "word_freq")
          if (!is.null(word_freq) && nrow(word_freq) > 0) {
            write.csv(word_freq, file, row.names = FALSE)
          } else {
            # Fallback if no word frequency
            empty_df <- data.frame(message = "No hay datos de frecuencia de palabras disponibles")
            write.csv(empty_df, file, row.names = FALSE)
          }
        }
      }
    )
    
    output$download_summary_excel <- downloadHandler(
      filename = function() {
        paste0("resumen_", input$question_select, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          # Fall back to csv if openxlsx is not available
          tmpfile <- tempfile(fileext = ".csv")
          on.exit(unlink(tmpfile))
          
          if (selectedScaleType() == "razon") {
            data <- filteredData()
            summary_stats <- data.frame(
              Statistic = c("Media", "Mediana", "Desviación Estándar", "Mínimo", "Máximo", "N"),
              Value = c(
                round(mean(data$value, na.rm = TRUE), 2),
                median(data$value, na.rm = TRUE),
                round(sd(data$value, na.rm = TRUE), 2),
                min(data$value, na.rm = TRUE),
                max(data$value, na.rm = TRUE),
                nrow(data)
              )
            )
            write.csv(summary_stats, tmpfile, row.names = FALSE)
          } else {
            # Similar fallbacks for other types...
            write.csv(data.frame(message = "Excel export not available"), tmpfile, row.names = FALSE)
          }
          
          file.copy(tmpfile, file)
          return()
        }
        
        # Create workbook
        wb <- openxlsx::createWorkbook()
        
        if (selectedScaleType() == "razon") {
          # Razon summary
          data <- filteredData()
          
          # Overall stats
          openxlsx::addWorksheet(wb, "Estadísticas Generales")
          summary_stats <- data.frame(
            Statistic = c("Media", "Mediana", "Desviación Estándar", "Mínimo", "Máximo", "N"),
            Value = c(
              round(mean(data$value, na.rm = TRUE), 2),
              median(data$value, na.rm = TRUE),
              round(sd(data$value, na.rm = TRUE), 2),
              min(data$value, na.rm = TRUE),
              max(data$value, na.rm = TRUE),
              nrow(data)
            )
          )
          openxlsx::writeData(wb, "Estadísticas Generales", summary_stats)
          
          # District stats
          openxlsx::addWorksheet(wb, "Por Distrito")
          district_stats <- data %>%
            group_by(district) %>%
            summarise(
              n = n(),
              Media = round(mean(value, na.rm = TRUE), 2),
              Mediana = median(value, na.rm = TRUE),
              DE = round(sd(value, na.rm = TRUE), 2),
              Min = min(value, na.rm = TRUE),
              Max = max(value, na.rm = TRUE)
            )
          openxlsx::writeData(wb, "Por Distrito", district_stats)
          
        } else if (selectedScaleType() %in% c("intervalo", "ordinal")) {
          # Interval/Ordinal summary
          data <- filteredData()
          numeric_values <- get_numeric_values(data)
          
          # Overall stats
          openxlsx::addWorksheet(wb, "Estadísticas Generales")
          summary_stats <- data.frame(
            Statistic = c("Media", "Mediana", "Desviación Estándar", "Mínimo", "Máximo", "N"),
            Value = c(
              round(mean(numeric_values, na.rm = TRUE), 2),
              median(numeric_values, na.rm = TRUE),
              round(sd(numeric_values, na.rm = TRUE), 2),
              min(numeric_values, na.rm = TRUE),
              max(numeric_values, na.rm = TRUE),
              length(numeric_values)
            )
          )
          openxlsx::writeData(wb, "Estadísticas Generales", summary_stats)
          
          # Value labels if available
          if (!is.null(attr(data, "value_labels"))) {
            openxlsx::addWorksheet(wb, "Etiquetas")
            labels_df <- data.frame(
              Valor = names(attr(data, "value_labels")),
              Etiqueta = unname(attr(data, "value_labels"))
            )
            openxlsx::writeData(wb, "Etiquetas", labels_df)
          }
          
        } else if (selectedScaleType() == "categorico") {
          # Categorical summary
          data <- filteredData()
          
          # Frequency table
          openxlsx::addWorksheet(wb, "Frecuencias")
          freq_table <- as.data.frame(table(data$value))
          names(freq_table) <- c("Categoría", "Frecuencia")
          freq_table$Porcentaje <- round(100 * freq_table$Frecuencia / sum(freq_table$Frecuencia), 2)
          openxlsx::writeData(wb, "Frecuencias", freq_table)
          
          # By district
          openxlsx::addWorksheet(wb, "Por Distrito")
          dist_table <- as.data.frame(table(data$district, data$value))
          names(dist_table) <- c("Distrito", "Categoría", "Frecuencia")
          openxlsx::writeData(wb, "Por Distrito", dist_table)
          
        } else if (selectedScaleType() == "binaria") {
          # Binary summary
          data <- filteredData()
          
          # Overall stats
          openxlsx::addWorksheet(wb, "Estadísticas Generales")
          binary_stats <- data.frame(
            Categoría = c("Sí/Verdadero", "No/Falso", "Total"),
            Conteo = c(
              sum(data$binary_value, na.rm = TRUE),
              sum(!data$binary_value, na.rm = TRUE),
              nrow(data)
            ),
            Porcentaje = c(
              round(100 * mean(data$binary_value, na.rm = TRUE), 2),
              round(100 * (1 - mean(data$binary_value, na.rm = TRUE)), 2),
              100
            )
          )
          openxlsx::writeData(wb, "Estadísticas Generales", binary_stats)
          
          # By district
          openxlsx::addWorksheet(wb, "Por Distrito")
          district_stats <- data %>%
            group_by(district) %>%
            summarise(
              Total = n(),
              Positivo = sum(binary_value, na.rm = TRUE),
              `Porcentaje Positivo` = round(100 * mean(binary_value, na.rm = TRUE), 2)
            )
          openxlsx::writeData(wb, "Por Distrito", district_stats)
          
        } else if (selectedScaleType() == "nominal") {
          # Nominal summary
          data <- filteredData()
          word_freq <- attr(data, "word_freq")
          
          if (!is.null(word_freq) && nrow(word_freq) > 0) {
            openxlsx::addWorksheet(wb, "Frecuencia de Palabras")
            openxlsx::writeData(wb, "Frecuencia de Palabras", word_freq)
          } else {
            openxlsx::addWorksheet(wb, "Información")
            openxlsx::writeData(wb, "Información", 
                               data.frame(message = "No hay datos de frecuencia de palabras disponibles"))
          }
        }
        
        # Save workbook
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # Download handler for maps
    output$download_map_png <- downloadHandler(
      filename = function() {
        scale_type <- selectedScaleType()
        viz_type <- input$viz_type
        paste0(scale_type, "_", viz_type, "_", input$question_select, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        # We need to save the map to a temporary file first
        tmp_html <- tempfile(fileext = ".html")
        
        # Get the appropriate map based on scale type
        map <- NULL
        
        if (selectedScaleType() == "razon" && input$viz_type == "district_map") {
          map <- create_district_map(filteredData(), geoData(), custom_theme = sectionTheme())
        } else if (selectedScaleType() %in% c("intervalo", "ordinal") && input$viz_type == "district_map") {
          map <- create_ordinal_district_map(
            filteredData(),
            geoData(),
            selected_responses = NULL,  # Show means by default
            highlight_extremes = input$highlight_extremes,
            use_gradient = input$use_gradient,
            color_scale = input$color_scale,
            custom_theme = sectionTheme()
          )
        } else if (selectedScaleType() == "binaria" && input$viz_type == "district_map") {
          map <- create_binary_district_map(
            filteredData(), 
            geoData(),
            highlight_extremes = input$highlight_extremes,
            focus_on_true = TRUE,  # Default to showing "Yes" percentages
            custom_theme = sectionTheme()
          )
        }
        
        if (is.null(map)) {
          # Return an error message if map couldn't be created
          showNotification("No se pudo generar el mapa para descargar", type = "error")
          return()
        }
        
        # Add title and footer to the map
        question_label <- get_question_label(input$question_select, surveyData()$metadata)
        
        map <- map %>%
          addControl(
            html = paste("<div style='background-color:white; padding:10px; border-radius:5px; font-weight:bold;'>", 
                        input$question_select, ": ", question_label, 
                        "</div>"),
            position = "topright"
          ) %>%
          addControl(
            html = paste("<div style='background-color:white; padding:8px; border-radius:5px; font-size:12px;'>", 
                        paste("Resultados de la Encuesta", ifelse(input$survey_type == "PER", 
                                                              "de Percepción Ciudadana", 
                                                              "de Participación Ciudadana y Buen Gobierno"), 
                              input$survey_year),
                        "</div>"),
            position = "bottomright"
          )
        
        # Save the map to HTML
        htmlwidgets::saveWidget(map, tmp_html, selfcontained = TRUE)
        
        # Use pagedown with Chrome headless if available
        if (requireNamespace("pagedown", quietly = TRUE)) {
          tryCatch({
            # First try with default browser path
            pagedown::chrome_print(
              input = tmp_html,
              output = file,
              options = list(
                printBackground = TRUE,
                scale = 2.0
              ),
              format = "png"
            )
          }, error = function(e) {
            # If that fails, try with a specific Chrome path
            tryCatch({
              pagedown::chrome_print(
                input = tmp_html,
                output = file,
                options = list(
                  printBackground = TRUE,
                  scale = 2.0
                ),
                format = "png",
                browser = "/usr/bin/google-chrome",
                extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
              )
            }, error = function(e2) {
              # If both methods fail, show error
              showNotification(paste("Error al generar imagen del mapa:", e2$message), type = "error")
            })
          })
        } else {
          showNotification("Paquete 'pagedown' no disponible para descargar mapas. Por favor instale este paquete.", 
                          type = "warning")
        }
        
        # Clean up temporary files
        if (file.exists(tmp_html)) {
          file.remove(tmp_html)
        }
      }
    )
    
    output$summary_output <- renderPrint({
      req(filteredData(), selectedScaleType())
      
      scale_type <- selectedScaleType()
      data <- filteredData()
      
      # Instead of trying to call another output function, we should duplicate the logic here
      if (scale_type == "razon") {
        # Razon summary
        cat("Estadísticas para Datos de Razón:\n")
        cat("\nEstadísticas Generales:\n")
        cat("Media:", round(mean(data$value, na.rm = TRUE), 2), "\n")
        cat("Mediana:", median(data$value, na.rm = TRUE), "\n")
        cat("Desviación Estándar:", round(sd(data$value, na.rm = TRUE), 2), "\n")
        cat("Mínimo:", min(data$value, na.rm = TRUE), "\n")
        cat("Máximo:", max(data$value, na.rm = TRUE), "\n")
        cat("N:", nrow(data), "\n")
        
        # Statistics by district
        cat("\nEstadísticas por Distrito:\n")
        district_stats <- data %>%
          group_by(district) %>%
          summarise(
            n = n(),
            Media = round(mean(value, na.rm = TRUE), 2),
            Mediana = median(value, na.rm = TRUE),
            DE = round(sd(value, na.rm = TRUE), 2),
            Min = min(value, na.rm = TRUE),
            Max = max(value, na.rm = TRUE),
            .groups = 'drop'
          )
        print(district_stats)
        
      } else if (scale_type %in% c("intervalo", "ordinal")) {
        # Interval/Ordinal summary
        cat("Estadísticas para Datos de ", ifelse(scale_type == "intervalo", "Intervalo", "Ordinales"), ":\n")
        
        # Get numeric values
        numeric_values <- get_numeric_values(data)
        
        cat("\nEstadísticas Generales:\n")
        cat("Media:", round(mean(numeric_values, na.rm = TRUE), 2), "\n")
        cat("Mediana:", median(numeric_values, na.rm = TRUE), "\n")
        cat("Desviación Estándar:", round(sd(numeric_values, na.rm = TRUE), 2), "\n")
        cat("Mínimo:", min(numeric_values, na.rm = TRUE), "\n")
        cat("Máximo:", max(numeric_values, na.rm = TRUE), "\n")
        cat("N:", length(numeric_values), "\n")
        
        # Show frequency distribution
        cat("\nDistribución de Frecuencias:\n")
        freq_table <- table(data$value)
        freq_df <- data.frame(
          Categoría = names(freq_table),
          Frecuencia = as.vector(freq_table),
          Porcentaje = round(100 * as.vector(freq_table) / sum(freq_table), 2)
        )
        print(freq_df)
        
        # Value labels if available
        if (!is.null(attr(data, "value_labels"))) {
          cat("\nEtiquetas de Valores:\n")
          for (i in 1:length(attr(data, "value_labels"))) {
            cat(names(attr(data, "value_labels"))[i], "=", attr(data, "value_labels")[i], "\n")
          }
        }
        
      } else if (scale_type == "categorico") {
        # Categorical summary
        cat("Estadísticas para Datos Categóricos:\n")
        
        # Show frequency distribution
        cat("\nDistribución de Frecuencias:\n")
        freq_table <- table(data$value)
        freq_df <- data.frame(
          Categoría = names(freq_table),
          Frecuencia = as.vector(freq_table),
          Porcentaje = round(100 * as.vector(freq_table) / sum(freq_table), 2)
        )
        # Sort by frequency
        freq_df <- freq_df[order(-freq_df$Frecuencia), ]
        print(freq_df)
        
        # Show distribution by district - most common category per district
        cat("\nDistribución por Distrito (categoría más frecuente para cada distrito):\n")
        district_breakdown <- data %>%
          group_by(district, value) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(district) %>%
          mutate(percentage = round(100 * count / sum(count), 1)) %>%
          slice_max(order_by = count, n = 1) %>%
          arrange(district)
          
        print(district_breakdown %>% select(district, Categoría = value, Frecuencia = count, Porcentaje = percentage))
        
      } else if (scale_type == "binaria") {
        # Binary summary
        cat("Estadísticas para Datos Binarios:\n")
        
        # Get the response counts
        total_responses <- nrow(data)
        
        # Calculate binary proportions
        true_count <- sum(data$binary_value, na.rm = TRUE)
        false_count <- sum(!data$binary_value, na.rm = TRUE)
        true_percent <- round(100 * true_count / (true_count + false_count), 2)
        false_percent <- round(100 * false_count / (true_count + false_count), 2)
        
        # Get the appropriate labels using the helper function
        labels <- get_binary_labels(data)
        true_label <- labels$true_label
        false_label <- labels$false_label
        
        cat("\nDistribución de valores:\n")
        cat(true_label, ":", true_count, sprintf("(%.2f%%)", true_percent), "\n")
        cat(false_label, ":", false_count, sprintf("(%.2f%%)", false_percent), "\n")
        
        # District breakdown
        cat("\nDistribución por Distrito:\n")
        district_breakdown <- data %>%
          group_by(district) %>%
          summarise(
            Total = n(),
            Positivo = sum(binary_value, na.rm = TRUE),
            `%Positivo` = round(100 * mean(binary_value, na.rm = TRUE), 2),
            .groups = 'drop'
          )
        
        print(district_breakdown)
        
      } else if (scale_type == "nominal") {
        # Nominal summary
        cat("Estadísticas para Datos Nominales (Abiertos):\n")
        
        word_freq <- attr(data, "word_freq")
        if (!is.null(word_freq) && nrow(word_freq) > 0) {
          cat("\nPalabras más frecuentes:\n")
          top_words <- head(word_freq, 15)
          print(top_words)
          
          cat("\nEstadísticas de longitud de respuesta:\n")
          response_lengths <- sapply(strsplit(data$preprocessed_text, "\\s+"), length)
          cat("Promedio de palabras por respuesta:", round(mean(response_lengths), 2), "\n")
          cat("Mediana de palabras por respuesta:", median(response_lengths), "\n")
          cat("Respuesta más corta:", min(response_lengths), "palabras\n")
          cat("Respuesta más larga:", max(response_lengths), "palabras\n")
        } else {
          cat("\nNo hay datos de frecuencia de palabras disponibles.\n")
        }
      }
    })
    
  
    
    # Razon visualizations
    output$razon_histogram <- renderPlotly({
      req(filteredData())
      create_histogram(
        filteredData(), 
        bins = input$histogram_bins,
        title = paste("Distribución de", input$question_select),
        custom_theme = sectionTheme()
      )
    })
    
    output$razon_district_map <- renderLeaflet({
      req(filteredData(), geoData())
      create_district_map(
        filteredData(), 
        geoData(),
        custom_theme = sectionTheme()
      )
    })
    
    output$razon_district_bars <- renderPlotly({
      req(filteredData())
      district_means <- calculate_district_means(filteredData())
      
      plot_functions$bar(
        district_means,
        x = "district",
        y = "mean_value",
        title = "Promedio por Distrito",
        xlab = "Distrito",
        ylab = "Valor Promedio",
        orientation = input$bar_orientation,
        color_by = "district",
        custom_theme = sectionTheme()
      )
    })
    
    # Interval/Ordinal visualizations
    output$interval_histogram <- renderPlotly({
      req(filteredData())
      create_ordinal_histogram(
        filteredData(), 
        bins = input$histogram_bins, 
        title = paste("Distribución de", input$question_select),
        custom_theme = sectionTheme()
      )
    })
    
    output$interval_pie <- renderPlotly({
      req(filteredData())
      create_ordinal_pie(
        filteredData(),
        custom_theme = sectionTheme()
      )
    })
    
    output$interval_district_map <- renderLeaflet({
      req(filteredData(), geoData())
      create_ordinal_district_map(
        filteredData(),
        geoData(),
        selected_responses = NULL,  # Show means by default
        highlight_extremes = input$highlight_extremes,
        use_gradient = input$use_gradient,
        color_scale = input$color_scale,
        custom_theme = sectionTheme()
      )
    })
    
    output$interval_district_bars <- renderPlotly({
      req(filteredData())
      create_ordinal_bars(
        filteredData(),
        orientation = input$bar_orientation,
        custom_theme = sectionTheme()
      )
    })
    
    # Categorical visualizations
    output$categorical_bars <- renderPlotly({
      req(filteredData())
      create_category_bars(
        filteredData(),
        max_categories = 15,
        title = paste("Distribución de", input$question_select),
        custom_theme = sectionTheme()
      )
    })
    
    output$categorical_pie <- renderPlotly({
      req(filteredData())
      create_category_pie(
        filteredData(),
        max_categories = 8,
        custom_theme = sectionTheme(),
        highlight_max = TRUE
      )
    })
    
    output$categorical_stacked_bars <- renderPlotly({
      req(filteredData())
      create_category_stacked_bars(
        filteredData(),
        max_categories = 7,
        custom_theme = sectionTheme()
      )
    })
    
    # Binary visualizations
    output$binary_bars <- renderPlotly({
      req(filteredData())
      create_binary_bar(
        filteredData(),
        title = paste("Distribución de", input$question_select),
        custom_theme = sectionTheme()
      )
    })
    
    output$binary_pie <- renderPlotly({
      req(filteredData())
      create_binary_pie(
        filteredData(),
        title = paste("Distribución de", input$question_select),
        custom_theme = sectionTheme()
      )
    })
    
    output$binary_district_map <- renderLeaflet({
      req(filteredData(), geoData())
      create_binary_district_map(
        filteredData(), 
        geoData(),
        highlight_extremes = input$highlight_extremes,
        focus_on_true = TRUE,  # Default to showing "Yes" percentages
        custom_theme = sectionTheme()
      )
    })
    
    output$binary_district_bars <- renderPlotly({
      req(filteredData())
      create_binary_district_bars(
        filteredData(),
        orientation = input$bar_orientation,
        custom_theme = sectionTheme()
      )
    })
    
    output$binary_comparison <- renderPlotly({
      req(input$compare_questions, length(input$compare_questions) > 0)
      
      # Extract question IDs from checkboxgroup input
      question_ids <- sapply(input$compare_questions, function(q) {
        strsplit(q, " - ")[[1]][1]
      })
      
      # Prepare data for all selected questions
      binary_data_list <- prepare_multiple_binary(surveyData()$responses, question_ids, surveyData()$metadata)
      
      # Create the comparison visualization
      create_multiple_binary_comparison(
        binary_data_list,
        comparison_type = input$comparison_type,
        top_n = input$top_n,
        custom_theme = sectionTheme()
      )
    })
    
    # Nominal visualizations
    output$nominal_word_freq <- renderPlotly({
      req(filteredData())
      create_word_freq_bars(
        filteredData(),
        max_words = input$max_words,
        exclude_stopwords = input$exclude_stopwords,
        min_chars = input$min_chars,
        custom_theme = sectionTheme()
      )
    })
  })
}