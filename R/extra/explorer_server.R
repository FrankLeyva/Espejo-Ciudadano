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
      
      # Translate theme names to Spanish
      translated_themes <- sapply(themes_list, function(theme) {
        theme_metadata$translate_theme_name(theme)
      })
      
      # Create named vector for choices (display Spanish, keep English as values)
      theme_choices <- setNames(themes_list, translated_themes)
      
      updateSelectInput(session, "theme_filter", 
                       choices = c("Todos" = "all", theme_choices),
                       selected = "all")
    })
    
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
        
        # Translate subtheme names to Spanish
        translated_subthemes <- sapply(subthemes, function(subtheme) {
          theme_metadata$translate_subtheme_name(subtheme)
        })
        
        # Create named vector for choices (display Spanish, keep English as values)
        subtheme_choices <- setNames(subthemes, translated_subthemes)
        
        updateSelectInput(session, "subtheme_filter", 
                         choices = c("Todos" = "all", subtheme_choices),
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
  
  # Get current survey ID
  current_survey_id <- currentSurveyId()
  if (grepl("_V[0-9]+$", current_survey_id)) {
    current_survey_id <- gsub("_V[0-9]+$", "", current_survey_id)
  }
  
  # Filter out nominal questions and Internal theme
  metadata <- surveyData()$metadata
  filtered_questions <- all_questions[!(
    all_questions %in% metadata$variable[metadata$scale_type == "Nominal (Abierta)"] | 
    all_questions %in% metadata$variable[grepl("Other", metadata$label, fixed = TRUE)]
  )]
  
  # Get theme data for filtering Internal theme
  relevant_themes <- allThemes() %>% 
    filter(survey_id == current_survey_id)
  
  # Remove Internal theme questions
  internal_questions <- relevant_themes %>%
    filter(MainTheme == "Internal"| MainTheme == "Dashboard Context") %>%
    pull(variable)
  
  filtered_questions <- filtered_questions[!(filtered_questions %in% internal_questions)]
  
  # Create question labels for the filtered questions
  question_labels <- sapply(filtered_questions, function(q) {
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
  if (input$theme_filter != "all" || input$subtheme_filter != "all") {
    # Filter by theme and subtheme
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
    # Resto del código de búsqueda sin cambios...
  }
  
  # Get labels for filtered questions
  filtered_labels <- question_labels[names(question_labels) %in% filtered_questions]
  
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
        uiOutput(ns("summary_output"))
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
          downloadButton(ns("download_summary_csv"), "Descargar CSV")
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
        paste0("datos_", input$question_select, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Simple export of district data
        if (!is.null(filteredData())) {
          data <- filteredData()
          
          # Create a simplified table for download
          export_data <- data %>%
            select(district, value) %>%
            rename(Distrito = district, !!input$question_select := value)
          
          write.csv(export_data, file, row.names = FALSE)
        } else {
          # Empty CSV with a message if no data
          write.csv(data.frame(message = "No hay datos disponibles"), file, row.names = FALSE)
        }
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
    output$summary_output <- renderUI({
      req(filteredData(), selectedScaleType())
      
      scale_type <- selectedScaleType()
      data <- filteredData()
      
      # Define a common style for all value boxes
      vbox_style <- list(
        bg = "#2d2d2d", 
        fg = "white"
      )
      
      if (scale_type == "razon") {
        # Razon (ratio) summary
        
        # Calculate key metrics
        mean_val <- round(mean(data$value, na.rm = TRUE), 2)
        median_val <- median(data$value, na.rm = TRUE)
        range_val <- paste(min(data$value, na.rm = TRUE), "-", max(data$value, na.rm = TRUE))
        valid_responses <- sum(!is.na(data$value))
        
        # District stats table
        district_stats <- data %>%
          group_by(district) %>%
          summarise(
            Respuestas = n(),
            Media = round(mean(value, na.rm = TRUE), 2),
            Mediana = median(value, na.rm = TRUE),
            DE = round(sd(value, na.rm = TRUE), 2),
            Min = min(value, na.rm = TRUE),
            Max = max(value, na.rm = TRUE),
            .groups = 'drop'
          )
        
        # Create UI
        tagList(
          
          # Value boxes
          fluidRow(
            column(
              width = 3,
              value_box(
                title = "Media",
                value = mean_val,
                showcase = bsicons::bs_icon("calculator"),
                p("Promedio aritmético"),
                !!!vbox_style
              )
            ),
            column(
              width = 3,
              value_box(
                title = "Mediana",
                value = median_val,
                showcase = bsicons::bs_icon("bar-chart-line"),
                p("Valor central"),
                !!!vbox_style
              )
            ),
            column(
              width = 3,
              value_box(
                title = "Rango",
                value = range_val,
                showcase = bsicons::bs_icon("arrows-expand"),
                p("Valores mínimo y máximo"),
                !!!vbox_style
              )
            ),
            column(
              width = 3,
              value_box(
                title = "Respuestas Válidas",
                value = paste0(valid_responses),
                showcase = bsicons::bs_icon("people-fill"),
                !!!vbox_style
              )
            )
          ),
          
          # District stats table
          h4("Estadísticas por Distrito", class = "mt-4 mb-3"),
          DT::renderDataTable({
            DT::datatable(
              district_stats,
              options = list(
                dom = 't',
                ordering = TRUE,
                paging = FALSE,
                searching = FALSE,
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                )
              ),
              rownames = FALSE,
              class = "compact stripe hover"
            )
          })
        )
        
      } else if (scale_type %in% c("intervalo", "ordinal")) {
        # Interval/Ordinal summary
        type_name <- ifelse(scale_type == "intervalo", "Intervalo", "Ordinales")
        
        # Get numeric values
        numeric_values <- get_numeric_values(data)
        
        # Calculate key metrics
        mean_val <- round(mean(numeric_values, na.rm = TRUE), 2)
        median_val <- median(numeric_values, na.rm = TRUE)
        range_val <- paste(min(numeric_values, na.rm = TRUE), "-", max(numeric_values, na.rm = TRUE))
        valid_responses <- sum(!is.na(numeric_values))
        
        # Prepare frequency table
        freq_table <- table(data$value)
        freq_df <- data.frame(
          Valor = names(freq_table),
          Frecuencia = as.vector(freq_table),
          Porcentaje = round(100 * as.vector(freq_table) / sum(freq_table), 2)
        )
        
        # Add labels if available
        if (!is.null(attr(data, "value_labels"))) {
          value_labels <- attr(data, "value_labels")
          freq_df$Etiqueta <- sapply(as.character(freq_df$Valor), function(val) {
            if (val %in% names(value_labels)) {
              value_labels[val]
            } else {
              NA
            }
          })
          
          # Reorder columns to put label after value
          freq_df <- freq_df[, c("Valor", "Etiqueta", "Frecuencia", "Porcentaje")]
        }
        
        if (scale_type == "ordinal") {
          # For ordinal, add most/least popular
          freq_df_sorted <- freq_df[order(-freq_df$Frecuencia),]
          most_popular <- paste0(freq_df_sorted$Valor[1], 
                              ifelse(!is.null(attr(data, "value_labels")) && 
                                    !is.na(freq_df_sorted$Etiqueta[1]), 
                                  paste0(" (", freq_df_sorted$Etiqueta[1], ")"), ""))
          least_popular <- paste0(freq_df_sorted$Valor[nrow(freq_df_sorted)], 
                                ifelse(!is.null(attr(data, "value_labels")) && 
                                      !is.na(freq_df_sorted$Etiqueta[nrow(freq_df_sorted)]), 
                                    paste0(" (", freq_df_sorted$Etiqueta[nrow(freq_df_sorted)], ")"), ""))
          
          # Create UI for ordinal data
          tagList(
            
            # Value boxes
            fluidRow(
              column(
                width = 3,
                value_box(
                  title = "Respuesta más popular",
                  value = most_popular,
                  showcase = bsicons::bs_icon("trophy"),
                  p(paste0(freq_df_sorted$Frecuencia[1], " respuestas")),
                  !!!vbox_style
                )
              ),
              column(
                width = 3,
                value_box(
                  title = "Respuesta menos popular",
                  value = least_popular,
                  showcase = bsicons::bs_icon("arrow-down"),
                  p(paste0(freq_df_sorted$Frecuencia[nrow(freq_df_sorted)], " respuestas")),
                  !!!vbox_style
                )
              ),
              column(
                width = 3,
                value_box(
                  title = "Mediana",
                  value = median_val,
                  showcase = bsicons::bs_icon("bar-chart-line"),
                  p("Valor central"),
                  !!!vbox_style
                )
              ),
              column(
                width = 3,
                value_box(
                  title = "Respuestas Válidas",
                  value = paste0(valid_responses),
                  showcase = bsicons::bs_icon("people-fill"),
                  !!!vbox_style
                )
              )
            ),
            
            # Category distribution table
            h4("Distribución de Categorías", class = "mt-4 mb-3"),
            DT::renderDataTable({
              DT::datatable(
                freq_df,
                options = list(
                  dom = 't',
                  ordering = TRUE,
                  paging = FALSE,
                  searching = FALSE,
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all")
                  )
                ),
                rownames = FALSE,
                class = "compact stripe hover"
              )
            }),
            
            # District stats if available
            h4("Estadísticas por Distrito", class = "mt-4 mb-3"),
            DT::renderDataTable({
              district_stats <- data %>%
                group_by(district) %>%
                summarise(
                  Respuestas = n(),
                  Media = round(mean(get_numeric_values(.), na.rm = TRUE), 2),
                  Mediana = median(get_numeric_values(.), na.rm = TRUE),
                  DE = round(sd(get_numeric_values(.), na.rm = TRUE), 2),
                  Min = min(get_numeric_values(.), na.rm = TRUE),
                  Max = max(get_numeric_values(.), na.rm = TRUE),
                  .groups = 'drop'
                )
              
              DT::datatable(
                district_stats,
                options = list(
                  dom = 't',
                  ordering = TRUE,
                  paging = FALSE,
                  searching = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all")
                  )
                ),
                rownames = FALSE,
                class = "compact stripe hover"
              )
            })
          )
        } else {
          # Create UI for interval data (which places more emphasis on numeric stats)
          tagList(
           
            # Value boxes
            fluidRow(
              column(
                width = 3,
                value_box(
                  title = "Media",
                  value = mean_val,
                  showcase = bsicons::bs_icon("calculator"),
                  p("Promedio aritmético"),
                  !!!vbox_style
                )
              ),
              column(
                width = 3,
                value_box(
                  title = "Mediana",
                  value = median_val,
                  showcase = bsicons::bs_icon("bar-chart-line"),
                  p("Valor central"),
                  !!!vbox_style
                )
              ),
              column(
                width = 3,
                value_box(
                  title = "Rango",
                  value = range_val,
                  showcase = bsicons::bs_icon("arrows-expand"),
                  p("Valores mínimo y máximo"),
                  !!!vbox_style
                )
              ),
              column(
                width = 3,
                value_box(
                  title = "Respuestas Válidas",
                  value = paste0(valid_responses),
                  showcase = bsicons::bs_icon("people-fill"),
                  !!!vbox_style
                )
              )
            ),
            
            # Category distribution table
            h4("Distribución de Categorías", class = "mt-4 mb-3"),
            DT::renderDataTable({
              DT::datatable(
                freq_df,
                options = list(
                  dom = 't',
                  ordering = TRUE,
                  paging = FALSE,
                  searching = FALSE,
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all")
                  )
                ),
                rownames = FALSE,
                class = "compact stripe hover"
              )
            }),
            
            # District stats
            h4("Estadísticas por Distrito", class = "mt-4 mb-3"),
            DT::renderDataTable({
              district_stats <- data %>%
                group_by(district) %>%
                summarise(
                  Respuestas = n(),
                  Media = round(mean(get_numeric_values(.), na.rm = TRUE), 2),
                  Mediana = median(get_numeric_values(.), na.rm = TRUE),
                  DE = round(sd(get_numeric_values(.), na.rm = TRUE), 2),
                  Min = min(get_numeric_values(.), na.rm = TRUE),
                  Max = max(get_numeric_values(.), na.rm = TRUE),
                  .groups = 'drop'
                )
              
              DT::datatable(
                district_stats,
                options = list(
                  dom = 't',
                  ordering = TRUE,
                  paging = FALSE,
                  searching = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all")
                  )
                ),
                rownames = FALSE,
                class = "compact stripe hover"
              )
            })
          )
        }
        
      } else if (scale_type == "categorico") {
        # Categorical summary
        
        # Prepare frequency table
        freq_table <- table(data$value)
        freq_df <- data.frame(
          Categoría = names(freq_table),
          Frecuencia = as.vector(freq_table),
          Porcentaje = round(100 * as.vector(freq_table) / sum(freq_table), 2)
        )
        
        # Sort by frequency
        freq_df <- freq_df[order(-freq_df$Frecuencia), ]
        
        # Calculate key metrics
        most_popular <- freq_df$Categoría[1]
        least_popular <- freq_df$Categoría[nrow(freq_df)]
        valid_responses <- sum(!is.na(data$value))
        
        # District breakdown - most common category per district
        district_breakdown <- data %>%
          group_by(district, value) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(district) %>%
          mutate(percentage = round(100 * count / sum(count), 1)) %>%
          slice_max(order_by = count, n = 1) %>%
          arrange(district) %>%
          select(
            Distrito = district, 
            Respuestas = count, 
            `Categoría más frecuente` = value, 
            Porcentaje = percentage
          )
        
        # Create UI
        tagList(
     
          # Value boxes
          fluidRow(
            column(
              width = 3,
              value_box(
                title = "Categoría más frecuente",
                value = most_popular,
                showcase = bsicons::bs_icon("trophy"),
                p(paste0(freq_df$Frecuencia[1], " respuestas")),
                !!!vbox_style
              )
            ),
            column(
              width = 3,
              value_box(
                title = "Categoría menos frecuente",
                value = least_popular,
                showcase = bsicons::bs_icon("arrow-down"),
                p(paste0(freq_df$Frecuencia[nrow(freq_df)], " respuestas")),
                !!!vbox_style
              )
            ),
            column(
              width = 3,
              value_box(
                title = "Total Categorías",
                value = nrow(freq_df),
                showcase = bsicons::bs_icon("list-check"),
                p("Categorías distintas"),
                !!!vbox_style
              )
            ),
            column(
              width = 3,
              value_box(
                title = "Respuestas Válidas",
                value = paste0(valid_responses),
                showcase = bsicons::bs_icon("people-fill"),
                !!!vbox_style
              )
            )
          ),
          
          # Category distribution table
          h4("Distribución de Categorías", class = "mt-4 mb-3"),
          DT::renderDataTable({
            DT::datatable(
              freq_df,
              options = list(
                dom = 't',
                ordering = TRUE,
                paging = FALSE,
                searching = FALSE,
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                )
              ),
              rownames = FALSE,
              class = "compact stripe hover"
            )
          }),
          
          # District stats table
          h4("Estadísticas por Distrito", class = "mt-4 mb-3"),
          DT::renderDataTable({
            DT::datatable(
              district_breakdown,
              options = list(
                dom = 't',
                ordering = TRUE,
                paging = FALSE,
                searching = FALSE,
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                )
              ),
              rownames = FALSE,
              class = "compact stripe hover"
            )
          })
        )
        
      } else if (scale_type == "binaria") {
        # Binary summary
        
        # Get binary counts
        total_responses <- nrow(data)
        true_count <- sum(data$binary_value, na.rm = TRUE)
        false_count <- sum(!data$binary_value, na.rm = TRUE)
        missing_count <- total_responses - true_count - false_count
        valid_responses <- total_responses - missing_count
        
        # Get binary labels
        labels <- get_binary_labels(data)
        true_label <- labels$true_label
        false_label <- labels$false_label
        
        # Calculate percentages
        true_percent <- round(100 * true_count / valid_responses, 2)
        false_percent <- round(100 * false_count / valid_responses, 2)
        
        # District breakdown
        district_breakdown <- data %>%
          group_by(district) %>%
          summarise(
            Total = n(),
            `Respuestas Sí` = sum(binary_value, na.rm = TRUE),
            `Respuestas No` = sum(!binary_value, na.rm = TRUE),
            `% Sí` = round(100 * mean(binary_value, na.rm = TRUE), 2),
            .groups = 'drop'
          )
        
        # Create UI
        tagList(
        
          # Value boxes
          fluidRow(
            column(
              width = 4,
              value_box(
                title = true_label,
                value = paste0(true_count, " (", true_percent, "%)"),
                showcase = bsicons::bs_icon("check-circle-fill"),
                p("Respuestas positivas"),
                !!!vbox_style
              )
            ),
            column(
              width = 4,
              value_box(
                title = false_label,
                value = paste0(false_count, " (", false_percent, "%)"),
                showcase = bsicons::bs_icon("x-circle-fill"),
                p("Respuestas negativas"),
                !!!vbox_style
              )
            ),
            column(
              width = 4,
              value_box(
                title = "Respuestas Válidas",
                value = paste0(valid_responses),
                showcase = bsicons::bs_icon("people-fill"),
                !!!vbox_style
              )
            )
          ),
          
          # District stats table
          h4("Estadísticas por Distrito", class = "mt-4 mb-3"),
          DT::renderDataTable({
            DT::datatable(
              district_breakdown,
              options = list(
                dom = 't',
                ordering = TRUE,
                paging = FALSE,
                searching = FALSE,
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                )
              ),
              rownames = FALSE,
              class = "compact stripe hover"
            )
          })
        )
        
      } else if (scale_type == "nominal") {
        # Nominal summary
        
        # Word frequency data
        word_freq <- attr(data, "word_freq")
        if (!is.null(word_freq) && nrow(word_freq) > 0) {
          # Calculate metrics
          response_lengths <- sapply(strsplit(data$preprocessed_text, "\\s+"), length)
          avg_words <- round(mean(response_lengths, na.rm = TRUE), 2)
          median_words <- median(response_lengths, na.rm = TRUE)
          min_words <- min(response_lengths, na.rm = TRUE)
          max_words <- max(response_lengths, na.rm = TRUE)
          
          # Create UI
          tagList(
            
            # Value boxes
            fluidRow(
              column(
                width = 3,
                value_box(
                  title = "Palabra más frecuente",
                  value = word_freq$word[1],
                  showcase = bsicons::bs_icon("chat-quote-fill"),
                  p(paste0(word_freq$freq[1], " apariciones")),
                  !!!vbox_style
                )
              ),
              column(
                width = 3,
                value_box(
                  title = "Promedio de palabras",
                  value = avg_words,
                  showcase = bsicons::bs_icon("calculator"),
                  p("Palabras por respuesta"),
                  !!!vbox_style
                )
              ),
              column(
                width = 3,
                value_box(
                  title = "Respuesta más larga",
                  value = max_words,
                  showcase = bsicons::bs_icon("chat-right-text-fill"),
                  p("Palabras"),
                  !!!vbox_style
                )
              ),
              column(
                width = 3,
                value_box(
                  title = "Respuestas",
                  value = length(response_lengths),
                  showcase = bsicons::bs_icon("people-fill"),
                  p("Total respuestas"),
                  !!!vbox_style
                )
              )
            ),
            
            # Word frequency table
            h4("Palabras más frecuentes", class = "mt-4 mb-3"),
            DT::renderDataTable({
              # Limit to top 15 words
              top_words <- head(word_freq, 15)
              
              DT::datatable(
                top_words,
                options = list(
                  dom = 't',
                  ordering = TRUE,
                  paging = FALSE,
                  searching = FALSE,
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all")
                  )
                ),
                rownames = FALSE,
                class = "compact stripe hover"
              )
            }),
            
            # Additional statistics
            h4("Estadísticas de longitud de respuesta", class = "mt-4 mb-3"),
            fluidRow(
              column(
                width = 12,
                div(
                  class = "table-responsive",
                  tags$table(
                    class = "table table-sm table-bordered",
                    tags$thead(
                      tags$tr(
                        tags$th("Estadística", style = "text-align: center;"),
                        tags$th("Valor", style = "text-align: center;")
                      )
                    ),
                    tags$tbody(
                      tags$tr(
                        tags$td("Promedio de palabras por respuesta"),
                        tags$td(avg_words, style = "text-align: center;")
                      ),
                      tags$tr(
                        tags$td("Mediana de palabras por respuesta"),
                        tags$td(median_words, style = "text-align: center;")
                      ),
                      tags$tr(
                        tags$td("Respuesta más corta (palabras)"),
                        tags$td(min_words, style = "text-align: center;")
                      ),
                      tags$tr(
                        tags$td("Respuesta más larga (palabras)"),
                        tags$td(max_words, style = "text-align: center;")
                      )
                    )
                  )
                )
              )
            )
          )
        } else {
          # No word frequency data available
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            "No hay datos de frecuencia de palabras disponibles para esta pregunta."
          )
        }
      } else {
        # Default case - unknown type
        div(
          class = "alert alert-info",
          icon("info-circle"),
          "Tipo de datos no soportado o desconocido para visualización de resumen."
        )
      }
    })
    
  
    
    # Razon visualizations
    output$razon_histogram <- renderPlotly({
      req(filteredData())
      create_histogram(
        filteredData(), 
        bins = input$histogram_bins,
        title = get_question_label(input$question_select, surveyData()$metadata),
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
        title = get_question_label(input$question_select, surveyData()$metadata),
        custom_theme = sectionTheme()
      )
    })
    
    output$interval_pie <- renderPlotly({
      req(filteredData())
      create_ordinal_pie(
        title = get_question_label(input$question_select, surveyData()$metadata),
        filteredData(),
        palette='sequential',
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
        title = get_question_label(input$question_select, surveyData()$metadata),
        custom_theme = sectionTheme()
      )
    })
    
    output$categorical_pie <- renderPlotly({
      req(filteredData())
      create_category_pie(
        tilte= get_question_label(input$question_select, surveyData()$metadata),
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
        title = get_question_label(input$question_select, surveyData()$metadata),
        custom_theme = sectionTheme()
      )
    })
    
    output$binary_pie <- renderPlotly({
      req(filteredData())
      create_binary_pie(
        filteredData(),
        title = get_question_label(input$question_select, surveyData()$metadata),
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