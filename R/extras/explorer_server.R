explorerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track active tab and reset functionality
    activeTab <- reactive({
      if (is.null(input$search_tabs) || length(input$search_tabs) == 0) {
        return(NULL)
      }
      input$search_tabs
    })
    
    observeEvent(activeTab(), {
      current_tab <- activeTab()
      if (is.null(current_tab) || length(current_tab) == 0) return()
      
      if (current_tab == "theme_search") {
        # Reset text search tab
        updateTextInput(session, "search_query", value = "")
        updateSelectInput(session, "question_select_search", 
                         choices = c("Haga clic en 'Buscar' para ver resultados" = ""),
                         selected = "")
      } else if (current_tab == "text_search") {
        # Reset theme search tab
        updateSelectInput(session, "theme_filter", selected = "")
        updateSelectInput(session, "subtheme_filter", 
                         choices = c("Primero seleccione un tema" = ""),
                         selected = "")
        updateSelectInput(session, "question_select_theme", 
                         choices = c("Primero seleccione un subtema" = ""),
                         selected = "")
      }
      
      # Reset common elements
      updateSelectInput(session, "district_filter", choices = NULL, selected = character(0))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Load all survey data and themes at startup
    allSurveyData <- reactive({
      # Load both surveys for both years
      all_data <- list()
      
      for (year in c("2023", "2024")) {
        for (type in c("PER", "PAR")) {
          survey_id <- paste0(type, "_", year)
          
          tryCatch({
            data <- load_survey_data(survey_id)
            if (!is.null(data)) {
              all_data[[survey_id]] <- data
            }
          }, error = function(e) {
            message(paste("Could not load", survey_id, ":", e$message))
          })
        }
      }
      
      return(all_data)
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
    
    # Load and process all theme metadata
    allThemes <- reactive({
      tryCatch({
        themes_data <- theme_metadata$load_thematic_classifications()
        
        # Filter out Internal and Dashboard Context themes
        themes_data <- themes_data %>%
          filter(!MainTheme %in% c("Internal", "Dashboard Context"))
        
        return(themes_data)
      }, error = function(e) {
        showNotification(
          paste("Error al cargar metadata de temas:", e$message), 
          type = "error",
          duration = 10
        )
        data.frame(
          variable = character(0),
          survey_id = character(0),
          MainTheme = character(0),
          Subtheme = character(0),
          stringsAsFactors = FALSE
        )
      })
    })
    
    # Create a comprehensive question catalog with theme info
    questionCatalog <- reactive({
      req(allSurveyData(), allThemes())
      
      catalog <- data.frame()
      themes_data <- allThemes()
      survey_data <- allSurveyData()
      
      for (survey_id in names(survey_data)) {
        if (is.null(survey_data[[survey_id]]$metadata)) next
        
        # Clean survey_id for matching with themes
        clean_survey_id <- gsub("_V[0-9]+$", "", survey_id)
        
        # Get questions for this survey
        metadata <- survey_data[[survey_id]]$metadata
        
        # Get theme info for this survey
        survey_themes <- themes_data %>%
          filter(survey_id == clean_survey_id)
        
        # Classify questions by scale type
        classifications <- tryCatch({
          classify_questions(metadata)
        }, error = function(e) {
          list(
            razon = character(0),
            intervalo = character(0),
            ordinal = character(0),
            categorico = character(0),
            binaria = character(0),
            nominal = character(0)
          )
        })
        
        # Determine scale type for each question
        all_questions <- unique(unlist(classifications))
        
        for (question_id in all_questions) {
          # Skip nominal questions and "Other" questions
          if (question_id %in% classifications$nominal ||
              question_id %in% metadata$variable[grepl("Other", metadata$label, fixed = TRUE)]) {
            next
          }
          
          # Find theme info
          theme_info <- survey_themes %>%
            filter(variable == question_id) %>%
            slice(1)
          
          if (nrow(theme_info) == 0) next
          
          # Get question label
          question_meta <- metadata %>%
            filter(variable == question_id) %>%
            slice(1)
          
          if (nrow(question_meta) == 0) next
          
          # Determine scale type
          scale_type <- "unknown"
          if (question_id %in% classifications$razon) scale_type <- "razon"
          else if (question_id %in% classifications$intervalo) scale_type <- "intervalo"
          else if (question_id %in% classifications$ordinal) scale_type <- "ordinal"
          else if (question_id %in% classifications$categorico) scale_type <- "categorico"
          else if (question_id %in% classifications$binaria) scale_type <- "binaria"
          
          # Add to catalog
          catalog <- rbind(catalog, data.frame(
            question_id = question_id,
            survey_id = survey_id,
            clean_survey_id = clean_survey_id,
            survey_type = substr(survey_id, 1, 3),
            survey_year = substr(survey_id, 5, 8),
            main_theme = theme_info$MainTheme,
            subtheme = theme_info$Subtheme,
            question_label = question_meta$label,
            scale_type = scale_type,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      return(catalog)
    })
    
    # Update theme dropdown (only for theme search tab)
    observe({
      req(questionCatalog())
      
      catalog <- questionCatalog()
      unique_themes <- unique(catalog$main_theme)
      
      # Translate theme names to Spanish
      translated_themes <- sapply(unique_themes, function(theme) {
        theme_metadata$translate_theme_name(theme)
      })
      
      # Create named vector for choices (display Spanish, keep English as values)
      theme_choices <- setNames(unique_themes, translated_themes)
      
      # Sort alphabetically by Spanish names
      theme_choices <- theme_choices[order(names(theme_choices))]
      
      updateSelectInput(session, "theme_filter", 
                       choices = c("Seleccione un tema..." = "", theme_choices),
                       selected = "")
    })
    
    # Update subtheme dropdown based on selected theme
    observe({
      req(input$theme_filter, input$theme_filter != "", questionCatalog())
      
      catalog <- questionCatalog()
      
      # Get subthemes for the selected theme
      subthemes <- catalog %>%
        filter(main_theme == input$theme_filter) %>%
        pull(subtheme) %>%
        unique() %>%
        sort()
      
      # Translate subtheme names to Spanish
      translated_subthemes <- sapply(subthemes, function(subtheme) {
        theme_metadata$translate_subtheme_name(subtheme)
      })
      
      # Create named vector for choices
      subtheme_choices <- setNames(subthemes, translated_subthemes)
      
      updateSelectInput(session, "subtheme_filter", 
                       choices = c("Seleccione un subtema..." = "", subtheme_choices),
                       selected = "")
    })
    
    # Reset subtheme when theme changes
    observeEvent(input$theme_filter, {
      if (input$theme_filter == "") {
        updateSelectInput(session, "subtheme_filter", 
                         choices = c("Primero seleccione un tema" = ""),
                         selected = "")
        updateSelectInput(session, "question_select_theme", 
                         choices = c("Primero seleccione un subtema" = ""),
                         selected = "")
      }
    })
    
    # Update question dropdown for theme search based on theme and subtheme
    observe({
      req(input$theme_filter, input$theme_filter != "",
          input$subtheme_filter, input$subtheme_filter != "",
          questionCatalog())
      
      catalog <- questionCatalog()
      
      # Filter questions by theme and subtheme
      filtered_questions <- catalog %>%
        filter(main_theme == input$theme_filter,
               subtheme == input$subtheme_filter)
      
      if (nrow(filtered_questions) == 0) {
        updateSelectInput(
          session,
          "question_select_theme",
          choices = c("No hay preguntas en este subtema" = ""),
          selected = ""
        )
        return()
      }
      
      # Create question choices with labels
      question_choices <- setNames(
        paste0(filtered_questions$question_id, "|", filtered_questions$survey_id),
        paste0(
          filtered_questions$question_id, 
          " (", filtered_questions$survey_type, " ", filtered_questions$survey_year, "): ",
          ifelse(nchar(filtered_questions$question_label) > 60,
                 paste0(substr(filtered_questions$question_label, 1, 57), "..."),
                 filtered_questions$question_label)
        )
      )
      
      # Sort alphabetically
      question_choices <- question_choices[order(names(question_choices))]
      
      updateSelectInput(
        session,
        "question_select_theme",
        choices = c("Seleccione una pregunta..." = "", question_choices),
        selected = ""
      )
    })
    
    # Reset question when subtheme changes
    observeEvent(input$subtheme_filter, {
      if (input$subtheme_filter == "") {
        updateSelectInput(session, "question_select_theme", 
                         choices = c("Primero seleccione un subtema" = ""),
                         selected = "")
      }
    })
    
    # Text search functionality
    searchResults <- eventReactive(input$search_button, {
      req(input$search_query, nchar(trimws(input$search_query)) > 0, questionCatalog())
      
      search_query <- trimws(input$search_query)
      if (search_query == "") return(data.frame())
      
      catalog <- questionCatalog()
      
      # Split search terms
      search_terms <- tolower(trimws(strsplit(search_query, "\\s+")[[1]]))
      
      # Filter questions that match any search term
      filtered_questions <- catalog %>%
        filter(
          Reduce(`|`, lapply(search_terms, function(term) {
            grepl(term, tolower(question_label), fixed = TRUE) |
            grepl(term, tolower(question_id), fixed = TRUE)
          }))
        ) %>%
        arrange(survey_type, survey_year, question_id)
      
      return(filtered_questions)
    })
    
    # Update search results info
    output$search_results_info <- renderUI({
      if (input$search_button == 0) return(NULL)
      
      req(input$search_query)
      search_query <- trimws(input$search_query)
      
      if (search_query == "") {
        return(div(
          class = "search-results-info",
          "Ingrese términos de búsqueda y haga clic en 'Buscar'"
        ))
      }
      
      results <- searchResults()
      
      if (nrow(results) == 0) {
        return(div(
          class = "search-results-info",
          paste("No se encontraron preguntas que contengan:", search_query)
        ))
      }
      
      # Count by survey
      survey_counts <- results %>%
        group_by(survey_type, survey_year) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(survey_label = paste0(survey_type, " ", survey_year, ": ", count)) %>%
        pull(survey_label)
      
      return(div(
        class = "search-results-info",
        paste0("Se encontraron ", nrow(results), " preguntas"),
        br(),
        paste(survey_counts, collapse = ", ")
      ))
    })
    
    # Update search results dropdown
    observe({
      req(input$search_button > 0)
      
      results <- searchResults()
      
      if (nrow(results) == 0) {
        updateSelectInput(
          session,
          "question_select_search",
          choices = c("No se encontraron resultados" = ""),
          selected = ""
        )
        return()
      }
      
      # Create question choices with labels
      question_choices <- setNames(
        paste0(results$question_id, "|", results$survey_id),
        paste0(
          results$question_id, 
          " (", results$survey_type, " ", results$survey_year, "): ",
          ifelse(nchar(results$question_label) > 60,
                 paste0(substr(results$question_label, 1, 57), "..."),
                 results$question_label)
        )
      )
      
      updateSelectInput(
        session,
        "question_select_search",
        choices = c("Seleccione una pregunta..." = "", question_choices),
        selected = ""
      )
    })
    
    # Unified question selection logic
    selectedQuestionInfo <- reactive({
      current_tab <- activeTab()
      
      if (is.null(current_tab) || length(current_tab) == 0) return(NULL)
      
      question_value <- NULL
      
      if (current_tab == "theme_search") {
        if (!is.null(input$question_select_theme) && 
            length(input$question_select_theme) > 0 &&
            input$question_select_theme != "" && 
            !grepl("Primero|Seleccione|No hay", input$question_select_theme)) {
          question_value <- input$question_select_theme
        }
      } else if (current_tab == "text_search") {
        if (!is.null(input$question_select_search) && 
            length(input$question_select_search) > 0 &&
            input$question_select_search != "" && 
            !grepl("Haga clic|Seleccione|No se encontraron", input$question_select_search)) {
          question_value <- input$question_select_search
        }
      }
      
      if (is.null(question_value) || length(question_value) == 0) return(NULL)
      
      # Parse the question_id|survey_id format
      parts <- strsplit(question_value, "\\|")[[1]]
      if (length(parts) != 2) return(NULL)
      
      list(
        question_id = parts[1],
        survey_id = parts[2]
      )
    })
    
    # Get current survey data based on selected question
    currentSurveyData <- reactive({
      req(selectedQuestionInfo(), allSurveyData())
      
      survey_id <- selectedQuestionInfo()$survey_id
      survey_data <- allSurveyData()[[survey_id]]
      
      if (is.null(survey_data)) {
        showNotification("No se pudo cargar los datos de la encuesta", type = "error")
        return(NULL)
      }
      
      return(survey_data)
    })
    
    # Determine scale type for the selected question
    selectedScaleType <- reactive({
      req(selectedQuestionInfo(), currentSurveyData())
      
      question_id <- selectedQuestionInfo()$question_id
      
      # Classify questions for this survey
      classifications <- tryCatch({
        classify_questions(currentSurveyData()$metadata)
      }, error = function(e) {
        return(list())
      })
      
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
        return("unknown")
      }
    })
    
    # Display data source info
    output$data_source_info <- renderText({
      req(selectedQuestionInfo())
      
      survey_id <- selectedQuestionInfo()$survey_id
      survey_type <- ifelse(startsWith(survey_id, "PER"), 
                           "Encuesta de Percepción Ciudadana", 
                           "Encuesta de Participación Ciudadana y Buen Gobierno")
      survey_year <- substr(survey_id, nchar(survey_id) - 3, nchar(survey_id))
      
      paste(survey_type, survey_year)
    })
    
    # Display question information
    output$question_text <- renderUI({
      req(selectedQuestionInfo(), currentSurveyData())
      
      question_id <- selectedQuestionInfo()$question_id
      label <- get_question_label(question_id, currentSurveyData()$metadata)
      scale_type <- selectedScaleType()
      
      # Get theme info for styling
      catalog <- questionCatalog()
      question_info <- catalog %>%
        filter(question_id == !!question_id, survey_id == !!selectedQuestionInfo()$survey_id) %>%
        slice(1)
      
      # Determine theme class for styling
      theme_class <- "theme-bienestar"  # default
      if (nrow(question_info) > 0) {
        main_theme <- question_info$main_theme
        if (main_theme == "Urban Mobility & Environment") theme_class <- "theme-movilidad"
        else if (main_theme == "Governance & Civic Engagement") theme_class <- "theme-gobierno"
        else if (main_theme == "Public Services") theme_class <- "theme-infraestructura"
        else if (main_theme == "Community Participation") theme_class <- "theme-participacion"
      }
      
      # Get Spanish translations
      theme_spanish <- ifelse(nrow(question_info) > 0, 
                             theme_metadata$translate_theme_name(question_info$main_theme), 
                             "")
      subtheme_spanish <- ifelse(nrow(question_info) > 0, 
                                theme_metadata$translate_subtheme_name(question_info$subtheme), 
                                "")
      
      scale_type_spanish <- switch(scale_type,
                                  "razon" = "Razón (numérica continua)",
                                  "intervalo" = "Intervalo (numérica con rangos)",
                                  "ordinal" = "Ordinal (categorías ordenadas)",
                                  "categorico" = "Categórica (categorías sin orden)",
                                  "binaria" = "Binaria (sí/no)",
                                  "nominal" = "Nominal (texto abierto)",
                                  "Desconocido")
      
      HTML(paste0(
        "<strong>ID:</strong> ", question_id, "<br/>",
        "<strong>Pregunta:</strong> ", label, "<br/>",
        "<strong>Tipo de datos:</strong> ", scale_type_spanish, "<br/>",
        "<strong>Tema:</strong> ", theme_spanish, 
        "<span class='theme-indicator ", theme_class, "'>", theme_spanish, "</span><br/>",
        "<strong>Subtema:</strong> ", subtheme_spanish
      ))
    })
    
    # Generate visualization title
    output$viz_title <- renderText({
      req(selectedQuestionInfo())
      
      if (is.null(selectedQuestionInfo())) {
        return("Seleccione una pregunta para visualizar")
      }
      
      "Resultados de la Visualización"
    })
    
    # Display visualization options based on scale type (rest of the server code remains the same)
    output$viz_options <- renderUI({
      req(selectedScaleType())
      
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
        # Default case or unknown
        selectInput(
          ns("viz_type"),
          "Tipo de Visualización:",
          choices = c("Resumen Estadístico" = "summary"),
          selected = "summary"
        )
      }
    })
    
    # The rest of the server logic (preparedData, filteredData, outputs, etc.) 
    # remains the same as in the original code...
    # [Include all the remaining reactive expressions and output handlers from the original]
    
    # Prepare data for the selected question
    preparedData <- reactive({
      req(selectedQuestionInfo(), currentSurveyData(), selectedScaleType())
      
      question_id <- selectedQuestionInfo()$question_id
      scale_type <- selectedScaleType()
      survey_data <- currentSurveyData()
      
      # Prepare data based on scale type
      if (scale_type == "razon") {
        prepare_razon_data(survey_data$responses, question_id, survey_data$metadata)
      } else if (scale_type == "intervalo") {
        prepare_interval_data(survey_data$responses, question_id, survey_data$metadata)
      } else if (scale_type == "ordinal") {
        prepare_ordinal_data(survey_data$responses, question_id, survey_data$metadata)
      } else if (scale_type == "categorico") {
        prepare_categorical_data(survey_data$responses, question_id, survey_data$metadata)
      } else if (scale_type == "binaria") {
        prepare_binary_data(survey_data$responses, question_id, survey_data$metadata)
      } else if (scale_type == "nominal") {
        prepare_nominal_data(survey_data$responses, question_id, survey_data$metadata)
      } else {
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
      
      return(data)
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
          p("Seleccione otras preguntas binarias del mismo tema para comparar:"),
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
    
# Custom visualization options
output$custom_viz_options <- renderUI({
  req(input$viz_type, selectedScaleType())
  
  scale_type <- selectedScaleType()
  viz_type <- input$viz_type
  
  if (scale_type == "binaria" && viz_type == "district_map") {
    tagList(
      checkboxInput(
        ns("highlight_extremes"),
        "Resaltar valores extremos",
        value = TRUE
      )
    )
  } else if (scale_type == "binaria" && viz_type == "district_bars") {
    radioButtons(
      ns("bar_orientation"),
      "Orientación:",
      choices = c(
        "Vertical" = "v",
        "Horizontal" = "h"
      ),
      selected = "v"
    )
  } else {
    div()
  }
})

# Get theme for styling
sectionTheme <- reactive({
  get_section_theme("extras")
})

# Generate the appropriate visualization
output$visualization <- renderUI({
  req(selectedQuestionInfo(), input$viz_type, selectedScaleType())
  
  scale_type <- selectedScaleType()
  viz_type <- input$viz_type
  
  if (viz_type == "summary") {
    uiOutput(ns("summary_output"))
  } else if (scale_type == "binaria") {
    if (viz_type == "bars") {
      plotlyOutput(ns("binary_bars"), height = "500px")
    } else if (viz_type == "pie") {
      plotlyOutput(ns("binary_pie"), height = "500px")
    } else if (viz_type == "district_map") {
      leafletOutput(ns("binary_district_map"), height = "600px")
    } else if (viz_type == "district_bars") {
      plotlyOutput(ns("binary_district_bars"), height = "500px")
    }
  }
})

# Download options
output$download_options <- renderUI({
  req(selectedQuestionInfo(), input$viz_type)
  
  viz_type <- input$viz_type
  
  if (viz_type == "summary") {
    div(
      class = "download-btn",
      downloadButton(ns("download_summary_csv"), "Descargar CSV", class = "btn btn-outline-primary btn-sm")
    )
  } else {
    div()
  }
})

# Download handler
output$download_summary_csv <- downloadHandler(
  filename = function() {
    question_id <- selectedQuestionInfo()$question_id
    survey_id <- selectedQuestionInfo()$survey_id
    paste0("datos_", question_id, "_", survey_id, "_", Sys.Date(), ".csv")
  },
  content = function(file) {
    if (!is.null(filteredData())) {
      data <- filteredData()
      
      # Create a simplified table for download - FIXED the !! issue
      if ("binary_value" %in% names(data)) {
        export_data <- data %>%
          select(district, binary_value)
        
        # Safe column renaming
        colnames(export_data) <- c("Distrito", selectedQuestionInfo()$question_id)
      } else {
        export_data <- data %>%
          select(district)
        colnames(export_data) <- "Distrito"
      }
      
      write.csv(export_data, file, row.names = FALSE)
    } else {
      write.csv(data.frame(message = "No hay datos disponibles"), file, row.names = FALSE)
    }
  }
)

# Summary output for binary data
output$summary_output <- renderUI({
  req(filteredData(), selectedScaleType())
  
  scale_type <- selectedScaleType()
  data <- filteredData()
  
  if (scale_type == "binaria") {
    # Binary summary
    total_responses <- nrow(data)
    true_count <- sum(data$binary_value, na.rm = TRUE)
    false_count <- sum(!data$binary_value, na.rm = TRUE)
    missing_count <- total_responses - true_count - false_count
    valid_responses <- total_responses - missing_count
    
    if (valid_responses > 0) {
      true_percent <- round(100 * true_count / valid_responses, 2)
      false_percent <- round(100 * false_count / valid_responses, 2)
    } else {
      true_percent <- 0
      false_percent <- 0
    }
    
    # Get labels
    labels <- tryCatch({
      get_binary_labels(data)
    }, error = function(e) {
      list(true_label = "Sí", false_label = "No")
    })
    
    true_label <- labels$true_label
    false_label <- labels$false_label
    
    tagList(
      fluidRow(
        column(
          width = 4,
          div(
            class = "card",
            div(
              class = "card-body text-center",
              h4(true_label, class = "card-title"),
              h2(paste0(true_count, " (", true_percent, "%)"), class = "text-success"),
              p("Respuestas positivas", class = "card-text")
            )
          )
        ),
        column(
          width = 4,
          div(
            class = "card",
            div(
              class = "card-body text-center",
              h4(false_label, class = "card-title"),
              h2(paste0(false_count, " (", false_percent, "%)"), class = "text-danger"),
              p("Respuestas negativas", class = "card-text")
            )
          )
        ),
        column(
          width = 4,
          div(
            class = "card",
            div(
              class = "card-body text-center",
              h4("Total", class = "card-title"),
              h2(paste0(valid_responses), class = "text-info"),
              p("Respuestas válidas", class = "card-text")
            )
          )
        )
      )
    )
  } else {
    div(
      class = "alert alert-info",
      p("Seleccione una visualización específica para ver los datos procesados.")
    )
  }
})

# Binary visualizations (simplified)
output$binary_bars <- renderPlotly({
  req(filteredData())
  create_binary_bar(
    filteredData(),
    title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
    custom_theme = sectionTheme()
  )
})

output$binary_pie <- renderPlotly({
  req(filteredData())
  create_binary_pie(
    filteredData(),
    title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
    custom_theme = sectionTheme()
  )
})

output$binary_district_map <- renderLeaflet({
  req(filteredData(), geoData())
  create_binary_district_map(
    filteredData(), 
    geoData(),
    highlight_extremes = input$highlight_extremes,
    focus_on_true = TRUE,
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
    
  })
}