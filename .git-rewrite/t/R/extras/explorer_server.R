explorerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get selected year from main app
    selectedYear <- session$userData$selectedYear
    
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
    
    # Load survey data for the selected year
    allSurveyData <- reactive({
      req(selectedYear())
      
      # Load both surveys for the selected year only
      all_data <- list()
      current_year <- selectedYear()
      
      message(paste("Loading survey data for year:", current_year))
      
      for (type in c("PER", "PAR")) {
        survey_id <- paste0(type, "_", current_year)
        
        tryCatch({
          message(paste("Attempting to load:", survey_id))
          data <- load_survey_data(survey_id)
          if (!is.null(data)) {
            all_data[[survey_id]] <- data
            message(paste("Successfully loaded", survey_id, "with", nrow(data$responses), "responses"))
            if (!is.null(data$metadata)) {
              message(paste("Metadata has", nrow(data$metadata), "questions"))
            }
          } else {
            message(paste("load_survey_data returned NULL for", survey_id))
          }
        }, error = function(e) {
          message(paste("Could not load", survey_id, ":", e$message))
          showNotification(
            paste("No se pudo cargar", survey_id, ":", e$message), 
            type = "warning",
            duration = 3
          )
        })
      }
      
      message(paste("Total surveys loaded:", length(all_data)))
      return(all_data)
    })
    
    # Load geo data for maps
    geoData <- reactive({
      tryCatch({
        # Try the .geojson file first, then fall back to .shp
        if (file.exists('data/geo/Jrz_Map.geojson')) {
          sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
        } else if (file.exists('data/spatial/Distritos_Juarez.shp')) {
          sf::st_read('data/spatial/Distritos_Juarez.shp', quiet = TRUE)
        } else {
          # Use the session data if available
          session_geo <- session$userData$geoData
          if (is.function(session_geo)) {
            session_geo()
          } else {
            NULL
          }
        }
      }, error = function(e) {
        showNotification(
          paste("Error al cargar datos geográficos:", e$message), 
          type = "warning",
          duration = 5
        )
        NULL
      })
    })
    
    # Load and process theme metadata for the selected year
    allThemes <- reactive({
      req(selectedYear())
      
      tryCatch({
        themes_data <- theme_metadata$load_thematic_classifications()
        current_year <- selectedYear()
        
        # Debug information
        message(paste("Total themes loaded:", nrow(themes_data)))
        if (nrow(themes_data) > 0) {
          unique_surveys <- unique(themes_data$survey_id)
          message(paste("Available surveys:", paste(unique_surveys, collapse = ", ")))
        }
        
        # Filter by selected year and exclude internal themes
        filtered_data <- themes_data %>%
          filter(
            !MainTheme %in% c("Internal", "Dashboard Context"),
            grepl(paste0("_", current_year), survey_id)
          )
        
        message(paste("Filtered themes for", current_year, ":", nrow(filtered_data)))
        if (nrow(filtered_data) > 0) {
          unique_themes <- unique(filtered_data$MainTheme)
          message(paste("Available themes for", current_year, ":", paste(unique_themes, collapse = ", ")))
        }
        
        return(filtered_data)
      }, error = function(e) {
        showNotification(
          paste("Error al cargar metadata de temas:", e$message), 
          type = "error",
          duration = 10
        )
        message(paste("Error in allThemes():", e$message))
        # Return empty data frame with correct structure
        data.frame(
          variable = character(0),
          survey_id = character(0),
          MainTheme = character(0),
          Subtheme = character(0),
          stringsAsFactors = FALSE
        )
      })
    })
    
    # Create a comprehensive question catalog with theme info for selected year
    questionCatalog <- reactive({
      req(allSurveyData(), allThemes(), selectedYear())
      
      tryCatch({
        themes_data <- allThemes()
        survey_data <- allSurveyData()
        current_year <- selectedYear()
        
        # Return empty data frame if no data
        if (nrow(themes_data) == 0 || length(survey_data) == 0) {
          return(data.frame(
            question_id = character(0),
            survey_id = character(0),
            clean_survey_id = character(0),
            survey_type = character(0),
            survey_year = character(0),
            main_theme = character(0),
            subtheme = character(0),
            question_label = character(0),
            scale_type = character(0),
            stringsAsFactors = FALSE
          ))
        }
        
        catalog <- data.frame()
        
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
              survey_year = current_year,  # Use current year consistently
              main_theme = theme_info$MainTheme,
              subtheme = theme_info$Subtheme,
              question_label = question_meta$label,
              scale_type = scale_type,
              stringsAsFactors = FALSE
            ))
          }
        }
        
        return(catalog)
      }, error = function(e) {
        warning(paste("Error creating question catalog:", e$message))
        # Return empty but properly structured data frame
        return(data.frame(
          question_id = character(0),
          survey_id = character(0),
          clean_survey_id = character(0),
          survey_type = character(0),
          survey_year = character(0),
          main_theme = character(0),
          subtheme = character(0),
          question_label = character(0),
          scale_type = character(0),
          stringsAsFactors = FALSE
        ))
      })
    })
    
    # Display current year info
    output$current_year_info <- renderText({
      current_year <- selectedYear()
      if (is.null(current_year)) {
        return("Cargando...")
      }
      return(current_year)
    })
    prepare_q65_data <- function(data, question_id, metadata) {
  # Monument mapping for Q65
  monument_mapping <- c(
    "1" = "La X", "2" = "El Monumento A Benito Juárez", "3" = "La Catedral",
    "4" = "El Parque Central", "5" = "El Chamizal", "6" = "La Casa De Juan Gabriel",
    "7" = "El Centro", "8" = "La Presidencia", "9" = "Umbral Del Milenio",
    "10" = "El Museo De La Ex-Aduana", "11" = "El Parque Borunda",
    "12" = "El Monumento A Zapata", "13" = "La Rodadora", "14" = "Letras JRZ",
    "15" = "La Plaza De Toros", "16" = "Monumento A Los Indomables",
    "17" = "Gimnasios Públicos", "18" = "La Torre Centinela", "19" = "El Gardie",
    "20" = "El Cigarro", "21" = "Estadio Benito Juárez", "22" = "La Iglesia De San Lorenzo",
    "23" = "Mercado Juárez", "24" = "Monumento A Tin Tan", "25" = "Monumento Al Trabajo",
    "26" = "Otro", "27" = "Ninguno"
  )
  
  # Check if standardized columns exist and handle missing ones
  data <- check_standardized_columns(data)
  
  # Extract the column data
  if (!question_id %in% names(data)) {
    warning(paste("Question", question_id, "not found in data"))
    return(NULL)
  }
  
  # Select relevant columns
  subset_data <- data %>%
    select(
      value = all_of(question_id),
      district = DISTRICT, 
      gender = GENDER,
      age_group = AGE_GROUP
    )
  
  # Apply the monument mapping
  subset_data <- subset_data %>%
    mutate(
      original_value = value,
      value = case_when(
        as.character(value) %in% names(monument_mapping) ~ monument_mapping[as.character(value)],
        TRUE ~ as.character(value)
      ),
      # Convert factors to characters for consistency
      district = as.factor(district),
      gender = as.factor(gender),
      age_group = as.factor(age_group)
    ) %>%
    # Remove rows with missing values
    filter(!is.na(value), value != "", !is.na(district))
  
  # Add attributes for reference
  attr(subset_data, "question_id") <- question_id
  attr(subset_data, "monument_mapping") <- monument_mapping
  attr(subset_data, "is_monument_question") <- TRUE
  
  # Get question label from metadata
  question_meta <- metadata %>%
    filter(variable == question_id) %>%
    first()
  
  if (!is.null(question_meta) && !is.na(question_meta$label)) {
    attr(subset_data, "question_label") <- question_meta$label
  }
  
  return(subset_data)
}
    # Update theme dropdown (only for theme search tab)
    observe({
      catalog <- questionCatalog()
      
      # Handle empty or NULL catalog
      if (is.null(catalog) || nrow(catalog) == 0) {
        updateSelectInput(session, "theme_filter", 
                         choices = c("No hay temas disponibles" = ""),
                         selected = "")
        return()
      }
      
      unique_themes <- unique(catalog$main_theme)
      
      # Handle empty themes
      if (length(unique_themes) == 0) {
        updateSelectInput(session, "theme_filter", 
                         choices = c("No hay temas disponibles" = ""),
                         selected = "")
        return()
      }
      
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
      req(input$theme_filter, input$theme_filter != "")
      
      catalog <- questionCatalog()
      
      # Handle empty or NULL catalog
      if (is.null(catalog) || nrow(catalog) == 0) {
        updateSelectInput(session, "subtheme_filter", 
                         choices = c("No hay datos disponibles" = ""),
                         selected = "")
        return()
      }
      
      # Check if main_theme column exists
      if (!"main_theme" %in% names(catalog)) {
        updateSelectInput(session, "subtheme_filter", 
                         choices = c("Error en los datos" = ""),
                         selected = "")
        return()
      }
      
      # Get subthemes for the selected theme
      subthemes <- catalog %>%
        filter(main_theme == input$theme_filter) %>%
        pull(subtheme) %>%
        unique() %>%
        sort()
      
      # Handle empty subthemes
      if (length(subthemes) == 0) {
        updateSelectInput(session, "subtheme_filter", 
                         choices = c("No hay subtemas disponibles" = ""),
                         selected = "")
        return()
      }
      
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
          input$subtheme_filter, input$subtheme_filter != "")
      
      catalog <- questionCatalog()
      
      # Handle empty or NULL catalog
      if (is.null(catalog) || nrow(catalog) == 0) {
        updateSelectInput(
          session,
          "question_select_theme",
          choices = c("No hay datos disponibles" = ""),
          selected = ""
        )
        return()
      }
      
      # Check if required columns exist
      if (!"main_theme" %in% names(catalog) || !"subtheme" %in% names(catalog)) {
        updateSelectInput(
          session,
          "question_select_theme",
          choices = c("Error en los datos" = ""),
          selected = ""
        )
        return()
      }
      
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
      req(input$search_query, nchar(trimws(input$search_query)) > 0)
      
      search_query <- trimws(input$search_query)
      if (search_query == "") return(data.frame())
      
      catalog <- questionCatalog()
      
      # Handle empty or NULL catalog
      if (is.null(catalog) || nrow(catalog) == 0) {
        return(data.frame())
      }
      
      # Split search terms
      search_terms <- tolower(trimws(strsplit(search_query, "\\s+")[[1]]))
      
      # Check if required columns exist
      if (!"question_label" %in% names(catalog) || !"question_id" %in% names(catalog)) {
        return(data.frame())
      }
      
      # Filter questions that match any search term
      filtered_questions <- catalog %>%
        filter(
          Reduce(`|`, lapply(search_terms, function(term) {
            grepl(term, tolower(question_label), fixed = TRUE) |
            grepl(term, tolower(question_id), fixed = TRUE)
          }))
        )
      
      # Handle case where no columns exist for sorting
      if (!"survey_type" %in% names(filtered_questions)) {
        return(filtered_questions)
      }
      
      filtered_questions <- filtered_questions %>%
        arrange(survey_type, survey_year, question_id)
      
      return(filtered_questions)
    })
    
    # Update search results info
output$search_results_info <- renderUI({
  if (input$search_button == 0) return(NULL)
  
  req(input$search_query, selectedYear())
  search_query <- trimws(input$search_query)
  current_year <- selectedYear()
  
  if (search_query == "") {
    return(div(
      class = "alert alert-secondary",
      "Ingrese términos de búsqueda y haga clic en 'Buscar'"
    ))
  }
  
  results <- searchResults()
  
  if (nrow(results) == 0) {
    return(div(
      class = "alert alert-warning",
      paste0("No se encontraron preguntas que contengan '", search_query, "' en el año ", current_year)
    ))
  }
  
  # Count by survey
  survey_counts <- results %>%
    group_by(survey_type, survey_year) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(survey_label = paste0(survey_type, " ", survey_year, ": ", count)) %>%
    pull(survey_label)
  
  return(div(
    class = "alert alert-success",
    paste0("Se encontraron ", nrow(results), " preguntas en ", current_year),
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
      tryCatch({
        req(selectedQuestionInfo())
        
        current_year <- selectedYear()
        if (is.null(current_year)) {
          return("Cargando información de la fuente...")
        }
        
        survey_id <- selectedQuestionInfo()$survey_id
        survey_type <- ifelse(startsWith(survey_id, "PER"), 
                             "Encuesta de Percepción Ciudadana", 
                             "Encuesta de Participación Ciudadana y Buen Gobierno")
        
        paste(survey_type, current_year)
      }, error = function(e) {
        return("Error cargando información de la fuente")
      })
    })
    
    # Display question information
output$question_text <- renderUI({
  tryCatch({
    req(selectedQuestionInfo(), currentSurveyData())
    
    question_id <- selectedQuestionInfo()$question_id
    label <- get_question_label(question_id, currentSurveyData()$metadata)
    scale_type <- selectedScaleType()
    
    # Get theme info for styling
    catalog <- questionCatalog()
    
    if (is.null(catalog) || nrow(catalog) == 0) {
      # Fallback without theme styling
      scale_type_spanish <- switch(scale_type,
                                "razon" = "Razón (numérica continua)",
                                "intervalo" = "Intervalo (numérica con rangos)",
                                "ordinal" = "Ordinal (categorías ordenadas)",
                                "categorico" = "Categórica (categorías sin orden)",
                                "binaria" = "Binaria (sí/no)",
                                "nominal" = "Nominal (texto abierto)",
                                "Desconocido")
      
      return(HTML(paste0(
        "<strong>ID:</strong> ", question_id, "<br/>",
        "<strong>Pregunta:</strong> ", label, "<br/>",
        "<strong>Tipo de datos:</strong> ", scale_type_spanish
      )))
    }
    
    question_info <- catalog %>%
      filter(question_id == !!question_id, survey_id == !!selectedQuestionInfo()$survey_id) %>%
      slice(1)
    
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
      "<strong>Tema:</strong> ", theme_spanish, "<br/>",
      "<strong>Subtema:</strong> ", subtheme_spanish
    ))
  }, error = function(e) {
    HTML("<strong>Error:</strong> No se pudo cargar la información de la pregunta.")
  })
})
    
    # Generate visualization title
output$viz_title <- renderText({
  current_year <- selectedYear()
  if (is.null(current_year)) {
    return("Así Estamos Juárez")
  }
  
  if (is.null(selectedQuestionInfo())) {
    return(paste("Así Estamos Juárez -", current_year))
  }
  
  paste("Resultados -", current_year)
})
    
    # Display visualization options based on scale type
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
    
    # Rest of the code remains the same from the original implementation...
    # (preparedData, filteredData, visualization outputs, etc.)
    
    # Prepare data for the selected question
   preparedData <- reactive({
  req(selectedQuestionInfo(), currentSurveyData(), selectedScaleType())
  
  question_id <- selectedQuestionInfo()$question_id
  scale_type <- selectedScaleType()
  survey_data <- currentSurveyData()
  
  # Special handling for Q65 (monuments question)
  if (question_id == "Q65") {
    return(prepare_q65_data(survey_data$responses, question_id, survey_data$metadata))
  }
  
  # Standard data preparation based on scale type
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
      
 if (scale_type %in% c("razon", "intervalo", "ordinal", "binaria") && viz_type == "district_map") {
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
    
    # Update binary comparison questions
    observe({
      req(selectedScaleType() == "binaria", input$viz_type == "multiple_comparison")
      
      # Get all binary questions from current survey
      survey_data <- currentSurveyData()
      if (is.null(survey_data)) return()
      
      # Classify questions for this survey
      classifications <- tryCatch({
        classify_questions(survey_data$metadata)
      }, error = function(e) {
        return(list(binaria = character(0)))
      })
      
      binary_questions <- classifications$binaria
      
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
        q_meta <- survey_data$metadata %>% filter(variable == qid) %>% first()
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
      
      # Set current question as selected by default
      current_question_id <- selectedQuestionInfo()$question_id
      default_selected <- if (current_question_id %in% binary_questions) {
        question_labels[current_question_id]
      } else {
        character(0)
      }
      
      updateCheckboxGroupInput(
        session, 
        "compare_questions",
        choices = question_labels,
        selected = default_selected
      )
    })

sectionTheme <- reactive({
  # Always use extras theme for explorer, but try to get question theme if available
  tryCatch({
    req(selectedQuestionInfo())
    
    catalog <- questionCatalog()
    
    # Return extras theme if no catalog or question info
    if (is.null(catalog) || nrow(catalog) == 0 || is.null(selectedQuestionInfo())) {
      return(get_section_theme("extras"))
    }
    
    question_info <- catalog %>%
      filter(question_id == selectedQuestionInfo()$question_id, 
             survey_id == selectedQuestionInfo()$survey_id) %>%
      slice(1)
    
    # Default to extras theme if no question info found
    if (nrow(question_info) == 0) {
      return(get_section_theme("extras"))
    }
    
    # Map MainTheme to section theme names - but always fallback to extras
    main_theme <- question_info$main_theme
    section_name <- switch(main_theme,
      "Social & Economic Wellbeing" = "bienestar",
      "Urban Mobility & Environment" = "movilidad", 
      "Governance & Civic Engagement" = "gobierno",
      "Public Services" = "infraestructura",
      "Community Participation" = "participacion",
      "extras"  # default fallback - always safe
    )
    
    get_section_theme(section_name)
  }, error = function(e) {
    # Always return a valid theme - extras is safe fallback
    get_section_theme("extras")
  })
})
    
    # Generate the appropriate visualization
    output$visualization <- renderUI({
      req(selectedQuestionInfo(), input$viz_type, selectedScaleType())
      
      scale_type <- selectedScaleType()
      viz_type <- input$viz_type
      
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

    # Download options
output$download_options <- renderUI({
  req(selectedQuestionInfo(), input$viz_type)
  
  viz_type <- input$viz_type
  
  if (viz_type == "summary") {
    downloadButton(
      ns("download_summary_csv"), 
      "", 
      icon = icon("download"), 
      class = "plot-action-btn",
      title = "Descargar CSV"
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
        current_year <- selectedYear()
        paste0("datos_", question_id, "_", survey_id, "_", current_year, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        if (!is.null(filteredData())) {
          data <- filteredData()
          
          # Create a simplified table for download
          if ("binary_value" %in% names(data)) {
            export_data <- data %>%
              select(district, binary_value)
            
            # Safe column renaming
            colnames(export_data) <- c("Distrito", selectedQuestionInfo()$question_id)
          } else if ("value" %in% names(data)) {
            export_data <- data %>%
              select(district, value)
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

    # COMPLETE COMPREHENSIVE SUMMARY OUTPUT - From old version
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
        Distrito = district,
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
  mutate(numeric_value = get_numeric_values(.)) %>%  # Extract numeric values first
  group_by(district) %>%
  summarise(
    Respuestas = n(),
    Media = round(mean(numeric_value, na.rm = TRUE), 2),
    Mediana = median(numeric_value, na.rm = TRUE),
    DE = round(sd(numeric_value, na.rm = TRUE), 2),
    Min = min(numeric_value, na.rm = TRUE),
    Max = max(numeric_value, na.rm = TRUE),
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
  mutate(numeric_value = get_numeric_values(.)) %>%  # Extract numeric values first
  group_by(district) %>%
  summarise(
    Respuestas = n(),
    Media = round(mean(numeric_value, na.rm = TRUE), 2),
    Mediana = median(numeric_value, na.rm = TRUE),
    DE = round(sd(numeric_value, na.rm = TRUE), 2),
    Min = min(numeric_value, na.rm = TRUE),
    Max = max(numeric_value, na.rm = TRUE),
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
 # Categorical summary with special handling for Q65
  
  # Check if this is the monuments question
  is_monument_question <- !is.null(attr(data, "is_monument_question")) && attr(data, "is_monument_question")
  
  clean_values <- data$value[!is.na(data$value)]
  clean_values <- trimws(as.character(clean_values))
  clean_values <- clean_values[clean_values != ""]

  # Create frequency table with cleaned data
  freq_table <- table(clean_values)
  freq_df <- data.frame(
    Categoría = names(freq_table),
    Frecuencia = as.vector(freq_table),
    Porcentaje = round(100 * as.vector(freq_table) / sum(freq_table), 2),
    stringsAsFactors = FALSE
  )

  # Sort by frequency (descending)
  freq_df <- freq_df[order(-freq_df$Frecuencia), ]

  # Calculate key metrics with safety checks
  if (nrow(freq_df) > 0 && !is.na(freq_df$Categoría[1]) && freq_df$Categoría[1] != "") {
    most_popular <- as.character(freq_df$Categoría[1])
  } else {
    most_popular <- "Sin datos"
  }

  if (nrow(freq_df) > 0 && !is.na(freq_df$Categoría[nrow(freq_df)]) && freq_df$Categoría[nrow(freq_df)] != "") {
    least_popular <- as.character(freq_df$Categoría[nrow(freq_df)])
  } else {
    least_popular <- "Sin datos"
  }

  # Use length of clean_values for valid responses
  valid_responses <- length(clean_values)

  # District breakdown - most common category per district
  district_breakdown <- data %>%
    filter(!is.na(value)) %>%
    mutate(clean_value = trimws(as.character(value))) %>%
    filter(clean_value != "") %>%
    group_by(district, clean_value) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(district) %>%
    mutate(percentage = round(100 * count / sum(count), 1)) %>%
    slice_max(order_by = count, n = 1) %>%
    arrange(district) %>%
    select(
      Distrito = district, 
      Respuestas = count, 
      `Categoría más frecuente` = clean_value, 
      Porcentaje = percentage
    )
  
  # Special title for monuments question
  section_title <- if (is_monument_question) {
    "Monumento/Lugar Más Representativo"
  } else {
    "Estadísticas Categóricas"
  }
  
  # Create UI with special handling for monuments
  tagList(
    
    # Add special header for monuments question
    if (is_monument_question) {
      div(
        class = "alert alert-info mb-3",
        icon("landmark"),
        " Esta pregunta se refiere a los monumentos y lugares más representativos de Ciudad Juárez."
      )
    },
    
    # Value boxes
    fluidRow(
      column(
        width = 3,
        value_box(
          title = if (is_monument_question) "Lugar más popular" else "Categoría más frecuente",
          value = most_popular,
          showcase = if (is_monument_question) bsicons::bs_icon("geo-alt-fill") else bsicons::bs_icon("trophy"),
          p(paste0(freq_df$Frecuencia[1], " respuestas")),
          !!!vbox_style
        )
      ),
      column(
        width = 3,
        value_box(
          title = if (is_monument_question) "Lugar menos popular" else "Categoría menos frecuente",
          value = least_popular,
          showcase = bsicons::bs_icon("arrow-down"),
          p(paste0(freq_df$Frecuencia[nrow(freq_df)], " respuestas")),
          !!!vbox_style
        )
      ),
      column(
        width = 3,
        value_box(
          title = if (is_monument_question) "Total lugares" else "Total categorías",
          value = nrow(freq_df),
          showcase = bsicons::bs_icon("list-check"),
          p(if (is_monument_question) "Lugares distintos" else "Categorías distintas"),
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
    h4(if (is_monument_question) "Distribución de Lugares Mencionados" else "Distribución de Categorías", 
       class = "mt-4 mb-3"),
    DT::renderDataTable({
      # For monuments, show more rows since they're meaningful
      display_df <- if (is_monument_question) freq_df else head(freq_df, 15)
      
      DT::datatable(
        display_df,
        options = list(
          dom = if (nrow(freq_df) > 15) 'ftp' else 't',
          ordering = TRUE,
          paging = if (nrow(freq_df) > 15) TRUE else FALSE,
          searching = if (nrow(freq_df) > 15) TRUE else FALSE,
          pageLength = 15,
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

    # RAZON VISUALIZATIONS
    output$razon_histogram <- renderPlotly({
      req(filteredData())
      create_histogram(
        filteredData(), 
        bins = input$histogram_bins,
        title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
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
        title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
        xlab = "Distrito",
        ylab = "Valor Promedio",
        orientation = input$bar_orientation,
        color_by = "district",
        custom_theme = sectionTheme()
      )
    })

    # INTERVAL/ORDINAL VISUALIZATIONS
    output$interval_histogram <- renderPlotly({
      req(filteredData())
      create_interval_histogram(
        filteredData(), 
        bins = input$histogram_bins, 
        title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
        custom_theme = sectionTheme()
      )
    })
    
    output$interval_pie <- renderPlotly({
      req(filteredData())
      create_interval_pie(
        filteredData(),
        title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
        custom_theme = sectionTheme()
      )
    })
    
    output$interval_district_map <- renderLeaflet({
      req(filteredData(), geoData())
      create_interval_district_map(
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
      create_interval_bars(
        filteredData(),
        title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
        orientation = input$bar_orientation,
        custom_theme = sectionTheme()
      )
    })

    # CATEGORICAL VISUALIZATIONS
output$categorical_bars <- renderPlotly({
  req(filteredData())
  
  data <- filteredData()
  is_monument_question <- !is.null(attr(data, "is_monument_question")) && attr(data, "is_monument_question")
  
  if (is_monument_question) {
    # Special handling for monuments - show more categories and better formatting
    create_category_bars(
      data,
      max_categories = 20,  # Show more for monuments
      title = "Monumentos y Lugares Más Representativos",

      custom_theme = sectionTheme()    )
  } else {
    # Standard categorical visualization
    create_category_bars(
      data,
      max_categories = 15,
      title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
      custom_theme = sectionTheme()
    )
  }
})
    
output$categorical_pie <- renderPlotly({
  req(filteredData())
  
  data <- filteredData()
  is_monument_question <- !is.null(attr(data, "is_monument_question")) && attr(data, "is_monument_question")
  
  if (is_monument_question) {
    # For monuments, show top 10 and group others
    create_category_pie(
      data,
      max_categories = 10,
      title = "Distribución de Monumentos y Lugares",
      custom_theme = sectionTheme(),
      highlight_max = TRUE    )
  } else {
    # Standard categorical pie
    create_category_pie(
      data,
      max_categories = 8,
      title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
      custom_theme = sectionTheme(),
      highlight_max = TRUE
    )
  }
})
    
    output$categorical_stacked_bars <- renderPlotly({
      req(filteredData())
      create_category_stacked_bars(
        filteredData(),
                title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
        max_categories = 7,
        custom_theme = sectionTheme()
      )
    })

    # BINARY VISUALIZATIONS (already present but updated)
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
                title = get_question_label(selectedQuestionInfo()$question_id, currentSurveyData()$metadata),
        orientation = input$bar_orientation,
        custom_theme = sectionTheme()
      )
    })
    
    # BINARY MULTIPLE COMPARISON
    output$binary_comparison <- renderPlotly({
      req(input$compare_questions, length(input$compare_questions) > 0)
      
      # Extract question IDs from checkboxgroup input
      question_ids <- sapply(input$compare_questions, function(q) {
        strsplit(q, " - ")[[1]][1]
      })
      
      # Prepare data for all selected questions
      binary_data_list <- prepare_multiple_binary(currentSurveyData()$responses, question_ids, currentSurveyData()$metadata)
      
      # Create the comparison visualization
      create_multiple_binary_comparison(
        binary_data_list,
        comparison_type = input$comparison_type,
        top_n = input$top_n,
        custom_theme = sectionTheme()
      )
    })

    # NOMINAL VISUALIZATIONS
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