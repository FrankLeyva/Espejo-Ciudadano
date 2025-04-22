# R/extra/explorer_server.R

explorerServer <- function(id, currentTheme = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Use year from user data (this accesses the year selector in the main app)
    selectedYear <- session$userData$selectedYear
    
    # Load survey data based on selected year
    data <- reactive({
      req(selectedYear())
      year <- selectedYear()
      survey_id <- paste0("PER_", year)
      
      # Load the survey data
      message("Explorer module loading survey data for year: ", year)
      
      tryCatch({
        survey_data <- load_survey_data(survey_id)
        return(survey_data)
      }, error = function(e) {
        message("Error loading survey data: ", e$message)
        return(NULL)
      })
    })
    
    # Load geo data for maps
    geo_data <- reactive({
      tryCatch({
        sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
      }, error = function(e) {
        message(paste("Error loading geo data:", e$message))
        NULL
      })
    })
    
    # Safely classify questions
    safe_classify_questions <- reactive({
      if (is.null(data()) || is.null(data()$metadata)) {
        # Return empty classifications if data is not available
        return(list(
          razon = character(0),
          intervalo = character(0),
          ordinal = character(0),
          categorico = character(0),
          binaria = character(0),
          nominal = character(0)
        ))
      }
      
      # Safely classify questions
      tryCatch({
        classify_questions(data()$metadata)
      }, error = function(e) {
        message("Error classifying questions: ", e$message)
        # Return empty classifications on error
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
    
    # Safely load theme metadata
    all_themes <- reactive({
      tryCatch({
        theme_metadata$load_thematic_classifications()
      }, error = function(e) {
        message("Error loading theme metadata: ", e$message)
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
    
    # Function to get theme based on question
    getQuestionTheme <- reactive({
      req(selectedQuestionId())
      
      # Default to extras theme
      section_theme <- "extras"
      
      # Try to get theme from metadata
      tryCatch({
        themes_data <- all_themes()
        
        # Get current survey ID
        current_survey_id <- paste0("PER_", selectedYear())
        if (grepl("_V[0-9]+$", current_survey_id)) {
          current_survey_id <- gsub("_V[0-9]+$", "", current_survey_id)
        }
        
        # Find theme info for this question
        question_theme_info <- themes_data %>%
          filter(variable == selectedQuestionId(), survey_id == current_survey_id) %>%
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
      
      return(section_theme)
    })
    
    # Set up current theme for visualizations with section awareness
    active_theme <- reactive({
      # First check if currentTheme is provided
      if (is.function(currentTheme)) {
        # If currentTheme is a reactive function, call it to get the value
        return(currentTheme())
      } else if (!is.null(currentTheme)) {
        # If it's a direct value, use it
        return(currentTheme)
      }
      
      # If no selected question, use default extras theme
      if (is.null(selectedQuestionId())) {
        return(get_section_theme("extras"))
      }
      
      # Get the theme based on question classification
      section_theme <- getQuestionTheme()
      if (!is.null(section_theme) && section_theme != "") {
        return(get_section_theme(section_theme))
      }
      
      # Fallback to the global theme config if everything else fails
      return(theme_config)
    })
    
    # Populate theme selectors
    observe({
      # Get theme list safely
      themes_list <- tryCatch({
        theme_metadata$get_all_themes()
      }, error = function(e) {
        message("Error getting themes: ", e$message)
        character(0)
      })
      
      updateSelectInput(session, "theme_filter", 
                       choices = c("Todos" = "all", themes_list),
                       selected = "all")
    })
    
    # Update subtheme selector based on selected theme
    observe({
      req(input$theme_filter)
      
      if (input$theme_filter != "all") {
        # Get subthemes safely
        subthemes <- tryCatch({
          theme_metadata$get_subthemes_by_theme(input$theme_filter)
        }, error = function(e) {
          message("Error getting subthemes: ", e$message)
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
    
    # Update questions based on selected module, theme, and subtheme
    observe({
      # Make sure we have the module type selected
      req(input$module_type)
      
      # Start with empty list in case data is not available
      questions <- character(0)
      
      # Check if we have classification data
      if (!is.null(safe_classify_questions()) && 
          input$module_type %in% names(safe_classify_questions())) {
        questions <- safe_classify_questions()[[input$module_type]]
      }
      
      # If we have no questions, update with empty selection
      if (length(questions) == 0) {
        updateSelectInput(
          session,
          "selected_question",
          choices = c("No hay preguntas disponibles" = ""),
          selected = ""
        )
        return()
      }
      
      # Filter by theme and subtheme if selected
      if (!is.null(input$theme_filter) && 
          (input$theme_filter != "all" || input$subtheme_filter != "all")) {
        
        # Safely get themes data
        themes_data <- all_themes()
        
        if (nrow(themes_data) > 0) {
          # Make sure we're using the correct survey ID
          current_survey_id <- paste0("PER_", selectedYear())
          if (grepl("_V[0-9]+$", current_survey_id)) {
            current_survey_id <- gsub("_V[0-9]+$", "", current_survey_id)
          }
          
          # Filter theme data for current survey
          relevant_themes_data <- themes_data %>% 
            filter(survey_id == current_survey_id)
          
          if (input$theme_filter != "all" && input$subtheme_filter == "all") {
            # Filter by theme only
            theme_questions <- relevant_themes_data %>%
              filter(MainTheme == input$theme_filter) %>%
              pull(variable)
            
            questions <- questions[questions %in% theme_questions]
          } else if (input$theme_filter != "all" && input$subtheme_filter != "all") {
            # Filter by theme and subtheme
            theme_questions <- relevant_themes_data %>%
              filter(MainTheme == input$theme_filter, 
                     Subtheme == input$subtheme_filter) %>%
              pull(variable)
            
            questions <- questions[questions %in% theme_questions]
          }
        }
      }
      
      # If we have no questions after filtering, update with empty selection
      if (length(questions) == 0) {
        updateSelectInput(
          session,
          "selected_question",
          choices = c("No hay preguntas con los filtros seleccionados" = ""),
          selected = ""
        )
        return()
      }
      
      # Verify these questions exist in the actual dataset
      valid_questions <- character(0)
      if (!is.null(data()) && !is.null(data()$responses)) {
        valid_questions <- questions[questions %in% names(data()$responses)]
      } else {
        valid_questions <- questions  # Keep all if we can't verify
      }
      
      # If we have no valid questions, update with empty selection
      if (length(valid_questions) == 0) {
        updateSelectInput(
          session,
          "selected_question",
          choices = c("No hay preguntas disponibles en este dataset" = ""),
          selected = ""
        )
        return()
      }
      
      # Get labels for each question - with safety checks
      question_labels <- sapply(valid_questions, function(q) {
        if (!is.null(data()) && !is.null(data()$metadata)) {
          label <- tryCatch({
            get_question_label(q, data()$metadata)
          }, error = function(e) {
            q  # Use question ID as label if error
          })
          
          if (is.na(label) || label == "") {
            label <- q  # Use question ID if label is empty
          }
          
          if (nchar(label) > 60) {
            # Truncate long labels
            truncated <- paste0(q, ": ", substr(label, 1, 57), "...")
          } else {
            truncated <- paste0(q, ": ", label)
          }
          return(truncated)
        } else {
          return(q)  # Just return the question ID if no metadata
        }
      })
      
      # Create dropdown choices
      choices <- setNames(valid_questions, question_labels)
      
      # Update the dropdown
      updateSelectInput(
        session,
        "selected_question",
        choices = choices,
        selected = NULL
      )
    })
    
    # Extract selected question ID
    selectedQuestionId <- reactive({
      req(input$selected_question)
      if (input$selected_question == "") return(NULL)
      
      # Return the question ID
      return(input$selected_question)
    })
    
    # Display question label
    output$question_label <- renderText({
      # Return empty string if no question selected
      if (is.null(selectedQuestionId()) || is.null(data()) || is.null(data()$metadata)) {
        return("")
      }
      
      # Safely get the question label
      tryCatch({
        get_question_label(selectedQuestionId(), data()$metadata)
      }, error = function(e) {
        message("Error getting question label: ", e$message)
        return(selectedQuestionId())  # Return the ID if error
      })
    })
    
    # Display theme information for the selected question
    output$question_theme_info <- renderUI({
      # Return empty if no question selected
      if (is.null(selectedQuestionId()) || is.null(all_themes())) {
        return(div())
      }
      
      # Safely get theme info
      question_theme_info <- tryCatch({
        themes_data <- all_themes()
        
        # Filter to match both variable name and survey ID
        current_survey_id <- paste0("PER_", selectedYear())
        # Convert 2023_V3 to 2023 format
        if (grepl("_V[0-9]+$", current_survey_id)) {
          current_survey_id <- gsub("_V[0-9]+$", "", current_survey_id)
        }
        
        themes_data %>%
          filter(variable == selectedQuestionId(), survey_id == current_survey_id) %>%
          select(MainTheme, Subtheme) %>%
          first()
      }, error = function(e) {
        message("Error getting theme info: ", e$message)
        return(NULL)
      })
      
      if (!is.null(question_theme_info) && !is.na(question_theme_info$MainTheme)) {
        # Safely get theme property
        theme_property <- tryCatch({
          theme_metadata$get_theme_property(question_theme_info$MainTheme)
        }, error = function(e) {
          message("Error getting theme property: ", e$message)
          # Default theme property
          list(
            color = "#6c757d",
            icon = "folder"
          )
        })
        
        div(
          tags$span(
            "Tema: ",
            tags$span(
              class = "badge",
              style = paste0("background-color: ", theme_property$color, "; color: white;"),
              icon(theme_property$icon), " ", question_theme_info$MainTheme
            )
          ),
          if (!is.na(question_theme_info$Subtheme)) {
            tags$span(
              " | Subtema: ",
              tags$span(
                class = "badge",
                style = "background-color: #6c757d; color: white;",
                question_theme_info$Subtheme
              )
            )
          }
        )
      } else {
        div(
          tags$span(
            "Tema: ",
            tags$span(
              class = "badge",
              style = "background-color: #6c757d; color: white;",
              "No clasificado"
            )
          )
        )
      }
    })
    
    # Verify if the question exists in the dataset before initializing modules
    validQuestion <- reactive({
      req(selectedQuestionId(), data(), data()$responses)
      
      # Check if the question exists in the dataset
      selectedQuestionId() %in% names(data()$responses)
    })
    
    # Initialize the appropriate module server based on selection
    observe({
      req(input$module_type, selectedQuestionId())
      
      # Only initialize if the question is valid
      if (!isTRUE(validQuestion())) {
        return()
      }
      
      # Get all binary questions for comparison feature in binary module
      all_binary_questions <- if (input$module_type == "binaria") {
        safe_classify_questions()[["binaria"]]
      } else {
        NULL
      }
      
      # Initialize the correct module server
      if (input$module_type == "razon") {
        razonServer(
          "razon_module",
          data = reactive(data()$responses),
          selected_question = reactive(selectedQuestionId()),
          geo_data = geo_data,
          metadata = reactive(data()$metadata),
          current_theme = active_theme
        )
      } else if (input$module_type == "intervalo") {
        intervalServer(
          "interval_module",
          data = reactive(data()$responses),
          metadata = reactive(data()$metadata),
          selected_question = reactive(selectedQuestionId()),
          geo_data = geo_data,
          current_theme = active_theme
        )
      } else if (input$module_type == "ordinal") {
        ordinalServer(
          "ordinal_module",
          data = reactive(data()$responses),
          metadata = reactive(data()$metadata),
          selected_question = reactive(selectedQuestionId()),
          geo_data = geo_data,
          current_theme = active_theme
        )
      } else if (input$module_type == "categorico") {
        categoricoServer(
          "categorico_module",
          data = reactive(data()$responses),
          metadata = reactive(data()$metadata),
          selected_question = reactive(selectedQuestionId()),
          geo_data = geo_data,
          current_theme = active_theme
        )
      } else if (input$module_type == "binaria") {
        binaryServer(
          "binary_module",
          data = reactive(data()$responses),
          metadata = reactive(data()$metadata),
          selected_question = reactive(selectedQuestionId()),
          geo_data = geo_data,
          all_binary_questions = all_binary_questions,
          current_theme = active_theme
        )
      } else if (input$module_type == "nominal") {
        nominalServer(
          "nominal_module",
          data = reactive(data()$responses),
          metadata = reactive(data()$metadata),
          selected_question = reactive(selectedQuestionId()),
          geo_data = geo_data,
          current_theme = active_theme
        )
      }
    })
  })
}