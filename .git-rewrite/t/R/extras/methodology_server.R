# Server function for Methodology and Data Download Dashboard
methodologyServer <- function(input, output, session, current_theme = NULL) {
  # Debug: Check session object
  message("🔧 methodologyServer called")
  message(sprintf("🔍 Session object type: %s", class(session)[1]))
  message(sprintf("🔍 Session methods available: %s", paste(names(session), collapse = ", ")))
  message(sprintf("🔍 Session has sendModal: %s", "sendModal" %in% names(session)))
  message(sprintf("🔍 Session has sendCustomMessage: %s", "sendCustomMessage" %in% names(session)))
  
  # Load utility functions for metadata
  source("R/utils.R")
  
  # Reactive values to store metadata
  metadata_values <- reactiveValues(
    per_2023_meta = NULL,
    par_2023_meta = NULL,
    per_2024_meta = NULL,
    par_2024_meta = NULL,
    current_dictionary = NULL,
    current_survey_id = NULL
  )
  
  # Initialize metadata on load for all survey years
  observe({
    message("🔧 Initializing methodology metadata...")
    
    tryCatch({
      # 2023 surveys
      metadata_values$per_2023_meta <- generate_survey_metadata("PER_2023")
      metadata_values$par_2023_meta <- generate_survey_metadata("PAR_2023")
      
      # 2024 surveys
      metadata_values$per_2024_meta <- generate_survey_metadata("PER_2024")
      metadata_values$par_2024_meta <- generate_survey_metadata("PAR_2024")
      
      message("✅ Methodology metadata initialized successfully")
      message(sprintf("📊 PER 2023: %s", ifelse(is.null(metadata_values$per_2023_meta$error), "OK", "Error")))
      message(sprintf("📊 PAR 2023: %s", ifelse(is.null(metadata_values$par_2023_meta$error), "OK", "Error")))
      message(sprintf("📊 PER 2024: %s", ifelse(is.null(metadata_values$per_2024_meta$error), "OK", "Error")))
      message(sprintf("📊 PAR 2024: %s", ifelse(is.null(metadata_values$par_2024_meta$error), "OK", "Error")))
      
    }, error = function(e) {
      message("❌ Error initializing methodology metadata: ", e$message)
    })
  })
  
  # Test modal to verify modal system is working
  observeEvent(input$show_metadata_modal, {
    message("🧪 Test modal button clicked")
    
    # Try using session$sendCustomMessage instead of showModal
    tryCatch({
      session$sendCustomMessage("show-test-modal", list(
        title = "Test Modal",
        message = "This is a test modal to verify the modal system is working."
      ))
      message("✅ Test modal message sent via sendCustomMessage")
    }, error = function(e) {
      message("❌ Error sending test modal message: ", e$message)
      
      # Fallback: try showModal
      tryCatch({
        showModal(modalDialog(
          title = "Test Modal",
          "This is a test modal to verify the modal system is working.",
          easyClose = TRUE
        ))
        message("✅ Test modal shown via showModal fallback")
      }, error = function(e2) {
        message("❌ Error showing test modal via showModal: ", e2$message)
      })
    })
  })
  
  # Debug observer to see if actionLink clicks are being detected
  observe({
    # Monitor all the actionLink inputs
    per_2024_clicked <- input$view_per_2024_metadata
    par_2024_clicked <- input$view_par_2024_metadata
    per_2023_clicked <- input$view_per_2023_metadata
    par_2023_clicked <- input$view_par_2023_metadata
    
    # Log any clicks
    if (!is.null(per_2024_clicked)) message("🔍 PER 2024 actionLink detected")
    if (!is.null(par_2024_clicked)) message("🔍 PAR 2024 actionLink detected")
    if (!is.null(per_2023_clicked)) message("🔍 PER 2023 actionLink detected")
    if (!is.null(par_2023_clicked)) message("🔍 PAR 2023 actionLink detected")
  })
  
  # Function to create a formatted data dictionary with the required columns
  create_data_dictionary <- function(survey_id) {
    tryCatch({
      # Load survey data
      survey_data <- load_survey_data(survey_id)
      
      # Get metadata from the survey
      survey_meta <- survey_data$metadata
      
      # Select and rename the required columns
      if (is.null(survey_meta) || nrow(survey_meta) == 0) {
        # If metadata is not available, create a dummy dictionary
        dictionary <- data.frame(
          variable = character(0),
          label = character(0),
          value_labels = character(0),
          scale_type = character(0),
          stringsAsFactors = FALSE
        )
      } else {
        # Check which columns exist and create them if missing
        required_cols <- c("variable", "label", "value_labels", "scale_type")
        existing_cols <- names(survey_meta)
        
        # Create missing columns with default values
        for (col in required_cols) {
          if (!col %in% existing_cols) {
            survey_meta[[col]] <- if (col == "variable") {
              # Try to use existing column names as variables
              if ("variable" %in% existing_cols) {
                survey_meta$variable
              } else if ("question" %in% existing_cols) {
                survey_meta$question
              } else {
                paste0("Q", seq_len(nrow(survey_meta)))
              }
            } else if (col == "label") {
              # Use existing label or description column
              if ("label" %in% existing_cols) {
                survey_meta$label
              } else if ("description" %in% existing_cols) {
                survey_meta$description
              } else {
                rep("", nrow(survey_meta))
              }
            } else if (col == "value_labels") {
              # Use existing value_labels or options column
              if ("value_labels" %in% existing_cols) {
                survey_meta$value_labels
              } else if ("options" %in% existing_cols) {
                survey_meta$options
              } else {
                rep("", nrow(survey_meta))
              }
            } else if (col == "scale_type") {
              # Use existing scale_type or type column
              if ("scale_type" %in% existing_cols) {
                survey_meta$scale_type
              } else if ("type" %in% existing_cols) {
                survey_meta$type
              } else {
                rep("", nrow(survey_meta))
              }
            }
          }
        }
        
        # Select the needed columns and rename them as required
        dictionary <- survey_meta %>%
          dplyr::select(
            variable = variable,
            label = label,
            value_labels = value_labels,
            scale_type = scale_type
          ) %>%
          # Convert NA to empty strings for better display
          dplyr::mutate(
            label = ifelse(is.na(label), "", as.character(label)),
            value_labels = ifelse(is.na(value_labels), "", as.character(value_labels)),
            scale_type = ifelse(is.na(scale_type), "", as.character(scale_type))
          )
      }
      
      return(dictionary)
      
    }, error = function(e) {
      # Return empty data frame with correct structure on error
      warning(paste("Error creating data dictionary for", survey_id, ":", e$message))
      data.frame(
        variable = character(0),
        label = character(0),
        value_labels = character(0),
        scale_type = character(0),
        stringsAsFactors = FALSE
      )
    })
  }
  
  # Function to prepare data for download
  prepare_download_data <- function(survey_data) {
    # Extract responses from the survey data
    responses <- survey_data$responses
    
    # Check if responses exist and have the right format
    if (is.null(responses) || nrow(responses) == 0) {
      return(data.frame(
        ERROR = "No hay respuestas disponibles para esta encuesta."
      ))
    }
    
    # Return the responses data frame directly
    return(responses)
  }
  
  # Handler for downloading 2024 surveys
  output$download_per_2024 <- downloadHandler(
    filename = function() {
      paste0("PER_2024_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      message("🔧 Starting PER_2024 download...")
      
      # Try to load the survey data
      tryCatch({
        # Load survey data using the existing function
        message("📊 Loading PER_2024 survey data...")
        per_data <- load_survey_data("PER_2024")
        
        if (is.null(per_data)) {
          stop("Failed to load PER_2024 data - load_survey_data returned NULL")
        }
        
        message(sprintf("📊 PER_2024 data loaded successfully with %d responses", nrow(per_data$responses)))
        
        # Prepare data for export
        export_data <- prepare_download_data(per_data)
        
        message(sprintf("📊 Export data prepared with %d rows and %d columns", nrow(export_data), ncol(export_data)))
        
        # Add any necessary metadata columns
        if (!"SURVEY_ID" %in% names(export_data)) {
          export_data$SURVEY_ID <- paste0("PER_2024_", seq_len(nrow(export_data)))
        }
        
        # Write to CSV file
        message("📊 Writing CSV file...")
        write.csv(export_data, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        
        message("✅ PER_2024 CSV file written successfully")
        
        # Show success notification
        showNotification("Datos descargados exitosamente", type = "message")
        
      }, error = function(e) {
        # Handle error if data cannot be loaded
        error_msg <- paste("Error al exportar datos PER_2024:", e$message)
        message("❌ ", error_msg)
        showNotification(error_msg, type = "error", duration = 5)
        
        # Create a simple CSV with error message for user to know something went wrong
        error_data <- data.frame(
          ERROR = "Los datos no pudieron ser cargados. Por favor contacte al administrador.",
          DETAILS = e$message,
          TIMESTAMP = Sys.time()
        )
        write.csv(error_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      })
    }
  )
  
  output$download_par_2024 <- downloadHandler(
    filename = function() {
      paste0("PAR_2024_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      message("🔧 Starting PAR_2024 download...")
      
      # Try to load the survey data
      tryCatch({
        # Load survey data using the existing function
        message("📊 Loading PAR_2024 survey data...")
        par_data <- load_survey_data("PAR_2024")
        
        if (is.null(par_data)) {
          stop("Failed to load PAR_2024 data - load_survey_data returned NULL")
        }
        
        message(sprintf("📊 PAR_2024 data loaded successfully with %d responses", nrow(par_data$responses)))
        
        # Prepare data for export
        export_data <- prepare_download_data(par_data)
        
        message(sprintf("📊 Export data prepared with %d rows and %d columns", nrow(export_data), ncol(export_data)))
        
        # Add any necessary metadata columns
        if (!"SURVEY_ID" %in% names(export_data)) {
          export_data$SURVEY_ID <- paste0("PAR_2024_", seq_len(nrow(export_data)))
        }
        
        # Write to CSV file
        message("📊 Writing CSV file...")
        write.csv(export_data, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        
        message("✅ PAR_2024 CSV file written successfully")
        
        # Show success notification
        showNotification("Datos descargados exitosamente", type = "message")
        
      }, error = function(e) {
        # Handle error if data cannot be loaded
        error_msg <- paste("Error al exportar datos PAR_2024:", e$message)
        message("❌ ", error_msg)
        showNotification(error_msg, type = "error", duration = 5)
        
        # Create a simple CSV with error message for user to know something went wrong
        error_data <- data.frame(
          ERROR = "Los datos no pudieron ser cargados. Por favor contacte al administrador.",
          DETAILS = e$message,
          TIMESTAMP = Sys.time()
        )
        write.csv(error_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      })
    }
  )
  
  # Handler for downloading 2023 surveys
  output$download_per_2023 <- downloadHandler(
    filename = function() {
      paste0("PER_2023_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      message("🔧 Starting PER_2023 download...")
      
      # Try to load the survey data
      tryCatch({
        # Load survey data using the existing function
        message("📊 Loading PER_2023 survey data...")
        per_data <- load_survey_data("PER_2023")
        
        if (is.null(per_data)) {
          stop("Failed to load PER_2023 data - load_survey_data returned NULL")
        }
        
        message(sprintf("📊 PER_2023 data loaded successfully with %d responses", nrow(per_data$responses)))
        
        # Prepare data for export
        export_data <- prepare_download_data(per_data)
        
        message(sprintf("📊 Export data prepared with %d rows and %d columns", nrow(export_data), ncol(export_data)))
        
        # Add any necessary metadata columns
        if (!"SURVEY_ID" %in% names(export_data)) {
          export_data$SURVEY_ID <- paste0("PER_2023_", seq_len(nrow(export_data)))
        }
        
        # Write to CSV file
        message("📊 Writing CSV file...")
        write.csv(export_data, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        
        message("✅ PER_2023 CSV file written successfully")
        
        # Show success notification
        showNotification("Datos descargados exitosamente", type = "message")
        
      }, error = function(e) {
        # Handle error if data cannot be loaded
        error_msg <- paste("Error al exportar datos PER_2023:", e$message)
        message("❌ ", error_msg)
        showNotification(error_msg, type = "error", duration = 5)
        
        # Create a simple CSV with error message for user to know something went wrong
        error_data <- data.frame(
          ERROR = "Los datos no pudieron ser cargados. Por favor contacte al administrador.",
          DETAILS = e$message,
          TIMESTAMP = Sys.time()
        )
        write.csv(error_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      })
    }
  )
  
  output$download_par_2023 <- downloadHandler(
    filename = function() {
      paste0("PAR_2023_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      message("🔧 Starting PAR_2023 download...")
      
      # Try to load the survey data
      tryCatch({
        # Load survey data using the existing function
        message("📊 Loading PAR_2023 survey data...")
        par_data <- load_survey_data("PAR_2023")
        
        if (is.null(par_data)) {
          stop("Failed to load PAR_2023 data - load_survey_data returned NULL")
        }
        
        message(sprintf("📊 PAR_2023 data loaded successfully with %d responses", nrow(par_data$responses)))
        
        # Prepare data for export
        export_data <- prepare_download_data(par_data)
        
        message(sprintf("📊 Export data prepared with %d rows and %d columns", nrow(export_data), ncol(export_data)))
        
        # Add any necessary metadata columns
        if (!"SURVEY_ID" %in% names(export_data)) {
          export_data$SURVEY_ID <- paste0("PAR_2023_", seq_len(nrow(export_data)))
        }
        
        # Write to CSV file
        message("📊 Writing CSV file...")
        write.csv(export_data, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        
        message("✅ PAR_2023 CSV file written successfully")
        
        # Show success notification
        showNotification("Datos descargados exitosamente", type = "message")
        
      }, error = function(e) {
        # Handle error if data cannot be loaded
        error_msg <- paste("Error al exportar datos PAR_2023:", e$message)
        message("❌ ", error_msg)
        showNotification(error_msg, type = "error", duration = 5)
        
        # Create a simple CSV with error message for user to know something went wrong
        error_data <- data.frame(
          ERROR = "Los datos no pudieron ser cargados. Por favor contacte al administrador.",
          DETAILS = e$message,
          TIMESTAMP = Sys.time()
        )
        write.csv(error_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      })
    }
  )
  
  # Render survey information for each survey
  # 2024 Surveys
  output$per_2024_info <- renderText({
    req(metadata_values$per_2024_meta)
    if (!is.null(metadata_values$per_2024_meta$total_respondents)) {
      sprintf("Tamaño de muestra: %d encuestados", metadata_values$per_2024_meta$total_respondents)
    } else {
      "Información no disponible"
    }
  })
  
  output$par_2024_info <- renderText({
    req(metadata_values$par_2024_meta)
    if (!is.null(metadata_values$par_2024_meta$total_respondents)) {
      sprintf("Tamaño de muestra: %d encuestados", metadata_values$par_2024_meta$total_respondents)
    } else {
      "Información no disponible"
    }
  })
  
  # 2023 Surveys
  output$per_2023_info <- renderText({
    req(metadata_values$per_2023_meta)
    if (!is.null(metadata_values$per_2023_meta$total_respondents)) {
      sprintf("Tamaño de muestra: %d encuestados", metadata_values$per_2023_meta$total_respondents)
    } else {
      "Información no disponible"
    }
  })
  
  output$par_2023_info <- renderText({
    req(metadata_values$par_2023_meta)
    if (!is.null(metadata_values$par_2023_meta$total_respondents)) {
      sprintf("Tamaño de muestra: %d encuestados", metadata_values$par_2023_meta$total_respondents)
    } else {
      "Información no disponible"
    }
  })
  
  # Data dictionary modal handlers for 2024 surveys
  observeEvent(input$view_per_2024_metadata, {
    message("🔍 PER 2024 metadata button clicked")
    
    tryCatch({
      # Load PER_2024 dictionary
      metadata_values$current_dictionary <- create_data_dictionary("PER_2024")
      metadata_values$current_survey_id <- "PER_2024"
      
      message(sprintf("📊 PER 2024 dictionary created with %d rows", nrow(metadata_values$current_dictionary)))
      
      # Debug: Check the data structure being sent
      message("🔍 Data structure check:")
      message(sprintf("  - Dictionary class: %s", class(metadata_values$current_dictionary)))
      message(sprintf("  - Dictionary columns: %s", paste(names(metadata_values$current_dictionary), collapse = ", ")))
      message(sprintf("  - First row: %s", paste(metadata_values$current_dictionary[1,], collapse = " | ")))
      
      # Convert data frame to list of lists for JSON serialization
      data_list <- lapply(1:nrow(metadata_values$current_dictionary), function(i) {
        row <- metadata_values$current_dictionary[i,]
        list(
          variable = as.character(row$variable),
          label = as.character(row$label),
          value_labels = as.character(row$value_labels),
          scale_type = as.character(row$scale_type)
        )
      })
      
      message(sprintf("🔍 Converted to list with %d items", length(data_list)))
      
      # Send custom message to show the full data dictionary modal
      session$sendCustomMessage("show-full-data-dictionary-modal", list(
        title = "Diccionario de Datos - Encuesta de Percepción Ciudadana 2024",
        survey_id = "PER_2024",
        row_count = nrow(metadata_values$current_dictionary),
        data = data_list
      ))
      message("✅ PER 2024 full modal message sent via sendCustomMessage")
      
    }, error = function(e) {
      message("❌ Error with PER 2024 modal: ", e$message)
      showNotification("Error al cargar el diccionario de datos", type = "error")
    })
  })
  
  observeEvent(input$view_par_2024_metadata, {
    message("🔍 PAR 2024 metadata button clicked")
    
    tryCatch({
      # Load PAR_2024 dictionary
      metadata_values$current_dictionary <- create_data_dictionary("PAR_2024")
      metadata_values$current_survey_id <- "PAR_2024"
      
      message(sprintf("📊 PAR 2024 dictionary created with %d rows", nrow(metadata_values$current_dictionary)))
      
      # Convert data frame to list of lists for JSON serialization
      data_list <- lapply(1:nrow(metadata_values$current_dictionary), function(i) {
        row <- metadata_values$current_dictionary[i,]
        list(
          variable = as.character(row$variable),
          label = as.character(row$label),
          value_labels = as.character(row$value_labels),
          scale_type = as.character(row$scale_type)
        )
      })
      
      # Send custom message to show the full data dictionary modal
      session$sendCustomMessage("show-full-data-dictionary-modal", list(
        title = "Diccionario de Datos - Encuesta de Participación Ciudadana 2024",
        survey_id = "PAR_2024",
        row_count = nrow(metadata_values$current_dictionary),
        data = data_list
      ))
      message("✅ PAR 2024 full modal message sent via sendCustomMessage")
      
    }, error = function(e) {
      message("❌ Error with PAR 2024 modal: ", e$message)
      showNotification("Error al cargar el diccionario de datos", type = "error")
    })
  })
  
  # Data dictionary modal handlers for 2023 surveys
  observeEvent(input$view_per_2023_metadata, {
    message("🔍 PER 2023 metadata button clicked")
    
    tryCatch({
      # Load PER_2023 dictionary
      metadata_values$current_dictionary <- create_data_dictionary("PER_2023")
      metadata_values$current_survey_id <- "PER_2023"
      
      message(sprintf("📊 PER 2023 dictionary created with %d rows", nrow(metadata_values$current_dictionary)))
      
      # Convert data frame to list of lists for JSON serialization
      data_list <- lapply(1:nrow(metadata_values$current_dictionary), function(i) {
        row <- metadata_values$current_dictionary[i,]
        list(
          variable = as.character(row$variable),
          label = as.character(row$label),
          value_labels = as.character(row$value_labels),
          scale_type = as.character(row$scale_type)
        )
      })
      
      # Send custom message to show the full data dictionary modal
      session$sendCustomMessage("show-full-data-dictionary-modal", list(
        title = "Diccionario de Datos - Encuesta de Percepción Ciudadana 2023",
        survey_id = "PER_2023",
        row_count = nrow(metadata_values$current_dictionary),
        data = data_list
      ))
      message("✅ PER 2023 full modal message sent via sendCustomMessage")
      
    }, error = function(e) {
      message("❌ Error with PER 2023 modal: ", e$message)
      showNotification("Error al cargar el diccionario de datos", type = "error")
    })
  })
  
  observeEvent(input$view_par_2023_metadata, {
    message("🔍 PAR 2023 metadata button clicked")
    
    tryCatch({
      # Load PAR_2023 dictionary
      metadata_values$current_dictionary <- create_data_dictionary("PAR_2023")
      metadata_values$current_survey_id <- "PAR_2023"
      
      message(sprintf("📊 PAR 2023 dictionary created with %d rows", nrow(metadata_values$current_dictionary)))
      
      # Convert data frame to list of lists for JSON serialization
      data_list <- lapply(1:nrow(metadata_values$current_dictionary), function(i) {
        row <- metadata_values$current_dictionary[i,]
        list(
          variable = as.character(row$variable),
          label = as.character(row$label),
          value_labels = as.character(row$value_labels),
          scale_type = as.character(row$scale_type)
        )
      })
      
      # Send custom message to show the full data dictionary modal
      session$sendCustomMessage("show-full-data-dictionary-modal", list(
        title = "Diccionario de Datos - Encuesta de Participación Ciudadana 2023",
        survey_id = "PAR_2023",
        row_count = nrow(metadata_values$current_dictionary),
        data = data_list
      ))
      message("✅ PAR 2023 full modal message sent via sendCustomMessage")
      
    }, error = function(e) {
      message("❌ Error with PAR 2023 modal: ", e$message)
      showNotification("Error al cargar el diccionario de datos", type = "error")
    })
  })
  
  # Handler for downloading data dictionary
  output$download_dictionary <- downloadHandler(
    filename = function() {
      req(metadata_values$current_survey_id)
      paste0(metadata_values$current_survey_id, "_Diccionario_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(metadata_values$current_dictionary)
      write.csv(metadata_values$current_dictionary, file, row.names = FALSE)
    }
  )
  
  # Listen for tab changes to update content
  observeEvent(input$download_year_tabs, {
    # This could be used to provide year-specific information or functionality
    # For now, we'll just use it to log the selection
    selected_year <- input$download_year_tabs
    message(paste("Selected download year tab:", selected_year))
  })
}