# Server function for Methodology and Data Download Dashboard
methodologyServer <- function(input, output, session, current_theme = NULL) {
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
    # 2023 surveys
    metadata_values$per_2023_meta <- generate_survey_metadata("PER_2023")
    metadata_values$par_2023_meta <- generate_survey_metadata("PAR_2023")
    
    # 2024 surveys
    metadata_values$per_2024_meta <- generate_survey_metadata("PER_2024")
    metadata_values$par_2024_meta <- generate_survey_metadata("PAR_2024")
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
            label = ifelse(is.na(label), "", label),
            value_labels = ifelse(is.na(value_labels), "", value_labels),
            scale_type = ifelse(is.na(scale_type), "", scale_type)
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
      # Try to load the survey data
      tryCatch({
        # Load survey data using the existing function
        per_data <- load_survey_data("PER_2024")
        
        # Prepare data for export
        export_data <- prepare_download_data(per_data)
        
        # Add any necessary metadata columns
        if (!"SURVEY_ID" %in% names(export_data)) {
          export_data$SURVEY_ID <- paste0("PER_", seq_len(nrow(export_data)))
        }
        
        # Write to CSV file
        write.csv(export_data, file, row.names = FALSE, na = "")
        
        # Show success notification
        showNotification("Datos descargados exitosamente", type = "message")
        
      }, error = function(e) {
        # Handle error if data cannot be loaded
        showNotification(paste("Error al exportar datos:", e$message), 
                        type = "error", duration = 5)
        
        # Create a simple CSV with error message for user to know something went wrong
        error_data <- data.frame(
          ERROR = "Los datos no pudieron ser cargados. Por favor contacte al administrador."
        )
        write.csv(error_data, file, row.names = FALSE)
      })
    }
  )
  
  output$download_par_2024 <- downloadHandler(
    filename = function() {
      paste0("PAR_2024_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      # Try to load the survey data
      tryCatch({
        # Load survey data using the existing function
        par_data <- load_survey_data("PAR_2024")
        
        # Prepare data for export
        export_data <- prepare_download_data(par_data)
        
        # Add any necessary metadata columns
        if (!"SURVEY_ID" %in% names(export_data)) {
          export_data$SURVEY_ID <- paste0("PAR_", seq_len(nrow(export_data)))
        }
        
        # Write to CSV file
        write.csv(export_data, file, row.names = FALSE, na = "")
        
        # Show success notification
        showNotification("Datos descargados exitosamente", type = "message")
        
      }, error = function(e) {
        # Handle error if data cannot be loaded
        showNotification(paste("Error al exportar datos:", e$message), 
                        type = "error", duration = 5)
        
        # Create a simple CSV with error message for user to know something went wrong
        error_data <- data.frame(
          ERROR = "Los datos no pudieron ser cargados. Por favor contacte al administrador."
        )
        write.csv(error_data, file, row.names = FALSE)
      })
    }
  )
  
  # Handler for downloading 2023 surveys
  output$download_per_2023 <- downloadHandler(
    filename = function() {
      paste0("PER_2023_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      # Try to load the survey data
      tryCatch({
        # Load survey data using the existing function
        per_data <- load_survey_data("PER_2023")
        
        # Prepare data for export
        export_data <- prepare_download_data(per_data)
        
        # Add any necessary metadata columns
        if (!"SURVEY_ID" %in% names(export_data)) {
          export_data$SURVEY_ID <- paste0("PER_", seq_len(nrow(export_data)))
        }
        
        # Write to CSV file
        write.csv(export_data, file, row.names = FALSE, na = "")
        
        # Show success notification
        showNotification("Datos descargados exitosamente", type = "message")
        
      }, error = function(e) {
        # Handle error if data cannot be loaded
        showNotification(paste("Error al exportar datos:", e$message), 
                        type = "error", duration = 5)
        
        # Create a simple CSV with error message for user to know something went wrong
        error_data <- data.frame(
          ERROR = "Los datos no pudieron ser cargados. Por favor contacte al administrador."
        )
        write.csv(error_data, file, row.names = FALSE)
      })
    }
  )
  
  output$download_par_2023 <- downloadHandler(
    filename = function() {
      paste0("PAR_2023_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      # Try to load the survey data
      tryCatch({
        # Load survey data using the existing function
        par_data <- load_survey_data("PAR_2023")
        
        # Prepare data for export
        export_data <- prepare_download_data(par_data)
        
        # Add any necessary metadata columns
        if (!"SURVEY_ID" %in% names(export_data)) {
          export_data$SURVEY_ID <- paste0("PAR_", seq_len(nrow(export_data)))
        }
        
        # Write to CSV file
        write.csv(export_data, file, row.names = FALSE, na = "")
        
        # Show success notification
        showNotification("Datos descargados exitosamente", type = "message")
        
      }, error = function(e) {
        # Handle error if data cannot be loaded
        showNotification(paste("Error al exportar datos:", e$message), 
                        type = "error", duration = 5)
        
        # Create a simple CSV with error message for user to know something went wrong
        error_data <- data.frame(
          ERROR = "Los datos no pudieron ser cargados. Por favor contacte al administrador."
        )
        write.csv(error_data, file, row.names = FALSE)
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
    # Load PER_2024 dictionary
    metadata_values$current_dictionary <- create_data_dictionary("PER_2024")
    metadata_values$current_survey_id <- "PER_2024"
    
    # Show modal with data dictionary
    showModal(modalDialog(
      title = "Diccionario de Datos - Encuesta de Percepción Ciudadana 2024",
      size = "xl",
      
      # Render data dictionary as DT table
      DT::renderDataTable({
        req(metadata_values$current_dictionary)
        DT::datatable(
          metadata_values$current_dictionary,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = "120px", targets = 0),  # variable
              list(width = "200px", targets = 1),  # label
              list(width = "300px", targets = 2),  # value_labels
              list(width = "100px", targets = 3)   # scale_type
            ),
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel'),
            lengthMenu = list(c(15, 30, 50, 100, -1), c('15', '30', '50', '100', 'Todos'))
          ),
          rownames = FALSE,
          colnames = c("Variable", "Descripción", "Etiquetas de Valores", "Tipo de Escala"),
          filter = 'top',
          class = 'cell-border stripe'
        ) %>%
        DT::formatStyle(
          'scale_type',
          backgroundColor = DT::styleEqual(
            c("nominal", "ordinal", "interval", "ratio", "binary"),
            c("#e3f2fd", "#e8f5e9", "#fff8e1", "#fce4ec", "#ede7f6")
          )
        )
      }),
      
      # Modal footer with download and close buttons
      footer = tagList(
        downloadButton("download_dictionary", "Descargar Diccionario (CSV)", class = "btn-primary"),
        modalButton("Cerrar")
      ),
      
      easyClose = TRUE,
      fade = TRUE
    ))
  })
  
  observeEvent(input$view_par_2024_metadata, {
    # Load PAR_2024 dictionary
    metadata_values$current_dictionary <- create_data_dictionary("PAR_2024")
    metadata_values$current_survey_id <- "PAR_2024"
    
    # Show modal with data dictionary
    showModal(modalDialog(
      title = "Diccionario de Datos - Encuesta de Participación Ciudadana 2024",
      size = "xl",
      
      # Render data dictionary as DT table
      DT::renderDataTable({
        req(metadata_values$current_dictionary)
        DT::datatable(
          metadata_values$current_dictionary,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = "120px", targets = 0),  # variable
              list(width = "200px", targets = 1),  # label
              list(width = "300px", targets = 2),  # value_labels
              list(width = "100px", targets = 3)   # scale_type
            ),
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel'),
            lengthMenu = list(c(15, 30, 50, 100, -1), c('15', '30', '50', '100', 'Todos'))
          ),
          rownames = FALSE,
          colnames = c("Variable", "Descripción", "Etiquetas de Valores", "Tipo de Escala"),
          filter = 'top',
          class = 'cell-border stripe'
        ) %>%
        DT::formatStyle(
          'scale_type',
          backgroundColor = DT::styleEqual(
            c("nominal", "ordinal", "interval", "ratio", "binary"),
            c("#e3f2fd", "#e8f5e9", "#fff8e1", "#fce4ec", "#ede7f6")
          )
        )
      }),
      
      # Modal footer with download and close buttons
      footer = tagList(
        downloadButton("download_dictionary", "Descargar Diccionario (CSV)", class = "btn-primary"),
        modalButton("Cerrar")
      ),
      
      easyClose = TRUE,
      fade = TRUE
    ))
  })
  
  # Data dictionary modal handlers for 2023 surveys
  observeEvent(input$view_per_2023_metadata, {
    # Load PER_2023 dictionary
    metadata_values$current_dictionary <- create_data_dictionary("PER_2023")
    metadata_values$current_survey_id <- "PER_2023"
    
    # Show modal with data dictionary
    showModal(modalDialog(
      title = "Diccionario de Datos - Encuesta de Percepción Ciudadana 2023",
      size = "xl",
      
      # Render data dictionary as DT table
      DT::renderDataTable({
        req(metadata_values$current_dictionary)
        DT::datatable(
          metadata_values$current_dictionary,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = "120px", targets = 0),  # variable
              list(width = "200px", targets = 1),  # label
              list(width = "300px", targets = 2),  # value_labels
              list(width = "100px", targets = 3)   # scale_type
            ),
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel'),
            lengthMenu = list(c(15, 30, 50, 100, -1), c('15', '30', '50', '100', 'Todos'))
          ),
          rownames = FALSE,
          colnames = c("Variable", "Descripción", "Etiquetas de Valores", "Tipo de Escala"),
          filter = 'top',
          class = 'cell-border stripe'
        ) %>%
        DT::formatStyle(
          'scale_type',
          backgroundColor = DT::styleEqual(
            c("nominal", "ordinal", "interval", "ratio", "binary"),
            c("#e3f2fd", "#e8f5e9", "#fff8e1", "#fce4ec", "#ede7f6")
          )
        )
      }),
      
      # Modal footer with download and close buttons
      footer = tagList(
        downloadButton("download_dictionary", "Descargar Diccionario (CSV)", class = "btn-primary"),
        modalButton("Cerrar")
      ),
      
      easyClose = TRUE,
      fade = TRUE
    ))
  })
  
  observeEvent(input$view_par_2023_metadata, {
    # Load PAR_2023 dictionary
    metadata_values$current_dictionary <- create_data_dictionary("PAR_2023")
    metadata_values$current_survey_id <- "PAR_2023"
    
    # Show modal with data dictionary
    showModal(modalDialog(
      title = "Diccionario de Datos - Encuesta de Participación Ciudadana 2023",
      size = "xl",
      
      # Render data dictionary as DT table
      DT::renderDataTable({
        req(metadata_values$current_dictionary)
        DT::datatable(
          metadata_values$current_dictionary,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(
              list(width = "120px", targets = 0),  # variable
              list(width = "200px", targets = 1),  # label
              list(width = "300px", targets = 2),  # value_labels
              list(width = "100px", targets = 3)   # scale_type
            ),
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel'),
            lengthMenu = list(c(15, 30, 50, 100, -1), c('15', '30', '50', '100', 'Todos'))
          ),
          rownames = FALSE,
          colnames = c("Variable", "Descripción", "Etiquetas de Valores", "Tipo de Escala"),
          filter = 'top',
          class = 'cell-border stripe'
        ) %>%
        DT::formatStyle(
          'scale_type',
          backgroundColor = DT::styleEqual(
            c("nominal", "ordinal", "interval", "ratio", "binary"),
            c("#e3f2fd", "#e8f5e9", "#fff8e1", "#fce4ec", "#ede7f6")
          )
        )
      }),
      
      # Modal footer with download and close buttons
      footer = tagList(
        downloadButton("download_dictionary", "Descargar Diccionario (CSV)", class = "btn-primary"),
        modalButton("Cerrar")
      ),
      
      easyClose = TRUE,
      fade = TRUE
    ))
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