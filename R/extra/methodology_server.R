# Server function for Methodology and Data Download Dashboard
methodologyServer <- function(input, output, session) {
  # Load utility functions for metadata
  source("R/utils.R")
  
  # Reactive values to store metadata
  metadata_values <- reactiveValues(
    per_meta = NULL,
    par_meta = NULL,
    current_dictionary = NULL,
    current_survey_id = NULL
  )
  
  # Initialize metadata on load
  observe({
    metadata_values$per_meta <- generate_survey_metadata("PER_2024")
    metadata_values$par_meta <- generate_survey_metadata("PAR_2024")
  })
  
  # Handler for downloading PER_2024 data
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
  
  # Handler for downloading PAR_2024 data
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
  
  # Render survey information
  output$per_info <- renderText({
    req(metadata_values$per_meta)
    if (!is.null(metadata_values$per_meta$total_respondents)) {
      sprintf("Tamaño de muestra: %d encuestados", metadata_values$per_meta$total_respondents)
    } else {
      "Información no disponible"
    }
  })
  
  output$par_info <- renderText({
    req(metadata_values$par_meta)
    if (!is.null(metadata_values$par_meta$total_respondents)) {
      sprintf("Tamaño de muestra: %d encuestados", metadata_values$par_meta$total_respondents)
    } else {
      "Información no disponible"
    }
  })
  
  # Data dictionary modal handlers
  observeEvent(input$view_per_metadata, {
    # Load PER_2024 dictionary
    metadata_values$current_dictionary <- create_data_dictionary("PER_2024")
    metadata_values$current_survey_id <- "PER_2024"
    
    # Show modal with data dictionary
    showModal(modalDialog(
      title = "Diccionario de Datos - Encuesta de Percepción Ciudadana",
      size = "l",
      
      # Render data dictionary as DT table
      DT::renderDataTable({
        req(metadata_values$current_dictionary)
        DT::datatable(
          metadata_values$current_dictionary,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv')
          ),
          rownames = FALSE
        )
      }),
      
      # Modal footer with download and close buttons
      footer = tagList(
        downloadButton("download_dictionary", "Descargar Diccionario (CSV)"),
        modalButton("Cerrar")
      )
    ))
  })
  
  observeEvent(input$view_par_metadata, {
    # Load PAR_2024 dictionary
    metadata_values$current_dictionary <- create_data_dictionary("PAR_2024")
    metadata_values$current_survey_id <- "PAR_2024"
    
    # Show modal with data dictionary
    showModal(modalDialog(
      title = "Diccionario de Datos - Encuesta de Participación Ciudadana",
      size = "l",
      
      # Render data dictionary as DT table
      DT::renderDataTable({
        req(metadata_values$current_dictionary)
        DT::datatable(
          metadata_values$current_dictionary,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv')
          ),
          rownames = FALSE
        )
      }),
      
      # Modal footer with download and close buttons
      footer = tagList(
        downloadButton("download_dictionary", "Descargar Diccionario (CSV)"),
        modalButton("Cerrar")
      )
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
}