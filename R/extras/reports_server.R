# R/extras/reports_server.R - SIMPLIFIED TRADITIONAL PATTERN

reportsServer <- function(input, output, session, current_theme = NULL) {
  
  message("🚀 Reports server starting (simplified traditional pattern)")
  
  # Load reports data with error handling
  reports_data <- reactive({
    message("🔄 Loading reports data...")
    
    tryCatch({
      result <- load_reports_metadata(generate_thumbnails_if_missing = FALSE)
      message("✅ Successfully loaded ", nrow(result), " reports")
      
      # Debug: Print first few reports
      if (nrow(result) > 0) {
        message("First 3 reports:")
        for (i in 1:min(3, nrow(result))) {
          message("  ", i, ". ", result$title[i], " (", result$year[i], ")")
        }
      }
      
      return(result)
      
    }, error = function(e) {
      message("❌ Error loading reports: ", e$message)
      showNotification(
        paste("Error loading reports:", e$message), 
        type = "error",
        duration = 10
      )
      
      # Return empty structure
      return(data.frame(
        filename = character(0),
        title = character(0),
        category = character(0),
        year = numeric(0),
        type = character(0),
        description = character(0),
        thumbnail_path = character(0),
        has_thumbnail = logical(0),
        stringsAsFactors = FALSE
      ))
    })
  })
  
  # Update filter choices - DIRECT NAMESPACED ACCESS
  observe({
    req(reports_data())
    data <- reports_data()
    
    message("🔄 Updating filter choices with ", nrow(data), " reports")
    
    if (nrow(data) > 0) {
      # Update category choices - DIRECT APPROACH
      categories <- c("Todos", sort(unique(data$category)))
      updateSelectInput(session, "reports-category_filter", choices = categories)
      message("Categories updated: ", paste(categories, collapse = ", "))
      
      # Update year choices - DIRECT APPROACH  
      years <- c("Todos", sort(unique(data$year), decreasing = TRUE))
      updateSelectInput(session, "reports-year_filter", choices = years)
      message("Years updated: ", paste(years, collapse = ", "))
      
    } else {
      updateSelectInput(session, "reports-category_filter", choices = c("Todos"))
      updateSelectInput(session, "reports-year_filter", choices = c("Todos"))
      message("No data - using default choices")
    }
  })
  
  # Filtered data - DIRECT NAMESPACED ACCESS
  filtered_reports <- reactive({
    req(reports_data())
    data <- reports_data()
    
    message("🔍 Applying filters to ", nrow(data), " reports")
    
    if (nrow(data) == 0) {
      message("No data to filter")
      return(data)
    }
    
    original_count <- nrow(data)
    
    # Apply category filter - DIRECT ACCESS
    category_input <- input[["reports-category_filter"]]
    message("Category filter input: ", category_input)
    if (!is.null(category_input) && category_input != "Todos") {
      data <- data[data$category == category_input, ]
      message("After category filter: ", nrow(data), " reports")
    }
    
    # Apply year filter - DIRECT ACCESS
    year_input <- input[["reports-year_filter"]]
    message("Year filter input: ", year_input)
    if (!is.null(year_input) && year_input != "Todos") {
      year_value <- as.numeric(year_input)
      data <- data[data$year == year_value, ]
      message("After year filter: ", nrow(data), " reports")
    }
    
    # Apply type filter - DIRECT ACCESS
    type_input <- input[["reports-type_filter"]]
    message("Type filter input: ", type_input)
    if (!is.null(type_input) && type_input != "all") {
      data <- data[data$type == type_input, ]
      message("After type filter: ", nrow(data), " reports")
    }
    
    # Apply search filter - DIRECT ACCESS
    search_input <- input[["reports-search_text"]]
    message("Search filter input: ", search_input)
    if (!is.null(search_input) && nchar(trimws(search_input)) > 0) {
      search_terms <- tolower(trimws(search_input))
      data <- data[
        grepl(search_terms, tolower(data$title), fixed = TRUE) |
        grepl(search_terms, tolower(data$description), fixed = TRUE) |
        grepl(search_terms, tolower(data$category), fixed = TRUE),
      ]
      message("After search filter: ", nrow(data), " reports")
    }
    
    message("🎯 Final filtered results: ", nrow(data), " of ", original_count, " reports")
    return(data)
  })
  
  # Statistics overview - DIRECT NAMESPACED OUTPUT
  output[["reports-stats_overview"]] <- renderUI({
    req(reports_data())
    data <- reports_data()
    
    message("📊 Rendering stats overview for ", nrow(data), " reports")
    
    if (nrow(data) == 0) {
      return(
        fluidRow(
          column(12,
            div(
              class = "alert alert-warning text-center",
              style = "margin: 2rem;",
              h4("⚠️ No hay informes disponibles"),
              p("Verifica que los archivos PDF estén en la carpeta 'www/reports'")
            )
          )
        )
      )
    }
    
    total_reports <- nrow(data)
    
    # Calculate year span safely
    years <- unique(data$year)
    years <- years[!is.na(years)]
    
    year_span <- if (length(years) > 1) {
      paste(min(years), "-", max(years))
    } else if (length(years) == 1) {
      as.character(years[1])
    } else {
      "N/A"
    }
    
    categories_count <- length(unique(data$category))
    
    fluidRow(
      column(4,
        div(class = "stat-item",
          div(class = "stat-number", total_reports),
          div(class = "stat-label", "Informes Totales")
        )
      ),
      column(4,
        div(class = "stat-item",
          div(class = "stat-number", year_span),
          div(class = "stat-label", "Período Cubierto")
        )
      ),
      column(4,
        div(class = "stat-item",
          div(class = "stat-number", categories_count),
          div(class = "stat-label", "Categorías")
        )
      )
    )
  })
  
  # Results count - DIRECT NAMESPACED OUTPUT
  output[["reports-results_count"]] <- renderUI({
    req(filtered_reports(), reports_data())
    
    filtered_count <- nrow(filtered_reports())
    total_count <- nrow(reports_data())
    
    message("📝 Rendering results count: ", filtered_count, " of ", total_count)
    
    if (total_count == 0) {
      p("No hay informes disponibles", class = "text-muted mb-0")
    } else if (filtered_count == total_count) {
      p(
        paste("Mostrando", total_count, "informes"), 
        class = "text-success mb-0",
        style = "font-family: var(--font-primary);"
      )
    } else {
      p(
        paste("Mostrando", filtered_count, "de", total_count, "informes"), 
        class = "text-info mb-0",
        style = "font-family: var(--font-primary);"
      )
    }
  })
  
  # Main reports display - DIRECT NAMESPACED OUTPUT
  output[["reports-reports_display"]] <- renderUI({
    req(filtered_reports())
    data <- filtered_reports()
    total_data <- reports_data()
    
    message("🖼️ Rendering reports display for ", nrow(data), " reports")
    
    # Get view mode - DIRECT ACCESS
    view_mode <- input[["reports-view_mode"]]
    message("View mode: ", view_mode)
    
    # Check if we have any data at all
    if (nrow(total_data) == 0) {
      message("No total data available")
      return(
        div(
          class = "empty-state",
          div(class = "empty-state-icon", icon("folder-open")),
          h4("No hay informes en el sistema"),
          p("Los archivos PDF deben estar en la carpeta 'www/reports'")
        )
      )
    }
    
    # Check if filters eliminated everything
    if (nrow(data) == 0) {
      message("Filters eliminated all results")
      return(
        div(
          class = "empty-state",
          div(class = "empty-state-icon", icon("search")),
          h5("No se encontraron informes"),
          p("Intenta ajustar los filtros de búsqueda para encontrar más resultados.")
        )
      )
    }
    
    # Create the display
    tryCatch({
      if (!is.null(view_mode) && view_mode == "timeline") {
        message("Creating timeline view")
        result <- create_timeline_view(data, current_theme)
      } else {
        message("Creating grid view")
        result <- create_grid_view(data, current_theme)
      }
      
      message("✅ Successfully created reports display")
      return(result)
      
    }, error = function(e) {
      message("❌ Error creating reports display: ", e$message)
      return(
        div(
          class = "alert alert-danger",
          h5("Error al mostrar informes"),
          p("Error técnico: ", e$message)
        )
      )
    })
  })
  
  # Download catalog - DIRECT NAMESPACED OUTPUT
  output[["reports-download_catalog"]] <- downloadHandler(
    filename = function() {
      paste0("catalogo_informes_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- reports_data()
      
      if (nrow(data) > 0) {
        export_data <- data %>%
          select(title, category, year, type, description, filename, has_thumbnail) %>%
          arrange(desc(year), category, title)
        
        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        write.csv(
          data.frame(message = "No hay informes disponibles"), 
          file, 
          row.names = FALSE
        )
      }
    }
  )
  
  # Regenerate thumbnails observers - DIRECT ACCESS
  observeEvent(input[["reports-regenerate_thumbnails"]], {
    if (exists("check_thumbnail_dependencies") && 
        exists("generate_all_thumbnails")) {
      showModal(modalDialog(
        title = "Regenerar Miniaturas",
        "¿Estás seguro de que quieres regenerar todas las miniaturas? Esto puede tomar varios minutos.",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_regenerate", "Regenerar", class = "btn-primary")
        )
      ))
    } else {
      showNotification(
        "Funcionalidad de miniaturas no disponible. Instala los paquetes 'pdftools' y 'magick'.",
        type = "warning"
      )
    }
  })
  
  observeEvent(input$confirm_regenerate, {
    removeModal()
    
    if (exists("generate_all_thumbnails")) {
      withProgress(message = "Generando miniaturas...", value = 0, {
        tryCatch({
          result <- generate_all_thumbnails(force_regenerate = TRUE)
          showNotification(
            paste("Miniaturas regeneradas:", sum(result$success), "exitosas,", 
                  sum(!result$success), "fallidas"),
            type = if(all(result$success)) "message" else "warning"
          )
        }, error = function(e) {
          showNotification(
            paste("Error generando miniaturas:", e$message),
            type = "error"
          )
        })
      })
    }
  })
  
  message("✅ Reports server initialization complete (simplified traditional pattern)")
}

# Helper function to create grid view
create_grid_view <- function(data, current_theme = NULL) {
  message("📱 Creating grid view for ", nrow(data), " reports")
  
  if (nrow(data) == 0) return(div("No hay datos para mostrar"))
  
  # Sort data
  data <- data[order(-data$year, data$category, data$title), ]
  
  # Create cards
  cards <- lapply(1:nrow(data), function(i) {
    tryCatch({
      report <- data[i, ]
      create_report_card(report)
    }, error = function(e) {
      message("Error creating card ", i, ": ", e$message)
      div(class = "alert alert-warning", "Error cargando reporte ", i)
    })
  })
  
  div(class = "reports-grid", cards)
}

# Helper function to create timeline view
create_timeline_view <- function(data, current_theme = NULL) {
  message("📅 Creating timeline view for ", nrow(data), " reports")
  
  if (nrow(data) == 0) return(div("No hay datos para mostrar"))
  
  # Group by year
  years <- sort(unique(data$year), decreasing = TRUE)
  years <- years[!is.na(years)]
  
  timeline_content <- lapply(years, function(year) {
    year_data <- data[data$year == year & !is.na(data$year), ]
    year_data <- year_data[order(year_data$category, year_data$title), ]
    
    if (nrow(year_data) == 0) return(NULL)
    
    year_cards <- lapply(1:nrow(year_data), function(i) {
      tryCatch({
        report <- year_data[i, ]
        div(class = "timeline-item", create_report_card(report))
      }, error = function(e) {
        message("Error creating timeline card ", i, ": ", e$message)
        div(class = "alert alert-warning", "Error cargando reporte")
      })
    })
    
    tagList(
      div(class = "timeline-year", year),
      year_cards
    )
  })
  
  # Remove NULL elements
  timeline_content <- timeline_content[!sapply(timeline_content, is.null)]
  
  div(class = "timeline-container", timeline_content)
}

# Helper function to create individual report cards
create_report_card <- function(report) {
  
  # Category colors
  category_colors <- list(
    "Bienestar" = "#d7a4d2",
    "Calidad de Vida" = "#d7a4d2", 
    "Servicios Públicos" = "#00c5c1",
    "Infraestructura" = "#00c5c1",
    "Gobierno" = "#ff769d",
    "Instituciones" = "#ff769d",
    "Economía" = "#d7a4d2",
    "Salud" = "#d7a4d2",
    "Educación" = "#d7a4d2",
    "Movilidad" = "#FFA058",
    "Medio Ambiente" = "#d7a4d2",
    "Participación" = "#ffc9de",
    "General" = "#2d2d2d"
  )
  
  category_color <- category_colors[[report$category]]
  if (is.null(category_color)) category_color <- "#2d2d2d"
  
  # Create thumbnail section
  thumbnail_section <- if (!is.null(report$has_thumbnail) && 
                           report$has_thumbnail && 
                           !is.null(report$thumbnail_path) && 
                           nchar(report$thumbnail_path) > 0) {
    div(
      class = "report-thumbnail-container",
      img(
        src = report$thumbnail_path,
        alt = paste("Vista previa de", report$title),
        class = "report-thumbnail",
        onerror = "this.style.display='none'; this.nextElementSibling.style.display='flex';"
      ),
      # Fallback
      div(
        class = "report-thumbnail-fallback",
        style = "display: none;",
        icon("file-pdf", class = "fa-3x"),
        div("Vista previa no disponible", class = "mt-2 small text-muted")
      )
    )
  } else {
    div(
      class = "report-thumbnail-fallback",
      icon("file-pdf", class = "fa-3x"),
      div("Vista previa no disponible", class = "mt-2 small text-muted")
    )
  }
  
  # Check if file exists for download link
  file_path <- file.path("www/reports", report$filename)
  file_exists <- file.exists(file_path)
  
  download_button <- if (file_exists) {
    tags$a(
      href = paste0("reports/", report$filename),
      target = "_blank",
      class = "btn btn-primary btn-sm w-100 download-btn-report",
      icon("download"), " Descargar PDF"
    )
  } else {
    div(
      class = "btn btn-secondary btn-sm w-100 disabled",
      icon("exclamation-triangle"), " Archivo no encontrado"
    )
  }
  
  # Create the card
  div(
    class = "report-card position-relative",
    style = "position: relative;",
    
    # Thumbnail at the top
    thumbnail_section,
    
    div(
      style = "padding: 1.5rem; display: flex; flex-direction: column; height: calc(100% - 200px);",
      
      # Category badge
      div(
        class = "report-category-badge",
        style = paste0("background-color: ", category_color, "; color: white; padding: 0.3rem 0.6rem; border-radius: 15px; font-size: 0.75rem; font-weight: 600; margin-bottom: 0.75rem; display: inline-block;"),
        report$category
      ),
      
      # Title and year
      div(
        class = "d-flex justify-content-between align-items-start mb-2",
        div(
          class = "report-title flex-grow-1",
          style = "font-family: var(--font-display); font-size: 1.1rem; font-weight: 600; line-height: 1.3; margin-bottom: 0.75rem; color: var(--text-color);",
          report$title
        ),
        div(
          class = "report-year-badge",
          style = "background-color: rgba(45, 45, 45, 0.8); color: white; font-size: 0.7rem; padding: 0.25rem 0.5rem; border-radius: 10px; font-weight: 600;",
          report$year
        )
      ),
      
      # Description
      div(
        class = "report-description flex-grow-1",
        style = "font-size: 0.9rem; color: #6c757d; line-height: 1.5; margin-bottom: 1rem; display: -webkit-box; -webkit-line-clamp: 3; -webkit-box-orient: vertical; overflow: hidden;",
        report$description
      ),
      
      # Meta information
      div(
        class = "report-meta",
        style = "font-size: 0.75rem; color: #8e9ba3; margin-bottom: 1rem;",
        paste("Archivo:", report$filename, "• Tipo:", 
              ifelse(report$type == "informe", "Informe", "Presentación"))
      ),
      
      # Download button
      div(class = "mt-auto", download_button)
    )
  )
}