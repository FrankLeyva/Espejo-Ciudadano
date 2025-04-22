# R/extra/dashboard_map_server.R
dashboardMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create dashboard structure
    dashboard_structure <- reactive({
      # This structure represents the entire dashboard hierarchy
      # It could be dynamically generated but for now we'll define it manually
      structure <- list(
        bienestar = list(
          title = "Bienestar Social y Económico",
          icon = "heart",
          color = "var(--bienestar-color)",
          sections = list(
            list(
              title = "Vista General",
              value = "wellness",
              visualizations = list(
                "Percepción de situación económica personal",
                "Frecuencia con que piensa en irse de la ciudad", 
                "Actividades realizadas en los últimos 3 meses"
              )
            ),
            list(
              title = "Condiciones Económicas", 
              value = "economic",
              visualizations = list(
                "Porcentaje de la población que considera que mejoró su economía",
                "Porcentaje de la población que considera si les alcanza/no les alcanza"
              )
            ),
            list(
              title = "Participación Cultural",
              value = "cultural",
              visualizations = list(
                "Actividades dentro del hogar",
                "Ejercicio o actividad física",
                "Contacto con la naturaleza",
                "Asistencia a actividades culturales",
                "Actividades de ocio y entretenimiento"
              )
            ),
            list(
              title = "Identidad y pertenencia",
              value = "identity",
              visualizations = list(
                "Monumentos importantes para la identidad juarense",
                "Orgullo por vivir en Ciudad Juárez",
                "Vínculo con la colonia o fraccionamiento",
                "Vínculo con los vecinos"
              )
            ),
            list(
              title = "Medio Ambiente",
              value = "environment",
              visualizations = list(
                "Satisfacción con Aspectos Ambientales",
                "Calidad del Aire",
                "Arbolado Urbano",
                "Limpieza de Calles",
                "Calidad del Agua",
                "Principales Problemas Ambientales por Colonia"
              )
            )
          )
        ),
        movilidad = list(
          title = "Movilidad Urbana",
          icon = "bus-alt",
          color = "var(--movilidad-color)",
          sections = list(
            list(
              title = "Vista General",
              value = "urban",
              visualizations = list(
                "Uso de Transporte Público (Trabajo)",
                "Uso de Vehículo Particular (General)",
                "Satisfacción con Camión/Rutera",
                "Satisfacción con Juárez Bus",
                "Satisfacción con aspectos ambientales"
              )
            ),
            list(
              title = "Movilidad",
              value = "mobility",
              visualizations = list(
                "Bicicletas por Hogar",
                "Vehículos Motorizados por Hogar",
                "Transporte al Trabajo",
                "Transporte General"
              )
            ),
            list(
              title = "Transporte Público",
              value = "transportation",
              visualizations = list(
                "Satisfacción con el Servicio de Transporte Público",
                "Camión/Rutera",
                "Juárez Bus",
                "Aspectos con los que no están satisfechos"
              )
            )
          )
        ),
        gobierno = list(
          title = "Gobierno",
          icon = "landmark",
          color = "var(--gobierno-color)",
          sections = list(
            list(
              title = "Vista General",
              value = "government",
              visualizations = list(
                "Conocimiento de Funcionarios Públicos",
                "Percepción de la Desigualdad",
                "Expectativas Ciudadanas sobre el Gobierno",
                "Problemas Importantes de Ciudad Juárez"
              )
            ),
            list(
              title = "Desigualdad",
              value = "inequality",
              visualizations = list(
                "Violación de derechos por distrito",
                "Percepción de la desigualdad por distrito",
                "Instituciones que contribuyen a reducir la desigualdad"
              )
            ),
            list(
              title = "Rendición de Cuentas",
              value = "accountability",
              visualizations = list(
                "Percepción sobre la Justicia en Juárez",
                "Percepción sobre Castigo a Servidores Públicos Corruptos",
                "Percepción sobre Actos de Corrupción en el Gobierno"
              )
            ),
            list(
              title = "Representación Política",
              value = "representation",
              visualizations = list(
                "Regidores: Representación de Intereses Ciudadanos",
                "Síndico(a): Representación de Intereses Ciudadanos",
                "Diputado(a) Local: Representación de Intereses Ciudadanos",
                "Diputado(a) Federal: Representación de Intereses Ciudadanos",
                "Conocimiento de Representantes por Distrito",
                "Conocimiento de Representantes Específicos"
              )
            ),
            list(
              title = "Expectativas",
              value = "expectations",
              visualizations = list(
                "Expectativas de los Ciudadanos sobre el Gobierno",
                "Comparación de Percepción Ciudadana por Nivel de Gobierno"
              )
            ),
            list(
              title = "Confianza Institucional",
              value = "trust",
              visualizations = list(
                "Confianza en las instituciones",
                "Niveles de confianza por distrito"
              )
            )
          )
        ),
        infraestructura = list(
          title = "Infraestructura",
          icon = "building",
          color = "var(--infraestructura-color)",
          sections = list(
            list(
              title = "Vista General",
              value = "infrastructure",
              visualizations = list(
                "Educación: Hogares con estudiantes por distrito",
                "Salud: Satisfacción con servicios de salud",
                "Servicios Públicos: Satisfacción por servicio",
                "Vivienda: Satisfacción por distrito"
              )
            ),
            list(
              title = "Servicios Públicos",
              value = "public_services",
              visualizations = list(
                "Servicio a evaluar",
                "Evaluación de áreas verdes y espacios públicos",
                "Suministro de Agua",
                "Servicio Eléctrico",
                "Recolección de Basura",
                "Reportes de Servicios Públicos"
              )
            ),
            list(
              title = "Educación",
              value = "education",
              visualizations = list(
                "Hogares con Estudiantes",
                "Satisfacción con Niveles Educativos",
                "Educación Básica",
                "Educación Media Superior",
                "Educación Superior",
                "Comparativa"
              )
            ),
            list(
              title = "Servicios de Salud",
              value = "healthcare",
              visualizations = list(
                "Satisfacción con los Servicios de Salud",
                "Servicios en General",
                "Instalaciones",
                "Tiempo de Atención",
                "Medicamentos",
                "Calidad de Servicio",
                "Distancia",
                "Proveedores de Servicios de Salud"
              )
            ),
            list(
              title = "Vivienda",
              value = "housing",
              visualizations = list(
                "Satisfacción con Aspectos de la Vivienda",
                "Calidad de Materiales",
                "Tamaño y Espacios",
                "Ubicación y Accesibilidad",
                "Comparativa"
              )
            )
          )
        ),
        participacion = list(
          title = "Participación",
          icon = "users",
          color = "var(--participacion-color)",
          sections = list(
            list(
              title = "Vista General",
              value = "participation",
              visualizations = list(
                "Apoyo activo a movimientos sociales en 2024",
                "Importancia del voto por distrito",
                "Interés en participar en política municipal"
              )
            ),
            list(
              title = "Participación Cívica",
              value = "civic",
              visualizations = list(
                "Interés en participar en política municipal por distrito",
                "¿Qué necesita para participar en asuntos públicos?",
                "Conocimiento de mecanismos de participación ciudadana"
              )
            ),
            list(
              title = "Participación Comunitaria",
              value = "community",
              visualizations = list(
                "Participación en organizaciones",
                "Actividades para resolver problemas comunitarios"
              )
            )
          )
        ),
        extras = list(
          title = "Extras",
          icon = "ellipsis-h",
          color = "#6c757d",
          sections = list(
            list(
              title = "Explorador de Encuesta",
              value = "explorer",
              visualizations = list(
                "Herramienta para exploración interactiva de preguntas de la encuesta"
              )
            ),
            list(
              title = "Metodología",
              value = "methodology",
              visualizations = list(
                "Descargar Datos de Encuestas",
                "Metodología de las Encuestas",
                "Notas técnicas sobre los datos"
              )
            ),
            list(
              title = "Acerca de",
              value = "about",
              visualizations = list(
                "Sobre el Dashboard",
                "Plan Estratégico de Juárez",
                "Información Técnica",
                "Contacto",
                "¿Cómo Utilizar Este Dashboard?"
              )
            ),
            list(
              title = "Mapa del Dashboard",
              value = "dashboard_map",
              visualizations = list(
                "Índice completo del dashboard"
              )
            )
          )
        )
      )
      
      return(structure)
    })
    
    # Render dashboard map
    output$dashboard_map <- renderUI({
      div("Test output")

    })
    
    # Implement search functionality
    observeEvent(input$search_input, {
      search_term <- tolower(input$search_input)
      
      if (nchar(search_term) < 3) {
        shinyjs::hide("search_results")
        return()
      }
      
      # Search through structure for matches
      structure <- dashboard_structure()
      results <- list()
      
      for (section_key in names(structure)) {
        section <- structure[[section_key]]
        
        # Check section title
        if (grepl(search_term, tolower(section$title))) {
          results[[length(results) + 1]] <- list(
            type = "section",
            title = section$title,
            path = section_key,
            value = section_key
          )
        }
        
        # Check subsections
        for (subsection in section$sections) {
          # Check subsection title
          if (grepl(search_term, tolower(subsection$title))) {
            results[[length(results) + 1]] <- list(
              type = "subsection",
              title = paste0(section$title, " > ", subsection$title),
              path = paste0(section_key, "/", subsection$value),
              value = subsection$value
            )
          }
          
          # Check visualizations
          for (viz in subsection$visualizations) {
            if (grepl(search_term, tolower(viz))) {
              results[[length(results) + 1]] <- list(
                type = "visualization",
                title = paste0(section$title, " > ", subsection$title, " > ", viz),
                path = paste0(section_key, "/", subsection$value, "/", gsub(" ", "-", tolower(viz))),
                value = subsection$value
              )
            }
          }
        }
      }
      
      # Display results
      if (length(results) > 0) {
        result_html <- lapply(results, function(result) {
          icon_class <- switch(result$type,
                       "section" = "folder",
                       "subsection" = "folder-symlink",
                       "visualization" = "graph-up")
          
          div(
            class = "search-result-item",
            tags$a(
              href = "#",
              class = "search-result-link",
              onclick = sprintf("Shiny.setInputValue('nav_target', '%s', {priority: 'event'}); return false;", result$value),
              div(
                class = "d-flex align-items-center",
                bsicons::bs_icon(icon_class, class = "me-2"),
                div(
                  class = "search-result-text",
                  p(class = "mb-0 search-result-title", result$title),
                  p(class = "mb-0 search-result-path text-muted small", result$path)
                )
              )
            )
          )
        })
        
        output$search_results <- renderUI({
          div(
            class = "search-results-container",
            div(
              class = "search-results-header d-flex justify-content-between",
              p(class = "mb-2", paste0("Resultados (", length(results), "):")),
              tags$a(
                href = "#",
                class = "clear-results",
                onclick = "$('#search_input').val(''); $(document).trigger('search-cleared'); return false;",
                "Limpiar"
              )
            ),
            div(
              class = "search-results-list",
              result_html
            )
          )
        })
        
        shinyjs::show("search_results")
      } else {
        output$search_results <- renderUI({
          div(
            class = "search-results-container",
            div(
              class = "search-results-header d-flex justify-content-between",
              p(class = "mb-2", "No se encontraron resultados")
            )
          )
        })
        
        shinyjs::show("search_results")
      }
    })
    
    # Hide search results when search is cleared
    observeEvent(input$clear_search, {
      shinyjs::hide("search_results")
    })
    
    # Also hide search results when input is cleared manually
    observeEvent(input$search_input, {
      if (input$search_input == "") {
        shinyjs::hide("search_results")
      }
    })
  })
}