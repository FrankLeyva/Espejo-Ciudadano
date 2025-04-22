# R/extra/simplified_dashboard_map.R
# A highly simplified implementation that works with minimal dependencies

# UI function
dashboardMapUI <- function(id) {
  ns <- NS(id)
  
  page_fluid(
    class = "section-extras",
    useShinyjs(),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Mapa del Dashboard", class = "text-center")
        )
      )
    ),
    
    # Introduction text
    card(
      card_body(
        div(
          class = "text-center mb-4",
          p("Este mapa proporciona una visión general de todos los contenidos disponibles en el dashboard Espejo Ciudadano."),
          p("Seleccione una sección para ver su contenido.")
        )
      )
    ),
    
    # Simple section selection with nicer UI
    card(
      card_header(
        div(
          style = "display: flex; align-items: center; justify-content: space-between;",
          h3("Navegación del Dashboard", style = "margin: 0; font-size: 1.25rem;"),
          div(
            style = "background-color: #f8f9fa; padding: 5px 10px; border-radius: 4px; font-size: 0.85rem;",
            "Seleccione una sección para ver su contenido"
          )
        )
      ),
      card_body(
        div(
          style = "margin-bottom: 20px;",
          selectInput(
            ns("selected_section"),
            label = div(
              style = "font-weight: bold; font-size: 1.1rem; margin-bottom: 8px;",
              "Seleccionar Sección del Dashboard:"
            ),
            choices = c(
              "Bienestar Social y Económico" = "bienestar",
              "Movilidad Urbana" = "movilidad",
              "Gobierno" = "gobierno",
              "Infraestructura" = "infraestructura",
              "Participación" = "participacion",
              "Extras" = "extras"
            ),
            width = "100%",
            selectize = TRUE
          )
        ),
        uiOutput(ns("section_contents"))
      )
    ),
    
    # Footer with a link to return to home
    div(
      class = "d-flex justify-content-center mt-5",
      actionButton(
        inputId = ns("go_to_home"),
        label = "Volver a Inicio",
        class = "btn btn-outline-primary",
        onclick = "Shiny.setInputValue('nav_target', 'overview', {priority: 'event'})"
      )
    ),
    
    # Generic dashboard footer
    create_dashboard_footer()
  )
}

# Server function
dashboardMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create dashboard structure
    dashboard_structure <- reactive({
      # This structure represents the entire dashboard hierarchy
      structure <- list(
        bienestar = list(
          title = "Bienestar Social y Económico",
          color = "#1E88E5",
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
          color = "#43A047",
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
          color = "#E57C00",
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
          color = "#F57C00",
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
          color = "#7E57C2",
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
    
    # Render section contents based on selection
    output$section_contents <- renderUI({
      req(input$selected_section)
      structure <- dashboard_structure()
      section_key <- input$selected_section
      section <- structure[[section_key]]
      
      # Create a card for each subsection
      subsection_cards <- lapply(section$sections, function(subsection) {
        card(
          style = paste0("border-left: 4px solid ", section$color, "; margin-bottom: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);"),
          card_header(
            style = paste0("background-color: rgba(", 
                         paste(col2rgb(section$color), collapse = ","),
                         ", 0.1); border-bottom: 1px solid rgba(", 
                         paste(col2rgb(section$color), collapse = ","),
                         ", 0.2);"),
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              h4(subsection$title, class = "m-0"),
              actionButton(
                inputId = ns(paste0("goto_", subsection$value)),
                label = div(
                  style = "display: flex; align-items: center;",
                  "Ir a sección ", 
                  tags$span(HTML("&nbsp;&#8594;"), style = "margin-left: 4px;")
                ),
                class = "btn btn-sm btn-link",
                style = "padding: 2px 8px; color: #0275d8; text-decoration: none; font-size: 0.85rem; border: none;",
                onclick = sprintf("window.scrollTo(0, 0); Shiny.setInputValue('nav_target', '%s', {priority: 'event'});", 
                                subsection$value)
              )
            )
          ),
          card_body(
            style = "padding: 15px;",
            h5("Visualizaciones:", style = "font-size: 1rem; margin-bottom: 10px; color: #555;"),
            tags$ul(
              style = "margin-bottom: 0; padding-left: 20px;",
              lapply(subsection$visualizations, function(viz) {
                tags$li(viz, style = "margin-bottom: 5px;")
              })
            )
          )
        )
      })
      
      div(
        style = paste0("border-top: 4px solid ", section$color, "; background-color: #fff; padding: 20px; border-radius: 5px;"),
        class = "mt-4",
        div(
          style = paste0("display: flex; align-items: center; margin-bottom: 25px; padding-bottom: 15px; border-bottom: 1px solid ", 
                       "rgba(", paste(col2rgb(section$color), collapse = ","), ", 0.3);"),
          div(
            style = paste0("width: 10px; height: 40px; background-color: ", section$color, "; margin-right: 15px; border-radius: 3px;")
          ),
          h3(section$title, 
             style = paste0("color: ", section$color, "; margin: 0; font-weight: 600;"))
        ),
        div(
          class = "subsection-list",
          subsection_cards
        )
      )
    })
  })
}