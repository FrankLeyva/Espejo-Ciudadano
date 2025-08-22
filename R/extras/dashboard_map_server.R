dashboardMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dashboard_structure <- reactive({
      structure <- list(
        bienestar = list(
          title = "Calidad de Vida",
          color = "#1E88E5",
          sections = list(
            list(
              title = "Vista General",
              value = "wellness",
              visualizations = list(
                "Percepción de situación económica personal",
                "Frecuencia con que piensa en irse de la ciudad", 
                "Actividades realizadas en los últimos 3 meses",
                "Educación: Hogares con estudiantes por distrito",
                "Salud: Satisfacción con servicios de salud"
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
            ), list(
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
          title = "Instituciones",
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
                "Servicios Públicos: Satisfacción por servicio",
                "Vivienda: Satisfacción por distrito",
                "Reportes de Servicios Públicos"
                
              )
            ),
            list(
              title = "Servicios Públicos",
              value = "public_services",
              visualizations = list(
                "Suministro de Agua",
                "Servicio Eléctrico",
                "Recolección de Basura"
                
              )
            ),
            list(
              title = "Equipamiento Público",
              value = "equipment",
              visualizations = list(
                "Evaluación de áreas verdes y espacios públicos",
                "Semaforización",
                "Calles y Pavimentación"
                
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
          style = "margin-bottom: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.07); border: 1px solid #e0e0e0; border-radius: 10px;",
          card_header(
            style = paste0("background-color: ", section$color, "; border-bottom: 1px solid #e0e0e0; display: flex; justify-content: space-between; align-items: center; border-radius: 10px 10px 0 0;"),
            h4(subsection$title, class = "m-0", style = "color: #fff; font-weight: 700; font-family: var(--font-display);"),
            actionButton(
              inputId = ns(paste0("goto_", subsection$value)),
              label = div(
                style = "display: flex; align-items: center; gap: 0.3rem;",
                "Ir a sección",
                tags$span(HTML("&#8594;"))
              ),
              class = "btn btn-primary btn-sm",
              style = "background: inherit; background-color: #fff0; color: #fff; font-weight: 600; font-size: 0.95rem; padding: 2px 16px; margin-left: 10px; border: none; box-shadow: 0 2px 6px rgba(0,0,0,0.08); background-color: inherit;",
              onclick = sprintf("window.scrollTo(0, 0); Shiny.setInputValue('nav_target', '%s', {priority: 'event'});", subsection$value)
            )
          ),
          card_body(
            style = "padding: 15px;",
            h5("Visualizaciones:", style = "font-size: 1.1rem; font-weight: 600; margin-bottom: 10px; color: #333;"),
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