# UI para Dashboard de Educación
educationUI <- function() {
  page_fluid(
    useShinyjs(),
      
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        /* General styling */
        .nav-tabs .nav-link.active {
          font-weight: bold;
          color: #0d6efd;
          background-color: #f8f9fa;
          border-bottom: 3px solid #0d6efd;
        }
        
        .nav-tabs .nav-link {
          color: #495057;
          transition: all 0.3s ease;
        }
        
        .nav-tabs .nav-link:hover {
          background-color: #f1f1f1;
        }
        
        /* Card styling */
        .card {
          margin-bottom: 20px;
          border-radius: 5px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        
        /* Info box */
        .info-box {
          padding: 15px;
          border-radius: 5px;
          margin-bottom: 15px;
          background-color: #f8f9fa;
        }
        
        .section-title {
          font-weight: 600;
          color: #495057;
          margin-bottom: 15px;
        }
        
        .badge-primary {
          background-color: #0d6efd;
          color: white;
          padding: 5px 10px;
          border-radius: 3px;
          font-size: 0.9rem;
        }
        
        .average-indicator {
          background-color: #333333;
          color: white;
          padding: 6px 12px;
          border-radius: 4px;
          font-weight: bold;
          display: inline-block;
          margin-top: 10px;
        }
      "))
    ),
    
    theme = bs_theme(
      version = 5,
      bootswatch = "litera",
      primary = "#0d6efd"
    ),
    
    # Encabezado
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Educación", class = "text-center")
        )
      )
    ),
    
    # Introducción
    card(
      div(
        class = "info-box",
        div(
          h4("Datos de Educación"),
          p("Esta sección presenta información sobre la distribución de estudiantes y la satisfacción con diferentes niveles educativos según la Encuesta de Percepción Ciudadana 2024."),
          p("Los niveles de satisfacción se miden en una escala del 1 al 10, donde 1 representa 'Nada satisfecho' y 10 representa 'Muy satisfecho'.")
        )
      )
    ),
    
    # Primera sección: Hogares con estudiantes (Q6)
    card(
      card_header(
        h4("Hogares con Estudiantes", class = "d-flex justify-content-between")
      ),
      p("Porcentaje de hogares que tienen al menos un estudiante en algún nivel educativo (básico, medio superior o superior) por distrito."),
      leafletOutput("students_map", height = "500px"),
      div(
        class = "d-flex justify-content-end mt-2",
        div(
          class = "average-indicator",
          "Promedio general: ",
          textOutput("students_avg", inline = TRUE)
        )
      )
    ),
    
    # Segunda sección: Satisfacción con niveles educativos (Q7, Q10, Q13)
    card(
      card_header(
        h4("Satisfacción con Niveles Educativos", class = "section-title")
      ),
      p("Evaluación de la satisfacción con diferentes niveles educativos por distrito."),
      
      navset_tab(
        id = "education_tabs",
        
        # Tab: Educación Básica (Q7)
        nav_panel(
          title = "Educación Básica",
          icon = bsicons::bs_icon("book"),
          
          card(
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center",
                span("Satisfacción con Educación Básica")
              ),
              class = "bg-light"
            ),
            leafletOutput("basic_education_map", height = "500px"),
            div(
              class = "d-flex justify-content-end mt-2",
              div(
                class = "average-indicator",
                "Promedio general: ",
                textOutput("basic_education_avg", inline = TRUE)
              )
            )
          )
        ),
        
        # Tab: Educación Media Superior (Q10)
        nav_panel(
          title = "Educación Media Superior",
          icon = bsicons::bs_icon("journal-text"),
          
          card(
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center",
                span("Satisfacción con Educación Media Superior")
              ),
              class = "bg-light"
            ),
            leafletOutput("highschool_education_map", height = "500px"),
            div(
              class = "d-flex justify-content-end mt-2",
              div(
                class = "average-indicator",
                "Promedio general: ",
                textOutput("highschool_education_avg", inline = TRUE)
              )
            )
          )
        ),
        
        # Tab: Educación Superior (Q13)
        nav_panel(
          title = "Educación Superior",
          icon = bsicons::bs_icon("mortarboard"),
          
          card(
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center",
                span("Satisfacción con Educación Superior")
              ),
              class = "bg-light"
            ),
            leafletOutput("college_education_map", height = "500px"),
            div(
              class = "d-flex justify-content-end mt-2",
              div(
                class = "average-indicator",
                "Promedio general: ",
                textOutput("college_education_avg", inline = TRUE)
              )
            )
          )
        ),
        
        # Tab: Comparativa
        nav_panel(
          title = "Comparativa",
          icon = bsicons::bs_icon("bar-chart"),
          
          card(
            card_header(
              "Comparación entre Niveles Educativos",
              class = "bg-light"
            ),
            plotlyOutput("education_comparison_plot", height = "450px"),
            p("Este gráfico muestra una comparación de la satisfacción promedio entre los distintos niveles educativos por distrito.")
          )
        )
      )
    ),
    
    # Pie de página
    card(
      p("Datos obtenidos de la Encuesta de Percepción Ciudadana 2024", class = "text-center text-muted"),
      p("Última actualización: Marzo 2025", class = "text-center text-muted")
    )
  )
}