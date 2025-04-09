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
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'infrastructure', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Infraestructura"
      )
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
    
    
    # Primera sección: Hogares con estudiantes (Q6)
    card(
      card_header(
        h4("Hogares con Estudiantes", class = "d-flex justify-content-between")
      ),
      leafletOutput("students_map", height = "500px")
    ),
    
    # Segunda sección: Satisfacción con niveles educativos (Q7, Q10, Q13)
    card(
      card_header(
        h4("Satisfacción con Niveles Educativos", class = "section-title")
      ),
      
      navset_tab(
        id = "education_tabs",
        
        # Tab: Educación Básica (Q7)
        nav_panel(
          title = "Educación Básica",
          icon = bsicons::bs_icon("book"),
          
          card(
            leafletOutput("basic_education_map", height = "500px")
          )
        ),
        
        # Tab: Educación Media Superior (Q10)
        nav_panel(
          title = "Educación Media Superior",
          icon = bsicons::bs_icon("journal-text"),
          
          card(
            
            leafletOutput("highschool_education_map", height = "500px")
          )
        ),
        
        # Tab: Educación Superior (Q13)
        nav_panel(
          title = "Educación Superior",
          icon = bsicons::bs_icon("mortarboard"),
          
          card(
           
            leafletOutput("college_education_map", height = "500px")
          )
        ),
        
        # Tab: Comparativa
        nav_panel(
          title = "Comparativa",
          icon = bsicons::bs_icon("bar-chart"),
          
          card(
            
            plotlyOutput("education_comparison_plot", height = "450px")          )
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