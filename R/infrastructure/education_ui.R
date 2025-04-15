# UI para Dashboard de Educación
educationUI <- function() {
  page_fluid(
    class = "section-infraestructura",

    useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Override pill navigation styling for this page */
        .infraestructura-pills .nav-pills .nav-link:not(.active) {
          background-color: rgba(240, 240, 240, 0.8);
color: var(--infraestructura-color) !important;
            border: 1px solid rgba(229, 126, 30, 0.2);
          font-weight: bold !important;
        }
        
        .infraestructura-pills .nav-pills .nav-link:hover:not(.active) {
          background-color: rgba(160, 115, 67, 0.1);
        }
             .infraestructura-pills .nav-pills .nav-link.active {
          background-color: var(--infraestructura-color) !important; 
          color: white !important;
          font-weight: bold !important;
          border: none !important;
        }
      "))
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
          style="border-top: 4px solid var(--infraestructura-color)",

          h2("Educación", class = "text-center")
        )
      )
    ),
    
    
    # Primera sección: Hogares con estudiantes (Q6)
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
        "Hogares con Estudiantes",
        downloadButton(
          "download_students_map", 
          "", 
          icon = icon("download"), 
          class = "btn-sm"
        )
      )
      ),
      leafletOutput("students_map", height = "500px")
    ),
    
    # Segunda sección: Satisfacción con niveles educativos (Q7, Q10, Q13)
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
"Satisfacción con Niveles Educativos",
downloadButton(
  "download_edu_satis_map", 
  "", 
  icon = icon("download"), 
  class = "btn-sm"
))),
      div(class = "gobierno-pills",
      navset_pill(
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
    )
    ),
    
    # Pie de página
    card(
      p("Datos obtenidos de la Encuesta de Percepción Ciudadana 2024", class = "text-center text-muted"),
      p("Última actualización: Marzo 2025", class = "text-center text-muted")
    )
  )
}