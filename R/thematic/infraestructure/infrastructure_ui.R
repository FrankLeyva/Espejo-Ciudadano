infrastructureUI <- function() {
  page_fluid(
    useShinyjs(),
    
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        /* Navigation card styles */
        .nav-card {
          transition: transform 0.3s, box-shadow 0.3s;
          cursor: pointer;
        }
        .nav-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 10px 20px rgba(0,0,0,0.1);
        }
        .nav-card-icon {
          font-size: 2rem;
          margin-bottom: 15px;
        }
        .nav-card-title {
          font-weight: bold;
          font-size: 1.2rem;
        }
        
        /* Section headers */
        .section-header {
          margin-top: 20px;
          margin-bottom: 15px;
          padding-left: 10px;
          border-left: 4px solid #0d6efd;
        }
      "))
    ),
    
    theme = bs_theme(
      version = 5,
      bootswatch = "litera",
      primary = "#0d6efd"
    ),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Panorama general de servicios públicos", class = "text-center")
        )
      )
    ),
    
    # Dashboard navigation section
    h3(class = "section-header", "Dashboards específicos"),
    
    layout_columns(
      col_widths = 3,
      
      # Education Dashboard Card
      div(
        class = "nav-card",
        id = "nav_education_card",  # Changed ID to match what overviewNavServer expects
        onclick = "Shiny.setInputValue('nav_education_card', true, {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-primary", 
                bsicons::bs_icon("book")),
            h4(class = "nav-card-title text-center", "Educación"),
            p(class = "text-center", "Análisis de indicadores educativos")
          )
        )
      ),
      
      # Healthcare Dashboard Card
      div(
        class = "nav-card",
        id = "nav_healthcare_card",  # Changed ID to match what overviewNavServer expects
        onclick = "Shiny.setInputValue('nav_healthcare_card', true, {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-danger", 
                bsicons::bs_icon("heart-pulse")),
            h4(class = "nav-card-title text-center", "Salud"),
            p(class = "text-center", "Estadísticas de servicios de salud")
          )
        )
      ),
      
      # Public Services Dashboard Card
      div(
        class = "nav-card",
        id = "nav_services_card",  # Changed ID to match what overviewNavServer expects
        onclick = "Shiny.setInputValue('nav_services_card', true, {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-success", 
                bsicons::bs_icon("gear")),
            h4(class = "nav-card-title text-center", "Servicios Públicos"),
            p(class = "text-center", "Evaluación de servicios e infraestructura")
          )
        )
      ),
      
      # Housing Dashboard Card
      div(
        class = "nav-card",
        id = "nav_housing_card",  # Changed ID to match what overviewNavServer expects
        onclick = "Shiny.setInputValue('nav_housing_card', true, {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-warning", 
                bsicons::bs_icon("house")),
            h4(class = "nav-card-title text-center", "Vivienda"),
            p(class = "text-center", "Análisis de condiciones de vivienda")
          )
        )
      )
    ),
    
    # Overview charts section
    h3(class = "section-header mt-5", "Indicadores clave"),
    
    # First row of indicators
    layout_columns(
      col_widths = c(6, 6),
      
      # Education Plot
      card(
        card_header("Educación: Hogares con estudiantes por distrito"),
        leafletOutput("education_plot", height = "400px")
      ),
      
      # Healthcare Plot
      card(
        card_header("Salud: Satisfacción con servicios de salud"),
        plotlyOutput("healthcare_plot", height = "400px")
      )
    ),
    
    # Second row of indicators
    layout_columns(
      col_widths = c(6, 6),
      
      # Utilities Plot
      card(
        card_header("Servicios Públicos: Satisfacción por servicio"),
        plotlyOutput("utilities_plot", height = "500px")
      ),
      
      # Housing Plot
      card(
        card_header("Vivienda: Satisfacción por distrito"),
        leafletOutput("housing_map", height = "500px")
      )
    )
  )
}