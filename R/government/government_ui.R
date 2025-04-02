# government_ui.R
governmentUI <- function() {
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
          h2("Panorama general de gobierno", class = "text-center")
        )
      )
    ),
    
    # Dashboard navigation section
    h3(class = "section-header", "Dashboards específicos"),
    
    layout_columns(
      col_widths = c(2.4,2.4,2.4,2.4,2.4),
      
      # Inequality Dashboard Card
      div(
        class = "nav-card",
        id = "nav_inequality_card",
        onclick = "Shiny.setInputValue('nav_target', 'inequality', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-primary", 
                bsicons::bs_icon("bar-chart")),
            h4(class = "nav-card-title text-center", "Desigualdad"),
            p(class = "text-center", "Análisis de indicadores de desigualdad")
          )
        )
      ),
      div(
        class = "nav-card",
        id = "nav_trust_card",
        onclick = "Shiny.setInputValue('nav_target', 'trust', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-primary", 
                bsicons::bs_icon("bar-chart")),
            h4(class = "nav-card-title text-center", "Confianza"),
            p(class = "text-center", "Análisis de indicadores de confianza en las instituciones")
          )
        )
      ),
      # Accountability Dashboard Card
      div(
        class = "nav-card",
        id = "nav_accountability_card",
        onclick = "Shiny.setInputValue('nav_target', 'accountability', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-danger", 
                bsicons::bs_icon("file-earmark-check")),
            h4(class = "nav-card-title text-center", "Rendición de Cuentas"),
            p(class = "text-center", "Estadísticas sobre transparencia y rendición")
          )
        )
      ),
      
      # Political Representation Dashboard Card
      div(
        class = "nav-card",
        id = "nav_political_card",
        onclick = "Shiny.setInputValue('nav_target', 'representation', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-success", 
                bsicons::bs_icon("person-vcard")),
            h4(class = "nav-card-title text-center", "Representación Política"),
            p(class = "text-center", "Evaluación de la representación política")
          )
        )
      ),
      
      # Government Expectations Dashboard Card
      div(
        class = "nav-card",
        id = "nav_gov_expectations_card",
        onclick = "Shiny.setInputValue('nav_target', 'expectations', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-warning", 
                bsicons::bs_icon("graph-up")),
            h4(class = "nav-card-title text-center", "Expectativas de Gobierno"),
            p(class = "text-center", "Análisis de las expectativas ciudadanas")
          )
        )
      )
    ),
    
    # Overview charts section
    h3(class = "section-header mt-5", "Indicadores clave"),
    
    # First row of indicators
    layout_columns(
      col_widths = c(6, 6),
      
      # Card 1: Knowledge of officials (tabset with pie charts)
      card(
        card_header("Conocimiento de Funcionarios Públicos"),
        tabsetPanel(
          tabPanel("Regidor/a", plotlyOutput("officials_knowledge_regidor_plot", height = "400px")),
          tabPanel("Síndico/a", plotlyOutput("officials_knowledge_sindico_plot", height = "400px")),
          tabPanel("Diputado/a Local y/o Estatal", plotlyOutput("officials_knowledge_diputadol_plot", height = "400px")),
          tabPanel("Diputado/a Federal", plotlyOutput("officials_knowledge_diputadof_plot", height = "400px"))
        )
      ),
      
      # Card 2: Perception of inequality
      card(
        card_header("Percepción de la Desigualdad"),
        plotlyOutput("inequality_perception_plot", height = "400px")
      )
    ),
    
    # Second row of indicators
    layout_columns(
      col_widths = c(6, 6),
      
      # Card 3: Government expectations
      card(
        card_header("Expectativas Ciudadanas sobre el Gobierno"),
        plotlyOutput("government_expectations_plot", height = "500px")
      ),
      
      # Card 4: Important problems
      card(
        card_header("Problemas Importantes de Ciudad Juárez"),
        plotlyOutput("important_problems_plot", height = "500px")
      )
    )
  )
}