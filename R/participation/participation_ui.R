# UI para Dashboard de Participación
participationUI <- function() {
  page_fluid(
    useShinyjs(),
      
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        .nav-card {
          transition: transform 0.3s, box-shadow 0.3s;
          cursor: pointer;
          height: 100%;
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
          h2("Participación Ciudadana", class = "text-center")
        )
      )
    ),
    
    # Navigation Cards
    layout_columns(
      col_widths = c(6, 6),
      
      # Civic Participation Card
      div(
        class = "nav-card",
        id = "nav_civic_card",
        onclick = "Shiny.setInputValue('nav_target', 'civic', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-primary", 
                bsicons::bs_icon("people")),
            h4(class = "nav-card-title text-center", "Participación Cívica"),
            p(class = "text-center", "Análisis de interés político, conocimiento de mecanismos y factores necesarios para participar")
          )
        )
      ),
      
      # Community Participation Card
      div(
        class = "nav-card",
        id = "nav_community_card",
        onclick = "Shiny.setInputValue('nav_target', 'community', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-success", 
                bsicons::bs_icon("building")),
            h4(class = "nav-card-title text-center", "Participación Comunitaria"),
            p(class = "text-center", "Análisis de participación en organizaciones y actividades para resolución de problemas")
          )
        )
      )
    ),
    
    # Active Social Movement Support (Callout Box)
    layout_columns(
      col_widths = 12,
      value_box(
        title = "Apoyo activo a movimientos sociales en 2024",
        value = textOutput("social_movement_support"),
        showcase = bsicons::bs_icon("megaphone-fill"),
        theme = value_box_theme(bg = "#E76F51", fg = "white")
      )
    ),
    
    # Voting Map Section
    card(
      card_header("Importancia del voto por distrito"),
      p("Porcentaje de personas que consideran que votar es importante o poco importante"),
      leafletOutput("voting_map", height = "500px")
    ),
    
    # Political Interest Pie Chart
    card(
      card_header("Interés en participar en política municipal"),
      p("Distribución del interés en participar en la política municipal, interviniendo en decisiones o realizando acciones de interés público"),
      plotlyOutput("interest_pie", height = "400px")
    )
  )
}