
wellnessUI <- function() {
  bienestar_theme <- get_section_theme("bienestar")

  page_fluid(
    class = "section-bienestar",
    useShinyjs(),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        style = "border-top: 4px solid var(--bienestar-color);", 
        card_header(
          h2("Bienestar", class = "text-center")
        )
      )
    ),
    
    # Navigation cards section
    layout_columns(
      col_widths = c(3, 3, 3,3),
      
      # Economy card
      div(
        class = "nav-card nav-card-economic",
        id = "nav_economic_card",
        onclick = "Shiny.setInputValue('nav_target', 'economic', {priority: 'event'})",
        card(
          
          card_body(
            div(class = "text-center nav-card-icon text-primary", 
                bsicons::bs_icon("cash-coin")),
            h4(class = "nav-card-title text-center", "Economía"),
            p(class = "text-center", "Análisis de condiciones económicas")
          )
        )
      ),
      
      # Cultural card
      div(
        class = "nav-card",
        id = "nav_cultural_card",
        onclick = "Shiny.setInputValue('nav_target', 'cultural', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-success", 
                bsicons::bs_icon("music-note-beamed")),
            h4(class = "nav-card-title text-center", "Participación Cultural"),
            p(class = "text-center", "Análisis de actividades culturales")
          )
        )
      ),
      
      # Identity card
      div(
        class = "nav-card",
        id = "nav_identity_card",
        onclick = "Shiny.setInputValue('nav_target', 'identity', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-warning", 
                bsicons::bs_icon("geo-alt-fill")),
            h4(class = "nav-card-title text-center", "Identidad y Pertenencia"),
            p(class = "text-center", "Sentido de pertenencia a la ciudad")
          )
        )
      ),      # Environment Card
      div(
        class = "nav-card",
        id = "nav_environment_card",
        onclick = "Shiny.setInputValue('nav_target', 'environment', {priority: 'event'})",
        card(
          card_body(
            div(class = "text-center nav-card-icon text-success", bsicons::bs_icon("tree-fill")),
            h4(class = "nav-card-title text-center", "Medio Ambiente"),
            p(class = "text-center", "Calidad ambiental")
          )
        )
      )
    ),
    
    # Content section 1: Economic situation and Migration intention
    layout_columns(
      col_widths = c(6, 6),
      
      # Economic situation pie chart
      card(
        card_header(
          "Percepción de situación económica personal"
        ),
        plotlyOutput("economic_situation_pie", height = "450px"),
      ),
      
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            "Frecuencia con que piensa en irse de la ciudad",
            downloadButton(
              "download_migration_map", 
              "", 
              icon = icon("download"), 
              class = "btn-sm"
            )
          )
        ),
        leafletOutput("migration_intention_map", height = "450px"),

      )
    ),
    
    # Content section 2: Activities chart
    layout_columns(
      card(
        card_header(
          "Actividades realizadas en los últimos 3 meses"
        ),
        plotlyOutput("activities_chart", height = "550px")
      )
    )
  )
}
