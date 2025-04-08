
wellnessUI <- function() {
  bienestar_theme <- get_section_theme("bienestar")

  page_fluid(
    useShinyjs(),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        style = paste0("border-top: 4px solid ", bienestar_theme$colors$primary, ";"),
        card_header(
          h2("Bienestar", class = "text-center", 
             style = paste0("color: ", bienestar_theme$colors$primary, ";"))
        )
      )
    ),
    
    # Navigation cards section
    layout_columns(
      col_widths = c(3, 3, 3,3),
      
      # Economy card
      div(
        class = "nav-card",
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
          "Percepción de situación económica personal",
          class = "bg-light"
        ),
        plotlyOutput("economic_situation_pie", height = "450px"),
        card_footer(
          "Comparación de situación económica personal respecto al año anterior (Q4)"
        )
      ),
      
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            h4("Frecuencia con que piensa en irse de la ciudad", class = "m-0"),
            downloadButton(
              "download_migration_map", 
              "", 
              icon = icon("download"), 
              class = "btn-sm"
            )
          ),
          class = "bg-light"
        ),
        leafletOutput("migration_intention_map", height = "450px"),
        card_footer(
          "Porcentaje de personas que pensaron irse de la ciudad muchas o algunas veces (Q63)"
        )
      )
    ),
    
    # Content section 2: Activities chart
    layout_columns(
      card(
        card_header(
          "Actividades realizadas en los últimos 3 meses",
          class = "bg-light"
        ),
        plotlyOutput("activities_chart", height = "550px"),
        card_footer(
          "Porcentaje de personas que reportan haber realizado cada actividad (Q16.1-Q16.15)"
        )
      )
    )
  )
}
