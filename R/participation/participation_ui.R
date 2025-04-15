# UI para Dashboard de Participación
participationUI <- function() {
  page_fluid(
    class = "section-participacion",

    useShinyjs(),
      
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--participacion-color) !important; 
          color: white !important; 
          font-weight: bolder !important; 
          text-align: center !important; 
          border-bottom: none !important;"),
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
          class = "nav-card-participacion",
          card_body(
            div(class = "text-center nav-card-icon", 
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
          class = "nav-card-participacion",
          card_body(
            div(class = "text-center nav-card-icon", 
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
        theme = value_box_theme(bg = "#8E24AA", fg = "white")
      )
    ),
    
    # Voting Map Section
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          "Importancia del voto por distrito",
          downloadButton(
            "download_voting_map", 
            "", 
            icon = icon("download"), 
            class = "btn-sm"
          ))),
      leafletOutput("voting_map", height = "500px")
    ),
    
    # Political Interest Pie Chart
    card(
      card_header("Interés en participar en política municipal"),
            plotlyOutput("interest_pie", height = "400px")
    )
  )
}