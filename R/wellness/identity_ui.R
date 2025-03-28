
# identity_ui.R

identityUI <- function() {
  page_fluid(
    useShinyjs(),
    
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      )
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
        onclick = "Shiny.setInputValue('nav_target', 'wellness', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Bienestar Social y Económico"
      )
    ),
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Identidad y Pertenencia", class = "text-center")
        )
      )
    ),
    
    # City monuments wordcloud and city pride pie chart
    layout_columns(
      col_widths = c(6, 6),
      
      # Wordcloud for city monuments
      card(
        card_header(
          "Monumentos importantes para la identidad juarense",
          class = "bg-light"
        ),
        htmlOutput("monuments_wordcloud"),
        card_footer(
          "Frecuencia de menciones de monumentos representativos de Ciudad Juárez (Q65H)"
        )
      ),
      
      # City pride pie chart
      card(
        card_header(
          "Orgullo por vivir en Ciudad Juárez",
          class = "bg-light"
        ),
        plotlyOutput("city_pride_pie", height = "400px"),
        card_footer(
          "Distribución de respuestas sobre el orgullo de ser o vivir en Ciudad Juárez (Q80)"
        )
      )
    ),
    
    # Maps of neighborhood and neighbors connection
    card(
      card_header(
        "Sentido de pertenencia",
        class = "bg-light"
      ),
      tabsetPanel(
        tabPanel(
          "Vínculo con la colonia o fraccionamiento",
          leafletOutput("neighborhood_connection_map", height = "500px"),
          p("Porcentaje de personas que se sienten bastante o muy vinculadas con su colonia o fraccionamiento (Q64.2)", 
            class = "mt-2 text-muted text-center")
        ),
        tabPanel(
          "Vínculo con los vecinos",
          leafletOutput("neighbors_connection_map", height = "500px"),
          p("Porcentaje de personas que se sienten bastante o muy vinculadas con sus vecinos (Q64.3)",
            class = "mt-2 text-muted text-center")
        )
      )
    ),
    
    # Summary statistics
    layout_columns(
      col_widths = c(4, 4, 4),
      
      # Statistics about neighborhood connection
      value_box(
        title = "Vínculo con la colonia",
        value = textOutput("neighborhood_connection_pct"),
        showcase = bsicons::bs_icon("houses-fill"),
        theme = value_box_theme(bg = "#2A9D8F", fg = "white")
      ),
      
      # Statistics about neighbors connection
      value_box(
        title = "Vínculo con los vecinos",
        value = textOutput("neighbors_connection_pct"),
        showcase = bsicons::bs_icon("people-fill"),
        theme = value_box_theme(bg = "#E9C46A", fg = "white")
      ),
      
      # Statistics about city pride
      value_box(
        title = "Orgullo de vivir en Ciudad Juárez",
        value = textOutput("city_pride_pct"),
        showcase = bsicons::bs_icon("star-fill"),
        theme = value_box_theme(bg = "#6969B3", fg = "white")
      )
    )
    
  )
}
