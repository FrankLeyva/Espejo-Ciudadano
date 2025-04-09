
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
        plotlyOutput("monuments_bar", height = "400px"),

      ),
      
      # City pride pie chart
      card(
        card_header(
          "Orgullo por vivir en Ciudad Juárez",
          class = "bg-light"
        ),
        plotlyOutput("city_pride_pie", height = "400px"),

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
            class = "mt-2 text-muted text-center")
        ,
        tabPanel(
          "Vínculo con los vecinos",
          leafletOutput("neighbors_connection_map", height = "500px"),
            class = "mt-2 text-muted text-center")
        )
      )
    )

}
