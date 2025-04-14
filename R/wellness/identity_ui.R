
# identity_ui.R

identityUI <- function() {

  page_fluid(
    class = "section-bienestar",

    useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Override pill navigation styling for this page */
        .bienestar-pills .nav-pills .nav-link:not(.active) {
          background-color: rgba(240, 240, 240, 0.8);
color: var(--bienestar-color) !important;
            border: 1px solid rgba(30, 136, 229, 0.2);
          font-weight: bold !important;
        }
        
        .bienestar-pills .nav-pills .nav-link:hover:not(.active) {
          background-color: rgba(30, 136, 229, 0.1);
        }
             .bienestar-pills .nav-pills .nav-link.active {
          background-color: var(--bienestar-color) !important; /* Bienestar primary color */
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
        onclick = "Shiny.setInputValue('nav_target', 'wellness', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Bienestar Social y Económico"
      )
    ),
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Identidad y Pertenencia", class = "text-center"),
        )
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      card(

        card_header(
          "Monumentos importantes para la identidad juarense",
        ),
        plotlyOutput("monuments_bar", height = "400px"),
      ),
      
      # City pride pie chart

      card(

        card_header(
          "Orgullo por vivir en Ciudad Juárez",
        ),
        plotlyOutput("city_pride_pie", height = "400px"),

      )
    ),
   # Add to identity_ui.R
card(
  card_header(
    div(
      class = "d-flex justify-content-between align-items-center",
      "Sentido de pertenencia",
      downloadButton(
        "download_connection_map", 
        "", 
        icon = icon("download"), 
        class = "btn-sm"
      )
    )
  ),
  div(class = "bienestar-pills",
    navset_pill(
      id = "neighborhood_tabs",
      nav_panel(
        title = "Vínculo con la colonia o fraccionamiento",
        icon = bsicons::bs_icon("house-fill"),
        leafletOutput("neighborhood_connection_map", height = "500px")
      ),
      nav_panel(
        title = "Vínculo con los vecinos",
        icon = bsicons::bs_icon("people-fill"),
        leafletOutput("neighbors_connection_map", height = "500px")
      )
    )
  )
)
    )

}
