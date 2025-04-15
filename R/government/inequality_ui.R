# UI for Inequality Dashboard
inequalityUI <- function() {
  page_fluid(
    class = "section-gobierno",

    useShinyjs(),
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'government', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Gobierno"
      )
    ),
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style="border-top: 4px solid var(--gobierno-color)",

          h2("Desigualdad en Ciudad Juárez", class = "text-center")
        )
      )
    ),
    
    # Maps row
    layout_columns(
      col_widths = c(6, 6),
      
      # Map for Q84 - Rights Violation
      card(
        card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
        "Violación de derechos por distrito",
        downloadButton(
          "download_rights_violation_map", 
          "", 
          icon = icon("download"), 
          class = "btn-sm"
        ))),
        leafletOutput("rights_violation_map", height = "500px"),

      
      
      ),
      
      # Map for Q87 - Inequality Perception
      card(
        card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
        "Percepción de la desigualdad por distrito",
        downloadButton(
          "download_inequality_map", 
          "", 
          icon = icon("download"), 
          class = "btn-sm"
        ))),
        leafletOutput("inequality_perception_map", height = "500px"),
       
      )
    ),
    
    # Institution bar chart
    card(
      card_header("Instituciones que contribuyen a reducir la desigualdad"),
      plotlyOutput("inequality_reduction_plot", height = "450px")
    )
  )
}