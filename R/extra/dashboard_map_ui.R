# R/extra/dashboard_map_ui.R
dashboardMapUI <- function(id) {
  ns <- NS(id)
  
  page_fluid(
    class = "section-extras",
    useShinyjs(),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Mapa del Dashboard", class = "text-center")
        )
      )
    ),
    
    # Dashboard map container
    div(
      class = "dashboard-map-container",
      
      # Search feature
      div(
        class = "search-container mb-4",
        div(
          class = "input-group",
          tags$span(
            class = "input-group-text",
            icon("search")
          ),
          tags$input(
            id = ns("search_input"),
            type = "text",
            class = "form-control",
            placeholder = "Buscar por tema, subtema o pregunta...",
            autocomplete = "off"
          ),
          tags$button(
            id = ns("clear_search"),
            class = "btn btn-outline-secondary",
            icon("times"),
            onclick = sprintf("document.getElementById('%s').value = ''; $(document).trigger('search-cleared');", ns("search_input"))
          )
        ),
        div(
          id = ns("search_results"),
          class = "search-results mt-2 d-none"
        )
      ),
      
      # Main accordion with dashboard structure
      div(
        id = ns("dashboard_map"),
        class = "dashboard-map-accordion"
      )
    )
  )
}