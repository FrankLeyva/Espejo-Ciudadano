
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
    
    # Introduction text
    card(
      card_body(
        div(
          class = "text-center mb-4",
          p("Este mapa proporciona una visión general de todos los contenidos disponibles en el dashboard Espejo Ciudadano."),
          p("Seleccione una sección para ver su contenido.")
        )
      )
    ),
    
    # Simple section selection with nicer UI
    card(
      card_header(
        div(
          style = "display: flex; align-items: center; justify-content: space-between;",
          h3("Navegación del Dashboard", style = "margin: 0; font-size: 1.25rem;"),
          div(
            style = "background-color: #f8f9fa; padding: 5px 10px; border-radius: 4px; font-size: 0.85rem;",
            "Seleccione una sección para ver su contenido"
          )
        )
      ),
      card_body(
        div(
          style = "margin-bottom: 20px;",
          selectInput(
            ns("selected_section"),
            label = div(
              style = "font-weight: bold; font-size: 1.1rem; margin-bottom: 8px;",
              "Seleccionar Sección del Dashboard:"
            ),
            choices = c(
              "Bienestar Social y Económico" = "bienestar",
              "Movilidad Urbana" = "movilidad",
              "Gobierno" = "gobierno",
              "Infraestructura" = "infraestructura",
              "Participación" = "participacion",
              "Extras" = "extras"
            ),
            width = "100%",
            selectize = TRUE
          )
        ),
        uiOutput(ns("section_contents"))
      )
    ),
    
    # Footer with a link to return to home
    div(
      class = "d-flex justify-content-center mt-5",
      actionButton(
        inputId = ns("go_to_home"),
        label = "Volver a Inicio",
        class = "btn btn-outline-primary",
        onclick = "Shiny.setInputValue('nav_target', 'overview', {priority: 'event'})"
      )
    ),
    
    # Generic dashboard footer
    create_dashboard_footer()
  )
}