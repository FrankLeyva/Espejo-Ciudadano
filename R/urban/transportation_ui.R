transportationUI <- function() {
  page_fluid(
    useShinyjs(),
    
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        .tab-content {
          padding-top: 20px;
        }
        .satisfaction-header {
          font-weight: bold;
          margin-bottom: 15px;
          font-size: 18px;
          color: #333;
        }
      "))
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
        onclick = "Shiny.setInputValue('nav_target', 'urban', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Desarrollo Urbano"
      )
    ),
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Transporte Público", class = "text-center")
        )
      )
    ),
    
    # Satisfaction maps tabset
    card(
      card_header(
        div(
          style = "background-color: transparent; border-bottom: none;",
          "Satisfacción con el Servicio de Transporte Público",
          class = "h5 fw-bold"
        )
      ),
      tabsetPanel(
        id = "satisfaction_tabs",
        tabPanel(
          "Camión/Rutera",
          leafletOutput("bus_satisfaction_map", height = "500px")
        ),
        tabPanel(
          "Juárez Bus",
          leafletOutput("juarez_bus_satisfaction_map", height = "500px")
        )
      )
    ),
    
    # Specific issues tabset
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          div(
            style = "background-color: transparent; border-bottom: none;",
            "Aspectos Específicos del Servicio",
            class = "h5 fw-bold"
          )
        ),
        tabsetPanel(
          id = "service_issues_tabs",
          tabPanel(
            "Camión/Rutera",
            div(class = "satisfaction-header", "Aspectos con los que no están satisfechos:"),
            plotlyOutput("bus_issues_plot", height = "450px")
          ),
          tabPanel(
            "Juárez Bus",
            div(class = "satisfaction-header", "Aspectos con los que no están satisfechos:"),
            plotlyOutput("juarez_bus_issues_plot", height = "450px")
          )
        )
      )
    )
  )
  #commit comment
}