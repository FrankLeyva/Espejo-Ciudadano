transportationUI <- function() {
  page_fluid(
    class = "section-movilidad",

    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        /* Override pill navigation styling for this page */
        .movilidad-pills .nav-pills .nav-link:not(.active) {
          background-color: rgba(240, 240, 240, 0.8);
color: var(--movilidad-color) !important;
            border: 1px solid rgba(30, 229, 57, 0.2);
          font-weight: bold !important;
        }
        
        .movilidad-pills .nav-pills .nav-link:hover:not(.active) {
          background-color: rgba(67, 160, 71, 0.1);
        }
             .movilidad-pills .nav-pills .nav-link.active {
          background-color: var(--movilidad-color) !important; /* Bienestar primary color */
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
          style="border-top: 4px solid var(--movilidad-color)",

          h2("Transporte Público", class = "text-center")
        )
      )
    ),
    layout_columns(
      col_widths = c(6,6),
    # Satisfaction maps tabset
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center",
          "Satisfacción con el Servicio de Transporte Público",
          create_dynamic_tooltip("trans_satisfaction_tooltip")
          ),
          downloadButton(
            "download_transport_map", 
            "", 
            icon = icon("download"), 
            class = "btn-sm"
          )
        )
      ),
      div(class = "movilidad-pills",
      navset_pill(
        id = "transport_tabs",
        tabPanel(
          "Camión/Rutera",
          leafletOutput("bus_satisfaction_map", height = "500px")
        ),
        tabPanel(
          "Juárez Bus",
          leafletOutput("juarez_bus_satisfaction_map", height = "500px")
        )
      )
    )
    ),
    
    # Specific issues tabset

      card(
        card_header(
          div(
            class = "d-flex align-items-center",
            "Aspectos con los que no están satisfechos:",
            create_dynamic_tooltip("trans_dissatisfaction_tooltip")
          )
        ),
        div(class = "movilidad-pills",
        navset_pill(
          id = "service_issues_tabs",
          tabPanel(
            "Camión/Rutera",
            plotlyOutput("bus_issues_plot", height = "450px")
          ),
          tabPanel(
            "Juárez Bus",
            plotlyOutput("juarez_bus_issues_plot", height = "450px")
          )
        )
      )
    )
    )
  )
}