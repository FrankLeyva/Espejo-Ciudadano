# UI for Accountability Dashboard
accountabilityUI <- function() {
  page_fluid(
    class = "section-gobierno",

    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        /* Override pill navigation styling for this page */
        .gobierno-pills .nav-pills .nav-link:not(.active) {
          background-color: rgba(240, 240, 240, 0.8);
color: var(--gobierno-color) !important;
            border: 1px solid rgba(229, 126, 30, 0.2);
          font-weight: bold !important;
        }
        
        .gobierno-pills .nav-pills .nav-link:hover:not(.active) {
          background-color: rgba(160, 115, 67, 0.1);
        }
             .gobierno-pills .nav-pills .nav-link.active {
          background-color: var(--gobierno-color) !important; 
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

          h2("Rendición de Cuentas", class = "text-center")
        )
      )
    ),
    
    # Callout for Q122 - "El que la hace la paga"
    layout_columns(
      col_widths = 12,
      card(
        div(
          class = "justice-callout",
          div(class = "justice-callout-title", "Percepción sobre la Justicia en Juárez"),
          div(class = "d-flex align-items-center mb-3",
              bsicons::bs_icon("shield-check", size = "2em", class = "me-3"),
              div(class = "justice-callout-value", textOutput("justice_perception"))
          ),
          div(class = "justice-callout-subtitle", "En Juárez, ¿el que la hace, la paga? Es decir, ¿se castiga a quien comete algún delito o infracción?"),
          div(class = "mt-3", "Escala: 1=Siempre, 2=Casi siempre, 3=Casi nunca, 4=Nunca")
        )
      )
    ),
    
    # Tabset for histograms: Corruption punishment (Q123, Q124, Q125)
    card(
      card_header("Percepción sobre Castigo a Servidores Públicos Corruptos"),
      div(class = "gobierno-pills",
      navset_pill(
        nav_panel(
          title = "Gobierno Municipal",
          plotlyOutput("municipal_punishment_hist", height = "400px")
        ),
        nav_panel(
          title = "Gobierno Estatal",
          plotlyOutput("state_punishment_hist", height = "400px")
        ),
        nav_panel(
          title = "Gobierno Federal",
          plotlyOutput("federal_punishment_hist", height = "400px")
        )
      )
    )
    ),
    
    # Tabset for pie charts: Corruption acts (Q15.1, Q16.1, Q17.1)
    card(
      card_header("Percepción sobre Actos de Corrupción en el Gobierno"),
      div(class = "gobierno-pills",
      navset_pill(
        nav_panel(
          title = "Gobierno Municipal",
          plotlyOutput("municipal_corruption_pie", height = "400px")
        ),
        nav_panel(
          title = "Gobierno Estatal",
          plotlyOutput("state_corruption_pie", height = "400px")
        ),
        nav_panel(
          title = "Gobierno Federal",
          plotlyOutput("federal_corruption_pie", height = "400px")
        )
      )
    )
    )
    
    
  )
}