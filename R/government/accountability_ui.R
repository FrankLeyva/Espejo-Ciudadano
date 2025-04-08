# UI for Accountability Dashboard
accountabilityUI <- function() {
  page_fluid(
    useShinyjs(),
    
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        .nav-tabs .nav-link.active {
          font-weight: bold;
          border-bottom-color: #0d6efd;
        }
        
        .tab-content {
          padding-top: 20px;
        }
        
        .info-box {
          padding: 15px;
          border-radius: 5px;
          margin-bottom: 15px;
          display: flex;
          align-items: flex-start;
        }
        
        .info-box-icon {
          margin-right: 15px;
          font-size: 24px;
          padding-top: 3px;
        }
        
        .info-box-content {
          flex-grow: 1;
        }
        
        .info-box-title {
          font-weight: bold;
          margin-bottom: 10px;
          font-size: 16px;
        }
        
        .info-box-value {
          font-size: 18px;
          line-height: 1.4;
        }
        
        .info-box-info {
          background-color: #d1ecf1;
          color: #0c5460;
        }
        
        .info-box-warning {
          background-color: #fff3cd;
          color: #856404;
        }
        
        .justice-callout {
          background-color: #6969B3;
          color: white;
          padding: 20px;
          border-radius: 5px;
          margin-bottom: 20px;
        }
        
        .justice-callout-title {
          font-size: 20px;
          font-weight: bold;
          margin-bottom: 10px;
        }
        
        .justice-callout-value {
          font-size: 28px;
          font-weight: bold;
          margin-bottom: 10px;
        }
        
        .justice-callout-subtitle {
          font-size: 16px;
          opacity: 0.9;
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
      
      navset_tab(
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
    ),
    
    # Tabset for pie charts: Corruption acts (Q15.1, Q16.1, Q17.1)
    card(
      card_header("Percepción sobre Actos de Corrupción en el Gobierno"),
      
      navset_tab(
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
}