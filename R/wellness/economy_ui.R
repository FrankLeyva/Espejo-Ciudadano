# economy_ui.R

economyUI <- function() {
  page_fluid(
    useShinyjs(),
    
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
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
        
        .info-box-success {
          background-color: #d4edda;
          color: #155724;
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
          h2("Condiciones Económicas", class = "text-center")
        )
      )
    ),
    
    # Main content
    layout_columns(
      col_widths = c(7, 5),
      
      # Left column: Map with economic situation improvement
      card(
        card_header(
          "Percepción de mejora económica por distrito",
          class = "bg-light"
        ),
        leafletOutput("economic_improvement_map", height = "500px"),
        card_footer(
          "Porcentaje de personas que reportan mejora económica (respuestas 'Mejorado algo' y 'Mejorado mucho')"
        )
      ),
      
      # Right column: Pie chart and summary stats
      card(
        card_header(
          "Situación económica familiar",
          class = "bg-light"
        ),
        plotlyOutput("income_situation_pie", height = "350px"),
        
        div(
          class = "mt-4",
          div(
            class = "info-box info-box-info",
            div(class = "info-box-icon", bsicons::bs_icon("wallet-fill")),
            div(
              class = "info-box-content",
              div(class = "info-box-title", "Suficiencia de ingresos"),
              div(class = "info-box-value", textOutput("income_sufficiency_text", inline = TRUE), 
                  " de los hogares reportan que sus ingresos les alcanzan bien o justo.")
            )
          )
        ),
        
        div(
          class = "mt-3",
          div(
            class = "info-box info-box-warning",
            div(class = "info-box-icon", bsicons::bs_icon("cash-stack")),
            div(
              class = "info-box-content",
              div(class = "info-box-title", "Capacidad de ahorro"),
              div(class = "info-box-value", textOutput("savings_capability_text", inline = TRUE), 
                  " de los hogares indican que les alcanza bien y pueden ahorrar.")
            )
          )
        )
      )
    )
    
    
  
  )
}
