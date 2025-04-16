representationUI <- function() {
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
          /* Allow chart containers to dynamically resize */
    .chart-container {
      min-height: 400px;
      height: auto !important;
    }
    
    /* Make sure the plotly chart takes up available space */
    .chart-container .plotly {
      height: 100% !important;
    }
    
    /* Fix for long labels in horizontal bar charts */
    .ytick text {
      text-anchor: end !important;
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
    layout_columns(
      col_widths = c(3, 3,3,3), 
      value_box(
        title = "Regidores: Representación de Intereses Ciudadanos",
        value = textOutput("regidores_rating"),
        showcase = bsicons::bs_icon("person-check-fill"),
        theme = value_box_theme(bg = "#423629", fg = "white")
      ),
      
      # Value box 2: Síndico representation rating
      value_box(
        title = "Síndico(a): Representación de Intereses Ciudadanos",
        value = textOutput("sindico_rating"),
        showcase = bsicons::bs_icon("person-check-fill"),
        theme = value_box_theme(bg = "#8A8178", fg = "black")
      ),
      
      # Value box 3: Local deputy representation rating
      value_box(
        title = "Diputado(a) Local: Representación de Intereses Ciudadanos",
        value = textOutput("diputado_local_rating"),
        showcase = bsicons::bs_icon("person-check-fill"),
        theme = value_box_theme(bg = "#423629", fg = "white")
      ),
      
      # Value box 4: Federal deputy representation rating
      value_box(
        title = "Diputado(a) Federal: Representación de Intereses Ciudadanos",
        value = textOutput("diputado_federal_rating"),
        showcase = bsicons::bs_icon("person-check-fill"),
        theme = value_box_theme(bg = "#8A8178", fg = "black")
      )
    ),
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style="border-top: 4px solid var(--gobierno-color)",

          h2("Representación Política", class = "text-center")
        )
      )
    ),
    
    # Main layout with side-by-side columns
    layout_columns(
      col_widths = c(6, 6),  
        card(
          card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
          "Conocimiento de Representantes por Distrito",
          downloadButton(
            "download_political_knowledge_map", 
            "", 
            icon = icon("download"), 
            class = "btn-sm"
          ))),
          div(class = "gobierno-pills",
          navset_pill(
            id = "knowledge_tabs",
            tabPanel("Regidor(a)", 
                     div(class = "chart-container", leafletOutput("regidor_knowledge_map"))),
            tabPanel("Síndico(a)", 
                     div(class = "chart-container", leafletOutput("sindico_knowledge_map"))),
            tabPanel("Diputado(a) Local y/o Estatal", 
                     div(class = "chart-container", leafletOutput("diputadol_knowledge_map"))),
            tabPanel("Diputado(a) Federal", 
                     div(class = "chart-container", leafletOutput("diputadof_knowledge_map")))
          )
        )
        ),
        
        # Second tabset: Representative knowledge bar charts
        card(
          card_header("Conocimiento de Representantes Específicos"),
          div(class = "gobierno-pills",
          navset_pill(
            id = "specific_knowledge_tabs",
            tabPanel("Regidores", 
                     div(class = "chart-container", plotlyOutput("regidores_knowledge_chart", height ="auto"))),
            tabPanel("Diputados Locales", 
                     div(class = "chart-container", plotlyOutput("diputados_locales_knowledge_chart", height ="auto"))),
            tabPanel("Diputados Federales", 
                     div(class = "chart-container", plotlyOutput("diputados_federales_knowledge_chart", height ="auto")))
          )
        )
        )
      )
      
    
  )
}