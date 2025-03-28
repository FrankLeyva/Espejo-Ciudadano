representationUI <- function() {
  page_fluid(
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
          h2("Representación Política", class = "text-center")
        )
      )
    ),
    
    # Main layout with side-by-side columns
    layout_columns(
      col_widths = c(8, 4),  # 8/12 for main content, 4/12 for value boxes
      
      # Main content column (left side)
      div(
        # First tabset: Knowledge of representatives maps
        card(
          card_header("Conocimiento de Representantes por Distrito"),
          tabsetPanel(
            id = "knowledge_tabs",
            tabPanel("Regidor(a)", 
                     div(style = "height: 500px;", leafletOutput("regidor_knowledge_map"))),
            tabPanel("Síndico(a)", 
                     div(style = "height: 500px;", leafletOutput("sindico_knowledge_map"))),
            tabPanel("Diputado(a)", 
                     div(style = "height: 500px;", leafletOutput("diputado_knowledge_map")))
          )
        ),
        
        # Second tabset: Representative knowledge bar charts
        card(
          card_header("Conocimiento de Representantes Específicos"),
          tabsetPanel(
            id = "specific_knowledge_tabs",
            tabPanel("Regidores", 
                     div(style = "height: 500px;", plotlyOutput("regidores_knowledge_chart"))),
            tabPanel("Diputados Locales", 
                     div(style = "height: 500px;", plotlyOutput("diputados_locales_knowledge_chart"))),
            tabPanel("Diputados Federales", 
                     div(style = "height: 500px;", plotlyOutput("diputados_federales_knowledge_chart")))
          )
        )
      ),
      
      # Value boxes column (right side)
      div(
        # Value box 1: Regidores representation rating
        value_box(
          title = "Regidores: Representación de Intereses Ciudadanos",
          value = textOutput("regidores_rating"),
          showcase = bsicons::bs_icon("person-check-fill"),
          theme = value_box_theme(bg = "#2A9D8F", fg = "white")
        ),
        
        # Value box 2: Síndico representation rating
        value_box(
          title = "Síndico(a): Representación de Intereses Ciudadanos",
          value = textOutput("sindico_rating"),
          showcase = bsicons::bs_icon("person-check-fill"),
          theme = value_box_theme(bg = "#6969B3", fg = "white")
        ),
        
        # Value box 3: Local deputy representation rating
        value_box(
          title = "Diputado(a) Local: Representación de Intereses Ciudadanos",
          value = textOutput("diputado_local_rating"),
          showcase = bsicons::bs_icon("person-check-fill"),
          theme = value_box_theme(bg = "#F4A261", fg = "white")
        ),
        
        # Value box 4: Federal deputy representation rating
        value_box(
          title = "Diputado(a) Federal: Representación de Intereses Ciudadanos",
          value = textOutput("diputado_federal_rating"),
          showcase = bsicons::bs_icon("person-check-fill"),
          theme = value_box_theme(bg = "#E86486", fg = "white")
        )
      )
    )
  )
}