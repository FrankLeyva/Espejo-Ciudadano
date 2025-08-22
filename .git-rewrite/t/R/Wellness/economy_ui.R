# economy_ui.R

economyUI <- function() {
  page_fluid(
    class = "section-bienestar",
    useShinyjs(),
    init_tooltips(),

    # Back navigation
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'wellness', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Calidad de Vida"
      )
    ),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--bienestar-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Condiciones Económicas", class = "dashboard-header")
        )
      )
    ),
    
    # Main content with plot cards
    div(
      class = "plot-cards-grid two-columns",
      
      # Economic Improvement Map Plot Card
      div(
        class = "plot-card plot-card-bienestar",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Porcentaje de la población que considera que mejoró su economía en el 2024",
              create_tooltip("<b>ID</b>: PER Q4 <br>
                <b>Pregunta</b>: En comparación con el año pasado, Diría usted que su situación económica personal ha mejorado, ha empeorado o sigue igual? <br>
                 <b>Escala</b>: 1=Empeorado mucho; 2=Empeorado algo; 3=Igual; 4=Mejorado algo; 5=Mejorado mucho; 6=NS/NC")
            )
          ),
          div(
            class = "plot-actions",
            downloadButton(
              "download_economy_map", 
              "", 
              icon = icon("download"), 
              class = "plot-action-btn",
              title = "Descargar mapa"
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          leafletOutput("economic_improvement_map", height = "500px")
        )
      ),
      
      # Income Situation Plot Card
      div(
        class = "plot-card plot-card-bienestar",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Porcentaje de la población que considera si les alcanza/no les alcanza",
              create_tooltip("<b>ID</b>: PER Q5 <br>
                <b>Pregunta</b>: Con el total del ingreso familiar, usted diría que...? <br>
                 <b>Escala</b>: 1=Les alcanza bien y pueden ahorrar; 2=Les alcanza justo, sin grandes dificultades; 3=No les alcanza y tienen dificultades; 4=No les alcanza y tienen grandes dificultades; 5=NS/NC")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("income_situation_pie", height = "500px")
        )
      )
    )
  )
}