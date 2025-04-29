# economy_ui.R

economyUI <- function() {
  page_fluid(
    class = "section-bienestar",

    useShinyjs(),
    
    
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
          style="border-top: 4px solid var(--bienestar-color)",

          h2("Condiciones Económicas", class = "text-center")
        )
      )
    ),
    
    # Main content
    layout_columns(
      col_widths = c(6, 6),
      
      # Left column: Map with economic situation improvement
      card(
        card_header(
          div(

          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center",
          "Porcentaje de la población que considera que mejoró su economía en el 2024",
          create_tooltip("<b>ID</b>: PER Q4 <br>
            <b>Pregunta</b>: En comparación con el año pasado, Diría usted que su situación económica personal ha mejorado, ha empeorado o sigue igual? <br>
             <b>Escala</b>: 1=Empeorado mucho; 2=Empeorado algo; 3=Igual; 4=Mejorado algo; 5=Mejorado mucho; 6=NS/NC")
       ),
          downloadButton(
            "download_economy_map", 
            "", 
            icon = icon("download"), 
            class = "btn-sm"
          )
 )
        ),
        leafletOutput("economic_improvement_map", height = "500px"),
      ),
      
      card(
        card_header(
          div(
            class = "d-flex align-items-center",
          "Porcentaje de la población que considera si les alcanza/no les alcanza",
          create_tooltip("<b>ID</b>: PER Q5 <br>
            <b>Pregunta</b>: Con el total del ingreso familiar, usted diría que...? <br>
             <b>Escala</b>: 1=Les alcanza bien y pueden ahorrar; 2=Les alcanza justo, sin grandes dificultades; 3=No les alcanza y tienen dificultades; 4=No les alcanza y tienen grandes dificultades; 5=NS/NC")
       )
        ),
        plotlyOutput("income_situation_pie", height = "350px"),
      )
    )
    
    
  
  )
}
