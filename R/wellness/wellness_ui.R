
wellnessUI <- function() {

  page_fluid(
    class = "section-bienestar",
    useShinyjs(),
    init_tooltips(),

    # Header
    layout_columns(
      fill = FALSE,
      card(
                card_header(
          style = paste0("background-color: var(--bienestar-color) !important; 
        color: white !important; 
        font-weight: bolder !important; 
        text-align: center !important; 
        border-bottom: none !important;"),
          h2("Bienestar Social y Económico", class = "text-center")
        )
      )
    ),
    
    # Navigation cards section
    layout_columns(
      col_widths = c(3, 3, 3,3),
      
      # Economy card
      div(
        id = "nav_economic_card",
        onclick = "Shiny.setInputValue('nav_target', 'economic', {priority: 'event'})",
        card(
          class = "nav-card-bienestar",

          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("cash-coin")),
            h4(class = "nav-card-title text-center", "Economía"),
            p(class = "text-center", "Análisis de condiciones económicas")
          )
        )
      ),
      
      # Cultural card
      div(
        id = "nav_cultural_card",
        onclick = "Shiny.setInputValue('nav_target', 'cultural', {priority: 'event'})",
        card(
          class = "nav-card-bienestar",

          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("music-note-beamed")),
            h4(class = "nav-card-title text-center", "Participación Cultural"),
            p(class = "text-center", "Análisis de actividades culturales")
          )
        )
      ),
      
      # Identity card
      div(
        id = "nav_identity_card",
        onclick = "Shiny.setInputValue('nav_target', 'identity', {priority: 'event'})",
        card(
          class = "nav-card-bienestar",
          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("geo-alt-fill")),
            h4(class = "nav-card-title text-center", "Identidad y Pertenencia"),
            p(class = "text-center", "Sentido de pertenencia a la ciudad")
          )
        )
      ),      # Environment Card
      div(
        id = "nav_environment_card",
        onclick = "Shiny.setInputValue('nav_target', 'environment', {priority: 'event'})",
        card(
          class = "nav-card-bienestar",

          card_body(
            div(class = "text-center nav-card-icon", bsicons::bs_icon("tree-fill")),
            h4(class = "nav-card-title text-center", "Medio Ambiente"),
            p(class = "text-center", "Calidad ambiental")
          )
        )
      )
    ),
    
    # Content section 1: Economic situation and Migration intention
    layout_columns(
      col_widths = c(6, 6),
      
      # Economic situation pie chart
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            div(
              class = "d-flex align-items-center",
              "Percepción de situación económica personal",
              create_tooltip("<b>ID</b>: PER Q4 <br>
                 <b>Pregunta</b>: En comparación con el año pasado, Diría usted que su situación económica personal ha mejorado, ha empeorado o sigue igual? <br>
                  <b>Escala</b>: 1=Empeorado mucho; 2=Empeorado algo; 3=Igual; 4=Mejorado algo; 5=Mejorado mucho; 6=NS/NC")
            )
          )
        ),
        plotlyOutput("economic_situation_pie", height = "450px"),
      ),
      
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            div(
              class = "d-flex align-items-center",
              "Frecuencia con que piensa en irse de la ciudad",
              create_tooltip("<b>ID</b>: PER Q5 <br>
                 <b>Pregunta</b>: Con el total del ingreso familiar, usted diría que...? <br>
                  <b>Escala</b>: 1=Les alcanza bien y pueden ahorrar; 2=Les alcanza justo, sin grandes dificultades; 3=No les alcanza y tienen dificultades; 4=No les alcanza y tienen grandes dificultades; 5=NS/NC")
            
            ),
            downloadButton(
              "download_migration_map", 
              "", 
              icon = icon("download"), 
              class = "btn-sm"
            )
        )
      ),
        leafletOutput("migration_intention_map", height = "450px")

    )
  ),
    
    # Content section 2: Activities chart
    layout_columns(
      card(
        card_header(
          div(
            class = "d-flex align-items-center",
            "Actividades realizadas en los últimos 3 meses",
            create_tooltip("<b>ID</b>: PER Q16 <br>
               <b>Pregunta</b>: ¿Qué actividades culturales, recreativas realizó en la ciudad en los últimos tres meses?  <br>
                <b>Escala</b>: 1=Sí; 2=No")
          )
          ),
        plotlyOutput("activities_chart", height = "550px")
      )
    )
  )
}
