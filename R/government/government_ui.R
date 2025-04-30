# government_ui.R
governmentUI <- function() {
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
    init_tooltips(),

    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--gobierno-color) !important; 
        color: white !important; 
        font-weight: bolder !important; 
        text-align: center !important; 
        border-bottom: none !important;"),
          h2("Percepción General del Gobierno", class = "text-center")
        )
      )
    ),
       
    layout_columns(
      col_widths = c(4,4,4),
      
      # Inequality Dashboard Card
      div(
        id = "nav_inequality_card",
        onclick = "Shiny.setInputValue('nav_target', 'inequality', {priority: 'event'})",
        card(
          class = "nav-card-gobierno",

          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("slash-circle")),
            h4(class = "nav-card-title text-center", "Desigualdad"),
            p(class = "text-center", "Análisis de indicadores de desigualdad")
          )
        )
      ),
      div(

        id = "nav_trust_card",
        onclick = "Shiny.setInputValue('nav_target', 'trust', {priority: 'event'})",
        card(
          card_body(
            class = "nav-card-gobierno",

            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("bar-chart")),
            h4(class = "nav-card-title text-center", "Confianza"),
            p(class = "text-center", "Análisis de indicadores de confianza en las instituciones")
          )
        )
      ),
      # Accountability Dashboard Card
      div(

        id = "nav_accountability_card",
        onclick = "Shiny.setInputValue('nav_target', 'accountability', {priority: 'event'})",
        card(
          class = "nav-card-gobierno",

          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("file-earmark-check")),
            h4(class = "nav-card-title text-center", "Rendición de Cuentas"),
            p(class = "text-center", "Estadísticas sobre transparencia y rendición")
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(-2,4,4,-2),
      div(

        id = "nav_political_card",
        onclick = "Shiny.setInputValue('nav_target', 'representation', {priority: 'event'})",
        card(
          class = "nav-card-gobierno",

          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("person-vcard")),
            h4(class = "nav-card-title text-center", "Representación Política"),
            p(class = "text-center", "Evaluación de la representación política")
          )
        )
      ),
      
      # Government Expectations Dashboard Card
      div(

        id = "nav_gov_expectations_card",
        onclick = "Shiny.setInputValue('nav_target', 'expectations', {priority: 'event'})",
        card(
          class = "nav-card-gobierno",

          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("graph-up")),
            h4(class = "nav-card-title text-center", "Expectativas de Gobierno"),
            p(class = "text-center", "Análisis de las expectativas ciudadanas")
          )
        )
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      # Card 1: Knowledge of officials (tabset with pie charts)
      card(
        card_header(
          div(
          class = "d-flex align-items-center",
          "Conocimiento de Funcionarios Públicos",
          create_dynamic_tooltip("knowledge_pub_tooltip")
          )
        ),
        div(class = "gobierno-pills",
        navset_pill(
          tabPanel("Regidor/a", plotlyOutput("officials_knowledge_regidor_plot", height = "400px")),
          tabPanel("Síndico/a", plotlyOutput("officials_knowledge_sindico_plot", height = "400px")),
          tabPanel("Diputado/a Local y/o Estatal", plotlyOutput("officials_knowledge_dipupadol_plot", height = "400px")),
          tabPanel("Diputado/a Federal", plotlyOutput("officials_knowledge_diputadof_plot", height = "400px"))
        )
      )
      ),
      
      # Card 2: Perception of inequality
      card(
        card_header(
          div(
            class = "d-flex align-items-center",
          "Percepción de la Desigualdad",
          create_tooltip("<b>ID</b>: PER Q87 <br>
            <b>Pregunta</b>: Por distintos motivos, no todas las personas que habitan en Juarez pueden acceder en condiciones de igualdad a los bienes y servicios, ni tienen las mismas oportunidades en la vida. Como describiría la desigualdad que se vive hoy en día? <br>
             <b>Escala</b>: 1=Muy alta;2=Alta;3=Media;4=Baja;5=Muy baja;6=No sabe/No contestó")
			 )
        ),
        plotlyOutput("inequality_perception_plot", height = "400px")
      )
    ),
    
    # Second row of indicators
    layout_columns(
      col_widths = c(6, 6),
      
      # Card 3: Government expectations
      card(
        card_header(
          div(
            class = "d-flex align-items-center",
            "Expectativas Ciudadanas sobre el Gobierno",
            create_tooltip("<b>ID</b>: PAR Q19 Q20 Q21 <br>
              <b>Pregunta</b>: Como calificaria la expectativa que tiene en este momento del gobierno Municipal  / Estatal / Federal ?<br>
               <b>Escala</b>: 1-10")
         )),
        plotlyOutput("government_expectations_plot", height = "500px")
      ),
      
      # Card 4: Important problems
      card(
        card_header(
          div(
            class = "d-flex align-items-center",
          "Problemas Importantes de Ciudad Juárez",
          create_tooltip("<b>ID</b>: PER Q81 Q82 <br>
              <b>Pregunta</b>: Para usted, cuales son los 2 problemas mas importantes de Juarez? <br>
               <b>Escala</b>: 19 categorías")
          )
        ),
        plotlyOutput("important_problems_plot", height = "500px")
      )
    )
  )
}