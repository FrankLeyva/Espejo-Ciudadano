trustUI <- function() {
  page_fluid(
    class = "section-gobierno",

    useShinyjs(),
    init_tooltips(),

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

          h2("Confianza en las Instituciones", class = "text-center")
        )
      )
    ),
    
    
    # First bar plot: Institutions of popular election
    card(
      card_header(
        div(
          class = "d-flex align-items-center",
          "Instituciones de Elección Popular",
          create_tooltip("<b>ID</b>: PAR Q21.1, PAR Q21.17 - PAR Q21.24 <br>
            <b>Pregunta</b>: Lista de instituciones de elección popular en la gráfica <br>
             <b>Escala</b>: 1=Mucha desconfianza; 2=Algo de desconfianza; 3=Algo de confianza; 4=Mucha confianza; 5=NS/NC")
			 )
        
      ),
      card_body(
        plotlyOutput("popular_election_institutions_plot", height = "500px")
      )
    ),
    
    # Second bar plot: Public institutions and media
    card(
      card_header(
        div(
          class = "d-flex align-items-center",
"Instituciones Públicas y Medios de Comunicación",
create_tooltip("<b>ID</b>: PAR Q21.2 - PAR Q21.9 <br>
  <b>Pregunta</b>: Lista de instituciones de públicas y medios de comunicación en la gráfica <br>
   <b>Escala</b>: 1=Mucha desconfianza; 2=Algo de desconfianza; 3=Algo de confianza; 4=Mucha confianza; 5=NS/NC")
        )
      ),
      card_body(
        plotlyOutput("public_institutions_media_plot", height = "500px")
      )
    ),
    
    # Third bar plot: Public safety institutions
    card(
      card_header(
        div(
          class = "d-flex align-items-center",
"Instituciones de Seguridad Pública",
create_tooltip("<b>ID</b>: PAR Q21.10 - PAR Q21.16 <br>
  <b>Pregunta</b>: Lista de instituciones de seguridad pública en la gráfica <br>
   <b>Escala</b>: 1=Mucha desconfianza; 2=Algo de desconfianza; 3=Algo de confianza; 4=Mucha confianza; 5=NS/NC")
      )
    ),
      card_body(
        plotlyOutput("public_safety_institutions_plot", height = "500px")
      )
    )
  )
}