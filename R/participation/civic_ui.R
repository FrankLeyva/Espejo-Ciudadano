# UI para Dashboard de Participación Cívica
civicUI <- function() {
  page_fluid(
    class = "section-participacion",

    useShinyjs(),
      
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'participation', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Participación Ciudadana"
      )
    ),
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style="border-top: 4px solid var(--participacion-color)",

          h2("Participación Cívica", class = "text-center")
        )
      )
    ),
    
    # Map section
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center",
          "Interés en participar en política municipal por distrito",
          create_tooltip("<b>ID</b>: PAR 130 <br>
            <b>Pregunta</b>: Que tanto le interesa participar en la policita del municipio, interviniendo en decisiones o realizando acciones de interes publico en beneficio de la ciudad? <br>
             <b>Escala</b>: 1=NADA; 2=POCO; 3=REGULAR; 4=ALGO; 5=MUCHO")
			 ),
          downloadButton(
            "download_interest_map", 
            "", 
            icon = icon("download"), 
            class = "btn-sm"
          )
        )
      ),
      leafletOutput("interest_map", height = "500px")
    ),
    # Participation requirements
    card(
      card_header(
        div(
            class = "d-flex align-items-center",
            "¿Qué necesita para participar en asuntos públicos?",
      create_tooltip("<b>ID</b>: PAR 131.1 - 131.5 <br>
        <b>Pregunta</b>: Informacion / Espacios para participar / Tiempo / Dinero / Confianza en las instituciones <br>
         <b>Escala</b>: 	0=No necesario; 1=Necesario")
    )
  ),
      plotlyOutput("requirements_plot", height = "350px")
    ),
    # Participation mechanisms knowledge
    card(
      card_header(
                div(
            class = "d-flex align-items-center",
            "Conocimiento de mecanismos de participación ciudadana",
            create_tooltip("<b>ID</b>: PAR Q138.1 - 138.13 <br>
              <b>Pregunta</b>: Ver gráfica para lista de mecanismos de participación <br>
               <b>Escala</b>: 1=Sí; 2=No")
         )
          ),
      plotlyOutput("mechanisms_plot", height = "450px")
    )
    
    
  )
}