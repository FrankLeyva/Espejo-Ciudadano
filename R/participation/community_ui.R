# UI para Dashboard de Participación Comunitaria
communityUI <- function() {
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

          h2("Participación Comunitaria", class = "text-center")
        )
      )
    ),
    
    # Organization participation
    card(
      
      card_header(
        div(
            class = "d-flex align-items-center",
            "Participación en organizaciones",
            create_tooltip("<b>ID</b>: PAR Q132.1 - 132.11 <br>
              <b>Pregunta</b>: Ver gráfica para lista de organizaciones <br>
               <b>Escala</b>: 	1=Sí; 2=No")
         )
          ),
      plotlyOutput("organizations_plot", height = "450px")
    ),
    
    # Problem-solving activities
    card(
      card_header(
        div(
            class = "d-flex align-items-center",
            "Actividades para resolver problemas comunitarios",
            create_tooltip("<b>ID</b>: PAR Q136.1 - Q136.14 <br>
              <b>Pregunta</b>: Ver gráfica para lista de actividades <br>
               <b>Escala</b>: 1=Sí; 2=No")
         )
        ),
      plotlyOutput("activities_plot", height = "500px")
    )
  )
}