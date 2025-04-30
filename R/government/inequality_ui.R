# UI for Inequality Dashboard
inequalityUI <- function() {
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

          h2("Desigualdad en Ciudad Juárez", class = "text-center")
        )
      )
    ),
    
    # Maps row
    layout_columns(
      col_widths = c(6, 6),
      
      # Map for Q84 - Rights Violation
      card(
        card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center",
        "Violación de derechos por distrito",
        create_tooltip("<b>ID</b>: PER Q84 <br>
            <b>Pregunta</b>: En el ultimo año (2024), siente que sus garantías o derechos han sido violentados por alguna autoridad o dependencia gubernamental? <br>
             <b>Escala</b>: 1=Sí; 2=No")
			 ),
        downloadButton(
          "download_rights_violation_map", 
          "", 
          icon = icon("download"), 
          class = "btn-sm"
        ))),
        leafletOutput("rights_violation_map", height = "500px"),

      
      
      ),
      
      # Map for Q87 - Inequality Perception
      card(
        card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center",
        "Percepción de la desigualdad por distrito",
        create_tooltip("<b>ID</b>: PER Q87 <br>
            <b>Pregunta</b>: Por distintos motivos, no todas las personas que habitan en Juarez pueden acceder en condiciones de igualdad a los bienes y servicios, ni tienen las mismas oportunidades en la vida. Como describiría la desigualdad que se vive hoy en día? <br>
             <b>Escala</b>: 1=Muy alta; 2=Alta; 3=Media; 4=Baja; 5=Muy baja; 6=No sabe/No contestó")
			 ),
        downloadButton(
          "download_inequality_map", 
          "", 
          icon = icon("download"), 
          class = "btn-sm"
        ))),
        leafletOutput("inequality_perception_map", height = "500px"),
       
      )
    ),
    
    # Institution bar chart
    card(
      card_header(
        div(
            class = "d-flex align-items-center",
            "Instituciones que contribuyen a reducir la desigualdad",
            create_tooltip("<b>ID</b>: PER Q88 <br>
            <b>Pregunta</b>: Que institución contribuye mas a reducir la desigualdad en Juarez? <br>
             <b>Escala</b>: 11 categorías")
			 )
          ),
      plotlyOutput("inequality_reduction_plot", height = "450px")
    )
  )
}