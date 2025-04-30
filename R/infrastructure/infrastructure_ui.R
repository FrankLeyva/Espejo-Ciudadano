infrastructureUI <- function() {
  page_fluid(
    class = "section-infrastructure",
    useShinyjs(),
    init_tooltips(),


    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--infraestructura-color) !important; 
          color: white !important; 
          font-weight: bolder !important; 
          text-align: center !important; 
          border-bottom: none !important;"),
          h2("Infraestructura Publica de la ciudad", class = "text-center")
        )
      )
    ),

    
    layout_columns(
      col_widths = 3,
      
      # Education Dashboard Card
      div(
        id = "nav_education_card",  # Changed ID to match what overviewNavServer expects
        onclick = "Shiny.setInputValue('nav_target', 'education', {priority: 'event'})",
        card(
          class = "nav-card-infraestructura",
          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("book")),
            h4(class = "nav-card-title text-center", "Educación"),
            p(class = "text-center", "Análisis de indicadores educativos")
          )
        )
      ),
      
      # Healthcare Dashboard Card
      div(
        id = "nav_healthcare_card",  # Changed ID to match what overviewNavServer expects
        onclick = "Shiny.setInputValue('nav_target', 'healthcare', {priority: 'event'})",
        card(
          class = "nav-card-infraestructura",

          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("heart-pulse")),
            h4(class = "nav-card-title text-center", "Salud"),
            p(class = "text-center", "Estadísticas de servicios de salud")
          )
        )
      ),
      
      # Public Services Dashboard Card
      div(
        id = "nav_services_card",  # Changed ID to match what overviewNavServer expects
        onclick = "Shiny.setInputValue('nav_target', 'public_services', {priority: 'event'})",
        card(
          class = "nav-card-infraestructura",

          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("gear")),
            h4(class = "nav-card-title text-center", "Servicios Públicos"),
            p(class = "text-center", "Evaluación de servicios e infraestructura")
          )
        )
      ),
      
      # Housing Dashboard Card
      div(
        id = "nav_housing_card",  # Changed ID to match what overviewNavServer expects
        onclick = "Shiny.setInputValue('nav_target', 'housing', {priority: 'event'})",
        card(
          class = "nav-card-infraestructura",

          card_body(
            div(class = "text-center nav-card-icon", 
                bsicons::bs_icon("house")),
            h4(class = "nav-card-title text-center", "Vivienda"),
            p(class = "text-center", "Análisis de condiciones de vivienda")
          )
        )
      )
    ),

    # First row of indicators
    layout_columns(
      col_widths = c(6, 6),
      
      # Education Plot
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            div(
              class = "d-flex align-items-center",
            "Educación: Hogares con estudiantes por distrito",
            create_tooltip("<b>ID</b>: PER Q6 <br>
              <b>Pregunta</b>: En su familia, hay por lo menos 1 o más estudiantes de cualquier nivel educativo?v <br>
               <b>Escala</b>: 1=Sí; 2=No")
         ),
            downloadButton(
              "download_gen_students_map", 
              "", 
              icon = icon("download"), 
              class = "btn-sm"
            )
        )
      ),
        leafletOutput("education_plot", height = "400px")
      ),
      
      # Healthcare Plot
      card(
        card_header(
          div(
            class = "d-flex align-items-center",
          "Salud: Satisfacción con servicios de salud"),
          create_tooltip("<b>ID</b>: PER Q19 <br>
            <b>Pregunta</b>: En una escala del 1 al 10, Que tan satisfecho/a esta en GENERAL con los servicios de salud que recibe del servicio medico que mas USA? <br>
             <b>Escala</b>: 1-10")
			 ),
        plotlyOutput("healthcare_plot", height = "400px")
      )
    ),
    
    # Second row of indicators
    layout_columns(
      col_widths = c(6, 6),
      
      # Utilities Plot
      card(
        card_header(
          div(
            class = "d-flex align-items-center",
            "Servicios Públicos: Satisfacción por servicio",
            create_tooltip("<b>ID</b>: PER Q29 - Q56 <br>
              <b>Pregunta</b>: Satisfacción con los servicios en la gráfica <br>
               <b>Escala</b>: 1-10")
         )
          ),
        plotlyOutput("utilities_plot", height = "500px")
      ),
      
      # Housing Plot
      card(
        card_header(        
          div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center",
          "Vivienda: Satisfacción por distrito",
          create_tooltip("<b>ID</b>: PER Q25 <br>
            <b>Pregunta</b>: En una escala del 1 al 10, Que tan satisfecho esta con LA CASA EN LA QUE VIVE? <br>
             <b>Escala</b>: 1-10")
			 ),
        
        downloadButton(
          "download_housing_map", 
          "", 
          icon = icon("download"), 
          class = "btn-sm"
        )
      )),
        leafletOutput("housing_map", height = "500px")
      )
    )
  )
}