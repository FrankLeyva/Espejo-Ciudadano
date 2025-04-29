mobilityUI <- function() {
  page_fluid(
    class = "section-movilidad",

    useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Override pill navigation styling for this page */
        .movilidad-pills .nav-pills .nav-link:not(.active) {
          background-color: rgba(240, 240, 240, 0.8);
color: var(--movilidad-color) !important;
            border: 1px solid rgba(30, 229, 57, 0.2);
          font-weight: bold !important;
        }
        
        .movilidad-pills .nav-pills .nav-link:hover:not(.active) {
          background-color: rgba(67, 160, 71, 0.1);
        }
             .movilidad-pills .nav-pills .nav-link.active {
          background-color: var(--movilidad-color) !important; /* Bienestar primary color */
          color: white !important;
          font-weight: bold !important;
          border: none !important;
        }
      "))
    ),
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'urban', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Desarrollo Urbano"
      )
    ),
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style="border-top: 4px solid var(--movilidad-color)",

          h2("Movilidad Urbana", class = "text-center")
        )
      )
    ),
    
    # Vehicle and bicycle distribution
    layout_columns(
      col_widths = c(6, 6),
      
      # Bicycles card
      card(
        card_header(
        div(
          class = "d-flex align-items-center",
          "Bicicletas por Hogar",
    create_tooltip("<b>ID</b>: Suma de PER Q68, Q69, Q70 <br>
          <b>Pregunta</b>: Cuantas bicicletas para NIÑOS/ADOLESCENTES/ADULTOS hay disponibles en su hogar? (poner 0 si no se cuenta con ninguna de este tipo) <br>
           <b>Escala</b>: Numérica")
     )
    ),
        plotlyOutput("bicycles_pie", height = "400px")
      ),
      
      # Vehicles card
      card(
        card_header(
            div(
              class = "d-flex align-items-center",
              "Vehículos Motorizados por Hogar",
        create_tooltip("<b>ID</b>: PER Q66 <br>
              <b>Pregunta</b>: 	Cuantos vehiculos de motor hay disponibles en su hogar? (Si no tiene ninguno, poner 0) <br>
               <b>Escala</b>: Numérica")
         )
        ),
        plotlyOutput("vehicles_pie", height = "400px")
      )
    ),
    
    # Transportation modes tabset panel
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center",
          "Modos de Transporte",
          create_dynamic_tooltip("transportation_tooltip")
        )
      )
    ),
      div(class = "movilidad-pills",
      navset_pill(
        id = "transport_tabs",
        tabPanel(
          "Transporte al Trabajo",
          plotlyOutput("work_transport_plot", height = "550px")
        ),
        tabPanel(
          "Transporte General",
          plotlyOutput("general_transport_plot", height = "550px")
        )
      )
    )
    )
  )
}