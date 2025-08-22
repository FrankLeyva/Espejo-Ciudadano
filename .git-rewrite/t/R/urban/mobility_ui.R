# mobility_ui.R - Updated to follow new styling patterns
mobilityUI <- function() {
  page_fluid(
    class = "section-movilidad", 
    useShinyjs(),
    init_tooltips(),

    # Back navigation
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'urban', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Movilidad Urbana"
      )
    ),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--movilidad-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Movilidad", class = "dashboard-header")
        )
      )
    ),
    
    # Vehicle and bicycle distribution - using plot cards grid
    div(
      class = "plot-cards-grid two-columns",
      
      # Bicycles Plot Card
      div(
        class = "plot-card plot-card-movilidad",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Bicicletas por Hogar",
              create_tooltip("<b>ID</b>: Suma de PER Q68, Q69, Q70 <br>
                <b>Pregunta</b>: Cuantas bicicletas para NIÑOS/ADOLESCENTES/ADULTOS hay disponibles en su hogar? (poner 0 si no se cuenta con ninguna de este tipo) <br>
                 <b>Escala</b>: Numérica")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("bicycles_pie", height = "400px")
        )
      ),
      
      # Vehicles Plot Card
      div(
        class = "plot-card plot-card-movilidad",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Vehículos Motorizados por Hogar",
              create_tooltip("<b>ID</b>: PER Q66 <br>
                <b>Pregunta</b>: Cuantos vehiculos de motor hay disponibles en su hogar? (Si no tiene ninguno, poner 0) <br>
                 <b>Escala</b>: Numérica")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("vehicles_pie", height = "400px")
        )
      )
    ),
    
    # Transportation modes tabset - using new tab structure
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "transport_modes_tabs",
        
        # Work Transport Tab
        nav_panel(
          title = "Transporte al Trabajo",
          value = "work_transport",
          
          div(
            class = "plot-card plot-card-movilidad",
            
            # Plot Header
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Modos de Transporte Utilizados para ir al Trabajo",
                  create_tooltip("<b>ID</b>: PER Q72.1-Q72.12 <br>
                    <b>Pregunta</b>: ¿Cuáles de los siguientes medios de transporte utiliza para ir al trabajo? <br>
                     <b>Escala</b>: 1=Sí; 2=No (múltiples opciones)")
                )
              )
            ),
            
            # Plot Content
            div(
              class = "plot-content",
              plotlyOutput("work_transport_plot", height = "550px")
            )
          )
        ),
        
        # General Transport Tab
        nav_panel(
          title = "Transporte General",
          value = "general_transport",
          
          div(
            class = "plot-card plot-card-movilidad",
            
            # Plot Header
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Modos de Transporte Utilizados de Manera General",
                  create_tooltip("<b>ID</b>: PER Q172.1-Q172.12 <br>
                    <b>Pregunta</b>: ¿Cuáles de los siguientes medios de transporte utiliza de manera general? <br>
                     <b>Escala</b>: 1=Sí; 2=No (múltiples opciones)")
                )
              )
            ),
            
            # Plot Content
            div(
              class = "plot-content",
              plotlyOutput("general_transport_plot", height = "550px")
            )
          )
        )
      )
    )
  )
}