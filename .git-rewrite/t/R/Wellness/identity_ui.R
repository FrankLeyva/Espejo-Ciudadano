# identity_ui.R

identityUI <- function() {
  page_fluid(
    class = "section-bienestar",
    useShinyjs(),
    init_tooltips(),

    tags$head(
      tags$style(HTML("
        /* Override pill navigation styling for this page */
        .bienestar-pills .nav-pills .nav-link:not(.active) {
          background-color: rgba(240, 240, 240, 0.8);
          color: var(--bienestar-color) !important;
          border: 1px solid rgba(30, 136, 229, 0.2);
          font-weight: bold !important;
        }
        
        .bienestar-pills .nav-pills .nav-link:hover:not(.active) {
          background-color: rgba(30, 136, 229, 0.1);
        }
        
        .bienestar-pills .nav-pills .nav-link.active {
          background-color: var(--bienestar-color) !important;
          color: white !important;
          font-weight: bold !important;
          border: none !important;
        }
      "))
    ),

    # Back navigation
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'wellness', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Calidad de Vida"
      )
    ),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--bienestar-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Identidad y Pertenencia", class = "dashboard-header")
        )
      )
    ),
    
    # First section: Monuments and City Pride
    div(
      class = "plot-cards-grid two-columns",
      
      # Monuments Plot Card
      div(
        class = "plot-card plot-card-bienestar",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Monumentos importantes para la identidad juarense",
              create_tooltip("<b>ID</b>: PER Q65 <br>
                <b>Pregunta</b>: Mencione un LUGAR / EDIFICIO / MONUMENTO que considere representativo de Juarez <br>
                 <b>Escala</b>: Abierta (Respuestas Homologadas)")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("monuments_bar", height = "400px")
        )
      ),
      
      # City Pride Plot Card
      div(
        class = "plot-card plot-card-bienestar",
        
        # Plot Header
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Orgullo por vivir en Ciudad Juárez",
              create_tooltip("<b>ID</b>: PER Q80 <br>
                <b>Pregunta</b>: Que tanto cree que nosotros los ciudadanos estemos orgullosos de ser o vivir en Ciudad Juarez? <br>
                 <b>Escala</b>: 1=Nada; 2=Poco; 3=Algo; 4=Mucho; 5=Ns/Nc")
            )
          )
        ),
        
        # Plot Content
        div(
          class = "plot-content",
          plotlyOutput("city_pride_pie", height = "400px")
        )
      )
    ),
    
    # Second section: Neighborhood Connection with Pill Navigation
    div(
      class = "plot-card plot-card-bienestar",
      style = "margin-top: 2rem;",
      
      # Plot Header
      div(
        class = "plot-header",
        div(
          class = "plot-header-content",
          h6(
            class = "plot-title",
            "Sentido de pertenencia",
            create_dynamic_tooltip("connection_tooltip")
          )
        ),
        div(
          class = "plot-actions",
          downloadButton(
            "download_connection_map", 
            "", 
            icon = icon("download"), 
            class = "plot-action-btn",
            title = "Descargar mapa"
          )
        )
      ),
      
      # Plot Content with Pill Navigation
      div(
        class = "plot-content with-padding",
        div(
          class = "bienestar-pills",
          navset_pill(
            id = "neighborhood_tabs",
            
            nav_panel(
              title = "Vínculo con la colonia o fraccionamiento",
              icon = bsicons::bs_icon("house-fill"),
              
              div(
                style = "margin-top: 1rem;",
                leafletOutput("neighborhood_connection_map", height = "500px")
              )
            ),
            
            nav_panel(
              title = "Vínculo con los vecinos",
              icon = bsicons::bs_icon("people-fill"),
              
              div(
                style = "margin-top: 1rem;",
                leafletOutput("neighbors_connection_map", height = "500px")
              )
            )
          )
        )
      )
    )
  )
}