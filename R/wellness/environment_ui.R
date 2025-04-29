# environmentUI.R
environmentUI <- function() {
  page_fluid(
    class = "section-bienestar", 
    useShinyjs(),
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
          background-color: var(--bienestar-color) !important; /* Bienestar primary color */
          color: white !important;
          font-weight: bold !important;
          border: none !important;
        }
      "))
    ),
    init_tooltips(),

   div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'wellness', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Bienestar Social y Económico"
      )
    ),
    # Header
    layout_columns(
      fill = FALSE,
      card(
        style = "border-top: 4px solid var(--bienestar-color);", 
        card_header(
          h2("Medio Ambiente", class = "text-center")
        )
      )
    ),
    
    # Environmental satisfaction maps
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center",
            "Satisfacción con Aspectos Ambientales",
            # Dynamic tooltip that will be updated based on selected tab
            create_dynamic_tooltip("env_satisfaction_tooltip")
          ),          downloadButton(
            "download_environment_map", 
            "", 
            icon = icon("download"), 
            class = "btn-sm"
          )
        )
      ),
      div(class = "bienestar-pills",
      navset_pill(
        id = "env_satisfaction_tabs",
        tabPanel(
          "Calidad del Aire",
          leafletOutput("air_quality_map", height = "500px")
        ),
        tabPanel(
          "Arbolado Urbano",
          leafletOutput("urban_trees_map", height = "500px")
        ),
        tabPanel(
          "Limpieza de Calles",
          leafletOutput("street_cleanliness_map", height = "500px")
        ),
        tabPanel(
          "Calidad del Agua",
          leafletOutput("water_quality_map", height = "500px")
        )
      )
    )
    ),
    
    # Environmental problems bar chart
    card(
      card_header(

        div(
          class = "d-flex align-items-center",
          "Principales Problemas Ambientales por Colonia",
          create_tooltip("<b>ID</b>: PER Q97 <br>
            <b>Pregunta</b>: 	De las siguientes problemáticas medioambientales, cual cree que sea el mayor problema de la colonia? <br>
             <b>Escala</b>: 1=Neumaticos/ llantas tiradas; 2=Calles sucias/ Basura en las calles; 3=Parque sucios/descuidados; 4=Falta de recoleccion de residuos; 5=Basureros Clandestinos/ Casas/terrenos;6=Terrenos baldios; 7=Otro")
       )
            ),
      plotlyOutput("env_problems_plot", height = "500px")
    ),

  )
}