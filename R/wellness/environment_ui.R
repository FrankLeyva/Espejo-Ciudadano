# environment_ui.R
environmentUI <- function() {
  page_fluid(
    class = "section-bienestar", 
    useShinyjs(),
    init_tooltips(),

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
          h1("Medio Ambiente", class = "dashboard-header")
        )
      )
    ),
    
    # Tab Navigation with Plot Cards (Environmental Satisfaction Maps)
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "environment_tabs",
        
        # Air Quality Tab
        nav_panel(
          title = "Calidad del Aire",
          value = "air_quality",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            # Plot Header
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con la Calidad del Aire por Distrito",
                  create_tooltip("<b>ID</b>: PER Q94.2 <br>
                    <b>Pregunta</b>: En una escala del 1 al 10, qué tan satisfecho está usted con la calidad del aire en su colonia? <br>
                     <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_air_quality_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            # Plot Content
            div(
              class = "plot-content",
              leafletOutput("air_quality_map", height = "500px")
            )
          )
        ),
        
        # Urban Trees Tab
        nav_panel(
          title = "Arbolado Urbano",
          value = "urban_trees",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con el Arbolado Urbano por Distrito",
                  create_tooltip("<b>ID</b>: PER Q94.3 <br>
                    <b>Pregunta</b>: En una escala del 1 al 10, qué tan satisfecho está usted con el arbolado urbano en su colonia? <br>
                     <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_urban_trees_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("urban_trees_map", height = "500px")
            )
          )
        ),
        
        # Street Cleanliness Tab
        nav_panel(
          title = "Limpieza de Calles",
          value = "street_cleanliness",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con la Limpieza de Calles por Distrito",
                  create_tooltip("<b>ID</b>: PER Q94.4 <br>
                    <b>Pregunta</b>: En una escala del 1 al 10, qué tan satisfecho está usted con la limpieza de las calles en su colonia? <br>
                     <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_street_cleanliness_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("street_cleanliness_map", height = "500px")
            )
          )
        ),
        
        # Water Quality Tab
        nav_panel(
          title = "Calidad del Agua",
          value = "water_quality",
          
          div(
            class = "plot-card plot-card-bienestar",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Satisfacción con la Calidad del Agua por Distrito",
                  create_tooltip("<b>ID</b>: PER Q94.1 <br>
                    <b>Pregunta</b>: En una escala del 1 al 10, qué tan satisfecho está usted con la calidad del agua en su colonia? <br>
                     <b>Escala</b>: 1-10 (1=Muy insatisfecho, 10=Muy satisfecho)")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_water_quality_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("water_quality_map", height = "500px")
            )
          )
        )
      )
    ),
    
    # Independent Environmental Problems Plot Card
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "plot-card plot-card-bienestar",
        
        div(
          class = "plot-header",
          div(
            class = "plot-header-content",
            h6(
              class = "plot-title",
              "Principales Problemas Ambientales por Colonia",
              create_tooltip("<b>ID</b>: PER Q97 <br>
                <b>Pregunta</b>: De las siguientes problemáticas medioambientales, ¿cuál cree que sea el mayor problema de la colonia? <br>
                 <b>Escala</b>: 1=Neumáticos/llantas tiradas; 2=Calles sucias/Basura en las calles; 3=Parques sucios/descuidados; 4=Falta de recolección de residuos; 5=Basureros clandestinos/Casas/terrenos; 6=Terrenos baldíos; 7=Otro")
            )
          )
        ),
        
        div(
          class = "plot-content",
          plotlyOutput("env_problems_plot", height = "500px")
        )
      )
    )
  )
}