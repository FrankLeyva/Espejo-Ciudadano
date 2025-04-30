# UI para Dashboard de Vivienda
housingUI <- function() {
  page_fluid(
    class = "section-infraestructura",

    useShinyjs(),
    init_tooltips(),

    tags$head(
      tags$style(HTML("
        /* Override pill navigation styling for this page */
        .infraestructura-pills .nav-pills .nav-link:not(.active) {
          background-color: rgba(240, 240, 240, 0.8);
color: var(--infraestructura-color) !important;
            border: 1px solid rgba(142, 36, 170, 0.2);
          font-weight: bold !important;
        }
        
        .infraestructura-pills .nav-pills .nav-link:hover:not(.active) {
          background-color: rgba(142, 36, 170, 0.1);
        }
             .infraestructura-pills .nav-pills .nav-link.active {
          background-color: var(--infraestructura-color) !important; 
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
        onclick = "Shiny.setInputValue('nav_target', 'infrastructure', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Infraestructura"
      )
    ),
    # Encabezado
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style="border-top: 4px solid var(--infraestructura-color)",

          h2("Vivienda", class = "text-center")
        )
      )
    ),

    card(
    
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center",
"Satisfacción con Aspectos de la Vivienda",
create_dynamic_tooltip("housing_tooltip")
          ),
downloadButton(
  "download_house_satis_map", 
  "", 
  icon = icon("download"), 
  class = "btn-sm"
))),
div(class = "gobierno-pills",
navset_pill(
  id = "housing_tabs",
      # Pestaña: Calidad de materiales
      nav_panel(
        title = "Calidad de Materiales",
        icon = bsicons::bs_icon("bricks"),
        

        
        card(
          leafletOutput("materials_map", height = "500px")
        )
      ),
      
      # Pestaña: Tamaño y espacios
      nav_panel(
        title = "Tamaño y Espacios",
        icon = bsicons::bs_icon("layout-text-window"),
        
        
        card(

          leafletOutput("spaces_map", height = "500px")
        )
      ),
      
      # Pestaña: Accesibilidad de la ubicación
      nav_panel(
        title = "Ubicación y Accesibilidad",
        icon = bsicons::bs_icon("geo-alt"),
      
        card(

          leafletOutput("location_map", height = "500px")
        )
      ),
      
      # Pestaña: Comparativa
      nav_panel(
        title = "Comparativa",
        icon = bsicons::bs_icon("graph-up"),
        
        card(

          plotlyOutput("comparison_plot", height = "500px")
        )
      )
    )
  )
    )
  )
}