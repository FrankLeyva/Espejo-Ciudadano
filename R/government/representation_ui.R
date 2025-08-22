# representation_ui.R - Updated to match new styling system
representationUI <- function() {
  page_fluid(
    class = "section-gobierno",
    useShinyjs(),
    init_tooltips(),

    tags$head(
      tags$script(HTML("
        $(document).ready(function() {
          // Function to resize all leaflet maps
          function resizeLeafletMaps() {
            setTimeout(function() {
              $('.leaflet-container').each(function() {
                var mapId = $(this).closest('[id]').attr('id');
                if (mapId && window[mapId] && window[mapId].getMap) {
                  window[mapId].getMap().invalidateSize();
                }
              });
              
              // Alternative method using HTMLWidgets
              if (window.HTMLWidgets) {
                HTMLWidgets.find('.leaflet').forEach(function(widget) {
                  if (widget.getMap) {
                    widget.getMap().invalidateSize();
                  }
                });
              }
            }, 100);
          }
          
          // Trigger resize when tabs change
          $(document).on('shown.bs.tab', 'a[data-bs-toggle=\"tab\"]', function (e) {
            resizeLeafletMaps();
          });
          
          // Also trigger on window resize
          $(window).resize(function() {
            resizeLeafletMaps();
          });
          
          // Initial resize after page load
          setTimeout(resizeLeafletMaps, 500);
        });
      "))
    ),

    # Back navigation
    div(
      class = "mb-4",
      tags$a(
        href = "#",
        class = "text-decoration-none",
        onclick = "Shiny.setInputValue('nav_target', 'government', {priority: 'event'}); return false;",
        tags$i(class = "fas fa-arrow-left me-2"),
        "Volver a Instituciones"
      )
    ),

    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--gobierno-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Representación Política", class = "dashboard-header")
        )
      )
    ),
    
    # Value boxes section
    layout_columns(
      col_widths = c(3, 3, 3, 3), 
      
      value_box_with_title_tooltip(
        title = "Regidores: Representación de Intereses",
        value = textOutput("regidores_rating"),
        showcase = bsicons::bs_icon("person-check-fill"),
        theme = value_box_theme(bg = "var(--gobierno-color)", fg = "white"),
        tooltip_text = "<b>ID</b>: PAR Q11 <br>
               <b>Pregunta</b>: ¿Qué tanto cree que los REGIDORES representan los intereses de los ciudadanos? <br>
                <b>Escala</b>: 1-10"
      ),
      
      value_box_with_title_tooltip(
        title = "Síndico(a): Representación de Intereses",
        value = textOutput("sindico_rating"),
        showcase = bsicons::bs_icon("person-check-fill"),
        theme = value_box_theme(bg = "var(--gobierno-color)", fg = "white"),
        tooltip_text = "<b>ID</b>: PAR Q12 <br>
               <b>Pregunta</b>: ¿Qué tanto cree que EL/LA SÍNDICO(A) represente los intereses de los ciudadanos? <br>
                <b>Escala</b>: 1-10",
        force_icon_color = "rgba(255, 255, 255, 0.8)"
      ),
      
      value_box_with_title_tooltip(
        title = "Diputado(a) Local: Representación de Intereses",
        value = textOutput("diputado_local_rating"),
        showcase = bsicons::bs_icon("person-check-fill"),
        theme = value_box_theme(bg = "var(--gobierno-color)", fg = "white"),
        tooltip_text = "<b>ID</b>: PAR Q13 <br>
               <b>Pregunta</b>: ¿Qué tanto cree que EL/LA DIPUTADO(A) LOCAL represente los intereses de los ciudadanos? <br>
                <b>Escala</b>: 1-10"
      ),
      
      value_box_with_title_tooltip(
        title = "Diputado(a) Federal: Representación de Intereses",
        value = textOutput("diputado_federal_rating"),
        showcase = bsicons::bs_icon("person-check-fill"),
        theme = value_box_theme(bg = "var(--gobierno-color)", fg = "white"),
        tooltip_text = "<b>ID</b>: PAR Q14 <br>
               <b>Pregunta</b>: ¿Qué tanto cree que EL/LA DIPUTADO(A) FEDERAL represente los intereses de los ciudadanos? <br>
                <b>Escala</b>: 1-10",
        force_icon_color = "rgba(255, 255, 255, 0.8)"
      )
    ),
    
    # Content Section: Representative Knowledge Maps
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "knowledge_tabs",
        
        # Regidor Knowledge Tab
        nav_panel(
          title = "Conocimiento de Regidor(a)",
          value = "regidor_knowledge",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento del Regidor(a) por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q16.3 <br>
                    <b>Pregunta</b>: ¿Conoce usted el nombre de su regidor/a? <br>
                     <b>Escala</b>: 1=Sí; 2=No")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_regidor_knowledge_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("regidor_knowledge_map", height = "500px")
            )
          )
        ),
        
        # Sindico Knowledge Tab
        nav_panel(
          title = "Conocimiento de Síndico(a)",
          value = "sindico_knowledge",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento del Síndico(a) por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q16.4 <br>
                    <b>Pregunta</b>: ¿Conoce usted el nombre de su síndico/a? <br>
                     <b>Escala</b>: 1=Sí; 2=No")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_sindico_knowledge_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("sindico_knowledge_map", height = "500px")
            )
          )
        ),
        
        # Local Deputy Knowledge Tab
        nav_panel(
          title = "Diputado(a) Local/Estatal",
          value = "diputado_local_knowledge",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento del Diputado(a) Local/Estatal por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q16.2 <br>
                    <b>Pregunta</b>: ¿Conoce usted el nombre de su diputado/a local y/o estatal? <br>
                     <b>Escala</b>: 1=Sí; 2=No")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_diputadol_knowledge_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("diputadol_knowledge_map", height = "500px")
            )
          )
        ),
        
        # Federal Deputy Knowledge Tab
        nav_panel(
          title = "Diputado(a) Federal",
          value = "diputado_federal_knowledge",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento del Diputado(a) Federal por Distrito",
                  create_tooltip("<b>ID</b>: PAR Q16.1 <br>
                    <b>Pregunta</b>: ¿Conoce usted el nombre de su diputado/a federal? <br>
                     <b>Escala</b>: 1=Sí; 2=No")
                )
              ),
              div(
                class = "plot-actions",
                downloadButton(
                  "download_diputadof_knowledge_map", 
                  "", 
                  icon = icon("download"), 
                  class = "plot-action-btn",
                  title = "Descargar mapa"
                )
              )
            ),
            
            div(
              class = "plot-content",
              leafletOutput("diputadof_knowledge_map", height = "500px")
            )
          )
        )
      )
    ),
    
    # Content Section: Specific Knowledge Charts
    div(
      class = "plot-tabs-container",
      
      navset_tab(
        id = "specific_knowledge_tabs",
        
        # Regidores Knowledge Chart Tab
        nav_panel(
          title = "Conocimiento Específico - Regidores",
          value = "regidores_specific",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento Específico de Regidores",
                  create_tooltip("<b>Descripción</b>: Nivel de conocimiento específico de los regidores por parte de los ciudadanos encuestados")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("regidores_knowledge_chart", height = "500px")
            )
          )
        ),
        
        # Local Deputies Knowledge Chart Tab
        nav_panel(
          title = "Conocimiento Específico - Diputados Locales",
          value = "diputados_locales_specific",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento Específico de Diputados Locales",
                  create_tooltip("<b>Descripción</b>: Nivel de conocimiento específico de los diputados locales por parte de los ciudadanos encuestados")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("diputados_locales_knowledge_chart", height = "500px")
            )
          )
        ),
        
        # Federal Deputies Knowledge Chart Tab
        nav_panel(
          title = "Conocimiento Específico - Diputados Federales",
          value = "diputados_federales_specific",
          
          div(
            class = "plot-card plot-card-gobierno",
            
            div(
              class = "plot-header",
              div(
                class = "plot-header-content",
                h6(
                  class = "plot-title",
                  "Conocimiento Específico de Diputados Federales",
                  create_tooltip("<b>Descripción</b>: Nivel de conocimiento específico de los diputados federales por parte de los ciudadanos encuestados")
                )
              )
            ),
            
            div(
              class = "plot-content",
              plotlyOutput("diputados_federales_knowledge_chart", height = "500px")
            )
          )
        )
      )
    )
  )
}