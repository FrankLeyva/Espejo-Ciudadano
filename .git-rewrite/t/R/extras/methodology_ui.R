# methodology_ui.R - Updated to match new style guide

methodologyUI <- function() {
  page_fluid(
    class = "section-extras",
    useShinyjs(),
    init_tooltips(),
    
    # Header
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          style = paste0("background-color: var(--extras-color) !important; 
            color: white !important; 
            font-family: var(--font-display) !important;
            font-weight: bolder !important; 
            text-align: center !important; 
            border-bottom: none !important;"),
          h1("Metodología y Descarga de Datos", class = "dashboard-header")
        )
      )
    ),
    
    # Downloads section - Full width card
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "page-card page-card-extras",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Descargar Datos de Encuestas")
          )
        ),
        
        # Card Content
        div(
          class = "page-content",
          # Tabs for switching between years
          div(
            class = "year-tabs",
            navset_pill(
              id = "download_year_tabs",
              
              nav_panel(
                title = "Datos 2024",
                value = "2024",
                
                div(
                  class = "plot-cards-grid two-columns",
                  
                  # PER_2024 Download Card
                  div(
                    class = "download-card",
                    style = "background: #f8f9fa; border: 2px solid #e9ecef; border-radius: var(--border-radius); padding: 1.5rem; text-align: center;",
                    div(
                      class = "download-icon mb-3",
                      style = "color: var(--extras-color);",
                      bsicons::bs_icon("clipboard-data", size = "2em")
                    ),
                    h5(class = "mb-3", style = "font-family: var(--font-display);", "Encuesta de Percepción Ciudadana 2024"),
                    p(class = "mb-2", "Datos recopilados en febrero 2025"),
                    p(class = "text-muted mb-3", textOutput("per_2024_info")),
                    div(
                      class = "d-grid gap-2",
                      downloadButton(
                        "download_per_2024", 
                        "Descargar PER_2024 (CSV)",
                        class = "btn btn-outline-primary",
                        style = "border-color: var(--extras-color); color: var(--extras-color);"
                      ),
                      actionLink(
                        "view_per_2024_metadata", 
                        "Ver diccionario de datos",
                        class = "text-center mt-2",
                        style = "color: var(--extras-color);"
                      )
                    )
                  ),
                  
                  # PAR_2024 Download Card
                  div(
                    class = "download-card",
                    style = "background: #f8f9fa; border: 2px solid #e9ecef; border-radius: var(--border-radius); padding: 1.5rem; text-align: center;",
                    div(
                      class = "download-icon mb-3",
                      style = "color: var(--extras-color);",
                      bsicons::bs_icon("people-fill", size = "2em")
                    ),
                    h5(class = "mb-3", style = "font-family: var(--font-display);", "Encuesta de Participación Ciudadana 2024"),
                    p(class = "mb-2", "Datos recopilados en febrero 2025"),
                    p(class = "text-muted mb-3", textOutput("par_2024_info")),
                    div(
                      class = "d-grid gap-2",
                      downloadButton(
                        "download_par_2024", 
                        "Descargar PAR_2024 (CSV)",
                        class = "btn btn-outline-primary",
                        style = "border-color: var(--extras-color); color: var(--extras-color);"
                      ),
                      actionLink(
                        "view_par_2024_metadata", 
                        "Ver diccionario de datos",
                        class = "text-center mt-2",
                        style = "color: var(--extras-color);"
                      )
                    )
                  )
                )
              ),
              
              nav_panel(
                title = "Datos 2023",
                value = "2023",
                
                div(
                  class = "plot-cards-grid two-columns",
                  
                  # PER_2023 Download Card
                  div(
                    class = "download-card",
                    style = "background: #f8f9fa; border: 2px solid #e9ecef; border-radius: var(--border-radius); padding: 1.5rem; text-align: center;",
                    div(
                      class = "download-icon mb-3",
                      style = "color: var(--extras-color);",
                      bsicons::bs_icon("clipboard-data", size = "2em")
                    ),
                    h5(class = "mb-3", style = "font-family: var(--font-display);", "Encuesta de Percepción Ciudadana 2023"),
                    p(class = "mb-2", "Datos recopilados en noviembre 2023"),
                    p(class = "text-muted mb-3", textOutput("per_2023_info")),
                    div(
                      class = "d-grid gap-2",
                      downloadButton(
                        "download_per_2023", 
                        "Descargar PER_2023 (CSV)",
                        class = "btn btn-outline-primary",
                        style = "border-color: var(--extras-color); color: var(--extras-color);"
                      ),
                      actionLink(
                        "view_per_2023_metadata", 
                        "Ver diccionario de datos",
                        class = "text-center mt-2",
                        style = "color: var(--extras-color);"
                      )
                    )
                  ),
                  
                  # PAR_2023 Download Card
                  div(
                    class = "download-card",
                    style = "background: #f8f9fa; border: 2px solid #e9ecef; border-radius: var(--border-radius); padding: 1.5rem; text-align: center;",
                    div(
                      class = "download-icon mb-3",
                      style = "color: var(--extras-color);",
                      bsicons::bs_icon("people-fill", size = "2em")
                    ),
                    h5(class = "mb-3", style = "font-family: var(--font-display);", "Encuesta de Participación Ciudadana 2023"),
                    p(class = "mb-2", "Datos recopilados en noviembre 2023"),
                    p(class = "text-muted mb-3", textOutput("par_2023_info")),
                    div(
                      class = "d-grid gap-2",
                      downloadButton(
                        "download_par_2023", 
                        "Descargar PAR_2023 (CSV)",
                        class = "btn btn-outline-primary",
                        style = "border-color: var(--extras-color); color: var(--extras-color);"
                      ),
                      actionLink(
                        "view_par_2023_metadata", 
                        "Ver diccionario de datos",
                        class = "text-center mt-2",
                        style = "color: var(--extras-color);"
                      )
                    )
                  )
                )
              )
            )
          ),
          
          # Information callout about data comparison
          div(
            class = "alert mt-4",
            style = "background-color: color-mix(in srgb, var(--extras-color) 10%, transparent); border-left: 4px solid var(--extras-color);",
            h5("Comparación de datos entre años", style = "font-family: var(--font-display);"),
            p("Las encuestas de 2023 y 2024 siguen la misma metodología, lo que permite hacer comparaciones directas entre ambos conjuntos de datos. Los usuarios pueden descargar ambos años para realizar análisis comparativos o de tendencias. Algunas preguntas específicas pueden haber cambiado o sido actualizadas entre los diferentes años, consulte el diccionario de datos para más detalles.")
          ),
          
          # Hidden modal controls
          tags$div(
            id = "metadata_modal_controls",
            style = "display: none;",
            actionButton("show_metadata_modal", "Show Data Dictionary")
          )
        )
      )
    ),
    
    # Methodology explanation - Full width card
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "page-card page-card-extras",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Metodología de las Encuestas")
          )
        ),
        
        # Card Content
        div(
          class = "page-content",
          div(
            class = "methodology-section",
            
            # Introduction
            p("Con el propósito de obtener información precisa, confiable y comparativa a lo largo del tiempo desde 2011, se lleva a cabo una investigación cuantitativa que combina tanto el enfoque transversal como longitudinal. Esta investigación se basa en dos encuestas dirigidas a la ciudadanía:"),
            
            tags$ol(
              tags$li(tags$strong("Encuesta de percepción ciudadana."), " Diseñada para evaluar las percepciones y experiencias de los ciudadanos respecto a diversos aspectos de la vida en Ciudad Juárez."),
              tags$li(tags$strong("Encuesta de participación ciudadana y buen gobierno."), " Enfocada en medir los niveles de involucramiento cívico y la percepción sobre la gobernanza local.")
            ),
            
            p("Ambos estudios fueron diseñados con características metodológicas distintas y muestras específicas. Sin embargo, se realizaron de forma simultánea durante el mismo período de tiempo, dado que forman parte de una consulta ciudadana integral que busca presentar un panorama completo de la realidad social de la ciudad."),
            
            p("Las encuestas están respaldadas por una metodología robusta de múltiples etapas y una muestra estadística representativa y adecuada de la población juarense de 18 años en adelante. Este enfoque combina diversas técnicas estadísticas para garantizar la fiabilidad y la relevancia de los resultados obtenidos."),
            
            # Methodological continuity section
            div(
              class = "alert",
              style = "background-color: color-mix(in srgb, var(--extras-color) 10%, transparent); border-left: 4px solid var(--extras-color);",
              h5("Continuidad metodológica en las encuestas 2023-2024", style = "font-family: var(--font-display);"),
              p("La metodología aplicada en las encuestas de 2023 y 2024 mantiene la misma estructura, técnicas de muestreo y selección de participantes. Esta consistencia metodológica es fundamental para asegurar la comparabilidad de los datos entre diferentes años, permitiendo análisis de tendencias y cambios en la percepción y participación ciudadana."),
              p("Las variaciones entre ambos años están principalmente relacionadas con ajustes menores en algunas preguntas específicas para reflejar mejor el contexto actual, pero sin alterar la esencia y objetivos de la investigación.")
            ),
            
            # Sample size determination
            h5("Determinación del tamaño de la muestra", style = "font-family: var(--font-display);"),
            
            p("El marco de muestra está conformado por la población mayor de 18 años de la zona urbana de Ciudad Juárez, Chihuahua, según los datos del Censo Nacional de Población y Vivienda de 2020 del INEGI, junto con las Áreas de Geoestadística Básica y la delimitación territorial de los distritos electorales locales proporcionados por el Instituto Nacional Electoral."),
            
            p("Para determinar el tamaño de la muestra, se emplea la ecuación estadística para proporciones poblacionales, que se presenta a continuación:"),
            
            # Formula in a styled box
            div(
              class = "formula-box",
              style = "background: #f8f9fa; border: 2px solid var(--extras-color); border-radius: var(--border-radius); padding: 2rem; text-align: center; margin: 2rem 0;",
              withMathJax(),
              "$$n = \\frac{Z^2(p*q)}{e^2 + \\frac{Z^2(p*q)}{N}}$$"
            ),
            
            # Variable definitions
            p("Donde:"),
            tags$ul(
              tags$li(tags$strong("n:"), " es el tamaño de la muestra a estimar."),
              tags$li(tags$strong("Z:"), " es el nivel de confianza deseado (95%), con referencia a municipios con más de 100,000 habitantes, utilizando la tabla Z de distribución normal."),
              tags$li(tags$strong("p:"), " es la probabilidad de éxito o la proporción de la población con la característica deseada (0.5 para maximizar el tamaño de la muestra)."),
              tags$li(tags$strong("q:"), " es la probabilidad de fracaso o la proporción de la población sin la característica deseada (0.5)."),
              tags$li(tags$strong("e:"), " es el nivel de error estadístico aceptado (±4.1%)."),
              tags$li(tags$strong("N:"), " es el tamaño de la población, considerando el número de viviendas en el municipio para este estudio.")
            )
          )
        )
      )
    ),
    
    # Sampling method - Full width card
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "page-card page-card-extras",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Método de Muestreo por Semillas")
          )
        ),
        
        # Card Content
        div(
          class = "page-content",
          
          p("Se utilizó un método de muestreo probabilístico para obtener una muestra representativa de la población de interés. Este método consistió en seleccionar aleatoriamente áreas geográficas específicas (AGEB) dentro de cada distrito electoral y recoger cuestionarios en puntos de levantamiento designados como «semillas»."),
          
          p("Para este estudio, una semilla se define como el AGEB seleccionado aleatoriamente mediante el método de Muestreo Aleatorio Simple y se seleccionaron 57 semillas distribuidas proporcionalmente entre los distritos electorales de la ciudad."),
          
          p("El número de cuestionarios por AGEB se determinó en función del peso proporcional de la población. En cada semilla, se eligió un punto de partida y, siguiendo el recorrido de las manecillas del reloj, se encuestó una vivienda por manzana, saltando al menos dos viviendas entre cada entrevistada y limitando las entrevistas a un máximo de cinco por manzana. Se permitió la sustitución en caso de no poder completar el cuestionario o si quedaba incompleto."),
          
          # Visual diagram of sampling methodology (keeping existing complex SVG structure)
          div(
            class = "methodology-diagram",
            style = "background: #f8f9fa; border: 2px solid #e9ecef; border-radius: var(--border-radius); padding: 2rem; margin: 2rem 0;",
            h6(style = "color: var(--extras-color); font-weight: 600; margin-bottom: 1.5rem; text-align: center; font-family: var(--font-display);", "Metodología de Muestreo por Semillas"),
            
            div(
              style = "width: 100%; max-width: 800px; margin: 0 auto;",
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                # Left column: Seed area
                div(
                  style = "flex: 1; padding: 15px;",
                  h6(style = "color: #666; font-weight: 600; margin-bottom: 15px; text-align: center; font-family: var(--font-display);", "Área semilla"),
                  # Grid of seed area (keeping existing complex structure)
                  div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    # Row 1
                    div(
                      style = "display: flex; justify-content: center;",
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;"),
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;"),
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;")
                    ),
                    # Row 2 with AGEB seed
                    div(
                      style = "display: flex; justify-content: center;",
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;"),
                      div(style = "width: 120px; height: 60px; background-color: var(--extras-color); margin: 3px; border: 2px solid var(--extras-color); border-radius: 15px; color: white; font-weight: bold; display: flex; justify-content: center; align-items: center; flex-direction: column;", 
                         div(style = "font-size: 14px;", "Semilla"),
                         div(style = "font-size: 12px;", "Cuadrada")),
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;")
                    ),
                    # Row 3
                    div(
                      style = "display: flex; justify-content: center;",
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;"),
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;"),
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;")
                    ),
                    # Arrows pointing to seed
                    div(
                      style = "position: relative; width: 100%; height: 0;",
                      div(style = "position: absolute; top: -140px; left: 50px; transform: rotate(-45deg);", 
                          bsicons::bs_icon("arrow-up", width = "24px", height = "24px", style = paste0("color: ", "var(--extras-color);"))),
                      div(style = "position: absolute; top: -55px; left: 20px; transform: rotate(-90deg);", 
                          bsicons::bs_icon("arrow-up", width = "24px", height = "24px", style = paste0("color: ", "var(--extras-color);"))),
                      div(style = "position: absolute; top: -140px; right: 50px; transform: rotate(45deg);", 
                          bsicons::bs_icon("arrow-up", width = "24px", height = "24px", style = paste0("color: ", "var(--extras-color);"))),
                      div(style = "position: absolute; top: -55px; right: 20px; transform: rotate(90deg);", 
                          bsicons::bs_icon("arrow-up", width = "24px", height = "24px", style = paste0("color: ", "var(--extras-color);")))
                    )
                  )
                ),
                
                # Connection arrow
                div(
                  style = "margin: 0 15px;",
                  bsicons::bs_icon("chevron-right", width = "40px", height = "40px", style = "color: #AAAAAA;")
                ),
                
                # Right column: Block within seed (keeping existing complex house SVG structure)
                div(
                  style = "flex: 1.2; padding: 15px;",
                  h6(style = "color: #666; font-weight: 600; margin-bottom: 15px; text-align: center; font-family: var(--font-display);", "Manzana dentro de semilla"),
                  # Visualization of households (keeping all existing SVG houses)
                  div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    # Right arrow
                    div(
                      style = "width: 100%; text-align: center; margin-bottom: 10px;",
                      bsicons::bs_icon("arrow-right", width = "32px", height = "32px", style = paste0("color: ", "var(--extras-color);"))
                    ),
                    # Row 1 of houses
                    div(
                      style = "display: flex; justify-content: space-between; width: 100%; margin-bottom: 15px;",
                      # House 1 - surveyed (updated color to use extras-color)
                      div(style = "width: 45px; height: 45px;", 
                          HTML(paste0('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="', 'var(--extras-color)', '" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="', 'var(--extras-color)', '" stroke="', 'var(--extras-color)', '" stroke-width="2"/>
                              </svg>'))),
                      # Houses 2-3 - not surveyed
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>')),
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>')),
                      # House 4 - surveyed
                      div(style = "width: 45px; height: 45px;", 
                          HTML(paste0('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="', 'var(--extras-color)', '" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="', 'var(--extras-color)', '" stroke="', 'var(--extras-color)', '" stroke-width="2"/>
                              </svg>'))),
                      # House 5 - not surveyed
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>'))
                    ),
                    # Row 2 of houses (similar pattern with updated colors)
                    div(
                      style = "display: flex; justify-content: space-between; width: 100%;",
                      # House 6 - surveyed
                      div(style = "width: 45px; height: 45px;", 
                          HTML(paste0('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="', 'var(--extras-color)', '" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="', 'var(--extras-color)', '" stroke="', 'var(--extras-color)', '" stroke-width="2"/>
                              </svg>'))),
                      # Houses 7-8 - not surveyed
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>')),
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>')),
                      # House 9 - surveyed
                      div(style = "width: 45px; height: 45px;", 
                          HTML(paste0('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="', 'var(--extras-color)', '" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="', 'var(--extras-color)', '" stroke="', 'var(--extras-color)', '" stroke-width="2"/>
                              </svg>'))),
                      # House 10 - not surveyed
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>'))
                    ),
                    # Left arrow
                    div(
                      style = "width: 100%; text-align: center; margin-top: 10px;",
                      bsicons::bs_icon("arrow-left", width = "32px", height = "32px", style = paste0("color: ", "var(--extras-color);"))
                    )
                  ),
                  
                  # Legend for houses
                  div(
                    style = "margin-top: 20px; display: flex; justify-content: flex-end;",
                    div(
                      style = "display: flex; flex-direction: column; gap: 8px;",
                      div(
                        style = "display: flex; align-items: center;",
                        div(style = "width: 30px; height: 30px; margin-right: 10px;", 
                            HTML(paste0('<svg width="30" height="30" viewBox="0 0 30 30" fill="none" xmlns="http://www.w3.org/2000/svg">
                                  <path d="M3 28V12L15 3L27 12V28H3Z" fill="white" stroke="', 'var(--extras-color)', '" stroke-width="2"/>
                                  <path d="M15 3L3 12H27L15 3Z" fill="', 'var(--extras-color)', '" stroke="', 'var(--extras-color)', '" stroke-width="1"/>
                                </svg>'))),
                        div(style = "color: #555; font-size: 14px;", "Domicilio encuestado")
                      ),
                      div(
                        style = "display: flex; align-items: center;",
                        div(style = "width: 30px; height: 30px; margin-right: 10px;", 
                            HTML('<svg width="30" height="30" viewBox="0 0 30 30" fill="none" xmlns="http://www.w3.org/2000/svg">
                                  <path d="M3 28V12L15 3L27 12V28H3Z" fill="white" stroke="#CCCCCC" stroke-width="2"/>
                                  <path d="M15 3L3 12H27L15 3Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="1"/>
                                </svg>')),
                        div(
                          style = "color: #555; font-size: 14px;", 
                          "Domicilio sustituido",
                          div(style = "color: #888; font-size: 12px; font-style: italic;", "(No encuestado)")
                        )
                      )
                    )
                  )
                )
              )
            ),
            
            # Explanation of the method
            div(
              style = "margin-top: 25px; text-align: left; max-width: 800px; margin: 25px auto 0;",
              p("Esta técnica de muestreo garantiza que la distribución de la muestra sea representativa de toda la extensión territorial de Ciudad Juárez, lo que permite obtener información estadísticamente válida sobre la población."),
              p("En cada AGEB seleccionada como semilla, se recorre una manzana siguiendo el sentido de las manecillas del reloj, seleccionando domicilios específicos y saltando al menos dos viviendas entre cada entrevista para asegurar la aleatoriedad en la selección de participantes.")
            )
          )
        )
      )
    ),
    
    # Profile and field work - Two column layout
    div(
      class = "plot-cards-grid two-columns",
      
      # Profile Card
      div(
        class = "page-card page-card-extras",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Perfil del Entrevistado")
          )
        ),
        
        div(
          class = "page-content",
          h5("Criterios de selección", style = "font-family: var(--font-display);"),
          p("Hombres y mujeres de 18 años o más con residencia en la vivienda seleccionada. Se aplicaron cuotas por género y grupo de edad para garantizar la representatividad demográfica de la muestra."),
          
          h5("Error muestral y nivel de confianza", style = "font-family: var(--font-display);"),
          p("El diseño muestral se realizó considerando un nivel de confianza del 95% y un margen de error de ±4.1%, lo que garantiza la robustez estadística de los resultados obtenidos. Se implementaron protocolos de validación y control de calidad en todas las etapas del proceso para asegurar la integridad de los datos recopilados.")
        )
      ),
      
      # Field work Card
      div(
        class = "page-card page-card-extras",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Levantamiento de Campo")
          )
        ),
        
        div(
          class = "page-content",
          h5("Fechas de aplicación", style = "font-family: var(--font-display);"),
          p("El levantamiento de campo se realizó en los siguientes periodos:"),
          tags$ul(
            tags$li(tags$strong("Encuestas 2023:"), " del 6 al 26 de noviembre del 2023."),
            tags$li(tags$strong("Encuestas 2024:"), " del 10 al 28 de febrero del 2025.")
          ),
          
          h5("Modalidad de aplicación", style = "font-family: var(--font-display);"),
          p("En todos los casos, se aplicó cada cuestionario cara a cara en la vivienda del entrevistado, con una duración aproximada de 25-30 minutos por encuesta."),
          
          h5("Procesamiento y análisis", style = "font-family: var(--font-display);"),
          p("Los datos recopilados se procesaron utilizando software estadístico especializado que permite realizar análisis descriptivos, inferenciales y comparativos. Se aplicaron técnicas de ponderación para ajustar posibles desviaciones en la representatividad de la muestra con respecto a los parámetros poblacionales establecidos por el INEGI.")
        )
      )
    ),
    
    # Technical notes - Full width card
    div(
      class = "plot-cards-grid single-column",
      
      div(
        class = "page-card page-card-extras",
        
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Notas Técnicas sobre los Datos")
          )
        ),
        
        div(
          class = "page-content",
          
          div(
            class = "alert",
            style = "background-color: color-mix(in srgb, var(--extras-color) 10%, transparent); border-left: 4px solid var(--extras-color);",
            h5("Formato de los archivos descargables", style = "font-family: var(--font-display);"),
            p("Los archivos CSV proporcionados contienen todas las respuestas anónimas de las encuestas. Cada fila representa un encuestado, y cada columna corresponde a una pregunta del cuestionario. Los datos han sido preprocesados para garantizar consistencia y calidad."),
            p("Las columnas generalmente siguen la siguiente nomenclatura:"),
            tags$ul(
              tags$li(tags$strong("Q[número]"), ": Respuestas a preguntas principales"),
              tags$li(tags$strong("Q[número].[subnúmero]"), ": Respuestas a subpreguntas o ítems dentro de una pregunta principal"),
              tags$li(tags$strong("DISTRICT"), ": Distrito electoral al que pertenece el encuestado"),
              tags$li(tags$strong("GENDER"), ": Género del encuestado"),
              tags$li(tags$strong("AGE_GROUP"), ": Grupo de edad del encuestado"),
              tags$li(tags$strong("TIMESTAMP"), ": Fecha y hora de la entrevista")
            )
          ),
          
          div(
            class = "alert mt-4",
            style = "background-color: color-mix(in srgb, #ffc107 10%, transparent); border-left: 4px solid #ffc107;",
            h5("Diferencias entre conjuntos de datos 2023 y 2024", style = "font-family: var(--font-display);"),
            p("Al comparar datos entre 2023 y 2024, tenga en cuenta que:"),
            tags$ul(
              tags$li("Algunas preguntas específicas pueden haber sido modificadas o actualizadas."),
              tags$li("La codificación de ciertas respuestas puede variar. Consulte el diccionario de datos para cada conjunto."),
              tags$li("Los identificadores de preguntas (Q[número]) son consistentes en la mayoría de los casos, pero puede haber excepciones."),
              tags$li("La estructura general de ambos conjuntos de datos es compatible para realizar análisis comparativos.")
            ),
            p("Para más información sobre la estructura de los datos o el diccionario de variables, puede consultar los diccionarios de datos disponibles para cada conjunto.")
          ),
          
          div(
            class = "alert mt-4",
            style = "background-color: color-mix(in srgb, var(--extras-color) 10%, transparent); border-left: 4px solid var(--extras-color);",
            h5("Interpretación de escalas y tipos de datos", style = "font-family: var(--font-display);"),
            p("En los diccionarios de datos, encontrará los siguientes tipos de escalas:"),
            tags$ul(
              tags$li(tags$strong("Nominal:"), " Categorías sin orden jerárquico (ej. género, estado civil)."),
              tags$li(tags$strong("Ordinal:"), " Categorías con un orden natural (ej. nivel de satisfacción: bajo, medio, alto)."),
              tags$li(tags$strong("Intervalo:"), " Escala numérica con intervalos iguales (ej. calificación de 1 a 10)."),
              tags$li(tags$strong("Razón:"), " Escala numérica con cero absoluto (ej. edad, ingresos)."),
              tags$li(tags$strong("Binario:"), " Respuestas dicotómicas (ej. sí/no, verdadero/falso).")
            ),
            p("Esta información es útil para determinar qué tipos de análisis estadísticos son apropiados para cada variable.")
          )
        )
      )
    ),
    
    # Footer
    create_dashboard_footer()
  )
}