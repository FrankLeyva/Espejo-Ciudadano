# UI para Dashboard de Metodología y Descarga de Datos
methodologyUI <- function() {
  header_color <- "#2d2d2d"
  page_fluid(
    useShinyjs(),
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
      ),
      tags$style(HTML("
        /* General styling */
        .methodology-section {
          font-size: 16px;
          line-height: 1.6;
        }
        
        .methodology-section h3 {
          margin-top: 25px;
          margin-bottom: 15px;
          color: #0d6efd;
          font-weight: 600;
          border-bottom: 2px solid #eaecef;
          padding-bottom: 8px;
        }
        
        .methodology-section h4 {
          margin-top: 20px;
          margin-bottom: 10px;
          font-weight: 600;
          color: #495057;
        }
        
        .methodology-section p {
          margin-bottom: 15px;
          text-align: justify;
        }
        
        .methodology-section ol, .methodology-section ul {
          padding-left: 22px;
          margin-bottom: 15px;
        }
        
        .methodology-section li {
          margin-bottom: 5px;
        }
        
        .formula-box {
          background-color: #f8f9fa;
          padding: 15px;
          border-radius: 8px;
          margin: 20px 0;
          font-family: Consolas, Monaco, 'Courier New', monospace;
          text-align: center;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05);
          border-left: 4px solid #0d6efd;
        }
        
        .download-card {
          transition: transform 0.3s, box-shadow 0.3s;
          height: 100%;
        }
        
        .download-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 10px 20px rgba(0,0,0,0.1);
        }
        
        .download-icon {
          font-size: 2.5rem;
          color: #0d6efd;
          margin-bottom: 15px;
        }
        
        .methodology-diagram {
          text-align: center;
          margin: 20px 0;
          padding: 20px;
          background-color: #f8f9fa;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }
        
        /* Custom callout styles */
        .callout {
          padding: 20px;
          margin: 20px 0;
          border: 1px solid #eee;
          border-left-width: 5px;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.03);
        }
        
        .callout-info {
          border-left-color: #5bc0de;
          background-color: #f4f8fa;
        }
        
        .callout-warning {
          border-left-color: #f0ad4e;
          background-color: #faf8f4;
        }
        
        /* Year tabs styling */
        .year-tabs .nav-link {
          padding: 10px 20px;
          border-radius: 8px 8px 0 0;
          font-weight: 600;
          transition: all 0.2s;
        }
        
        .year-tabs .nav-link.active {
          background-color: #0d6efd;
          color: white;
          border-color: #0d6efd;
          box-shadow: 0 -2px 5px rgba(0,0,0,0.1);
        }
        
        /* Data table styling */
        .dataTables_wrapper {
          font-size: 14px;
        }
        
        .dataTables_wrapper .dataTables_length, 
        .dataTables_wrapper .dataTables_filter {
          margin-bottom: 15px;
        }
        
        .dataTables_wrapper .dataTable th {
          background-color: #f8f9fa;
          font-weight: 600;
        }
        
        /* Card styling */
        .card {
          border-radius: 8px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.05);
          margin-bottom: 20px;
          overflow: hidden;
        }
        
        /* Modified to match about_ui.R */
        .card-header {
          background-color: #2d2d2d;
          color: white;
          border-bottom: 1px solid rgba(255, 255, 255, 0.1);
        }
        
        /* Page title styling */
        .page-title {
          font-weight: 700;
          color: white;
          margin-bottom: 20px;
        }
        
        /* Button styling */
        .btn-primary {
          transition: all 0.2s;
        }
        
        .btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
        
        /* Modal styling */
        .modal-header {
          background-color: #f8f9fa;
          border-bottom: 2px solid #eaecef;
        }
        
        .modal-title {
          font-weight: 600;
          color: #0d6efd;
        }
      "))
    ),
    
    theme = bs_theme(
      version = 5,
      bootswatch = "litera",
      primary = "#0d6efd"
    ),
    
    # Encabezado
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          h2("Metodología y Descarga de Datos", class = "text-center page-title"),
          style = paste0("background-color: ", header_color, "; color: white;")
        )
      )
    ),
    
    card(
      card_header(
        h3("Descargar Datos de Encuestas", class = "m-0 fw-bold"),
        style = paste0("background-color: ", header_color, "; color: white;")
      ),
      card_body(
        # Add class to the container div instead
        div(
          class = "year-tabs",
          # Tabs for switching between years - removing the class parameter
          navset_pill(
            id = "download_year_tabs",
            
            nav_panel(
              title = "Datos 2024",
              value = "2024",
              
              layout_columns(
                col_widths = c(6, 6),
                
                # Tarjeta de descarga para PER_2024
                div(
                  class = "download-card",
                  card(
                    card_body(
                      div(class = "text-center"),
                      div(class = "download-icon", bsicons::bs_icon("clipboard-data")),
                      h4(class = "text-center", "Encuesta de Percepción Ciudadana 2024"),
                      p(class = "text-center", "Datos recopilados en febrero 2025"),
                      p(class = "text-center text-muted", textOutput("per_2024_info")),
                      div(
                        class = "d-grid gap-2",
                        downloadButton(
                          "download_per_2024", 
                          "Descargar PER_2024 (CSV)",
                          class = "btn-primary"
                        ),
                        actionLink(
                          "view_per_2024_metadata", 
                          "Ver diccionario de datos",
                          class = "text-center mt-2"
                        )
                      )
                    )
                  )
                ),
                
                # Tarjeta de descarga para PAR_2024
                div(
                  class = "download-card",
                  card(
                    card_body(
                      div(class = "text-center"),
                      div(class = "download-icon", bsicons::bs_icon("people-fill")),
                      h4(class = "text-center", "Encuesta de Participación Ciudadana 2024"),
                      p(class = "text-center", "Datos recopilados en febrero 2025"),
                      p(class = "text-center text-muted", textOutput("par_2024_info")),
                      div(
                        class = "d-grid gap-2",
                        downloadButton(
                          "download_par_2024", 
                          "Descargar PAR_2024 (CSV)",
                          class = "btn-primary"
                        ),
                        actionLink(
                          "view_par_2024_metadata", 
                          "Ver diccionario de datos",
                          class = "text-center mt-2"
                        )
                      )
                    )
                  )
                )
              )
            ),
            
            nav_panel(
              title = "Datos 2023",
              value = "2023",
              
              layout_columns(
                col_widths = c(6, 6),
                
                # Tarjeta de descarga para PER_2023
                div(
                  class = "download-card",
                  card(
                    card_body(
                      div(class = "text-center"),
                      div(class = "download-icon", bsicons::bs_icon("clipboard-data")),
                      h4(class = "text-center", "Encuesta de Percepción Ciudadana 2023"),
                      p(class = "text-center", "Datos recopilados en noviembre 2023"),
                      p(class = "text-center text-muted", textOutput("per_2023_info")),
                      div(
                        class = "d-grid gap-2",
                        downloadButton(
                          "download_per_2023", 
                          "Descargar PER_2023 (CSV)",
                          class = "btn-primary"
                        ),
                        actionLink(
                          "view_per_2023_metadata", 
                          "Ver diccionario de datos",
                          class = "text-center mt-2"
                        )
                      )
                    )
                  )
                ),
                
                # Tarjeta de descarga para PAR_2023
                div(
                  class = "download-card",
                  card(
                    card_body(
                      div(class = "text-center"),
                      div(class = "download-icon", bsicons::bs_icon("people-fill")),
                      h4(class = "text-center", "Encuesta de Participación Ciudadana 2023"),
                      p(class = "text-center", "Datos recopilados en noviembre 2023"),
                      p(class = "text-center text-muted", textOutput("par_2023_info")),
                      div(
                        class = "d-grid gap-2",
                        downloadButton(
                          "download_par_2023", 
                          "Descargar PAR_2023 (CSV)",
                          class = "btn-primary"
                        ),
                        actionLink(
                          "view_par_2023_metadata", 
                          "Ver diccionario de datos",
                          class = "text-center mt-2"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        # Information callout about data comparison
        div(
          class = "callout callout-info mt-4",
          h4("Comparación de datos entre años"),
          p("Las encuestas de 2023 y 2024 siguen la misma metodología, lo que permite hacer comparaciones directas entre ambos conjuntos de datos. Los usuarios pueden descargar ambos años para realizar análisis comparativos o de tendencias. Algunas preguntas específicas pueden haber cambiado o sido actualizadas entre los diferentes años, consulte el diccionario de datos para más detalles.")
        ),
        
        # Data dictionary modal controls (hidden)
        tags$div(
          id = "metadata_modal_controls",
          style = "display: none;",
          actionButton("show_metadata_modal", "Show Data Dictionary")
        )
      )
    ),
    
    # Explicación de metodología
    card(
      card_header(
        h3("Metodología de las Encuestas", class = "m-0")
      ),
      card_body(
        div(
          class = "methodology-section",
          
          # Introducción
          p("Con el propósito de obtener información precisa, confiable y comparativa a lo largo del tiempo desde 2011, se lleva a cabo una investigación cuantitativa que combina tanto el enfoque transversal como longitudinal. Esta investigación se basa en dos encuestas dirigidas a la ciudadanía:"),
          
          tags$ol(
            tags$li(strong("Encuesta de percepción ciudadana."), " Diseñada para evaluar las percepciones y experiencias de los ciudadanos respecto a diversos aspectos de la vida en Ciudad Juárez."),
            tags$li(strong("Encuesta de participación ciudadana y buen gobierno."), " Enfocada en medir los niveles de involucramiento cívico y la percepción sobre la gobernanza local.")
          ),
          
          p("Ambos estudios fueron diseñados con características metodológicas distintas y muestras específicas. Sin embargo, se realizaron de forma simultánea durante el mismo período de tiempo, dado que forman parte de una consulta ciudadana integral que busca presentar un panorama completo de la realidad social de la ciudad."),
          
          p("Las encuestas están respaldadas por una metodología robusta de múltiples etapas y una muestra estadística representativa y adecuada de la población juarense de 18 años en adelante. Este enfoque combina diversas técnicas estadísticas para garantizar la fiabilidad y la relevancia de los resultados obtenidos."),
          
          # Nueva sección sobre continuidad metodológica
          div(
            class = "callout callout-info",
            h4("Continuidad metodológica en las encuestas 2023-2024"),
            p("La metodología aplicada en las encuestas de 2023 y 2024 mantiene la misma estructura, técnicas de muestreo y selección de participantes. Esta consistencia metodológica es fundamental para asegurar la comparabilidad de los datos entre diferentes años, permitiendo análisis de tendencias y cambios en la percepción y participación ciudadana."),
            p("Las variaciones entre ambos años están principalmente relacionadas con ajustes menores en algunas preguntas específicas para reflejar mejor el contexto actual, pero sin alterar la esencia y objetivos de la investigación.")
          ),
          
          # Determinación del tamaño de la muestra
          h3("Determinación del tamaño de la muestra"),
          
          p("El marco de muestra está conformado por la población mayor de 18 años de la zona urbana de Ciudad Juárez, Chihuahua, según los datos del Censo Nacional de Población y Vivienda de 2020 del INEGI, junto con las Áreas de Geoestadística Básica y la delimitación territorial de los distritos electorales locales proporcionados por el Instituto Nacional Electoral."),
          
          p("Para determinar el tamaño de la muestra, se emplea la ecuación estadística para proporciones poblacionales, que se presenta a continuación:"),
          
          # Fórmula en un recuadro estilizado - actualizada según la imagen proporcionada
          div(
            class = "formula-box",
            withMathJax(),
            "$$n = \\frac{Z^2(p*q)}{e^2 + \\frac{Z^2(p*q)}{N}}$$"
          ),
          # Definición de variables en la fórmula
          p("Donde:"),
          tags$ul(
            tags$li(tags$strong("n:"), " es el tamaño de la muestra a estimar."),
            tags$li(tags$strong("Z:"), " es el nivel de confianza deseado (95%), con referencia a municipios con más de 100,000 habitantes, utilizando la tabla Z de distribución normal."),
            tags$li(tags$strong("p:"), " es la probabilidad de éxito o la proporción de la población con la característica deseada (0.5 para maximizar el tamaño de la muestra)."),
            tags$li(tags$strong("q:"), " es la probabilidad de fracaso o la proporción de la población sin la característica deseada (0.5)."),
            tags$li(tags$strong("e:"), " es el nivel de error estadístico aceptado (±4.1%)."),
            tags$li(tags$strong("N:"), " es el tamaño de la población, considerando el número de viviendas en el municipio para este estudio.")
          ),
          
          # Método de muestreo por semillas
          h3("Método de muestreo por semillas"),
          
          p("Se utilizó un método de muestreo probabilístico para obtener una muestra representativa de la población de interés. Este método consistió en seleccionar aleatoriamente áreas geográficas específicas (AGEB) dentro de cada distrito electoral y recoger cuestionarios en puntos de levantamiento designados como «semillas»."),
          
          p("Para este estudio, una semilla se define como el AGEB seleccionado aleatoriamente mediante el método de Muestreo Aleatorio Simple y se seleccionaron 57 semillas distribuidas proporcionalmente entre los distritos electorales de la ciudad."),
          
          p("El número de cuestionarios por AGEB se determinó en función del peso proporcional de la población. En cada semilla, se eligió un punto de partida y, siguiendo el recorrido de las manecillas del reloj, se encuestó una vivienda por manzana, saltando al menos dos viviendas entre cada entrevistada y limitando las entrevistas a un máximo de cinco por manzana. Se permitió la sustitución en caso de no poder completar el cuestionario o si quedaba incompleto."),
          
          # Diagrama visual mejorado del método de muestreo por semillas estilo profesional
          div(
            class = "methodology-diagram",
            tags$strong("Metodología de Muestreo por Semillas"),
            br(),
            br(),
            div(
              style = "width: 100%; max-width: 800px; margin: 0 auto;",
              div(
                style = "display: flex; justify-content: space-between; align-items: center;",
                # Columna izquierda: Área semilla
                div(
                  style = "flex: 1; padding: 15px;",
                  h4(style = "color: #666; font-weight: 600; margin-bottom: 15px; text-align: center;", "Área semilla"),
                  # Cuadrícula de área semilla
                  div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    # Fila 1
                    div(
                      style = "display: flex; justify-content: center;",
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;"),
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;"),
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;")
                    ),
                    # Fila 2 con AGEB semilla
                    div(
                      style = "display: flex; justify-content: center;",
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;"),
                      div(style = "width: 120px; height: 60px; background-color: #F8AF59; margin: 3px; border: 2px solid #F8AF59; border-radius: 15px; color: white; font-weight: bold; display: flex; justify-content: center; align-items: center; flex-direction: column;", 
                         div(style = "font-size: 14px;", "Semilla"),
                         div(style = "font-size: 12px;", "Cuadrada")),
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;")
                    ),
                    # Fila 3
                    div(
                      style = "display: flex; justify-content: center;",
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;"),
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;"),
                      div(style = "width: 55px; height: 40px; background-color: #f8f9fa; margin: 3px; border: 2px solid #CCCCCC; border-radius: 10px;")
                    ),
                    # Flechas señalando la semilla
                    div(
                      style = "position: relative; width: 100%; height: 0;",
                      div(style = "position: absolute; top: -140px; left: 50px; transform: rotate(-45deg);", 
                          bsicons::bs_icon("arrow-up", width = "24px", height = "24px", fill = "#F8AF59")),
                      div(style = "position: absolute; top: -55px; left: 20px; transform: rotate(-90deg);", 
                          bsicons::bs_icon("arrow-up", width = "24px", height = "24px", fill = "#F8AF59")),
                      div(style = "position: absolute; top: -140px; right: 50px; transform: rotate(45deg);", 
                          bsicons::bs_icon("arrow-up", width = "24px", height = "24px", fill = "#F8AF59")),
                      div(style = "position: absolute; top: -55px; right: 20px; transform: rotate(90deg);", 
                          bsicons::bs_icon("arrow-up", width = "24px", height = "24px", fill = "#F8AF59"))
                    )
                  )
                ),
                
                # Flecha de conexión
                div(
                  style = "margin: 0 15px;",
                  bsicons::bs_icon("chevron-right", width = "40px", height = "40px", fill = "#AAAAAA")
                ),
                
                # Columna derecha: Manzana dentro de semilla
                div(
                  style = "flex: 1.2; padding: 15px;",
                  h4(style = "color: #666; font-weight: 600; margin-bottom: 15px; text-align: center;", "Manzana dentro de semilla"),
                  # Visualización de domicilios
                  div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    # Flecha derecha
                    div(
                      style = "width: 100%; text-align: center; margin-bottom: 10px;",
                      bsicons::bs_icon("arrow-right", width = "32px", height = "32px", fill = "#F8AF59")
                    ),
                    # Fila 1 de casas
                    div(
                      style = "display: flex; justify-content: space-between; width: 100%; margin-bottom: 15px;",
                      # Casa 1 - encuestada
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#F8AF59" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#F8AF59" stroke="#F8AF59" stroke-width="2"/>
                              </svg>')),
                      # Casa 2 - no encuestada
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>')),
                      # Casa 3 - no encuestada
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>')),
                      # Casa 4 - encuestada
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#F8AF59" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#F8AF59" stroke="#F8AF59" stroke-width="2"/>
                              </svg>')),
                      # Casa 5 - no encuestada
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>'))
                    ),
                    # Fila 2 de casas
                    div(
                      style = "display: flex; justify-content: space-between; width: 100%;",
                      # Casa 6 - encuestada
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#F8AF59" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#F8AF59" stroke="#F8AF59" stroke-width="2"/>
                              </svg>')),
                      # Casa 7 - no encuestada
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>')),
                      # Casa 8 - no encuestada
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>')),
                      # Casa 9 - encuestada
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#F8AF59" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#F8AF59" stroke="#F8AF59" stroke-width="2"/>
                              </svg>')),
                      # Casa 10 - no encuestada
                      div(style = "width: 45px; height: 45px;", 
                          HTML('<svg width="45" height="45" viewBox="0 0 45 45" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5 42.5V17.5L22.5 5L40 17.5V42.5H5Z" fill="white" stroke="#CCCCCC" stroke-width="3"/>
                                <path d="M22.5 5L5 17.5H40L22.5 5Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="2"/>
                              </svg>'))
                    ),
                    # Flecha izquierda
                    div(
                      style = "width: 100%; text-align: center; margin-top: 10px;",
                      bsicons::bs_icon("arrow-left", width = "32px", height = "32px", fill = "#F8AF59")
                    )
                  ),
                  
                  # Leyenda de casas
                  div(
                    style = "margin-top: 20px; display: flex; justify-content: flex-end;",
                    div(
                      style = "display: flex; flex-direction: column; gap: 8px;",
                      div(
                        style = "display: flex; align-items: center;",
                        div(style = "width: 30px; height: 30px; margin-right: 10px;", 
                            HTML('<svg width="30" height="30" viewBox="0 0 30 30" fill="none" xmlns="http://www.w3.org/2000/svg">
                                  <path d="M3 28V12L15 3L27 12V28H3Z" fill="white" stroke="#F8AF59" stroke-width="2"/>
                                  <path d="M15 3L3 12H27L15 3Z" fill="#F8AF59" stroke="#F8AF59" stroke-width="1"/>
                                </svg>')),
                        div(style = "color: #555; font-size: 14px;", "Domicilio encuestado")
                      ),
                      div(
                        style = "display: flex; align-items: center;",
                        div(style = "width: 30px; height: 30px; margin-right: 10px;", 
                            HTML('<svg width="30" height="30" viewBox="0 0 30 30" fill="none" xmlns="http://www.w3.org/2000/svg">
                                  <path d="M3 28V12L15 3L27 12V28H3Z" fill="white" stroke="#CCCCCC" stroke-width="2"/>
                                  <path d="M15 3L3 12H27L15 3Z" fill="#CCCCCC" stroke="#CCCCCC" stroke-width="1"/>
                                </svg>')),
                        div(style = "color: #555; font-size: 14px;", 
                            "Domicilio sustituido",
                            div(style = "color: #888; font-size: 12px; font-style: italic;", "(No encuestado)")
                      )
                    )
                  )
                )
              )
            ),
            
            # Explicación del método
            div(
              style = "margin-top: 25px; text-align: left; max-width: 800px; margin: 25px auto 0;",
              p("Esta técnica de muestreo garantiza que la distribución de la muestra sea representativa de toda la extensión territorial de Ciudad Juárez, lo que permite obtener información estadísticamente válida sobre la población."),
              p("En cada AGEB seleccionada como semilla, se recorre una manzana siguiendo el sentido de las manecillas del reloj, seleccionando domicilios específicos y saltando al menos dos viviendas entre cada entrevista para asegurar la aleatoriedad en la selección de participantes.")
            )
          ),
          
          # Perfil y levantamiento
          h3("Perfil del entrevistado y método de levantamiento"),
          
          div(class = "row mb-4",
            div(class = "col-md-6",
              h4("Perfil del entrevistado"),
              p("Hombres y mujeres de 18 años o más con residencia en la vivienda seleccionada. Se aplicaron cuotas por género y grupo de edad para garantizar la representatividad demográfica de la muestra."),
            ),
            div(class = "col-md-6",
              h4("Levantamiento de campo"),
              div(
                id = "fechas_levantamiento",
                p("El levantamiento de campo se realizó en los siguientes periodos:"),
                tags$ul(
                  tags$li(tags$strong("Encuestas 2023:"), " del 6 al 26 de noviembre del 2023."),
                  tags$li(tags$strong("Encuestas 2024:"), " del 10 al 28 de febrero del 2025.")
                ),
                p("En todos los casos, se aplicó cada cuestionario cara a cara en la vivienda del entrevistado, con una duración aproximada de 25-30 minutos por encuesta.")
              )
            )
          ),
          
          # Error muestral y confiabilidad
          h3("Error muestral y nivel de confianza"),
          p("El diseño muestral se realizó considerando un nivel de confianza del 95% y un margen de error de ±4.1%, lo que garantiza la robustez estadística de los resultados obtenidos. Se implementaron protocolos de validación y control de calidad en todas las etapas del proceso para asegurar la integridad de los datos recopilados."),
          
          # Procesamiento de datos
          h3("Procesamiento y análisis de datos"),
          p("Los datos recopilados se procesaron utilizando software estadístico especializado que permite realizar análisis descriptivos, inferenciales y comparativos. Se aplicaron técnicas de ponderación para ajustar posibles desviaciones en la representatividad de la muestra con respecto a los parámetros poblacionales establecidos por el INEGI. Todos los análisis presentados en este dashboard se derivan de estos datos procesados, permitiendo visualizar de manera clara y accesible la información recopilada.")
        )
      )
    ),
    
    # Información adicional - Notas técnicas
    card(
      card_header(
        h3("Notas técnicas sobre los datos", class = "m-0")
      ),
      card_body(
        div(
          class = "callout callout-info",
          h4("Formato de los archivos descargables"),
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
          class = "callout callout-warning mt-4",
          h4("Diferencias entre conjuntos de datos 2023 y 2024"),
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
          class = "callout callout-info mt-4",
          h4("Interpretación de escalas y tipos de datos"),
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
    ),
    
    # Pie de página
    card(
      layout_columns(
        col_widths = c(6, 6),
        div(
          p("Encuestas realizadas por Plan Estratégico de Juárez", class = "text-center text-muted my-2")
        ),
        div(
          p("Última actualización: Abril 2025", class = "text-center text-muted my-2")
        )
      )
    )
  )
)
}