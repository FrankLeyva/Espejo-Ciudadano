# UI para Dashboard de Metodología y Descarga de Datos
methodologyUI <- function() {
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
        }
        
        .methodology-section h4 {
          margin-top: 20px;
          margin-bottom: 10px;
          font-weight: 600;
        }
        
        .methodology-section p {
          margin-bottom: 15px;
          text-align: justify;
        }
        
        .methodology-section ol {
          padding-left: 22px;
          margin-bottom: 15px;
        }
        
        .methodology-section li {
          margin-bottom: 5px;
        }
        
        .formula-box {
          background-color: #f8f9fa;
          padding: 15px;
          border-radius: 5px;
          margin: 20px 0;
          font-family: Consolas, Monaco, 'Courier New', monospace;
          text-align: center;
        }
        
        .download-card {
          transition: transform 0.3s;
        }
        
        .download-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 10px 20px rgba(0,0,0,0.1);
        }
        
        .download-icon {
          font-size: 2rem;
          color: #0d6efd;
          margin-bottom: 15px;
        }
        
        .methodology-diagram {
          text-align: center;
          margin: 20px 0;
          padding: 15px;
          background-color: #f8f9fa;
          border-radius: 5px;
        }
        
        /* Custom callout styles */
        .callout {
          padding: 20px;
          margin: 20px 0;
          border: 1px solid #eee;
          border-left-width: 5px;
          border-radius: 5px;
        }
        
        .callout-info {
          border-left-color: #5bc0de;
          background-color: #f4f8fa;
        }
        
        .callout-warning {
          border-left-color: #f0ad4e;
          background-color: #faf8f4;
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
          h2("Metodología y Descarga de Datos", class = "text-center")
        )
      )
    ),
    
    # Sección de descargas (al principio para facilitar acceso)
    card(
      card_header(
        h3("Descargar Datos de Encuestas", class = "m-0")
      ),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          
          # Tarjeta de descarga para PER_2024
          div(
            class = "download-card",
            card(
              card_body(
                div(class = "text-center"),
                div(class = "download-icon", bsicons::bs_icon("clipboard-data")),
                h4(class = "text-center", "Encuesta de Percepción Ciudadana"),
                p(class = "text-center", "Datos recopilados en febrero 2025"),
                p(class = "text-center text-muted", textOutput("per_info")),
                div(
                  class = "d-grid gap-2",
                  downloadButton(
                    "download_per_2024", 
                    "Descargar PER_2024 (CSV)",
                    class = "btn-primary"
                  ),
                  actionLink(
                    "view_per_metadata", 
                    "Ver diccionario de datos",
                    class = "text-center"
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
                h4(class = "text-center", "Encuesta de Participación Ciudadana"),
                p(class = "text-center", "Datos recopilados en febrero 2025"),
                p(class = "text-center text-muted", textOutput("par_info")),
                div(
                  class = "d-grid gap-2",
                  downloadButton(
                    "download_par_2024", 
                    "Descargar PAR_2024 (CSV)",
                    class = "btn-primary"
                  ),
                  actionLink(
                    "view_par_metadata", 
                    "Ver diccionario de datos",
                    class = "text-center"
                  )
                )
              )
            )
          )
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
            tags$li("Encuesta de percepción ciudadana."),
            tags$li("Encuesta de participación ciudadana y buen gobierno.")
          ),
          
          p("Ambos estudios fueron diseñados con características metodológicas distintas y muestras específicas. Sin embargo, se realizaron de forma simultánea durante el mismo período de tiempo, dado que forman parte de una consulta ciudadana integral."),
          
          p("Las encuestas están respaldadas por una metodología robusta de múltiples etapas y una muestra estadística representativa y adecuada de la población juarense de 18 años en adelante. Este enfoque combina diversas técnicas estadísticas para garantizar la fiabilidad y la relevancia de los resultados obtenidos."),
          
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
            tags$li(tags$strong("Z:"), " es el nivel de confianza deseado, con referencia a municipios con más de 100,000 habitantes, utilizando la tabla Z de distribución normal."),
            tags$li(tags$strong("p:"), " es la probabilidad de éxito o la proporción de la población con la característica deseada (siempre entre 0 y 1)."),
            tags$li(tags$strong("q:"), " es la probabilidad de fracaso o la proporción de la población sin la característica deseada (siempre entre 0 y 1)."),
            tags$li(tags$strong("e:"), " es el nivel de error estadístico aceptado por el investigador, recomendado máximo de ±5%."),
            tags$li(tags$strong("N:"), " es el tamaño de la población, considerando el número de viviendas en el municipio para este estudio.")
          ),
          
          # Método de muestreo por semillas
          h3("Método de muestreo por semillas"),
          
          p("Se utilizó un método de muestreo probabilístico para obtener una muestra representativa de la población de interés. Este método consistió en seleccionar aleatoriamente áreas geográficas específicas (AGEB) dentro de cada distrito electoral y recoger cuestionarios en puntos de levantamiento designados como «semillas»."),
          
          p("Para este estudio, una semilla se define como el AGEB seleccionado aleatoriamente mediante el método de Muestreo Aleatorio Simple y se seleccionaron 57 semillas."),
          
          p("El número de cuestionarios por AGEB se determinó en función del peso proporcional de la población. En cada semilla, se eligió un punto de partida y, siguiendo el recorrido de las manecillas del reloj, se encuestó una vivienda por manzana, saltando al menos dos viviendas entre cada entrevistada y limitando las entrevistas a un máximo de cinco por manzana. Se permitió la sustitución en caso de no poder completar el cuestionario o si quedaba incompleto."),
          
          # Diagrama visual mejorado del método de muestreo por semillas
          div(
            class = "methodology-diagram",
            tags$strong("Metodología de Muestreo por Semillas"),
            br(),
            br(),
            div(
              style = "width: 100%; max-width: 700px; margin: 0 auto;",
              div(
                style = "display: flex; flex-direction: column; align-items: center;",
                # Representación del área semilla con cuadrículas
                div(
                  style = "display: flex; flex-direction: column; margin-bottom: 20px;",
                  div(
                    style = "display: flex; justify-content: center; align-items: center;",
                    div(style = "width: 60px; height: 60px; background-color: #E0E0E0; margin: 5px; border: 1px solid #B0B0B0; display: flex; justify-content: center; align-items: center;", ""),
                    div(style = "width: 60px; height: 60px; background-color: #E0E0E0; margin: 5px; border: 1px solid #B0B0B0; display: flex; justify-content: center; align-items: center;", ""),
                    div(style = "width: 60px; height: 60px; background-color: #E0E0E0; margin: 5px; border: 1px solid #B0B0B0; display: flex; justify-content: center; align-items: center;", "")
                  ),
                  div(
                    style = "display: flex; justify-content: center; align-items: center;",
                    div(style = "width: 60px; height: 60px; background-color: #E0E0E0; margin: 5px; border: 1px solid #B0B0B0; display: flex; justify-content: center; align-items: center;", ""),
                    div(style = "width: 60px; height: 60px; background-color: #e74c3c; margin: 5px; border: 1px solid #B0B0B0; color: white; font-weight: bold; display: flex; justify-content: center; align-items: center;", "AGEB Semilla"),
                    div(style = "width: 60px; height: 60px; background-color: #E0E0E0; margin: 5px; border: 1px solid #B0B0B0; display: flex; justify-content: center; align-items: center;", "")
                  ),
                  div(
                    style = "display: flex; justify-content: center; align-items: center;",
                    div(style = "width: 60px; height: 60px; background-color: #E0E0E0; margin: 5px; border: 1px solid #B0B0B0; display: flex; justify-content: center; align-items: center;", ""),
                    div(style = "width: 60px; height: 60px; background-color: #E0E0E0; margin: 5px; border: 1px solid #B0B0B0; display: flex; justify-content: center; align-items: center;", ""),
                    div(style = "width: 60px; height: 60px; background-color: #E0E0E0; margin: 5px; border: 1px solid #B0B0B0; display: flex; justify-content: center; align-items: center;", "")
                  )
                ),
                
                # Flechas de conexión
                bsicons::bs_icon("arrow-down", width = "32px", height = "32px"),
                
                # Representación detallada de una manzana dentro del AGEB semilla
                div(
                  style = "margin-top: 15px; border: 2px solid #333; padding: 10px; background-color: #f8f9fa;",
                  div(
                    style = "text-align: center; margin-bottom: 10px; font-weight: bold;",
                    "Manzanas dentro del AGEB Semilla"
                  ),
                  div(
                    style = "display: grid; grid-template-columns: repeat(5, 1fr); gap: 10px;",
                    div(style = "width: 50px; height: 50px; background-color: #f8f9fa; border: 1px solid #ddd; display: flex; justify-content: center; align-items: center;", bsicons::bs_icon("check-circle-fill", size = "1.5em", class = "text-success")),
                    div(style = "width: 50px; height: 50px; background-color: #f8f9fa; border: 1px solid #ddd; display: flex; justify-content: center; align-items: center;", bsicons::bs_icon("x-circle-fill", size = "1.5em", class = "text-danger")),
                    div(style = "width: 50px; height: 50px; background-color: #f8f9fa; border: 1px solid #ddd; display: flex; justify-content: center; align-items: center;", bsicons::bs_icon("x-circle-fill", size = "1.5em", class = "text-danger")),
                    div(style = "width: 50px; height: 50px; background-color: #f8f9fa; border: 1px solid #ddd; display: flex; justify-content: center; align-items: center;", bsicons::bs_icon("check-circle-fill", size = "1.5em", class = "text-success")),
                    div(style = "width: 50px; height: 50px; background-color: #f8f9fa; border: 1px solid #ddd; display: flex; justify-content: center; align-items: center;", bsicons::bs_icon("x-circle-fill", size = "1.5em", class = "text-danger"))
                  ),
                  div(
                    style = "margin-top: 15px; display: flex; justify-content: space-around;",
                    div(style = "display: flex; align-items: center;", 
                        bsicons::bs_icon("check-circle-fill", size = "1em", class = "text-success"), 
                        span(style = "margin-left: 5px;", "Domicilio encuestado")),
                    div(style = "display: flex; align-items: center;", 
                        bsicons::bs_icon("x-circle-fill", size = "1em", class = "text-danger"), 
                        span(style = "margin-left: 5px;", "Domicilio no encuestado"))
                  )
                )
              )
            ),
            div(
              style = "margin-top: 20px;",
              p("Este tipo de técnicas garantizan que la distribución de la muestra sea a lo largo y ancho de la región analizada y que se obtenga información que sea representativa."),
              p("En cada AGEB seleccionada como semilla, se recorre una manzana siguiendo el sentido de las manecillas del reloj, seleccionando domicilios específicos y saltando al menos dos viviendas entre cada entrevista.")
            )
          ),
          
          # Perfil y levantamiento
          h3("Perfil del entrevistado"),
          p("Hombres y mujeres mayores de edad con residencia en la vivienda seleccionada."),
          
          h3("Levantamiento de campo"),
          p("El levantamiento de campo se llevó a cabo del 10 al 28 de febrero del 2025. Se aplicó cada cuestionario cara a cara en la vivienda del entrevistado.")
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
            tags$li(tags$strong("TIMESTAMP"), ": Fecha y hora de la entrevista")
          ),
          p("Para más información sobre la estructura de los datos o el diccionario de variables, puede contactar al equipo de investigación.")
        )
      )
    ),
    
    # Pie de página
    card(
      p("Encuestas realizadas por Plan Estratégico de Juárez", class = "text-center text-muted"),
      p("Última actualización: Marzo 2025", class = "text-center text-muted")
    )
  )
}