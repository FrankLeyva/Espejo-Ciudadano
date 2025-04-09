# about_ui.R

aboutUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container mt-4",
      
      # Header section with logo and title
      div(
        class = "row mb-4 align-items-center",
        div(
          class = "col-md-3 text-center",
          img(src = "logo_pej.png", alt = "Plan Estratégico de Juárez Logo", width = "90%", class = "img-fluid mb-3")
        ),
        div(
          class = "col-md-9",
          h2("Acerca del Dashboard", class = "display-5 fw-bold text-primary"),
          p(class = "lead", "Una herramienta para visualizar los resultados de las encuestas ciudadanas de Ciudad Juárez")
        )
      ),
      
      # Main content organized in cards
      div(
        class = "row g-4",
        
        # About the Dashboard
        div(
          class = "col-md-6",
          card(
            card_header(
              h3("Sobre el Dashboard", class = "m-0 fw-bold"),
              class = "bg-primary bg-opacity-10"
            ),
            card_body(
              h4("Propósito", class = "mb-3"),
              p("Este dashboard tiene como objetivo principal proporcionar a la ciudadanía acceso interactivo a los datos obtenidos en las encuestas:", class = "mb-2"),
              tags$ul(
                tags$li(tags$strong("Encuesta de Percepción Ciudadana"), "— Evaluación de la percepción de los juarenses sobre diversos aspectos de la ciudad"),
                tags$li(tags$strong("Encuesta de Participación Ciudadana y Buen Gobierno"), "— Evaluación de la participación ciudadana y la calidad del gobierno municipal")
              ),
              h4("Funcionalidades", class = "mb-3 mt-4"),
              p("El dashboard ofrece:"),
              tags$ul(
                tags$li("Visualizaciones interactivas de los resultados más relevantes"),
                tags$li("Mapas georreferenciados para análisis territorial"),
                tags$li("Posibilidad de descargar datos y resúmenes para análisis adicionales")
              )
            )
          )
        ),
        
        # About Plan Estratégico de Juárez
        div(
          class = "col-md-6",
          card(
            card_header(
              h3("Plan Estratégico de Juárez", class = "m-0 fw-bold"),
              class = "bg-primary bg-opacity-10"
            ),
            card_body(
              tags$blockquote(
                class = "blockquote mb-4",
                p("Somos una organización ciudadana, apartidista y sin fines de lucro. Nuestro objetivo es promover y defender el derecho a la participación ciudadana para exigir mejor calidad de vida."),
                tags$footer(class = "blockquote-footer", "Plan Estratégico de Juárez")
              ),
              h4("Historia", class = "mb-3"),
              p("Desde 1999, Plan Estratégico de Juárez ha trabajado con un enfoque ciudadano, proponiendo y exigiendo a la autoridad local de manera respetuosa pero firme."),
              h4("Misión", class = "mb-2 mt-3"),
              p("Promover y defender el derecho a la participación ciudadana para exigir mejor calidad de vida."),
              h4("Visión", class = "mb-2 mt-3"),
              p("En 2030 la ciudadanía incide en la mejora significativa del gobierno municipal."),
              div(
                class = "text-center mt-4",
                a(
                  href = "https://planjuarez.org/", 
                  target = "_blank",
                  class = "btn btn-outline-primary",
                  "Visitar sitio web oficial"
                )
              )
            )
          )
        ),
        
        # Technical Information Section
        div(
          class = "col-md-6",
          card(
            card_header(
              h3("Información Técnica", class = "m-0 fw-bold"),
              class = "bg-primary bg-opacity-10"
            ),
            card_body(
              h4("Tecnologías Utilizadas", class = "mb-3"),
              p("Este dashboard ha sido desarrollado utilizando:"),
              tags$ul(
                tags$li(tags$strong("R Shiny"), "— Framework para aplicaciones web interactivas"),
                tags$li(tags$strong("Plotly"), "— Biblioteca para visualizaciones interactivas"),
                tags$li(tags$strong("Leaflet"), "— Biblioteca para mapas interactivos"),
                tags$li(tags$strong("bslib"), "— Componentes Bootstrap modernos para R Shiny")
              ),
              h4("Metodología de los Datos", class = "mb-3 mt-4"),
              p("Los datos visualizados provienen de encuestas diseñadas con:"),
              tags$ul(
                tags$li("Muestreo estratificado por distrito"),
                tags$li("Representatividad estadística a nivel ciudad"),
                tags$li("Análisis de validez y confiabilidad en los instrumentos")
              ),
              div(
                class = "text-center mt-4",
                a(
                  class = "btn btn-outline-secondary",
                  onclick = "Shiny.setInputValue('nav_target', 'methodology', {priority: 'event'})",
                  "Ver detalles metodológicos"
                )
              )
            )
          )
        ),
        
        # Contacto section (replacing developer section)
        div(
          class = "col-md-6",
          card(
            card_header(
              h3("Contacto", class = "m-0 fw-bold"),
              class = "bg-primary bg-opacity-10"
            ),
            card_body(
              h4("¿Necesitas más información?", class = "mb-3"),
              p("Contacta a Plan Estratégico de Juárez para cualquier consulta sobre este dashboard:"),
              div(
                class = "list-group mb-4",
                div(
                  class = "list-group-item d-flex align-items-center",
                  div(
                    class = "me-3",
                    icon("circle-question", class = "text-primary")
                  ),
                  div(
                    h5("Consultas generales", class = "mb-1"),
                    p(class = "mb-0", a(href = "mailto:contacto@planjuarez.org", "contacto@planjuarez.org"))
                  )
                ),
                div(
                  class = "list-group-item d-flex align-items-center",
                  div(
                    class = "me-3",
                    icon("chart-line", class = "text-primary")
                  ),
                  div(
                    h5("Aspectos metodológicos", class = "mb-1"),
                    p(class = "mb-0", a(href = "mailto:jcarrillo@planjuarez.org", "jcarrillo@planjuarez.org"))
                  )
                ),
                div(
                  class = "list-group-item d-flex align-items-center",
                  div(
                    class = "me-3",
                    icon("code", class = "text-primary")
                  ),
                  div(
                    h5("Soporte técnico", class = "mb-1"),
                    p(class = "mb-0", a(href = "mailto:f.leyva@planjuarez.org", "f.leyva@planjuarez.org"))
                  )
                )
              ),
              h4("Más recursos", class = "mb-3 mt-4"),
              p("Para conocer más sobre nuestro trabajo:"),
              div(
                class = "hstack gap-3 mt-3 justify-content-center",
                a(
                  href = "https://planjuarez.org/", 
                  target = "_blank",
                  class = "btn btn-outline-primary",
                  icon("globe"), " Sitio Web"
                ),
                a(
                  href = "https://www.facebook.com/PlanEstrategicoJuarez/", 
                  target = "_blank",
                  class = "btn btn-outline-primary",
                  icon("facebook"), " Facebook"
                ),
                a(
                  href = "https://twitter.com/planjuarez", 
                  target = "_blank",
                  class = "btn btn-outline-primary",
                  icon("twitter"), " Twitter"
                )
              )
            )
          )
        ),
        
        # Usage Guide Section
        div(
          class = "col-12 mt-4",
          card(
            card_header(
              h3("¿Cómo Utilizar Este Dashboard?", class = "m-0 fw-bold"),
              class = "bg-primary bg-opacity-10"
            ),
            card_body(
              div(
                class = "row",
                div(
                  class = "col-md-6",
                  h4("Para Usuarios No Técnicos", class = "mb-3"),
                  tags$ol(
                    tags$li(tags$strong("Navegue por las secciones:"), " Use la barra de navegación superior para explorar diferentes temas."),
                    tags$li(tags$strong("Seleccione visualizaciones:"), " Cada sección ofrece diferentes tipos de gráficos y mapas."),
                    tags$li(tags$strong("Interactúe con los gráficos:"), " Pase el cursor sobre los elementos para ver más detalles."),
                    tags$li(tags$strong("Aplique filtros:"), " Use los controles laterales para personalizar la información mostrada.")
                  ),
                  p(class = "mt-3", "Si encuentra algo interesante o tiene preguntas, puede descargar los datos o contactar a Plan Estratégico de Juárez.")
                ),
                div(
                  class = "col-md-6",
                  h4("Interpretación de Resultados", class = "mb-3"),
                  p("Al analizar la información presentada en este dashboard:"),
                  tags$ul(
                    tags$li("Considere el tamaño de la muestra y los márgenes de error en cada visualización"),
                    tags$li("Las comparaciones entre distritos ayudan a identificar disparidades territoriales")
                  ),
                  p(class = "mt-2", "Los datos presentados están disponibles para descarga y análisis adicional.")
                )
              ),
              div(
                class = "alert alert-info mt-4",
                icon("info-circle"), " ",
                tags$strong("Conócenos más:"),
                " Este dashboard es parte del compromiso de Plan Estratégico de Juárez con la transparencia y el empoderamiento ciudadano. Visita ",
                a(href = "https://planjuarez.org", "planjuarez.org"),
                " para conocer más sobre nuestros programas y cómo puedes involucrarte."
              )
            )
          )
        )
      ),
      
      # Footer with attribution
      div(
        class = "row mt-5 pt-4 border-top",
        div(
          class = "col-12 text-center",
          p(
            class = "text-muted small",
            "Dashboard desarrollado por Plan Estratégico de Juárez A.C. © ",
            format(Sys.Date(), "%Y"),
            ". Todos los derechos reservados."
          )
        )
      )
    )
  )
}