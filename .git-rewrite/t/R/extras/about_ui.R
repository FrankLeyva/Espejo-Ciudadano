# about_ui.R - Fixed version without unnecessary namespace

aboutUI <- function() {
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
          h1("Acerca del Dashboard", class = "dashboard-header")
        )
      )
    ),
    
    # Main content with page-card styling
    div(
      class = "plot-cards-grid two-columns",
      
      # About the Dashboard Card
      div(
        class = "page-card page-card-extras",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Sobre el Dashboard")
          )
        ),
        
        # Card Content
        div(
          class = "page-content",
          h5("Propósito", class = "mb-3", style = "font-family: var(--font-display);"),
          p("Este dashboard tiene como objetivo principal proporcionar a la ciudadanía acceso interactivo a los datos obtenidos en las encuestas:", class = "mb-2"),
          tags$ul(
            tags$li(tags$strong("Encuesta de Percepción Ciudadana"), "— Evaluación de la percepción de los juarenses sobre diversos aspectos de la ciudad"),
            tags$li(tags$strong("Encuesta de Participación Ciudadana y Buen Gobierno"), "— Evaluación de la participación ciudadana y la calidad del gobierno municipal")
          ),
          
          h5("Funcionalidades", class = "mb-3 mt-4", style = "font-family: var(--font-display);"),
          p("El dashboard ofrece:"),
          tags$ul(
            tags$li("Visualizaciones interactivas de los resultados más relevantes"),
            tags$li("Mapas georreferenciados para análisis territorial"),
            tags$li("Posibilidad de descargar datos y resúmenes para análisis adicionales")
          )
        )
      ),
      
      # About Plan Estratégico de Juárez Card
      div(
        class = "page-card page-card-extras",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Plan Estratégico de Juárez")
          )
        ),
        
        # Card Content
        div(
          class = "page-content",
          tags$blockquote(
            class = "blockquote mb-4",
            style = "border-left: 4px solid var(--extras-color); padding-left: 1rem;",
            p("Somos una organización ciudadana, apartidista y sin fines de lucro. Nuestro objetivo es promover y defender el derecho a la participación ciudadana para exigir mejor calidad de vida."),
            tags$footer(class = "blockquote-footer", "Plan Estratégico de Juárez")
          ),
          
          h5("Historia", class = "mb-3", style = "font-family: var(--font-display);"),
          p("Desde 1999, Plan Estratégico de Juárez ha trabajado con un enfoque ciudadano, proponiendo y exigiendo a la autoridad local de manera respetuosa pero firme."),
          
          h5("Misión", class = "mb-2 mt-3", style = "font-family: var(--font-display);"),
          p("Promover y defender el derecho a la participación ciudadana para exigir mejor calidad de vida."),
          
          h5("Visión", class = "mb-2 mt-3", style = "font-family: var(--font-display);"),
          p("En 2030 la ciudadanía incide en la mejora significativa del gobierno municipal."),
          
          div(
            class = "text-center mt-4",
            tags$a(
              href = "https://planjuarez.org/", 
              target = "_blank",
              class = "btn btn-outline-primary",
              style = "border-color: var(--extras-color); color: var(--extras-color);",
              rel = "noopener noreferrer",
              "Visitar sitio web oficial"
            )
          )
        )
      )
    ),
    
    # Second row of cards
    div(
      class = "plot-cards-grid two-columns",
      
      # Technical Information Card
      div(
        class = "page-card page-card-extras",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Información Técnica")
          )
        ),
        
        # Card Content
        div(
          class = "page-content",
          h5("Tecnologías Utilizadas", class = "mb-3", style = "font-family: var(--font-display);"),
          p("Este dashboard ha sido desarrollado utilizando:"),
          tags$ul(
            tags$li(tags$strong("R Shiny"), "— Framework para aplicaciones web interactivas"),
            tags$li(tags$strong("Plotly"), "— Biblioteca para visualizaciones interactivas"),
            tags$li(tags$strong("Leaflet"), "— Biblioteca para mapas interactivos"),
            tags$li(tags$strong("bslib"), "— Componentes Bootstrap modernos para R Shiny")
          ),
          
          h5("Metodología de los Datos", class = "mb-3 mt-4", style = "font-family: var(--font-display);"),
          p("Los datos visualizados provienen de encuestas diseñadas con:"),
          tags$ul(
            tags$li("Muestreo estratificado por distrito"),
            tags$li("Representatividad estadística a nivel ciudad")
          ),
          
          div(
            class = "text-center mt-4",
            actionButton(
              "nav_to_methodology",
              "Ver detalles metodológicos",
              class = "btn btn-outline-primary",
              style = "border-color: var(--extras-color); color: var(--extras-color);"
            )
          )
        )
      ),
      
      # Contact Card
      div(
        class = "page-card page-card-extras",
        
        # Card Header
        div(
          class = "page-header",
          div(
            class = "page-header-content",
            h4(class = "page-card-title", style = "font-family: var(--font-display);", "Contacto")
          )
        ),
        
        # Card Content
        div(
          class = "page-content",
          h5("¿Necesitas más información?", class = "mb-3", style = "font-family: var(--font-display);"),
          p("Contacta a Plan Estratégico de Juárez para cualquier consulta sobre este dashboard:"),
          
          div(
            class = "mb-4",
            # Contact items with improved styling
            div(
              class = "d-flex align-items-start mb-3 p-3 rounded",
              style = "background-color: #f8f9fa; border-left: 3px solid var(--extras-color);",
              div(
                class = "me-3 pt-1",
                icon("circle-question", style = "color: var(--extras-color);")
              ),
              div(
                h6("Consultas Generales", class = "mb-1", style = "font-family: var(--font-display);"),
                p(class = "mb-0", 
                  tags$a(
                    href = "mailto:contacto@planjuarez.org", 
                    "contacto@planjuarez.org",
                    onclick = "window.open(this.href); return false;"
                  )
                )
              )
            ),
            div(
              class = "d-flex align-items-start mb-3 p-3 rounded",
              style = "background-color: #f8f9fa; border-left: 3px solid var(--extras-color);",
              div(
                class = "me-3 pt-1",
                icon("chart-line", style = "color: var(--extras-color);")
              ),
              div(
                h6("Aspectos metodológicos", class = "mb-1", style = "font-family: var(--font-display);"),
                h6("Judith Carrillo", class = "mb-1", style = "font-family: var(--font-display);"),
                p(class = "mb-0", 
                  tags$a(
                    href = "mailto:jcarrillo@planjuarez.org", 
                    "jcarrillo@planjuarez.org",
                    onclick = "window.open(this.href); return false;"
                  )
                )
              )
            ),
            div(
              class = "d-flex align-items-start mb-3 p-3 rounded",
              style = "background-color: #f8f9fa; border-left: 3px solid var(--extras-color);",
              div(
                class = "me-3 pt-1",
                icon("code", style = "color: var(--extras-color);")
              ),
              div(
                h6("Desarrollador del dashboard", class = "mb-1", style = "font-family: var(--font-display);"),
                h6("Francisco Leyva", class = "mb-1", style = "font-family: var(--font-display);"),
                p(class = "mb-0", 
                  tags$a(
                    href = "mailto:f.leyva@planjuarez.org", 
                    "f.leyva@planjuarez.org",
                    onclick = "window.open(this.href); return false;"
                  )
                )
              )
            )
          ),
          
          h5("Más recursos", class = "mb-3 mt-4", style = "font-family: var(--font-display);"),
          p("Para conocer más sobre nuestro trabajo:"),
          div(
            class = "d-flex gap-2 mt-3 justify-content-center flex-wrap",
            tags$a(
              href = "https://planjuarez.org/", 
              target = "_blank",
              rel = "noopener noreferrer",
              class = "btn btn-outline-primary btn-sm",
              style = "border-color: var(--extras-color); color: var(--extras-color);",
              onclick = "window.open(this.href, '_blank'); return false;",
              icon("globe"), " Sitio Web"
            ),
            tags$a(
              href = "https://www.facebook.com/PlanJuarez/", 
              target = "_blank",
              rel = "noopener noreferrer",
              class = "btn btn-outline-primary btn-sm",
              style = "border-color: var(--extras-color); color: var(--extras-color);",
              onclick = "window.open(this.href, '_blank'); return false;",
              icon("facebook"), " Facebook"
            ),
            tags$a(
              href = "https://twitter.com/planjuarez", 
              target = "_blank",
              rel = "noopener noreferrer",
              class = "btn btn-outline-primary btn-sm",
              style = "border-color: var(--extras-color); color: var(--extras-color);",
              onclick = "window.open(this.href, '_blank'); return false;",
              icon("twitter"), " Twitter"
            )
          )
        )
      )
    ),
    
    # Footer
    create_dashboard_footer()
  )
}