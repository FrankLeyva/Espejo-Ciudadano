library(shiny)
library(bslib)
library(htmltools)
library(DT)
library(dplyr)
library(plotly)
library(leaflet)
library(sf)
library(tidyverse)
library(tidyr)
library(igraph)
library(wordcloud2)
library(ggridges)
library(openxlsx)
library(stringr)
library(shinyjs)
library(colourpicker)
library(RColorBrewer)
library(viridisLite)

# Source all necessary files
source("R/global_theme.R")
source("R/utils.R")
source("R/survey_config.R")
source('R/razon_module.R')
source('R/interval_module.R')
source('R/ordinal_module.R')
source('R/categorical_module.R')
source('R/nominal_module.R')
source('R/binary_module.R')
source('R/special_module.R')
source("R/data_loader.R")
source("R/question_classifier.R")
source("R/themes_metadata.R")    
source("R/public_services_ui.R")
source("R/public_services_server.R")
source("R/housing_ui.R")
source("R/housing_server.R")
source("R/healthcare_ui.R")
source("R/healthcare_server.R")
source("R/education_ui.R")
source("R/education_server.R")
source("R/infrastructure_ui.R")
source("R/infrastructure_server.R")

# Define UI
ui <- page_sidebar(
  # Page title
  title = "Dashboard AEJ 2024",
  # Add custom JavaScript for navigation
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('refreshUI', function(message) {
        // Force redraw by triggering window resize
        window.dispatchEvent(new Event('resize'));
      });
    "))
  ),
  
  # Sidebar for main navigation
  sidebar = sidebar(
    # Logo and title
    div(
      class = "p-3 text-center",
      img(src = "logo.png", width = "80px", class = "mb-2"),
      h4("Así Estamos Juárez", class = "mt-2")
    ),
    
    # Main navigation menu
    accordion(
      open = FALSE, # Start closed
      
      accordion_panel(
        "Dashboard General",
        # Dashboard overview link
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_overview", "Vista general", 
                    icon = icon("dashboard"), class = "text-decoration-none")
        )
      ),
      
      accordion_panel(
        "Bienestar Social y Económico",
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_economic", "Condiciones Económicas", 
                    icon = icon("money-bill"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_cultural", "Participación Cultural", 
                    icon = icon("palette"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_demographic", "Tendencias Demográficas", 
                    icon = icon("users"), class = "text-decoration-none")
        )
      ),
      
      accordion_panel(
        "Movilidad Urbana y Medio Ambiente",
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_mobility", "Movilidad", 
                    icon = icon("route"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_public_transport", "Transporte Público", 
                    icon = icon("bus"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_environment", "Medio Ambiente", 
                    icon = icon("leaf"), class = "text-decoration-none")
        )
      ),
      
      accordion_panel(
        "Gobierno",
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_inequality", "Desigualdad", 
                    icon = icon("balance-scale"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_accountability", "Rendición de Cuentas", 
                    icon = icon("file-contract"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_political", "Representación Política", 
                    icon = icon("vote-yea"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_gov_expectations", "Expectativas en el gobierno", 
                    icon = icon("chart-line"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_trust", "Confianza en las instituciones", 
                    icon = icon("landmark"), class = "text-decoration-none")
        )
      ),
      
      accordion_panel(
        "Infraestructura y Servicios",
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_infrastructure", "Infraestructura", 
                    icon = icon("road"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_public_services", "Servicios Públicos", 
                    icon = icon("gear"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_education", "Educación", 
                    icon = icon("book"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_healthcare", "Servicios de Salud", 
                    icon = icon("heart-pulse"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_housing", "Vivienda", 
                    icon = icon("house"), class = "text-decoration-none")
        )
      ),
      
      accordion_panel(
        "Participación Ciudadana",
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_civic", "Participación Cívica", 
                    icon = icon("handshake"), class = "text-decoration-none")
        ),
        div(
          class = "nav-item py-2 px-3",
          actionLink("nav_community", "Participación Comunitaria", 
                    icon = icon("people-group"), class = "text-decoration-none")
        )
      )
    ),
    
    hr(),
    
    # Additional menu items at bottom
    div(
      class = "nav-item py-2 px-3",
      actionLink("nav_reports", "Reportes Personalizados", 
                icon = icon("file-chart-line"), class = "text-decoration-none")
    ),
    div(
      class = "nav-item py-2 px-3",
      actionLink("nav_methodology", "Metodología y Descarga de Datos", 
                icon = icon("download"), class = "text-decoration-none")
    ),
    div(
      class = "nav-item py-2 px-3",
      actionLink("nav_about", "Acerca del dashboard", 
                icon = icon("info-circle"), class = "text-decoration-none")
    )
  ),
  
  # Main content area
  layout_columns(
    # Breadcrumb navigation
    col_widths = c(12),
    div(
      id = "breadcrumb_container",
      class = "mb-3",
      uiOutput("breadcrumb")
    )
  ),
  
  # Dynamic content based on current page
  uiOutput("page_content")
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to track navigation state
  current_main <- reactiveVal("overview")
  current_sub <- reactiveVal(NULL)
  
  # Navigation history for breadcrumbs
  nav_history <- reactiveVal(list(
    main = "Dashboard General",
    sub = NULL
  ))
  
  # Define category mappings for breadcrumb organization
  category_mappings <- list(
    # Bienestar Social y Económico
    economic = list(main = "Bienestar Social y Económico", sub = "Condiciones Económicas"),
    cultural = list(main = "Bienestar Social y Económico", sub = "Participación Cultural"),
    demographic = list(main = "Bienestar Social y Económico", sub = "Tendencias Demográficas"),
    
    # Movilidad Urbana y Medio Ambiente
    mobility = list(main = "Movilidad Urbana y Medio Ambiente", sub = "Movilidad"),
    public_transport = list(main = "Movilidad Urbana y Medio Ambiente", sub = "Transporte Público"),
    environment = list(main = "Movilidad Urbana y Medio Ambiente", sub = "Medio Ambiente"),
    
    # Gobierno
    inequality = list(main = "Gobierno", sub = "Desigualdad"),
    accountability = list(main = "Gobierno", sub = "Rendición de Cuentas"),
    political = list(main = "Gobierno", sub = "Representación Política"),
    gov_expectations = list(main = "Gobierno", sub = "Expectativas en el gobierno"),
    trust = list(main = "Gobierno", sub = "Confianza en las instituciones"),
    
    # Infraestructura y Servicios
    infrastructure = list(main = "Infraestructura y Servicios", sub = "Infraestructura"),
    public_services = list(main = "Infraestructura y Servicios", sub = "Servicios Públicos"),
    education = list(main = "Infraestructura y Servicios", sub = "Educación"),
    healthcare = list(main = "Infraestructura y Servicios", sub = "Servicios de Salud"),
    housing = list(main = "Infraestructura y Servicios", sub = "Vivienda"),
    
    # Participación Ciudadana
    civic = list(main = "Participación Ciudadana", sub = "Participación Cívica"),
    community = list(main = "Participación Ciudadana", sub = "Participación Comunitaria"),
    
    # Other standalone sections
    reports = list(main = "Reportes Personalizados", sub = NULL),
    methodology = list(main = "Metodología y Descarga de Datos", sub = NULL),
    about = list(main = "Acerca del dashboard", sub = NULL)
  )
  
  # Generate breadcrumbs based on navigation history
  output$breadcrumb <- renderUI({
    history <- nav_history()
    
    breadcrumbList <- tags$ol(class = "breadcrumb")
    
    # Home link - always clickable
    breadcrumbList <- tagAppendChild(
      breadcrumbList,
      tags$li(
        class = "breadcrumb-item",
        tags$a(
          href = "javascript:void(0)",
          onclick = "Shiny.setInputValue('breadcrumb_home', Math.random(), {priority: 'event'})",
          "Inicio"
        )
      )
    )
    
    # Main category - clickable if we're in a subcategory
    if (!is.null(history$main)) {
      breadcrumbList <- tagAppendChild(
        breadcrumbList,
        tags$li(
          class = if(is.null(history$sub)) "breadcrumb-item active" else "breadcrumb-item",
          if(is.null(history$sub)) {
            history$main  # Not clickable if we're at this level
          } else {
            tags$a(
              href = "javascript:void(0)",
              onclick = sprintf("Shiny.setInputValue('breadcrumb_main_%s', Math.random(), {priority: 'event'})", 
                               gsub(" ", "_", tolower(history$main))),
              history$main
            )
          }
        )
      )
    }
    
    # Subcategory - never clickable as it's the current location
    if (!is.null(history$sub)) {
      breadcrumbList <- tagAppendChild(
        breadcrumbList,
        tags$li(
          class = "breadcrumb-item active",
          history$sub
        )
      )
    }
    
    breadcrumbList
  })
  
  # Main content renderer
  output$page_content <- renderUI({
    main <- current_main()
    sub <- current_sub()
    
    # Log current navigation state for debugging
    cat("Rendering page content for:", main, "\n")
    cat("Subcategory:", sub, "\n")
    
    # Override main with a placeholder if section is not yet implemented
    implemented_sections <- c("overview", "infrastructure", "public_services", "education", "healthcare", "housing", "reports", "methodology", "about")
    
    if (!(main %in% implemented_sections)) {
      return(div(
        class = "container mt-5",
        div(
          class = "alert alert-info",
          icon("info-circle"), 
          tags$strong("Sección en desarrollo"),
          p(
            class = "mt-2",
            sprintf("La sección '%s%s' estará disponible próximamente.", 
                   ifelse(is.null(sub), "", paste0(sub, " en ")), 
                   ifelse(is.null(sub), main, main))
          )
        )
      ))
    }
    
    # Render the appropriate UI based on the current navigation state
    if (main == "overview") {
      # This would call your general dashboard UI function
      overviewUI()
    } else if (main == "infrastructure") {
      infrastructureUI()
    } else if (main == "public_services") {
      publicServicesUI()
    } else if (main == "education") {
      educationUI()
    } else if (main == "healthcare") {
      healthcareUI()
    } else if (main == "housing") {
      housingUI()
    } else if (main == "reports") {
      # Custom reports UI
      div(
        h2("Reportes Personalizados"),
        p("Esta sección le permitirá generar reportes personalizados según sus necesidades específicas.")
      )
    } else if (main == "methodology") {
      # Methodology UI
      div(
        h2("Metodología y Descarga de Datos"),
        p("Información sobre la metodología utilizada en las encuestas y opciones para descargar los datos.")
      )
    } else if (main == "about") {
      # About UI
      div(
        h2("Acerca del Dashboard"),
        p("Información sobre el propósito de este dashboard y el equipo detrás de él.")
      )
    } else {
      # Placeholder for other pages that might be implemented later
      div(
        h2(paste("Contenido para", main, if(!is.null(sub)) paste("-", sub) else "")),
        p("Esta página está en desarrollo.")
      )
    }
  })
  
  # Initialize server modules based on current page
  observe({
    main <- current_main()
    
    if (main == "infrastructure") {
      infrastructureServer(input, output, session)
    } else if (main == "public_services") {
      publicServicesServer(input, output, session)
    } else if (main == "education") {
      educationServer(input, output, session)
    } else if (main == "healthcare") {
      healthcareServer(input, output, session)
    } else if (main == "housing") {
      housingServer(input, output, session)
    }
    
    # Other server modules would be initialized here as they are implemented
  })
  
  # Generic navigation event handler
  handle_nav <- function(id) {
    # Extract the section name from the input ID (remove "nav_" prefix)
    section <- substr(id, 5, nchar(id))
    
    # Get mapping for this section
    mapping <- category_mappings[[section]]
    
    if (!is.null(mapping)) {
      current_main(section)
      current_sub(NULL)
      nav_history(list(main = mapping$main, sub = mapping$sub))
    }
  }
  
  # Set up observers for all navigation links
  nav_links <- c(
    "nav_overview", 
    # Bienestar
    "nav_economic", "nav_cultural", "nav_demographic",
    # Movilidad
    "nav_mobility", "nav_public_transport", "nav_environment",
    # Gobierno
    "nav_inequality", "nav_accountability", "nav_political", 
    "nav_gov_expectations", "nav_trust",
    # Infraestructura
    "nav_infrastructure", "nav_public_services", "nav_education", 
    "nav_healthcare", "nav_housing",
    # Participación
    "nav_civic", "nav_community",
    # Other
    "nav_reports", "nav_methodology", "nav_about"
  )
  
  # Create observers for all navigation links
  for (link in nav_links) {
    local({
      local_link <- link
      observeEvent(input[[local_link]], {
        handle_nav(local_link)
      })
    })
  }
  
  # Handle navigation from overview cards
  observe({
    # We need to handle navigation events from the overview page
    if (current_main() == "overview") {
      overviewNavServer(input, output, session, 
                        current_main, current_sub, nav_history, category_mappings)
    }
  })
  
  # Breadcrumb navigation handlers
  observeEvent(input$breadcrumb_home, {
    # Print for debugging
    cat("Breadcrumb home clicked\n")
    # Force navigation to overview
    current_main("overview")
    current_sub(NULL)
    nav_history(list(main = "Dashboard General", sub = NULL))
    # Force UI refresh
    session$sendCustomMessage("refreshUI", list())
  })
  
  # Create observers for breadcrumb main navigation
  main_categories <- c(
    "bienestar_social_y_económico", 
    "movilidad_urbana_y_medio_ambiente", 
    "gobierno", 
    "infraestructura_y_servicios", 
    "participación_ciudadana"
  )
  
  for (category in main_categories) {
    local({
      local_category <- category
      # Create an observer for this category's breadcrumb
      observeEvent(input[[paste0("breadcrumb_main_", local_category)]], {
        # Find first section in this category
        for (section in names(category_mappings)) {
          if (tolower(gsub(" ", "_", category_mappings[[section]]$main)) == local_category) {
            # Use the first section we find in this category
            current_main(section)
            current_sub(NULL)
            nav_history(list(
              main = category_mappings[[section]]$main, 
              sub = NULL
            ))
            break
          }
        }
      })
    })
  }
}

# Function to handle navigation from overview
overviewNavServer <- function(input, output, session, current_main, current_sub, nav_history, category_mappings) {
  # Handle clicks on cards in the overview dashboard
  observeEvent(input$nav_infrastructure_card, {
    current_main("infrastructure")
    current_sub(NULL)
    nav_history(list(
      main = category_mappings[["infrastructure"]]$main,
      sub = category_mappings[["infrastructure"]]$sub
    ))
  })
  
  observeEvent(input$nav_education_card, {
    current_main("education")
    current_sub(NULL)
    nav_history(list(
      main = category_mappings[["education"]]$main,
      sub = category_mappings[["education"]]$sub
    ))
  })
  
  observeEvent(input$nav_healthcare_card, {
    current_main("healthcare")
    current_sub(NULL)
    nav_history(list(
      main = category_mappings[["healthcare"]]$main,
      sub = category_mappings[["healthcare"]]$sub
    ))
  })
  
  observeEvent(input$nav_public_services_card, {
    current_main("public_services")
    current_sub(NULL)
    nav_history(list(
      main = category_mappings[["public_services"]]$main,
      sub = category_mappings[["public_services"]]$sub
    ))
  })
  
  observeEvent(input$nav_housing_card, {
    current_main("housing")
    current_sub(NULL)
    nav_history(list(
      main = category_mappings[["housing"]]$main,
      sub = category_mappings[["housing"]]$sub
    ))
  })
  
  # Add more card navigation handlers as needed
}

# Basic overview UI (replace with your actual implementation)
overviewUI <- function() {
  div(
    h2("Dashboard General"),
    p("Vista general del estado de Ciudad Juárez según las encuestas más recientes."),
    
    # Grid of category cards
    layout_columns(
      fill = FALSE,
      
      # Infraestructura y Servicios section cards
      card(
        card_header("Infraestructura y Servicios"),
        layout_columns(
          col_widths = c(6, 6, 6, 6, 6),
          card(
            id = "nav_infrastructure_card", 
            class = "cursor-pointer",
            "Infraestructura", 
            tags$small("Calles, pavimentación, instalaciones")
          ),
          card(
            id = "nav_public_services_card", 
            class = "cursor-pointer",
            "Servicios Públicos", 
            tags$small("Agua, electricidad, recolección")
          ),
          card(
            id = "nav_education_card", 
            class = "cursor-pointer",
            "Educación", 
            tags$small("Escuelas, acceso, calidad")
          ),
          card(
            id = "nav_healthcare_card", 
            class = "cursor-pointer",
            "Servicios de Salud", 
            tags$small("Hospitales, atención médica")
          ),
          card(
            id = "nav_housing_card", 
            class = "cursor-pointer",
            "Vivienda", 
            tags$small("Condiciones, acceso")
          )
        )
      ),
      
      # Other sections as needed
      # Bienestar Social
      card(
        card_header("Bienestar Social y Económico"),
        # Cards for this section
      ),
      
      # Movilidad Urbana
      card(
        card_header("Movilidad Urbana y Medio Ambiente"),
        # Cards for this section
      ),
      
      # Gobierno
      card(
        card_header("Gobierno"),
        # Cards for this section
      ),
      
      # Participación
      card(
        card_header("Participación Ciudadana"),
        # Cards for this section
      )
    )
  )
}

# Run the app
shinyApp(ui, server)