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
library(bsicons)

# Source all necessary files
# Technical modules
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

# Source all theme UI and server files for all categories
# Bienestar Social y Económico
source("R/wellness/wellness_ui.R")
source("R/wellness/wellness_server.R")
source("R/wellness/economy_ui.R")
source("R/wellness/economy_server.R")
source("R/wellness/cultural_ui.R")
source("R/wellness/cultural_server.R")
source("R/wellness/identity_ui.R")
source("R/wellness/identity_server.R")
source("R/wellness/environment_ui.R")
source("R/wellness/environment_server.R")
# Movilidad Urbana y Medio Ambiente
source("R/urban/urban_ui.R")
source("R/urban/urban_server.R")
source("R/urban/mobility_ui.R")
source("R/urban/mobility_server.R")
source("R/urban/transportation_ui.R")
source("R/urban/transportation_server.R")


# Gobierno
source("R/government/government_ui.R")
source("R/government/government_server.R")
source("R/government/inequality_ui.R")
source("R/government/inequality_server.R")
source("R/government/accountability_ui.R")
source("R/government/accountability_server.R")
source("R/government/representation_ui.R")
source("R/government/representation_server.R")
source("R/government/expectations_ui.R")
source("R/government/expectations_server.R")
source("R/government/trust_ui.R")
source("R/government/trust_server.R")

# Infraestructura y Servicios
source("R/infrastructure/infrastructure_ui.R")
source("R/infrastructure/infrastructure_server.R")
source("R/infrastructure/public_services_ui.R")
source("R/infrastructure/public_services_server.R")
source("R/infrastructure/education_ui.R")
source("R/infrastructure/education_server.R")
source("R/infrastructure/healthcare_ui.R")
source("R/infrastructure/healthcare_server.R")
source("R/infrastructure/housing_ui.R")
source("R/infrastructure/housing_server.R")

# Participación Ciudadana
source("R/participation/participation_ui.R")
source("R/participation/participation_server.R")
source("R/participation/civic_ui.R")
source("R/participation/civic_server.R")
source("R/participation/community_ui.R")
source("R/participation/community_server.R")

# Extra section
source("R/extra/methodology_ui.R")
source("R/extra/methodology_server.R")



# Header component function
create_dashboard_header <- function(title, subtitle = NULL) {
  div(
    class = "dashboard-header mb-4",
    div(
      class = "container-fluid",  # Use fluid container for full width
      div(
        class = "row align-items-center",
        div(
          class = "col",
          h1(class = "display-5 fw-bold text-primary", title),
          if (!is.null(subtitle)) {
            p(class = "lead text-muted", subtitle)
          }
        ),
        # Only include logo if you have one
        # div(
        #   class = "col-auto",
        #   img(src = "logo.png", height = "60px", alt = "Logo")
        # )
      )
    )
  )
}

# Footer component function
create_dashboard_footer <- function() {
  div(
    class = "border-top mt-5 pt-4 pb-4 text-center text-muted",
    div(
      class = "container-fluid",
      p("Dashboard creado por Plan Estratégico de Juárez", class = "mb-1"),
      p("Datos actualizados: Marzo 2025", class = "mb-0 small")
    )
  )
}



# Define UI using page_navbar
ui <- page_navbar(
  # Title with properly styled dropdown
  title = div(
    class = "navbar-title-container",
    span("Dashboard AEJ", class = "me-3"),
    # Custom styled dropdown
    div(
      class = "year-selector",
      div(
        class = "dropdown",
        tags$button(
          class = "btn dropdown-toggle year-dropdown-btn",
          type = "button",
          id = "yearDropdown",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = "false",
          "2024"
        ),
        tags$ul(
          class = "dropdown-menu",
          `aria-labelledby` = "yearDropdown",
          tags$li(tags$a(class = "dropdown-item", href = "#", onclick = "Shiny.setInputValue('surveyYear', '2023')", "2023")),
          tags$li(tags$a(class = "dropdown-item", href = "#", onclick = "Shiny.setInputValue('surveyYear', '2024')", "2024"))
        )
      )
    )
  ),
  id = "navbar",
  bg = "#0d6efd", 
  inverse = TRUE,
  
  # Custom CSS for styling
  header = tags$head(
    # Add link to external CSS file
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"),
    
    # Updated CSS for year selector dropdown
    tags$style(HTML("
      /* Navbar styling */
      .navbar {
        background-color: #0d6efd !important;
        padding: 0.5rem 1rem;
      }
      
      .navbar-title-container {
        display: flex;
        align-items: center;
        padding: 0;
      }
      
      /* Year dropdown styling */
      .year-dropdown-btn {
        background-color: rgba(255, 255, 255, 0.25);
        border: none;
        border-radius: 20px;
        color: white;
        font-weight: bold;
        padding: 0.375rem 1rem;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        display: flex;
        align-items: center;
        justify-content: space-between;
        min-width: 90px;
      }
      
      .year-dropdown-btn:hover, .year-dropdown-btn:focus {
        background-color: rgba(255, 255, 255, 0.35);
        color: white;
        box-shadow: 0 2px 8px rgba(0,0,0,0.2);
      }
      
      .year-selector .dropdown-menu {
        background-color: #0d6efd;
        border-radius: 15px;
        margin-top: 5px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.2);
        border: none;
        min-width: 5rem;
        padding: 0.5rem 0;
      }
      
      .year-selector .dropdown-item {
        color: white;
        font-weight: 500;
        text-align: center;
        padding: 0.5rem 1rem;
      }
      
      .year-selector .dropdown-item:hover, .year-selector .dropdown-item:focus {
        background-color: rgba(255, 255, 255, 0.15);
      }
      
      /* Make sure navbar elements don't overflow */
      .container-fluid {
        padding-left: 1rem;
        padding-right: 1rem;
      }
      
      /* Ensure hamburger menu is visible */
      .navbar-toggler {
        display: block !important;
        margin-left: auto;
        z-index: 1050;
        padding: 0.375rem 0.5rem;
      }
      
      /* Responsive behavior */
      @media (max-width: 992px) {
        .navbar-collapse {
          position: absolute;
          top: 56px;
          left: 0;
          right: 0;
          background-color: #0d6efd;
          padding: 1rem;
          z-index: 1040;
        }
      }
      
      @media (max-width: 576px) {
        .navbar {
          padding-left: 0.5rem;
          padding-right: 0.5rem;
        }
        
        .navbar-title-container {
          width: auto;
        }
        
        .navbar-brand {
          font-size: 1rem;
          margin-right: 0.5rem;
        }
        
        .year-dropdown-btn {
          padding: 0.25rem 0.75rem;
          min-width: 80px;
          font-size: 0.9rem;
        }
        
        /* Ensure no horizontal overflow */
        body {
          overflow-x: hidden;
        }
      }
    ")),
    
    # Add JavaScript to update dropdown text
    tags$script(HTML("
      $(document).ready(function() {
        // Update dropdown button text based on selected year
        Shiny.addCustomMessageHandler('updateYearDropdown', function(year) {
          $('#yearDropdown').text(year);
        });
        
        // Initialize dropdown to 2024
        $('#yearDropdown').text('2024');
      });
    "))
  ),
  
  # Main overview tab
  nav_panel(
    title = "Inicio",
    icon = icon("home"),
    value = "overview",
    create_dashboard_header("Dashboard General", "Vista general del estado de Ciudad Juárez según las encuestas más recientes."),
    # Dashboard overview content
    div(
 
      # Grid of category cards
      div(
        class = "row",
        
        # Bienestar Social y Económico
        div(
          class = "col-md-4 mb-4",
          div(
            class = "card h-100",
            div(
              class = "card-body d-flex flex-column align-items-center text-center",
              div(
                class = "nav-card w-100",
                id = "nav_wellness_card",
                onclick = "Shiny.setInputValue('nav_target', 'wellness', {priority: 'event'})",
                div(class = "nav-card-icon", icon("heart")),
                h4(class = "nav-card-title", "Bienestar"),
                p("Análisis de condiciones económicas, participación cultural y tendencias demográficas")
              )
            )
          )
        ),
        
        # Movilidad Urbana y Medio Ambiente
        div(
          class = "col-md-4 mb-4",
          div(
            class = "card h-100",
            div(
              class = "card-body d-flex flex-column align-items-center text-center",
              div(
                class = "nav-card w-100",
                id = "nav_urban_card",
                onclick = "Shiny.setInputValue('nav_target', 'urban', {priority: 'event'})",
                div(class = "nav-card-icon", icon("bus-alt")),
                h4(class = "nav-card-title", "Movilidad"),
                p("Información sobre movilidad, transporte público y medio ambiente")
              )
            )
          )
        ),
        
        # Gobierno
        div(
          class = "col-md-4 mb-4",
          div(
            class = "card h-100",
            div(
              class = "card-body d-flex flex-column align-items-center text-center",
              div(
                class = "nav-card w-100",
                id = "nav_government_card",
                onclick = "Shiny.setInputValue('nav_target', 'government', {priority: 'event'})",
                div(class = "nav-card-icon", icon("landmark")),
                h4(class = "nav-card-title", "Gobierno"),
                p("Análisis de desigualdad, rendición de cuentas, representación y confianza")
              )
            )
          )
        ),
        
        # Infraestructura y Servicios
        div(
          class = "col-md-6 mb-4",
          div(
            class = "card h-100",
            div(
              class = "card-body d-flex flex-column align-items-center text-center",
              div(
                class = "nav-card w-100",
                id = "nav_infrastructure_card",
                onclick = "Shiny.setInputValue('nav_target', 'infrastructure', {priority: 'event'})",
                div(class = "nav-card-icon", icon("building")),
                h4(class = "nav-card-title", "Infraestructura"),
                p("Información sobre servicios públicos, educación, salud y vivienda")
              )
            )
          )
        ),
        
        # Participación Ciudadana
        div(
          class = "col-md-6 mb-4",
          div(
            class = "card h-100",
            div(
              class = "card-body d-flex flex-column align-items-center text-center",
              div(
                class = "nav-card w-100",
                id = "nav_participation_card",
                onclick = "Shiny.setInputValue('nav_target', 'participation', {priority: 'event'})",
                div(class = "nav-card-icon", icon("users")),
                h4(class = "nav-card-title", "Participación"),
                p("Análisis de la participación cívica y comunitaria")
              )
            )
          )
        )
      )
    ),create_dashboard_footer()
  ),
  
  # ---- Bienestar Social y Económico ----
  nav_menu(
    title = "Bienestar Social y Económico",
    icon = icon("heart"),
    
    # Main Wellness panel
    nav_panel(
      title = "Vista General",
      value = "wellness",
      wellnessUI()
    ),
    
    # Condiciones Económicas
    nav_panel(
      title = "Condiciones Económicas",
      value = "economic", 
      economyUI()
    ),
    
    # Participación Cultural
    nav_panel(
      title = "Participación Cultural",
      value = "cultural",
      culturalUI()
    ),
    
    # Tendencias Demográficas
    nav_panel(
      title = "Identidad y pertenencia",
      value = "identity",
      identityUI()
    ),
    
    # Medio Ambiente
    nav_panel(
      title = "Medio Ambiente",
      value = "environment",
      environmentUI()
    )
  ),
  
  # ---- Movilidad Urbana y Medio Ambiente ----
  nav_menu(
    title = "Movilidad Urbana",
    icon = icon("bus-alt"),
    
    # Main Urban panel
    nav_panel(
      title = "Vista General",
      value = "urban",
      urbanUI()
    ),
    
    # Movilidad
    nav_panel(
      title = "Movilidad",
      value = "mobility",
      mobilityUI()
    ),
    
    # Transporte Público
    nav_panel(
      title = "Transporte Público",
      value = "transportation",
      transportationUI()
    )
  ),
  
  # ---- Gobierno ----
  nav_menu(
    title = "Gobierno",
    icon = icon("landmark"),
    
    # Main Government panel
    nav_panel(
      title = "Vista General",
      value = "government",
      governmentUI()
    ),
    
    # Desigualdad
    nav_panel(
      title = "Desigualdad",
      value = "inequality",
      inequalityUI()
    ),
    
    # Rendición de Cuentas
    nav_panel(
      title = "Rendición de Cuentas",
      value = "accountability",
      accountabilityUI()
    ),
    
    # Representación Política
    nav_panel(
      title = "Representación Política",
      value = "representation",
      representationUI()
    ),
    
    # Expectativas en el gobierno
    nav_panel(
      title = "Expectativas",
      value = "expectations",
      expectationsUI()
    ),
    
    # Confianza en las instituciones
    nav_panel(
      title = "Confianza Institucional",
      value = "trust",
      trustUI()
    )
  ),
  
  # ---- Infraestructura y Servicios ----
  nav_menu(
    title = "Infraestructura",
    icon = icon("building"),
    
    # Main Infrastructure panel
    nav_panel(
      title = "Vista General",
      value = "infrastructure",
      infrastructureUI()
    ),
    
    # Servicios Públicos
    nav_panel(
      title = "Servicios Públicos",
      value = "public_services",
      publicServicesUI()
    ),
    
    # Educación
    nav_panel(
      title = "Educación",
      value = "education",
      educationUI()
    ),
    
    # Servicios de Salud
    nav_panel(
      title = "Servicios de Salud",
      value = "healthcare",
      healthcareUI()
    ),
    
    # Vivienda
    nav_panel(
      title = "Vivienda",
      value = "housing",
      housingUI()
    )
  ),
  
  # ---- Participación Ciudadana ----
  nav_menu(
    title = "Participación",
    icon = icon("users"),
    
    # Main Participation panel
    nav_panel(
      title = "Vista General",
      value = "participation",
      participationUI()
    ),
    
    # Participación Cívica
    nav_panel(
      title = "Participación Cívica",
      value = "civic",
      civicUI()
    ),
    
    # Participación Comunitaria
    nav_panel(
      title = "Participación Comunitaria",
      value = "community",
      communityUI()
    )
  ),
  
  # ---- Other Sections ----
  nav_menu(
    title = "Extras",
    icon = icon("ellipsis-h"),
    nav_panel(
      title = "Reportes",
      icon = icon("file-alt"),
      value = "reports",
      div(
        class = "container mt-4",
        h2("Reportes Personalizados"),
        p("Esta sección le permitirá generar reportes personalizados según sus necesidades específicas.")
      )
    ),
    nav_panel(
      title = "Metodología",
      icon = icon("download"),
      value = "methodology",
      methodologyUI()
    ),
    nav_panel(
      title = "Acerca de",
      icon = icon("info-circle"),
      value = "about",
      div(
        class = "container mt-4",
        h2("Acerca del Dashboard"),
        p("Información sobre el propósito de este dashboard y el equipo detrás de él.")
      )
    )
  ),
)

# Define server
server <- function(input, output, session) {
# Create a reactive value for the selected year
selectedYear <- reactive({
  input$surveyYear
})

# Store the selected year in session$userData for access by other modules
observe({
  session$userData$selectedYear <- selectedYear()
})

  observeEvent(input$nav_target, {
    nav_value <- input$nav_target
    updateNavbarPage(session, "navbar", selected = nav_value)
  
    
    # Initialize the appropriate server module
    if (current_tab == "wellness") {
      wellnessServer(input, output, session)
    } else if (current_tab == "economic") {
      economyServer(input, output, session)
    } else if (current_tab == "cultural") {
      culturalServer(input, output, session)
    } else if (current_tab == "identity") {
      identityServer(input, output, session)
    } else if (current_tab == "urban") {
      urbanServer(input, output, session)
    } else if (current_tab == "mobility") {
      mobilityServer(input, output, session)
    } else if (current_tab == "transportation") {
      transportationServer(input, output, session)
    } else if (current_tab == "environment") {
      environmentServer(input, output, session)
    } else if (current_tab == "government") {
      governmentServer(input, output, session)
    } else if (current_tab == "inequality") {
      inequalityServer(input, output, session)
    } else if (current_tab == "accountability") {
      accountabilityServer(input, output, session)
    } else if (current_tab == "representation") {
      representationServer(input, output, session)
    } else if (current_tab == "expectations") {
      expectationsServer(input, output, session)
    } else if (current_tab == "trust") {
      trustServer(input, output, session)
    } else if (current_tab == "infrastructure") {
      infrastructureServer(input, output, session)
    } else if (current_tab == "public_services") {
      publicServicesServer(input, output, session)
    } else if (current_tab == "education") {
      educationServer(input, output, session)
    } else if (current_tab == "healthcare") {
      healthcareServer(input, output, session)
    } else if (current_tab == "housing") {
      housingServer(input, output, session)
    } else if (current_tab == "participation") {
      participationServer(input, output, session)
    } else if (current_tab == "civic") {
      civicServer(input, output, session)
    } else if (current_tab == "community") {
      communityServer(input, output, session)
    }else if (current_tab == "methodology") {
      methodologyServer(input, output, session)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)