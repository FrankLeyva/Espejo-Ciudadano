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

# Define UI using page_navbar
ui <- page_navbar(
  # Title and theme
  title = div(
    span("Dashboard AEJ 2024"),
    span(
      style = "float: right; margin-right: 20px;",
      radioButtons("surveyYear", "Año:", 
                  choices = c("2023", "2024"), 
                  selected = "2024",
                  inline = TRUE)
    )
  ),
  id = "navbar",
  bg = "#0d6efd",
  inverse = TRUE,
  
  # Custom CSS for styling
  header = tags$head(
    tags$style(HTML("
      /* Card styling */
      .card {
        margin-bottom: 20px;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      
      /* Navigation card styles */
      .nav-card {
        transition: transform 0.3s, box-shadow 0.3s;
        cursor: pointer;
        height: 100%;
      }
      .nav-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 10px 20px rgba(0,0,0,0.1);
      }
      .nav-card-icon {
        font-size: 2.5rem;
        margin-bottom: 15px;
        color: #0d6efd;
      }
      .nav-card-title {
        font-weight: bold;
        font-size: 1.2rem;
        margin-bottom: 10px;
      }
      
      
      /* Back button styling */
      .back-link {
        margin-bottom: 15px;
        padding: 6px 12px;
        background-color: #f8f9fa;
        border-radius: 4px;
        display: inline-block;
        text-decoration: none;
        color: #495057;
        cursor: pointer;
      }
      .back-link:hover {
        background-color: #e9ecef;
      }
    ")),
    tags$script(HTML("
    $(document).ready(function() {
      // Function to close all navbar dropdowns
      function closeAllDropdowns() {
        $('.navbar .dropdown-menu').removeClass('show');
        $('.navbar .dropdown').removeClass('show');
        $('.navbar .nav-item.dropdown').removeClass('show');
        $('.navbar .dropdown-toggle').attr('aria-expanded', 'false');
      }
      
      // Close dropdowns after navigation with a 3-second delay
      $(document).on('shown.bs.tab', function() {
        setTimeout(closeAllDropdowns, 3000);
      });
      
      // Also close dropdowns when clicking on nav cards - immediate closing
      $(document).on('click', '.nav-card', function() {
        // Still show the dropdown for visual feedback
        setTimeout(closeAllDropdowns, 3000);
      });
      
      // Additional handler for any navigation
      Shiny.addCustomMessageHandler('closeNavDropdowns', function(message) {
        setTimeout(closeAllDropdowns, 3000);
      });
    });
  "))
),
  
  # Main overview tab
  nav_panel(
    title = "Inicio",
    icon = icon("home"),
    value = "overview",
    
    # Dashboard overview content
    div(
      class = "container mt-4",
      h2("Dashboard General", class = "mb-4"),
      p("Vista general del estado de Ciudad Juárez según las encuestas más recientes.", class = "lead mb-4"),
      
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
    )
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
)

# Define server
server <- function(input, output, session) {
  # Handle navigation from the overview cards
  selectedYear <- reactive({
    input$surveyYear
  })
  session$userData$selectedYear <- selectedYear

  observeEvent(input$nav_target, {
    nav_value <- input$nav_target
    updateNavbarPage(session, "navbar", selected = nav_value)
  })
  
  # Initialize servers based on the current tab
  observe({
    # Get the current tab value
    current_tab <- input$navbar
    
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