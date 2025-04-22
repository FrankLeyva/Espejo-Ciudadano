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
library(pagedown)
library(htmlwidgets)

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
source("R/extra/about_ui.R")
source("R/extra/about_server.R")
source("R/extra/explorer_ui.R")
source("R/extra/explorer_server.R")





ui <- page_navbar(
    title = div(
    class = "navbar-title-container",
    # Use a div with display:flex for better alignment
    div(
      class = "navbar-brand-container",
      span("Espejo Ciudadano", class = "navbar-brand")
    ),
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
  navbar_options = navbar_options(
    theme = "dark"  # Instead of inverse = TRUE
  ),
  
  # Add these additional styles to fix alignment
  header = tags$head(
    # Your existing links
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"),
    tags$script(HTML("
    $(document).ready(function() {
      // Global click handler for any element with 'nav-card' class anywhere in the app
      $(document).on('click', '[class*=\"nav-card\"]', function() {
        // Wait for navigation to complete
        setTimeout(function() {
          // Close any expanded navbar dropdown
          $('.navbar-collapse.show').collapse('hide');
          
          // Also handle dropdown menus
          $('.dropdown-menu.show').removeClass('show');
          $('.nav-item.dropdown.show').removeClass('show');
          $('[aria-expanded=true]').attr('aria-expanded', 'false');
        }, 500); // 300ms delay
      });
      
      // Also handle any element that sets nav_target input
      $(document).on('click', '[onclick*=\"nav_target\"]', function() {
        // Wait for navigation to complete
        setTimeout(function() {
          // Close any expanded navbar dropdown
          $('.navbar-collapse.show').collapse('hide');
          
          // Also handle dropdown menus
          $('.dropdown-menu.show').removeClass('show');
          $('.nav-item.dropdown.show').removeClass('show');
          $('[aria-expanded=true]').attr('aria-expanded', 'false');
        }, 500); // 300ms delay
      });
    });
  ")),
    tags$script(HTML("
  $(document).ready(function() {
    // Update dropdown button text when year changes
    Shiny.addCustomMessageHandler('updateYearDropdown', function(year) {
      $('#yearDropdown').text(year);
      
      // Also visually update which dropdown item appears selected
      $('.year-selector .dropdown-item').removeClass('active');
      $('.year-selector .dropdown-item').each(function() {
        if ($(this).text() === year) {
          $(this).addClass('active');
        }
      });
    });
    
    // Initialize dropdown to 2024
    $('#yearDropdown').text('2024');
    
    // Make sure clicking dropdown items properly triggers the Shiny input
    $('.year-selector .dropdown-item').on('click', function(e) {
      e.preventDefault();
      var year = $(this).text();
      Shiny.setInputValue('surveyYear', year);
      $('#yearDropdown').text(year);
    });
  });
")),
tags$script(src = "custom.js")
  ),
  
  # Main overview tab
  nav_panel(
    title = "Inicio",
    icon = icon("home"),
    value = "overview",
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
          class = "nav-card nav-card-bienestar w-100",
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
          class = "nav-card nav-card-movilidad w-100",
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
          class = "nav-card nav-card-gobierno w-100",
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
          class = "nav-card nav-card-infraestructura w-100",
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
          class = "nav-card nav-card-participacion w-100",
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
    # nav_panel(
    #  title = "Reportes",
    #  icon = icon("file-alt"),
     # value = "reports",
   #   div(
   #     class = "container mt-4",
   #     h2("Reportes Personalizados"),
    #    p("Esta sección le permitirá generar reportes personalizados según sus necesidades específicas.")
   #   )
 #   ),

 nav_panel(
  title = "Explorador de Encuesta",
  icon = icon("search"),
  value = "explorer",
  explorerUI("survey_explorer")
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
      aboutUI("about_section")
    )
  ),
)

server <- function(input, output, session) {

  current_theme <- reactive({
    # Get the current tab value
    current_tab <- input$navbar
    
    # Map tabs to sections
    if (grepl("^wellness|^economic|^cultural|^identity|^environment", current_tab)) {
      section <- "bienestar"
    } else if (grepl("^urban|^mobility|^transportation", current_tab)) {
      section <- "movilidad"
    } else if (grepl("^government|^inequality|^accountability|^representation|^expectations|^trust", current_tab)) {
      section <- "gobierno"
    } else if (grepl("^infrastructure|^public_services|^education|^healthcare|^housing", current_tab)) {
      section <- "infraestructura"
    } else if (grepl("^participation|^civic|^community", current_tab)) {
      section <- "participacion"
    } else {
      section <- NULL  # Default theme
    }
    
    # Return the appropriate theme
    get_section_theme(section)
  })
 # Use reactiveVal with default value of "2024"
 selectedYearVal <- reactiveVal("2024")
  
 # Create a reactive expression that reads from the reactiveVal
 selectedYear <- reactive({
   selectedYearVal()
 })
 
 # Update the reactiveVal when user selects a year
 observeEvent(input$surveyYear, {
   if (!is.null(input$surveyYear)) {
     selectedYearVal(input$surveyYear)
     # Update dropdown button text
     session$sendCustomMessage("updateYearDropdown", input$surveyYear)
   }
 }, ignoreInit = FALSE)
 
 # Store the reactive expression in session$userData for module access
 session$userData$selectedYear <- selectedYear
 
 # Set initial dropdown text on load
 # This ensures the dropdown shows 2024 even before any user interaction
 observe({
  # This will update whenever selectedYear() changes
  session$sendCustomMessage(
    "setCurrentYear", 
    selectedYear()
  )
})
  # Handle navigation between tabs
  observeEvent(input$nav_target, {
    nav_value <- input$nav_target
    updateNavbarPage(session, "navbar", selected = nav_value)
  })
  
  # Initialize servers based on the current tab
  observe({
    req(input$navbar)
    # Get the current tab value
    current_tab <- input$navbar
    
    # Initialize the appropriate server module
    if (current_tab == "wellness") {
      wellnessServer(input, output, session, current_theme)
    } else if (current_tab == "economic") {
      economyServer(input, output, session, current_theme)
    } else if (current_tab == "cultural") {
      culturalServer(input, output, session, current_theme)
    } else if (current_tab == "identity") {
      identityServer(input, output, session, current_theme)
    } else if (current_tab == "urban") {
      urbanServer(input, output, session, current_theme)
    } else if (current_tab == "mobility") {
      mobilityServer(input, output, session, current_theme)
    } else if (current_tab == "transportation") {
      transportationServer(input, output, session, current_theme)
    } else if (current_tab == "environment") {
      environmentServer(input, output, session, current_theme)
    } else if (current_tab == "government") {
      governmentServer(input, output, session, current_theme)
    } else if (current_tab == "inequality") {
      inequalityServer(input, output, session, current_theme)
    } else if (current_tab == "accountability") {
      accountabilityServer(input, output, session, current_theme)
    } else if (current_tab == "representation") {
      representationServer(input, output, session, current_theme)
    } else if (current_tab == "expectations") {
      expectationsServer(input, output, session, current_theme)
    } else if (current_tab == "trust") {
      trustServer(input, output, session, current_theme)
    } else if (current_tab == "infrastructure") {
      infrastructureServer(input, output, session, current_theme)
    } else if (current_tab == "public_services") {
      publicServicesServer(input, output, session, current_theme)
    } else if (current_tab == "education") {
      educationServer(input, output, session, current_theme)
    } else if (current_tab == "healthcare") {
      healthcareServer(input, output, session, current_theme)
    } else if (current_tab == "housing") {
      housingServer(input, output, session, current_theme)
    } else if (current_tab == "participation") {
      participationServer(input, output, session, current_theme)
    } else if (current_tab == "civic") {
      civicServer(input, output, session, current_theme)
    } else if (current_tab == "community") {
      communityServer(input, output, session, current_theme)
    } else if (current_tab == "methodology") {
      methodologyServer(input, output, session, current_theme)
    } else if (current_tab == "about") {
      aboutServer("about_section")
    } else if (current_tab == "explorer") {
      explorerServer("survey_explorer")
    } 
  })
 # Add this to the observeEvent(input$navbar) in app.R
observeEvent(input$navbar, {
  # Get current section
  current_tab <- input$navbar
  
  # Remove all section classes
  shinyjs::removeClass(selector = "body", class = "section-bienestar")
  shinyjs::removeClass(selector = "body", class = "section-movilidad")
  shinyjs::removeClass(selector = "body", class = "section-gobierno")
  shinyjs::removeClass(selector = "body", class = "section-infraestructura")
  shinyjs::removeClass(selector = "body", class = "section-participacion")
  shinyjs::removeClass(selector = "body", class = "section-extras")
  
  # Add current section class
  if (grepl("^wellness|^economic|^cultural|^identity|^environment", current_tab)) {
    shinyjs::addClass(selector = "body", class = "section-bienestar")
  } else if (grepl("^urban|^mobility|^transportation", current_tab)) {
    shinyjs::addClass(selector = "body", class = "section-movilidad")
  } else if (grepl("^government|^inequality|^accountability|^representation|^expectations|^trust", current_tab)) {
    shinyjs::addClass(selector = "body", class = "section-gobierno")
  } else if (grepl("^infrastructure|^public_services|^education|^healthcare|^housing", current_tab)) {
    shinyjs::addClass(selector = "body", class = "section-infraestructura")
  } else if (grepl("^participation|^civic|^community", current_tab)) {
    shinyjs::addClass(selector = "body", class = "section-participacion")
  } else if (grepl("^methodology|^about", current_tab)) {
    shinyjs::addClass(selector = "body", class = "section-extras")
  }
})
}

# Run the application
shinyApp(ui = ui, server = server)