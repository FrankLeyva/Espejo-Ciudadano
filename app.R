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

# Load only essential core modules at startup
source("R/global_theme.R")
source("R/utils.R")
source("R/survey_config.R")
source("R/data_loader.R")
source("R/question_classifier.R")
source("R/themes_metadata.R")  

# Helper function for lazy loading modules
load_module <- function(module_path, module_name = NULL) {
  if (is.null(module_name)) {
    # Extract module name from path if not provided
    module_name <- basename(tools::file_path_sans_ext(module_path))
  }

  # Only source if not already loaded
  if (!exists(paste0(module_name, "UI"), envir = .GlobalEnv) || 
      !exists(paste0(module_name, "Server"), envir = .GlobalEnv)) {
    
    tryCatch({
      source(module_path)
      message(paste("Loaded module:", module_path))
      return(TRUE)
    }, error = function(e) {
      warning(paste("Failed to load module:", module_path, "-", e$message))
      return(FALSE)
    })
  }
  return(TRUE)
}

# Function to load UI for a specific module
load_ui_module <- function(section, module_name) {
  module_path <- file.path("R", section, paste0(module_name, "_ui.R"))
  load_module(module_path, module_name)
}

# Function to load Server for a specific module
load_server_module <- function(section, module_name) {
  module_path <- file.path("R", section, paste0(module_name, "_server.R"))
  load_module(module_path, module_name)
}

# Preload core modules needed for main page
load_module("R/binary_module.R", "binary")
load_module("R/categorical_module.R", "categorical")
load_module("R/interval_module.R", "interval")
load_module("R/ordinal_module.R", "ordinal")
load_module("R/nominal_module.R", "nominal")
load_module("R/razon_module.R", "razon")
load_module("R/special_module.R", "special")


geo_data_global <- NULL

# Attempt to load the geo data
tryCatch({
  geo_data_global <- sf::st_read('data/geo/Jrz_Map.geojson', quiet = TRUE)
  message("Geographic data loaded successfully")
}, error = function(e) {
  message(paste("Error loading geographic data:", e$message))
})


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
    tags$link(rel = "stylesheet", href = "dashboard-map.css"),

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
      uiOutput("wellness_ui_container")
    ),
    
    # Condiciones Económicas
    nav_panel(
      title = "Condiciones Económicas",
      value = "economic", 
      uiOutput("economic_ui_container")
    ),
    
    # Participación Cultural
    nav_panel(
      title = "Participación Cultural",
      value = "cultural",
      uiOutput("cultural_ui_container")
    ),
    
    # Tendencias Demográficas
    nav_panel(
      title = "Identidad y pertenencia",
      value = "identity",
      uiOutput("identity_ui_container")
    ),
    
    # Medio Ambiente
    nav_panel(
      title = "Medio Ambiente",
      value = "environment",
      uiOutput("environment_ui_container")
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
      uiOutput("urban_ui_container")
    ),
    
    # Movilidad
    nav_panel(
      title = "Movilidad",
      value = "mobility",
      uiOutput("mobility_ui_container")

    ),
    
    # Transporte Público
    nav_panel(
      title = "Transporte Público",
      value = "transportation",
      uiOutput("transportation_ui_container")

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
      uiOutput("government_ui_container")

    ),
    
    # Desigualdad
    nav_panel(
      title = "Desigualdad",
      value = "inequality",
      uiOutput("inequality_ui_container")

    ),
    
    # Rendición de Cuentas
    nav_panel(
      title = "Rendición de Cuentas",
      value = "accountability",
      uiOutput("accountability_ui_container")

    ),
    
    # Representación Política
    nav_panel(
      title = "Representación Política",
      value = "representation",
      uiOutput("representation_ui_container")

    ),
    
    # Expectativas en el gobierno
    nav_panel(
      title = "Expectativas",
      value = "expectations",
      uiOutput("expectations_ui_container")

    ),
    
    # Confianza en las instituciones
    nav_panel(
      title = "Confianza Institucional",
      value = "trust",
      uiOutput("trust_ui_container")

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
      uiOutput("infrastructure_ui_container")

    ),
    
    # Servicios Públicos
    nav_panel(
      title = "Servicios Públicos",
      value = "public_services",
      uiOutput("public_services_ui_container")

    ),
    
    # Educación
    nav_panel(
      title = "Educación",
      value = "education",
      uiOutput("education_ui_container")

    ),
    
    # Servicios de Salud
    nav_panel(
      title = "Servicios de Salud",
      value = "healthcare",
      uiOutput("healthcare_ui_container")

    ),
    
    # Vivienda
    nav_panel(
      title = "Vivienda",
      value = "housing",
      uiOutput("housing_ui_container")

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
      uiOutput("participation_ui_container")

    ),
    
    # Participación Cívica
    nav_panel(
      title = "Participación Cívica",
      value = "civic",
      uiOutput("civic_ui_container")

    ),
    
    # Participación Comunitaria
    nav_panel(
      title = "Participación Comunitaria",
      value = "community",
      uiOutput("community_ui_container")

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
  uiOutput("explorer_ui_container")

),
nav_panel(
  title = "Mapa del Dashboard",
  icon = icon("sitemap"),
  value = "dashboard_map",
  uiOutput("dashboard_map_ui_container")

),
    nav_panel(
      title = "Metodología",
      icon = icon("download"),
      value = "methodology",
      uiOutput("methodology_ui_container")

    ),
    nav_panel(
      title = "Acerca de",
      icon = icon("info-circle"),
      value = "about",
      uiOutput("about_ui_container")

    )
  )
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
 surveyCache <- reactiveVal(list())

  
  # Create a reactive that handles survey loading
loadSurveyData <- function(survey_type, year) {
  # Create a unique key for this survey
  cache_key <- paste0(survey_type, "_", year)
  
  # Check if survey exists in cache
  current_cache <- surveyCache()
  if (!is.null(current_cache[[cache_key]])) {
    return(current_cache[[cache_key]])
  }
  
  # If not in cache, load it
  survey_id <- paste0(survey_type, "_", year)
  survey_data <- load_survey_data(survey_id)
  
  # Store in cache
  current_cache[[cache_key]] <- survey_data
  surveyCache(current_cache)
  
  return(survey_data)
}
perSurveyData <- reactive({
  loadSurveyData("PER", selectedYear())
})

parSurveyData <- reactive({
  loadSurveyData("PAR", selectedYear())
})
  
# In app.R, add an observer to invalidate cache on year change
observeEvent(selectedYear(), {
  # Reset the cache when year changes
  surveyCache(list())
})
# Store these reactive functions in session$userData
session$userData$perSurveyData <- perSurveyData
session$userData$parSurveyData <- parSurveyData  
  
geoData <- reactive({
  # Return the global geo data
  geo_data_global
})

# Store in session$userData
session$userData$geoData <- geoData 
  
  
  
output$wellness_ui_container <- renderUI({
  load_ui_module("wellness", "wellness")
  wellnessUI()
})

output$economic_ui_container <- renderUI({
  load_ui_module("wellness", "economy")
  economyUI()
})
  
output$cultural_ui_container <- renderUI({
  load_ui_module("wellness", "cultural")
  culturalUI()
})
output$identity_ui_container <- renderUI({
  load_ui_module("wellness", "identity")
  identityUI()
})
  
output$environment_ui_container <- renderUI({
  load_ui_module("wellness", "environment")
  environmentUI()
})
  
  
output$urban_ui_container <- renderUI({
  load_ui_module("urban", "urban")
  urbanUI()
})
output$mobility_ui_container <- renderUI({
  load_ui_module("urban", "mobility")
  mobilityUI()
})
output$transportation_ui_container <- renderUI({
  load_ui_module("urban", "transportation")
  transportationUI()
})
  
output$participation_ui_container <- renderUI({
  load_ui_module("participation", "participation")
  participationUI()
})

output$civic_ui_container <- renderUI({
  load_ui_module("participation", "civic")
  civicUI()
})
output$community_ui_container <- renderUI({
  load_ui_module("participation", "community")
  communityUI()
})
  
output$infrastructure_ui_container <- renderUI({
  load_ui_module("infrastructure", "infrastructure")
  infrastructureUI()
})
  
output$education_ui_container <- renderUI({
  load_ui_module("infrastructure", "education")
  educationUI()
})
output$healthcare_ui_container <- renderUI({
  load_ui_module("infrastructure", "healthcare")
  healthcareUI()
})
output$housing_ui_container <- renderUI({
  load_ui_module("infrastructure", "housing")
  housingUI()
})
output$public_services_ui_container <- renderUI({
  load_ui_module("infrastructure", "public_services")
  publicServicesUI()
})
  
output$government_ui_container <- renderUI({
  load_ui_module("government", "government")
  governmentUI()
})
  
output$accountability_ui_container <- renderUI({
  load_ui_module("government", "accountability")
  accountabilityUI()
})
output$expectations_ui_container <- renderUI({
  load_ui_module("government", "expectations")
  expectationsUI()
})
output$inequality_ui_container <- renderUI({
  load_ui_module("government", "inequality")
  inequalityUI()
})
output$representation_ui_container <- renderUI({
  load_ui_module("government", "representation")
  representationUI()
})
output$trust_ui_container <- renderUI({
  load_ui_module("government", "trust")
  trustUI()
})
  
output$about_ui_container <- renderUI({
  load_ui_module("extras", "about")
  aboutUI('about')
})
output$methodology_ui_container <- renderUI({
  load_ui_module("extras", "methodology")
  methodologyUI()
}) 
  output$explorer_ui_container <- renderUI({
  load_ui_module("extras", "explorer")
  explorerUI('survey_explorer')
})
  output$dashboard_map_ui_container <- renderUI({
  load_module("R/extra/dashboard_map.R", "dashboard_map")
  dashboardMapUI('dashboard_map')
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
      load_server_module("wellness", "wellness")
      wellnessServer(input, output, session, current_theme)
    } else if (current_tab == "economic") {
      load_server_module("wellness", "economy")

      economyServer(input, output, session, current_theme)
    } else if (current_tab == "cultural") {
      load_server_module("wellness", "cultural")
      culturalServer(input, output, session, current_theme)
    } else if (current_tab == "identity") {
      load_server_module("wellness", "identity")
      identityServer(input, output, session, current_theme)
    } else if (current_tab == "urban") {
      load_server_module("urban", "urban")

      urbanServer(input, output, session, current_theme)
    } else if (current_tab == "mobility") {
      load_server_module("urban", "mobility")

      mobilityServer(input, output, session, current_theme)
    } else if (current_tab == "transportation") {
      load_server_module("urban", "transportation")

      transportationServer(input, output, session, current_theme)
    } else if (current_tab == "environment") {
      load_server_module("wellness", "environment")

      environmentServer(input, output, session, current_theme)
    } else if (current_tab == "government") {
      load_server_module("government", "government")

      governmentServer(input, output, session, current_theme)
    } else if (current_tab == "inequality") {
      load_server_module("government", "inequality")

      inequalityServer(input, output, session, current_theme)
    } else if (current_tab == "accountability") {
      load_server_module("government", "accountability")

      accountabilityServer(input, output, session, current_theme)
    } else if (current_tab == "representation") {
      load_server_module("government", "representation")

      representationServer(input, output, session, current_theme)
    } else if (current_tab == "expectations") {
      load_server_module("government", "expectations")

      expectationsServer(input, output, session, current_theme)
    } else if (current_tab == "trust") {
      load_server_module("government", "trust")

      trustServer(input, output, session, current_theme)
    } else if (current_tab == "infrastructure") {
      load_server_module("infrastructure", "infrastructure")

      infrastructureServer(input, output, session, current_theme)
    } else if (current_tab == "public_services") {
      load_server_module("infrastructure", "public_services")

      publicServicesServer(input, output, session, current_theme)
    } else if (current_tab == "education") {
      load_server_module("infrastructure", "education")

      educationServer(input, output, session, current_theme)
    } else if (current_tab == "healthcare") {
      load_server_module("infrastructure", "healthcare")

      healthcareServer(input, output, session, current_theme)
    } else if (current_tab == "housing") {
      load_server_module("infrastructure", "housing")

      housingServer(input, output, session, current_theme)
    } else if (current_tab == "participation") {
      load_server_module("participation", "participation")

      participationServer(input, output, session, current_theme)
    } else if (current_tab == "civic") {
      load_server_module("participation", "civic")

      civicServer(input, output, session, current_theme)
    } else if (current_tab == "community") {
      load_server_module("participation", "community")

      communityServer(input, output, session, current_theme)
    } else if (current_tab == "methodology") {
      load_server_module("extras", "methodology")

      methodologyServer(input, output, session, current_theme)
    } else if (current_tab == "about") {
      load_server_module("extras", "about")
      aboutServer("about_section")
    } else if (current_tab == "explorer") {
      load_server_module("extras", "explorer")

      explorerServer("survey_explorer")
    } else if (current_tab == "dashboard_map") {
      dashboardMapServer("dashboard_map")
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