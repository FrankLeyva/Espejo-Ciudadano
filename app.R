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
library(memoise)
library(R6)

# Load core modules
source("R/global_theme.R")
source("R/utils.R")
source("R/survey_config.R")
source("R/data_loader.R")
source("R/question_classifier.R")
source("R/themes_metadata.R")
source("R/data_manager.R")  # Use the enhanced data manager

# Helper function for lazy loading modules
load_module <- function(module_path, module_name = NULL) {
  if (is.null(module_name)) {
    module_name <- basename(tools::file_path_sans_ext(module_path))
  }

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

ui <- page_navbar(
    title = div(
    class = "navbar-title-container",
    div(
      class = "navbar-brand-container",
      span("Espejo Ciudadano", class = "navbar-brand")
    ),
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
  navbar_options = navbar_options(theme = "dark"),
  
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"),
    tags$link(rel = "stylesheet", href = "dashboard-map.css"),
    tags$script(HTML("
    $(document).ready(function() {
      $(document).on('click', '[class*=\"nav-card\"]', function() {
        setTimeout(function() {
          $('.navbar-collapse.show').collapse('hide');
          $('.dropdown-menu.show').removeClass('show');
          $('.nav-item.dropdown.show').removeClass('show');
          $('[aria-expanded=true]').attr('aria-expanded', 'false');
        }, 500);
      });
      
      $(document).on('click', '[onclick*=\"nav_target\"]', function() {
        setTimeout(function() {
          $('.navbar-collapse.show').collapse('hide');
          $('.dropdown-menu.show').removeClass('show');
          $('.nav-item.dropdown.show').removeClass('show');
          $('[aria-expanded=true]').attr('aria-expanded', 'false');
        }, 500);
      });
    });
  ")),
    tags$script(HTML("
  $(document).ready(function() {
    Shiny.addCustomMessageHandler('updateYearDropdown', function(year) {
      $('#yearDropdown').text(year);
      $('.year-selector .dropdown-item').removeClass('active');
      $('.year-selector .dropdown-item').each(function() {
        if ($(this).text() === year) {
          $(this).addClass('active');
        }
      });
    });
    
    $('#yearDropdown').text('2024');
    
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

  init_tooltips(),

  # Main overview tab
  nav_panel(
    title = "Inicio",
    icon = icon("home"),
    value = "overview",
    div(
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
      ),
      create_dashboard_footer()
    )
  ),
  
  # ---- Bienestar Social y Económico ----
  nav_menu(
    title = "Bienestar Social y Económico",
    icon = icon("heart"),
    
    nav_panel(title = "Vista General", value = "wellness", uiOutput("wellness_ui_container")),
    nav_panel(title = "Condiciones Económicas", value = "economic", uiOutput("economic_ui_container")),
    nav_panel(title = "Participación Cultural", value = "cultural", uiOutput("cultural_ui_container")),
    nav_panel(title = "Identidad y pertenencia", value = "identity", uiOutput("identity_ui_container")),
    nav_panel(title = "Medio Ambiente", value = "environment", uiOutput("environment_ui_container"))
  ),
  
  # ---- Movilidad Urbana y Medio Ambiente ----
  nav_menu(
    title = "Movilidad Urbana",
    icon = icon("bus-alt"),
    
    nav_panel(title = "Vista General", value = "urban", uiOutput("urban_ui_container")),
    nav_panel(title = "Movilidad", value = "mobility", uiOutput("mobility_ui_container")),
    nav_panel(title = "Transporte Público", value = "transportation", uiOutput("transportation_ui_container"))
  ),
  
  # ---- Gobierno ----
  nav_menu(
    title = "Gobierno",
    icon = icon("landmark"),
    
    nav_panel(title = "Vista General", value = "government", uiOutput("government_ui_container")),
    nav_panel(title = "Desigualdad", value = "inequality", uiOutput("inequality_ui_container")),
    nav_panel(title = "Rendición de Cuentas", value = "accountability", uiOutput("accountability_ui_container")),
    nav_panel(title = "Representación Política", value = "representation", uiOutput("representation_ui_container")),
    nav_panel(title = "Expectativas", value = "expectations", uiOutput("expectations_ui_container")),
    nav_panel(title = "Confianza Institucional", value = "trust", uiOutput("trust_ui_container"))
  ),
  
  # ---- Infraestructura y Servicios ----
  nav_menu(
    title = "Infraestructura",
    icon = icon("building"),
    
    nav_panel(title = "Vista General", value = "infrastructure", uiOutput("infrastructure_ui_container")),
    nav_panel(title = "Servicios Públicos", value = "public_services", uiOutput("public_services_ui_container")),
    nav_panel(title = "Educación", value = "education", uiOutput("education_ui_container")),
    nav_panel(title = "Servicios de Salud", value = "healthcare", uiOutput("healthcare_ui_container")),
    nav_panel(title = "Vivienda", value = "housing", uiOutput("housing_ui_container"))
  ),
  
  # ---- Participación Ciudadana ----
  nav_menu(
    title = "Participación",
    icon = icon("users"),
    
    nav_panel(title = "Vista General", value = "participation", uiOutput("participation_ui_container")),
    nav_panel(title = "Participación Cívica", value = "civic", uiOutput("civic_ui_container")),
    nav_panel(title = "Participación Comunitaria", value = "community", uiOutput("community_ui_container"))
  ),
  
  # ---- Other Sections ----
  nav_menu(
    title = "Extras",
    icon = icon("ellipsis-h"),
    
    nav_panel(title = "Explorador de Encuesta", icon = icon("search"), value = "explorer", uiOutput("explorer_ui_container")),
    nav_panel(title = "Mapa del Dashboard", icon = icon("sitemap"), value = "dashboard_map", uiOutput("dashboard_map_ui_container")),
    nav_panel(title = "Metodología", icon = icon("download"), value = "methodology", uiOutput("methodology_ui_container")),
    nav_panel(title = "Acerca de", icon = icon("info-circle"), value = "about", uiOutput("about_ui_container"))
  )
)

server <- function(input, output, session) {

  current_theme <- reactive({
    current_tab <- input$navbar
    
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
      section <- NULL
    }
    
    get_section_theme(section)
  })
  
  # Use reactiveVal with default value of "2024"
  selectedYearVal <- reactiveVal("2024")
  
  # Create a reactive expression that reads from the reactiveVal
  selectedYear <- reactive({
    selectedYearVal()
  })
  
  # Initialize the enhanced data manager
  data_manager <- EnhancedDataManager$new()
  
  # Create reactive functions for survey data using the data manager
  perSurveyData <- reactive({
    req(selectedYear())
    survey_id <- paste0("PER_", selectedYear())
    data_manager$get_survey_data(survey_id)
  })

  parSurveyData <- reactive({
    req(selectedYear())
    survey_id <- paste0("PAR_", selectedYear())
    data_manager$get_survey_data(survey_id)
  })
  
  # Geographic data reactive
  geoData <- reactive({
    data_manager$get_geo_data()
  })
  
  # Store reactive functions in session$userData for module access
  session$userData$perSurveyData <- perSurveyData
  session$userData$parSurveyData <- parSurveyData
  session$userData$geoData <- geoData
  session$userData$selectedYear <- selectedYear
  session$userData$data_manager <- data_manager
  
  # Preload data when the app starts
  observe({
    data_manager$preload_data(
      years = c(2023, 2024),
      sections = c("wellness", "economic", "cultural", "identity", "environment",
                  "urban", "mobility", "transportation", "government", "inequality",
                  "accountability", "representation", "expectations", "trust",
                  "infrastructure", "public_services", "education", "healthcare",
                  "housing", "participation", "civic", "community"),
      survey_types = c("PER", "PAR")
    )
  }, priority = -1)
  
  # Clear cache when year changes
 
  
  # UI Container outputs for all sections
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
    load_module("extras", "dashboard_map")
    dashboardMapUI('dashboard_map')
  })
    
  # Update the reactiveVal when user selects a year
  observeEvent(input$surveyYear, {
    if (!is.null(input$surveyYear)) {
      selectedYearVal(input$surveyYear)
      session$sendCustomMessage("updateYearDropdown", input$surveyYear)
    }
  }, ignoreInit = FALSE)
  
  # Set initial dropdown text on load
  observe({
    session$sendCustomMessage("setCurrentYear", selectedYear())
  })
  
  # Handle navigation between tabs
  observeEvent(input$nav_target, {
    nav_value <- input$nav_target
    updateNavbarPage(session, "navbar", selected = nav_value)
  })
  
  # Initialize servers based on the current tab
  observe({
    req(input$navbar)
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
  
  # Add section classes for styling
  observeEvent(input$navbar, {
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