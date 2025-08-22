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
library(future)
library(promises)
library(later)
library(DBI)
library(RSQLite)
library(jsonlite)
library(httr)
library(RMySQL)
library(pool)
available_cores <- future::availableCores()
docker_cores <- min(available_cores, 4)  # Limit to 4 cores max in Docker

plan(sequential)
message(sprintf("Configured background processing with %f cores", docker_cores))

# Load core modules
source("R/global_theme.R")
source("R/utils.R")
source("R/survey_config.R")
source("R/data_loader.R")
source("R/question_classifier.R")
source("R/themes_metadata.R")
source("R/data_manager.R")
source("R/reports_metadata.R")
source("R/background_preloader.R")
source("R/analytics.R")

# --- Ensure dashboard map server module is loaded ---
source("R/extras/dashboard_map_server.R", local = .GlobalEnv)

# New function specifically for navbar icons
navbar_svg_icon <- function(filename, width = "16", height = "16") {
  svg_path <- file.path("svg", filename)
  
  # Check if file exists
  full_path <- file.path("www", svg_path)
  if (!file.exists(full_path)) {
    warning(paste("Navbar SVG file not found:", full_path))
    return(icon("circle"))  # Fallback to FontAwesome icon
  }
  
  # Return HTML structure that mimics Shiny's icon() function
  tags$i(
    class = "navbar-svg-icon",
    style = paste0("display: inline-block; margin-right: 8px; vertical-align: middle;"),
    tags$img(
      src = svg_path,
      alt = tools::file_path_sans_ext(filename),
      width = width,
      height = height,
      style = "vertical-align: middle; filter: brightness(0) invert(1);" # Makes icon white for dark navbar
    )
  )
}

svg_icon <- function(filename, class = "nav-card-icon", width = "48", height = "48") {
  svg_path <- file.path("svg", filename)
  
  # Check if file exists
  full_path <- file.path("www", svg_path)
  if (!file.exists(full_path)) {
    warning(paste("SVG file not found:", full_path))
    return(div(class = class, "⚠️"))  # Fallback icon
  }
  
  tags$div(
    class = class,
    tags$img(
      src = svg_path,
      alt = tools::file_path_sans_ext(filename),
      width = width,
      height = height,
      style = "max-width: 100%; height: auto;"
    )
  )
}

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

# ONLY preload the essential visualization modules needed for main page
load_module("R/binary_module.R", "binary")
load_module("R/categorical_module.R", "categorical")
load_module("R/interval_module.R", "interval")
load_module("R/ordinal_module.R", "ordinal")
load_module("R/nominal_module.R", "nominal")
load_module("R/razon_module.R", "razon")
load_module("R/special_module.R", "special")

ui <- page_navbar(
  # IMPROVED NAVBAR TITLE - Now clickable and better styled
  title = div(
    class = "navbar-title-container",
    div(
      class = "navbar-brand-container",
      # Brand link
      tags$a(
        href = "#",
        class = "navbar-brand",
        onclick = "Shiny.setInputValue('nav_target', 'overview', {priority: 'event'}); return false;",
        style = "text-decoration: none; color: white !important; font-family: var(--font-display); font-weight: 600; ",
        "Así Estamos Juárez"
      )
    ),
    div(
      class = "year-selector",
      # Add explicit styling and structure for better dropdown behavior
      div(
        class = "dropdown",
        style = "position: relative; z-index: 1050;", # Ensure high z-index
        tags$button(
          class = "btn dropdown-toggle year-dropdown-btn",
          type = "button",
          id = "yearDropdown",
          `data-bs-toggle` = "dropdown",
          `aria-expanded` = "false",
          style = "position: relative; z-index: 1050;", # Ensure button is positioned correctly
          "2024"
        ),
        tags$ul(
          class = "dropdown-menu",
          `aria-labelledby` = "yearDropdown",
          style = "position: absolute !important; top: 100% !important; left: 0 !important; z-index: 1051 !important; min-width: 90px !important;", # Force positioning
          tags$li(
            tags$a(
              class = "dropdown-item", 
              href = "#", 
              onclick = "Shiny.setInputValue('surveyYear', '2023'); $('#yearDropdown').dropdown('hide'); return false;", # Auto-hide after selection
              "2023"
            )
          ),
          tags$li(
            tags$a(
              class = "dropdown-item", 
              href = "#", 
              onclick = "Shiny.setInputValue('surveyYear', '2024'); $('#yearDropdown').dropdown('hide'); return false;", # Auto-hide after selection
              "2024"
            )
          )
        )
      )
    ),
    # Hamburger on the right
    tags$button(
      class = "custom-navbar-toggle d-lg-none navbar-toggle-right",
      `aria-label` = "Toggle navigation menu",
      onclick = "$('.navbar-collapse').collapse('toggle');",
      icon("bars")
    )
  ),
  id = "navbar",
  
  header = tags$head(
      tags$title("Así Estamos Juárez - Dashboard Ciudadano"),
       tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      // Set the title and prevent it from being changed
      document.title = 'Así Estamos Juárez - Dashboard Ciudadano';
      
      // Override any attempts to change the title
      var originalTitle = document.title;
      Object.defineProperty(document, 'title', {
        get: function() {
          return originalTitle;
        },
        set: function(newTitle) {
          // Only allow our specific title
          if (newTitle === 'Así Estamos Juárez - Dashboard Ciudadano') {
            originalTitle = newTitle;
          }
          // Ignore other title changes
        }
      });
      
      console.log('Page title locked to:', originalTitle);
    });
  ")),
  tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"),
    tags$link(rel = "stylesheet", href = "dashboard-map.css"),
    tags$link(rel = "stylesheet", href = "https://use.typekit.net/iee7bqm.css"),
    
    # DataTables library for methodology modals
    tags$link(rel = "stylesheet", href = "https://cdn.datatables.net/1.13.7/css/dataTables.bootstrap5.min.css"),
    tags$link(rel = "stylesheet", href = "https://cdn.datatables.net/buttons/2.4.2/css/buttons.bootstrap5.min.css"),
    tags$script(src = "https://cdn.datatables.net/1.13.7/js/jquery.dataTables.min.js"),
    tags$script(src = "https://cdn.datatables.net/1.13.7/js/dataTables.bootstrap5.min.js"),
    tags$script(src = "https://cdn.datatables.net/buttons/2.4.2/js/dataTables.buttons.min.js"),
    tags$script(src = "https://cdn.datatables.net/buttons/2.4.2/js/buttons.bootstrap5.min.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.10.1/jszip.min.js"),
    tags$script(src = "https://cdn.datatables.net/buttons/2.4.2/js/buttons.html5.min.js"),
    
    tags$style(HTML("
/* Analytics Modal Enhancements */
.analytics-card {
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  transition: transform 0.2s ease;
}

.analytics-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(0,0,0,0.15);
}

.analytics-insight {
  background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
  border-left: 4px solid #007bff;
  padding: 15px;
  margin: 10px 0;
  border-radius: 6px;
}

.analytics-stat {
  font-size: 1.8rem;
  font-weight: 600;
  color: #007bff;
}

.analytics-label {
  font-size: 0.9rem;
  color: #6c757d;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

/* Banner Hero Section - Fixed for actual banner dimensions */
.hero-banner-section {
  padding: 2rem 1rem 1rem !important;
}

.hero-banner-container {
  max-width: 1200px;
  margin: 0 auto;
}

.hero-banner {
  position: relative;
  width: 100%;
  /* Set height to maintain 1200x200 aspect ratio */
  height: calc(100vw * 200 / 1200);
  max-height: 200px; /* Cap at actual banner height */
  background-image: url('banner-1200.jpg');
  background-size: contain; /* Changed from cover to contain */
  background-position: center;
  background-repeat: no-repeat;
  border-radius: 12px;
  overflow: hidden;
  box-shadow: 0 4px 20px rgba(0,0,0,0.1);
  transition: all 0.3s ease;
  cursor: pointer;
  /* Ensure minimum height */
  min-height: 120px;
}

/* Card-like hover effect */
.clickable-banner:hover {
  transform: translateY(-4px);
  box-shadow: 0 8px 30px rgba(0,0,0,0.15);
}

/* For containers smaller than 1200px, scale proportionally */
@media (max-width: 1200px) {
  .hero-banner {
    /* Maintain 6:1 aspect ratio (1200:200) */
    height: calc(100vw * 200 / 1200 * 0.9); /* Slightly smaller for margins */
    max-height: 180px;
  }
}

/* Switch to mobile banner for tablets and phones */
@media (max-width: 768px) {
  .hero-banner {
    background-image: url('banner-600.jpg');
    /* Maintain 5:1 aspect ratio (600:120) */
    height: calc(100vw * 120 / 600 * 0.9);
    max-height: 120px;
    min-height: 80px;
  }
  
  .hero-banner-section {
    padding: 1.5rem 1rem 1rem !important;
  }
}

/* Very small phones */
@media (max-width: 480px) {
  .hero-banner {
    /* Keep 5:1 ratio but smaller */
    height: calc(100vw * 120 / 600 * 0.85);
    max-height: 100px;
    min-height: 70px;
    border-radius: 8px;
  }
  
  .hero-banner-section {
    padding: 1rem 0.5rem 1rem !important;
  }
}

/* Tablet portrait - use mobile banner */
@media (orientation: portrait) and (max-width: 900px) {
  .hero-banner {
    background-image: url('banner-600.jpg');
  }
}

/* Large screens - ensure banner doesn't get too big */
@media (min-width: 1400px) {
  .hero-banner {
    height: 200px; /* Fixed height for very large screens */
  }
}

/* Accessibility: Focus states */
.clickable-banner:focus {
  outline: 3px solid #0d6efd;
  outline-offset: 2px;
}

/* Keyboard navigation */
.clickable-banner[tabindex]:focus {
  transform: translateY(-2px);
  box-shadow: 0 6px 25px rgba(13, 110, 253, 0.3);
}
    ")),
    
    # ENHANCED TRANSITION JAVASCRIPT HANDLERS
# ENHANCED TRANSITION JAVASCRIPT HANDLERS
# ENHANCED TRANSITION JAVASCRIPT HANDLERS - Less Aggressive Approach
    tags$script(HTML("
$(document).ready(function() {
  
  console.log('🔧 Starting refined dropdown prevention system...');
  
  // ===== CONFIGURATION =====
  
  const DROPDOWN_CONFIG = {
    // Selectors for elements that should not trigger dropdowns
    NAV_CARD_SELECTORS: '.page-card, .section-nav-card, [class*=\"nav-card\"], .highlight-tag',
    
    // Selectors for legitimate dropdowns we want to preserve
    ALLOWED_DROPDOWN_SELECTORS: '.year-selector, .year-selector *',
    
    // Timing for checks (less frequent than before)
    CHECK_DELAY_MS: 500,
    RECHECK_DELAY_MS: 1500,
    PERIODIC_CHECK_INTERVAL_MS: 3000, // Reduced from 1000ms
    
    // Debug mode
    DEBUG: true
  };
  
  // ===== UTILITY FUNCTIONS =====
  
  function log(message, data = null) {
    if (DROPDOWN_CONFIG.DEBUG) {
      if (data) {
        console.log('🔧 ' + message, data);
      } else {
        console.log('🔧 ' + message);
      }
    }
  }
  
  function isAllowedDropdown($element) {
    return $element.closest(DROPDOWN_CONFIG.ALLOWED_DROPDOWN_SELECTORS).length > 0;
  }
  
  function isNavCardContext($element) {
    return $element.closest(DROPDOWN_CONFIG.NAV_CARD_SELECTORS).length > 0;
  }
  
  // ===== TARGETED DROPDOWN DETECTION =====
  
  function getProblematicDropdowns() {
    const problematic = [];
    
    // Find dropdowns that are open and not in allowed contexts
    $('.dropdown-menu.show').each(function() {
      const $dropdown = $(this);
      if (!isAllowedDropdown($dropdown)) {
        problematic.push({
          element: this,
          $element: $dropdown,
          context: 'dropdown-menu',
          parent_classes: $dropdown.parent().attr('class') || 'none'
        });
      }
    });
    
    // Find nav dropdowns that shouldn't be open
    $('.nav-item.dropdown').each(function() {
      const $navItem = $(this);
      const isOpen = $navItem.hasClass('show') || 
                    $navItem.find('[aria-expanded=\"true\"]').length > 0;
      
      if (isOpen && !isAllowedDropdown($navItem)) {
        problematic.push({
          element: this,
          $element: $navItem,
          context: 'nav-item',
          parent_classes: $navItem.attr('class') || 'none'
        });
      }
    });
    
    return problematic;
  }
  
  // ===== GENTLE DROPDOWN CLOSING =====
  
  function closeProblematicDropdowns() {
    const problematic = getProblematicDropdowns();
    
    if (problematic.length === 0) {
      return 0;
    }
    
    log(`Found ${problematic.length} problematic dropdowns to close`, problematic.map(p => p.context));
    
    let closed = 0;
    
    problematic.forEach(function(item) {
      const $element = item.$element;
      
      try {
        // Method 1: Use Bootstrap's hide method if available
        if (typeof $element.dropdown === 'function') {
          $element.dropdown('hide');
          closed++;
          log(`Closed dropdown via Bootstrap hide(): ${item.context}`);
          return;
        }
        
        // Method 2: Remove Bootstrap classes
        if (item.context === 'dropdown-menu') {
          $element.removeClass('show');
          $element.parent().removeClass('show');
          closed++;
        } else if (item.context === 'nav-item') {
          $element.removeClass('show');
          $element.find('[aria-expanded]').attr('aria-expanded', 'false');
          $element.find('.dropdown-menu').removeClass('show');
          closed++;
        }
        
        log(`Closed dropdown via class removal: ${item.context}`);
        
      } catch (error) {
        log(`Error closing dropdown: ${error.message}`);
      }
    });
    
    if (closed > 0) {
      log(`Successfully closed ${closed} problematic dropdowns`);
    }
    
    return closed;
  }
  
  
  // ===== NAV CARD CLICK HANDLING =====
  
  $(document).on('click', DROPDOWN_CONFIG.NAV_CARD_SELECTORS, function(e) {
    const $clicked = $(this);
    
    // Skip if this is within an allowed dropdown context
    if (isAllowedDropdown($clicked)) {
      return;
    }
    
    log('Nav card clicked', {
      element: this.tagName,
      classes: $clicked.attr('class'),
      text: $clicked.text().substring(0, 30) + '...'
    });
    
    // Immediate visual feedback
    $clicked.addClass('clicking');
    setTimeout(() => $clicked.removeClass('clicking'), 200);
    
    // Single cleanup check after a reasonable delay
    setTimeout(function() {
      const closed = closeProblematicDropdowns();
      if (closed > 0) {
        log(`Nav card click cleanup: closed ${closed} dropdowns`);
      }
    }, DROPDOWN_CONFIG.CHECK_DELAY_MS);
  });
  
  // ===== YEAR SELECTOR PROTECTION =====
  
  // Protect year selector from interference
  $(document).on('click', DROPDOWN_CONFIG.ALLOWED_DROPDOWN_SELECTORS, function(e) {
    log('Allowing dropdown in year selector');
    e.stopPropagation(); // Prevent nav card handlers from interfering
  });
  
  // Handle year selection
  $('.year-selector .dropdown-item').on('click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    const year = $(this).text().trim();
    const $button = $('#yearDropdown');
    
    log(`Year selected: ${year}`);
    
    // Update button text with loading indicator
    $button.addClass('updating').text(year + ' ⟳');
    
    // Send to Shiny
    Shiny.setInputValue('surveyYear', year, {priority: 'event'});
    
    // Hide dropdown
    setTimeout(function() {
      $button.dropdown('hide');
      $button.removeClass('updating').text(year);
    }, 100);
    
    // Update active state
    $('.year-selector .dropdown-item').removeClass('active');
    $(this).addClass('active');
    
    return false;
  });
  
  // ===== REDUCED MONITORING =====
  
  // Simplified periodic check (less frequent)
  let checkCount = 0;
  setInterval(function() {
    checkCount++;
    const problematic = getProblematicDropdowns();
    
    if (problematic.length > 0) {
      log(`Periodic check #${checkCount}: Found ${problematic.length} issues`);
      closeProblematicDropdowns();
    }
    
    // Log stats every 10 checks (30 seconds)
    if (checkCount % 10 === 0) {
      log(`System status check #${checkCount}: All clear`);
    }
    
  }, DROPDOWN_CONFIG.PERIODIC_CHECK_INTERVAL_MS);
  
  // ===== ESCAPE KEY HANDLER =====
  
  $(document).on('keydown', function(e) {
    if (e.key === 'Escape') {
      log('Escape key pressed - closing problematic dropdowns');
      closeProblematicDropdowns();
    }
  });
  
  // ===== WINDOW FOCUS HANDLER =====
  
  $(window).on('focus', function() {
    // Clean up any stuck dropdowns when window regains focus
    setTimeout(function() {
      const closed = closeProblematicDropdowns();
      if (closed > 0) {
        log(`Window focus cleanup: closed ${closed} dropdowns`);
      }
    }, 300);
  });
  
  // ===== INITIALIZATION =====
  
  // Initial cleanup
  setTimeout(function() {
    log('Initial system cleanup...');
    closeProblematicDropdowns();
    log('Refined dropdown prevention system ready ✅');
  }, 1000);
  
  // Log configuration
  log('Configuration loaded', DROPDOWN_CONFIG);
});
")),

    # BASIC STYLING FOR VISUAL FEEDBACK
    tags$style(HTML("
/* Visual feedback for nav card clicks */
.clicking {
  transform: scale(0.98) !important;
  opacity: 0.8 !important;
  transition: all 0.15s ease !important;
}

/* Ensure year selector dropdown positioning */
.year-selector .dropdown-menu {
  position: absolute !important;
  top: 100% !important;
  left: 0 !important;
  z-index: 1051 !important;
  background-color: var(--navbar-bg) !important;
  min-width: 90px !important;
  border: 1px solid rgba(255,255,255,0.1) !important;
}

.year-selector .dropdown-item {
  color: rgba(255,255,255,0.8) !important;
  transition: all 0.2s ease !important;
}

.year-selector .dropdown-item:hover {
  background-color: rgba(255,255,255,0.1) !important;
  color: white !important;
}

.year-selector .dropdown-item.active {
  background-color: rgba(255,255,255,0.2) !important;
  color: white !important;
  font-weight: 500 !important;
}

/* Update indicator for year button */
.year-dropdown-btn.updating {
  opacity: 0.7 !important;
  transition: opacity 0.3s ease !important;
}

/* Prevent unwanted hover effects during transitions */
.nav-card-icon {
  pointer-events: none !important;
}

/* Ensure nav cards don't interfere with legitimate dropdowns */
.page-card, .section-nav-card, [class*='nav-card'] {
  position: relative !important;
}

.page-card *, .section-nav-card *, [class*='nav-card'] * {
  pointer-events: none !important;
}

.page-card, .section-nav-card, [class*='nav-card'] {
  pointer-events: auto !important;
}

/* Make highlight tags behave properly */
.highlight-tag {
  pointer-events: auto !important;
  cursor: pointer !important;
  position: relative !important;
  z-index: 1 !important;
}

.highlight-tag * {
  pointer-events: none !important;
}
")),

    tags$script(src = "custom.js"),
   
tags$script(HTML("
$(document).ready(function() {
  console.log('📊 Simple analytics access ready');
  
  let keySequence = [];
  let clickSequence = [];
  let clickTimer = null;
  
  // Method 1: Ctrl+Shift+A (most reliable)
  $(document).keydown(function(e) {
    if (e.ctrlKey && e.shiftKey && e.code === 'KeyA') {
      e.preventDefault();
      triggerAnalytics('keyboard');
      return false;
    }
    
    // Method 2: Triple Escape
    if (e.code === 'Escape') {
      keySequence.push('Escape');
      setTimeout(() => {
        keySequence = keySequence.filter(k => k !== 'Escape');
      }, 2000);
      
      if (keySequence.filter(k => k === 'Escape').length >= 3) {
        triggerAnalytics('triple_escape');
        keySequence = [];
      }
    }
  });
  
  // Method 3: Triple click title
  $(document).on('click', '.vista-rapida-title', function(e) {
    clickSequence.push('title');
    resetClickTimer();
    
    if (clickSequence.filter(c => c === 'title').length >= 3) {
      triggerAnalytics('title_click');
      clickSequence = [];
    }
  });
  
  // Method 4: Footer trigger
  $(document).on('click', '[data-analytics-secret]', function(e) {
    clickSequence.push('footer');
    resetClickTimer();
    
    if (clickSequence.filter(c => c === 'footer').length >= 5) {
      triggerAnalytics('footer_click');
      clickSequence = [];
    }
  });
  
  function resetClickTimer() {
    if (clickTimer) clearTimeout(clickTimer);
    clickTimer = setTimeout(() => { clickSequence = []; }, 3000);
  }
  
  function triggerAnalytics(method) {
    console.log('🎯 Analytics triggered:', method);
    
    // Simple toast notification
    const toast = $('<div style=\"position:fixed;top:20px;right:20px;background:#007bff;color:white;padding:10px 15px;border-radius:5px;z-index:9999;font-size:14px;\">📊 Analytics Panel</div>');
    $('body').append(toast);
    setTimeout(() => toast.fadeOut(() => toast.remove()), 2000);
    
    // Trigger Shiny
    Shiny.setInputValue('secret_analytics_trigger', 'show_analytics', {priority: 'event'});
  }
  
  // Console access
  window.showAnalytics = function() {
    triggerAnalytics('console');
  };
  
  // Help command
  window.analyticsHelp = function() {
    console.log('📊 Analytics Access Methods:\\n- Ctrl+Shift+A\\n- Triple-click \"Vista Rápida\"\\n- Escape×3\\n- showAnalytics()');
  };
});
")),
  ),

  init_tooltips(),

  # IMPROVED MAIN OVERVIEW TAB
  nav_panel(
    title = "Inicio",
    icon = icon("home"),
    value = "overview",
    
    # IMPROVED HERO SECTION - Smaller with better button
div(
  class = "hero-banner-section",
  div(
    class = "hero-banner-container",
    div(
      class = "hero-banner clickable-banner",
      onclick = "Shiny.setInputValue('nav_target', 'explorer', {priority: 'event'})",
      role = "button",
      tabindex = "0",
      `aria-label` = "Ir al Panel Principal - Así Vemos Juárez"
    )
  )
),
    
    # Main Content Container
    div(
      class = "insights-section",
      
      # NEW: Vista Rápida Heading
      div(
        class = "vista-rapida-header mb-4",
        div(
          class = "container",
          h2(
            class = "vista-rapida-title",
            "Vista Rápida"
          ),
          p(
            class = "vista-rapida-subtitle",
            "Explora las principales áreas temáticas del Informe"
          )
        )
      ),

      # IMPROVED PAGE-LIKE CARDS WITH CLICKABLE TAGS
      div(
        class = "page-cards-grid",
        
        # Calidad de Vida Card
        div(
          class = "page-card page-card-bienestar",
          onclick = "Shiny.setInputValue('nav_target', 'wellness', {priority: 'event'})",
          
          # Card Header with gradient accent
          div(
            class = "page-header",
            div(
              class = "page-header-content",
              h4(class = "page-card-title", "Calidad de Vida")
            ),
            div(class = "page-icon", svg_icon("Cap--37.svg"))
          ),
          
          # Card Content
          div(
            class = "page-content",
            div(
              p(class = "page-description",
                span(class = "page-question", "¿Cómo se sienten los juarenses sobre su bienestar? ")
              ),
              div(
                class = "highlight-tags",
                # CLICKABLE TAGS - Lead to subsections
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'economic', {priority: 'event'})",
                  "Situación económica"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'cultural', {priority: 'event'})",
                  "Actividades Recreativas"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'identity', {priority: 'event'})",
                  "Identidad Juarense"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'environment', {priority: 'event'})",
                  "Medio Ambiente"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'healthcare', {priority: 'event'})",
                  "Servicios de Salud"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'education', {priority: 'event'})",
                  "Calidad de la Educación"
                )
              )
            )
          ),
          
          # Card Footer
          div(
            class = "page-footer",
            a(class = "page-action", "Ver detalles", bsicons::bs_icon("arrow-right"))
          )
        ),
        
        # Instituciones Card
        div(
          class = "page-card page-card-gobierno",
          onclick = "Shiny.setInputValue('nav_target', 'government', {priority: 'event'})",
          
          # Card Header
          div(
            class = "page-header",
            div(
              class = "page-header-content",
              h4(class = "page-card-title", "Instituciones")
            ),
            div(class = "page-icon", svg_icon("Cap--18.svg"))
          ),
          
          # Card Content
          div(
            class = "page-content",
            div(
              p(class = "page-description",
                span(class = "page-question", "¿Confían los ciudadanos en sus instituciones? ")
              ),
              div(
                class = "highlight-tags",
                # CLICKABLE TAGS
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'trust', {priority: 'event'})",
                  "Confianza Institucional"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'accountability', {priority: 'event'})",
                  "Rendición de Cuentas"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'representation', {priority: 'event'})",
                  "Representación"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'expectations', {priority: 'event'})",
                  "Expectativas"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'inequality', {priority: 'event'})",
                  "Desigualdad"
                )
              )
            )
          ),
          
          # Card Footer
          div(
            class = "page-footer",
            a(class = "page-action", "Ver detalles", bsicons::bs_icon("arrow-right"))
          )
        ),
        
        # Movilidad Card
        div(
          class = "page-card page-card-movilidad",
          onclick = "Shiny.setInputValue('nav_target', 'urban', {priority: 'event'})",
          
          # Card Header
          div(
            class = "page-header",
            div(
              class = "page-header-content",
              h4(class = "page-card-title", "Movilidad Urbana")         ),
            div(class = "page-icon", svg_icon("Cap--17.svg"))
          ),
          
          # Card Content
          div(
            class = "page-content",
            div(
              p(class = "page-description",
                span(class = "page-question", "¿Cómo se mueven los juarenses por su ciudad? ")
              ),
              div(
                class = "highlight-tags",
                # CLICKABLE TAGS
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'transportation', {priority: 'event'})",
                  "Transporte Público"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'mobility', {priority: 'event'})",
                  "Movilidad Peatonal"
                )
              )
            )
          ),
          
          # Card Footer
          div(
            class = "page-footer",
            a(class = "page-action", "Ver detalles", bsicons::bs_icon("arrow-right"))
          )
        ),
        
        # Infraestructura Card
        div(
          class = "page-card page-card-infraestructura",
          onclick = "Shiny.setInputValue('nav_target', 'infrastructure', {priority: 'event'})",
          
          # Card Header
          div(
            class = "page-header",
            div(
              class = "page-header-content",
              h4(class = "page-card-title", "Infraestructura")          ),
            div(class = "page-icon", svg_icon("Cap--38.svg"))
          ),
          
          # Card Content
          div(
            class = "page-content",
            div(
              p(class = "page-description",
                span(class = "page-question", "¿Qué tal funcionan los servicios públicos? ")
              ),
              div(
                class = "highlight-tags",
                # CLICKABLE TAGS
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'public_services', {priority: 'event'})",
                  "Servicios Públicos"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'housing', {priority: 'event'})",
                  "Vivienda"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'equipment', {priority: 'event'})",
                  "Equipamiento Público"
                )
              )
            )
          ),
          
          # Card Footer
          div(
            class = "page-footer",
            a(class = "page-action", "Ver detalles", bsicons::bs_icon("arrow-right"))
          )
        ),
        
        # Participación Card
        div(
          class = "page-card page-card-participacion",
          onclick = "Shiny.setInputValue('nav_target', 'participation', {priority: 'event'})",
          
          # Card Header
          div(
            class = "page-header",
            div(
              class = "page-header-content",
              h4(class = "page-card-title", "Participación Ciudadana")
            ),
            div(class = "page-icon", svg_icon("Cap--43.svg"))
          ),
          
          # Card Content
          div(
            class = "page-content",
            div(
              p(class = "page-description",
                span(class = "page-question", "¿Qué tan activos son los ciudadanos en su comunidad? ")
              ),
              div(
                class = "highlight-tags",
                # CLICKABLE TAGS
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'civic', {priority: 'event'})",
                  "Participación Cívica"
                ),
                span(
                  class = "highlight-tag", 
                  onclick = "event.stopPropagation(); Shiny.setInputValue('nav_target', 'community', {priority: 'event'})",
                  "Participación Comunitaria"
                )
              )
            )
          ),
          
          # Card Footer
          div(
            class = "page-footer",
            a(class = "page-action", "Ver detalles", bsicons::bs_icon("arrow-right"))
          )
        )
      ),
      
      # Footer
      create_dashboard_footer()
    )
  ),
  
  # ---- Bienestar Social y Económico ----
  nav_menu(
    title = "Calidad de Vida",
    icon = navbar_svg_icon("Cap--24.svg"),
    
    nav_panel(title = "Vista General", value = "wellness", uiOutput("wellness_ui_container")),
    nav_panel(title = "Condiciones Económicas", value = "economic", uiOutput("economic_ui_container")),
    nav_panel(title = "Participación Cultural", value = "cultural", uiOutput("cultural_ui_container")),
    nav_panel(title = "Identidad y pertenencia", value = "identity", uiOutput("identity_ui_container")),
    nav_panel(title = "Medio Ambiente", value = "environment", uiOutput("environment_ui_container")),
    nav_panel(title = "Servicios de Salud", value = "healthcare", uiOutput("healthcare_ui_container")),
    nav_panel(title = "Vivienda", value = "housing", uiOutput("housing_ui_container"))
  ),
  
  # ---- Movilidad Urbana y Medio Ambiente ----
  nav_menu(
    title = "Movilidad Urbana",
    icon = navbar_svg_icon("Cap--31.svg"),
    
    nav_panel(title = "Vista General", value = "urban", uiOutput("urban_ui_container")),
    nav_panel(title = "Movilidad", value = "mobility", uiOutput("mobility_ui_container")),
    nav_panel(title = "Transporte Público", value = "transportation", uiOutput("transportation_ui_container"))
  ),
  
  # ---- Gobierno ----
  nav_menu(
    title = "Instituciones",
    icon = navbar_svg_icon("Cap--32.svg"),
    
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
    icon = navbar_svg_icon("Cap--28.svg"),
    
    nav_panel(title = "Vista General", value = "infrastructure", uiOutput("infrastructure_ui_container")),
    nav_panel(title = "Servicios Públicos", value = "public_services", uiOutput("public_services_ui_container")),
    nav_panel(title = "Equipamiento Público", value = "equipment", uiOutput("equipment_ui_container")),
    nav_panel(title = "Educación", value = "education", uiOutput("education_ui_container"))
  ),
  
  # ---- Participación Ciudadana ----
  nav_menu(
    title = "Participación",
    icon = navbar_svg_icon("Cap--36.svg"),
    
    nav_panel(title = "Vista General", value = "participation", uiOutput("participation_ui_container")),
    nav_panel(title = "Participación Cívica", value = "civic", uiOutput("civic_ui_container")),
    nav_panel(title = "Participación Comunitaria", value = "community", uiOutput("community_ui_container"))
  ),
  
  # ---- Other Sections ----
  nav_menu(
    title = "Extras",
    icon = icon("ellipsis-h"),
    nav_panel(title = "Biblioteca de Informes", icon = icon("book"), value = "reports", uiOutput("reports_ui_container")),
    nav_panel(title = "Explorador de Datos", icon = icon("search"), value = "explorer", uiOutput("explorer_ui_container")),
    nav_panel(title = "Mapa del Dashboard", icon = icon("sitemap"), value = "dashboard_map", uiOutput("dashboard_map_ui_container")),
    nav_panel(title = "Metodología", icon = icon("download"), value = "methodology", uiOutput("methodology_ui_container")),
    nav_panel(title = "Acerca de", icon = icon("info-circle"), value = "about", uiOutput("about_ui_container"))
  )
)

server <- function(input, output, session) {
  # Early validation check
  if (!exists("DataManager", envir = .GlobalEnv)) {
    warning("DataManager not found in global environment")
  }
  
  if (!exists("BackgroundPreloader", envir = .GlobalEnv)) {
    warning("BackgroundPreloader not found in global environment")
  }
  
  # CRITICAL: Initialize background_preloader variable
  background_preloader <- NULL
# ===== USE GLOBAL ANALYTICS INSTANCE =====
  
  # Get or create single analytics instance
  if (!exists("GLOBAL_ANALYTICS_MANAGER", envir = .GlobalEnv)) {
    message("🔧 Creating single global analytics manager...")
    assign("GLOBAL_ANALYTICS_MANAGER", tryCatch({
      if (!dir.exists("data")) {
        dir.create("data", recursive = TRUE)
      }
      
      analytics_manager <- AnalyticsManager$new()
      if (analytics_manager$is_enabled()) {
        message("✅ Global analytics manager initialized successfully")
      } else {
        message("⚠️ Global analytics manager initialized but disabled")
      }
      analytics_manager
    }, error = function(e) {
      warning(paste("Failed to initialize global analytics:", e$message))
      NULL
    }), envir = .GlobalEnv)
  }
  
  # Use the global instance
  analytics <- get("GLOBAL_ANALYTICS_MANAGER", envir = .GlobalEnv)
  
  # Analytics health monitoring
  analytics_health_check <- reactiveTimer(300000)  # Check every 5 minutes
  
  observe({
    analytics_health_check()
    
    if (!is.null(analytics)) {
      health_status <- analytics$health_check()
      
      # Log health status periodically
      if (health_status$status == "healthy") {
        message(sprintf("📊 Analytics health: %s (Sessions: %s)", 
                       health_status$message, health_status$session_count))
      } else {
        message(sprintf("⚠️ Analytics health: %s - %s", 
                       health_status$status, health_status$message))
        
        # Attempt recovery if analytics is disabled
        if (health_status$status == "disabled") {
          message("🔄 Attempting analytics recovery...")
          if (analytics$retry_initialization()) {
            message("✅ Analytics recovered successfully")
          } else {
            message("❌ Analytics recovery failed")
          }
        }
      }
    }
  })
  
  # Get session info safely
  session_id <- session$token
  
  # Initialize analytics within reactive context with enhanced error handling
observe({
  if (!is.null(analytics) && analytics$is_enabled()) {
    message("🔍 Starting analytics for session: ", session_id)
    
    user_ip <- isolate({
      tryCatch({
        hostname <- session$clientData$url_hostname %||% "unknown"
      }, error = function(e) "unknown")
    })

    user_agent <- isolate({
      tryCatch({
        agent <- session$request$HTTP_USER_AGENT %||% "unknown"
      }, error = function(e) "unknown")
    })
    
    success <- analytics$start_session(session_id, user_ip, user_agent)
    if (success) {
      message("✅ Analytics session started successfully")
    }
  }
}) %>% bindEvent(session$clientData$output_clientid_height, once = TRUE)
  
  # Enhanced session end tracking
  session$onSessionEnded(function() {
    tryCatch({
      if (!is.null(analytics) && analytics$is_enabled()) {
        analytics$end_session(session_id)
        message("✅ Analytics session ended successfully")
      }
    }, error = function(e) {
      message("❌ Analytics session end error: ", e$message)
    })
  })
  # ===== FIXED: Use global variables instead of reactives for non-reactive contexts =====
  assign("YEAR_CHANGE_IN_PROGRESS", FALSE, envir = .GlobalEnv)
  assign("LOADED_SERVER_MODULES", character(0), envir = .GlobalEnv)  # NEW: Global tracking
  year_change_in_progress <- reactiveVal(FALSE)

  # --- Always initialize dashboard map module at top level ---
  dashboardMapServer("dashboard_map")

  # Send transition start signal to client
  send_transition_start <- function(session, section, message = NULL) {
    if (is.null(message)) {
      message_map <- list(
        'wellness' = 'Cargando datos de Calidad de Vida...',
        'economic' = 'Cargando información económica...',
        'cultural' = 'Cargando datos culturales...',
        'identity' = 'Cargando datos de identidad...',
        'environment' = 'Cargando datos ambientales...',
        'education' = 'Cargando información educativa...',
        'healthcare' = 'Cargando datos de salud...',
        'housing' = 'Cargando información de vivienda...',
        'urban' = 'Cargando datos de movilidad urbana...',
        'mobility' = 'Cargando información de movilidad...',
        'transportation' = 'Cargando datos de transporte...',
        'government' = 'Cargando datos institucionales...',
        'inequality' = 'Cargando datos de desigualdad...',
        'accountability' = 'Cargando información de rendición de cuentas...',
        'representation' = 'Cargando datos de representación...',
        'expectations' = 'Cargando expectativas gubernamentales...',
        'trust' = 'Cargando datos de confianza institucional...',
        'infrastructure' = 'Cargando datos de infraestructura...',
        'public_services' = 'Cargando información de servicios públicos...',
        'equipment' = 'Cargando datos de equipamiento...',
        'participation' = 'Cargando datos de participación...',
        'civic' = 'Cargando participación cívica...',
        'community' = 'Cargando participación comunitaria...',
        'explorer' = 'Cargando explorador de datos...',
        'reports' = 'Cargando biblioteca de informes...',
        'methodology' = 'Cargando metodología...',
        'about' = 'Cargando información...',
        'dashboard_map' = 'Cargando mapa del dashboard...'
      )
      message <- if (section %in% names(message_map)) message_map[[section]] else 'Cargando contenido...'
    }
    
    session$sendCustomMessage("transition-start", list(
      section = section,
      message = message,
      timestamp = Sys.time()
    ))
    
    message(sprintf("Transition started: %s at %s", section, Sys.time()))
  }
  
  # Send transition complete signal to client
  send_transition_complete <- function(session, section) {
    session$sendCustomMessage("transition-complete", list(
      section = section,
      timestamp = Sys.time()
    ))
    
    message(sprintf("Transition completed: %s at %s", section, Sys.time()))
  }
  
  # Send preloader status updates
  send_preloader_status <- function(session, status) {
    session$sendCustomMessage("preloader-status", status)
  }
  
  # Existing theme reactive
  current_theme <- reactive({
    current_tab <- input$navbar
    
    if (grepl("^wellness|^economic|^cultural|^identity|^environment|^healthcare|^housing", current_tab)) {
      section <- "bienestar"
    } else if (grepl("^urban|^mobility|^transportation", current_tab)) {
      section <- "movilidad"
    } else if (grepl("^government|^inequality|^accountability|^representation|^expectations|^trust", current_tab)) {
      section <- "gobierno"
    } else if (grepl("^infrastructure|^public_services|^equipment|^education", current_tab)) {
      section <- "infraestructura"
    } else if (grepl("^participation|^civic|^community", current_tab)) {
      section <- "participacion"
    } else {
      section <- NULL
    }
    
    get_section_theme(section)
  })
  
  # Existing year management
  selectedYearVal <- reactiveVal("2024")
  selectedYear <- reactive({
    selectedYearVal()
  })
  
  data_manager <- DataManager$new(max_cache_mb = 500)
  
  # CHANGED: Keep reactive version for UI, but use global for non-reactive contexts
  loaded_server_modules <- reactiveVal(character(0))
  
  preload_progress <- reactiveVal(list(
    status = "not_started",
    completed_tasks = 0,
    total_tasks = 0,
    current_task = "Waiting to start",
    estimated_completion = NULL,
    completed_data = 0,
    completed_ui = 0,
    completed_server = 0,
    data_tasks = 0,
    ui_tasks = 0,
    server_tasks = 0
  ))

  # FIXED: Background preloader initialization
  observe({
    # Start preloading after app initialization
    invalidateLater(2000, session)
    
    # Check if background_preloader is NULL (now properly initialized)
    if (is.null(background_preloader)) {
      tryCatch({
        # CRITICAL: Pass session reference for safe UI updates
        background_preloader <<- BackgroundPreloader$new(
          data_manager = data_manager,
          max_concurrent = 1,
          session = session  # Pass session for safe updates
        )
        
        # Start preloading (this no longer uses reactive contexts)
        background_preloader$start_preloading()
        message("Background preloading initiated successfully")
        
      }, error = function(e) {
        warning(sprintf("Failed to initialize background preloader: %s", e$message))
        background_preloader <<- NULL
      })
    }
  })
  
  # SIMPLIFIED: Progress monitoring
  observe({
    # Only update UI progress periodically and safely
    invalidateLater(5000)  # Update every 5 seconds
    
    if (exists("PRELOADER_PROGRESS", envir = .GlobalEnv)) {
      # Get progress from global environment (not reactive)
      global_progress <- get("PRELOADER_PROGRESS", envir = .GlobalEnv)
      
      # Update local reactive only if different
      current_progress <- preload_progress()
      if (current_progress$completed_tasks != global_progress$completed_tasks) {
        preload_progress(global_progress)
      }
    }
  })
  
  # ===== HELPER FUNCTIONS FOR MODULE AND DATA MANAGEMENT =====

# ===== ENHANCED HELPER FUNCTIONS FOR MODULE AND DATA MANAGEMENT =====
  
  # Enhanced modules_ready function with better detection
  modules_ready <- function(section) {
    ui_name <- switch(section,
      "economic" = "economyUI",
      "public_services" = "publicServicesUI",
      "dashboard_map" = "dashboardMapUI",
            "explorer" = "explorerUI",  # Add explicit explorer support

      paste0(section, "UI")
    )
    
    server_name <- switch(section,
      "economic" = "economyServer", 
      "public_services" = "publicServicesServer",
      "dashboard_map" = "dashboardMapServer",
            "explorer" = "explorerServer",  # Add explicit explorer support

      paste0(section, "Server")
    )
    
    ui_ready <- exists(ui_name, envir = .GlobalEnv)
    server_ready <- exists(server_name, envir = .GlobalEnv)
    
    # Additional check: are they actually functions?
    if (ui_ready) {
      ui_ready <- is.function(get(ui_name, envir = .GlobalEnv))
    }
    if (server_ready) {
      server_ready <- is.function(get(server_name, envir = .GlobalEnv))
    }
    
    return(list(ui = ui_ready, server = server_ready, both = ui_ready && server_ready))
  }
  
  # Enhanced ensure_modules_loaded with better feedback
  ensure_modules_loaded <- function(section, folder, module_name = NULL) {
    if (is.null(module_name)) module_name <- section
    
    readiness <- modules_ready(section)
    
    modules_loaded <- FALSE
    
    if (!readiness$ui) {
      message(sprintf("Loading UI module for %s", section))
      success <- load_ui_module(folder, module_name)
      if (success) modules_loaded <- TRUE
    } else {
      message(sprintf("UI module for %s already preloaded ✓", section))
    }
    
    if (!readiness$server) {
      message(sprintf("Loading server module for %s", section))
      success <- load_server_module(folder, module_name)
      if (success) modules_loaded <- TRUE
    } else {
      message(sprintf("Server module for %s already preloaded ✓", section))
    }
    
    return(modules_loaded)
  }
  
# In your app.R file, update the section_data_ready function:

section_data_ready <- function(section, year) {
  # Special handling for sections that don't use standard data pattern
  if (section %in% c("reports", "methodology", "about", "dashboard_map")) {
    return(TRUE)  # These sections are always "ready" since they don't use cached data
  }
  
  # Special handling for explorer section
  if (section == "explorer") {
    # Check for explorer-specific survey data
    per_key <- paste0("survey_PER_", year)
    par_key <- paste0("survey_PAR_", year)
    
    per_ready <- exists(per_key, envir = .GlobalEnv$GLOBAL_CACHE)
    par_ready <- exists(par_key, envir = .GlobalEnv$GLOBAL_CACHE)
    
    # For explorer, we need at least one survey dataset
    explorer_ready <- per_ready || par_ready
    
    if (explorer_ready) {
      ready_surveys <- c(
        if(per_ready) "PER",
        if(par_ready) "PAR"
      )
      message(sprintf("Explorer %s has cached surveys: %s", year, paste(ready_surveys, collapse = ", ")))
    }
    
    return(explorer_ready)
  }
  
  # Standard data checks for other sections
  plots_key <- paste0("plots_", section, "_", year)
  maps_key <- paste0("maps_", section, "_", year)
  pct_key <- paste0("pct_", section, "_", year)
  
  plots_ready <- exists(plots_key, envir = .GlobalEnv$GLOBAL_CACHE)
  maps_ready <- exists(maps_key, envir = .GlobalEnv$GLOBAL_CACHE)
  pct_ready <- exists(pct_key, envir = .GlobalEnv$GLOBAL_CACHE)
  
  # For most sections, plots are the minimum requirement
  essential_ready <- plots_ready
  
  # Log what we found
  if (plots_ready || maps_ready || pct_ready) {
    ready_items <- c(
      if(plots_ready) "plots",
      if(maps_ready) "maps", 
      if(pct_ready) "percentages"
    )
    message(sprintf("Section %s %s has cached: %s", section, year, paste(ready_items, collapse = ", ")))
  }
  
  return(essential_ready)
}

ensure_section_data <- function(section, year, silent = FALSE) {
  # Special handling for sections that don't use standard data pattern
  if (section %in% c("reports", "methodology", "about", "dashboard_map")) {
    if (!silent) {
      message(sprintf("Section %s doesn't use standard data loading - skipping", section))
    }
    return(FALSE)  # No data loading needed
  }
  
  if (!section_data_ready(section, year)) {
    if (!silent) {
      if (section == "explorer") {
        message(sprintf("Explorer data for %s not ready - will be loaded via observeEvent", year))
      } else {
        message(sprintf("Data for %s %s not ready - loading directly", section, year))
      }
    }
    
    # For explorer, don't try to load here - let the observeEvent handle it
    if (section == "explorer") {
      return(TRUE)  # Indicates data loading will be handled elsewhere
    }
    
    # For other sections, load directly
    data_manager$smart_preload(section, year)
    return(TRUE)  # Indicates data was loaded
  } else {
    if (!silent) {
      message(sprintf("Data for %s %s already preloaded ✓", section, year))
    }
    return(FALSE)  # Indicates data was already ready
  }
}

  # Enhanced load_server_once_with_transition with better state tracking
  load_server_once_with_transition <- function(section_folder, module_name, server_function, section_key = NULL, year = "2024") {
    if (is.null(section_key)) section_key <- module_name
    
    module_key <- paste0(section_folder, "_", module_name)
    
    # FIXED: Use global variable instead of reactive to prevent context errors
    if (exists("YEAR_CHANGE_IN_PROGRESS", envir = .GlobalEnv) && 
        get("YEAR_CHANGE_IN_PROGRESS", envir = .GlobalEnv)) {
      message(sprintf("Skipping server loading for %s - year change in progress", module_key))
      return(FALSE)
    }
    
    # Track what we're doing
    modules_loaded <- ensure_modules_loaded(section_key, section_folder, module_name)
    data_loaded <- ensure_section_data(section_key, year, silent = TRUE)
    
    # FIXED: Use global variable instead of reactive
    loaded_modules <- if (exists("LOADED_SERVER_MODULES", envir = .GlobalEnv)) {
      get("LOADED_SERVER_MODULES", envir = .GlobalEnv)
    } else {
      character(0)
    }
    
    server_already_initialized <- module_key %in% loaded_modules
    
    if (!server_already_initialized || modules_loaded || data_loaded) {
      tryCatch({
        # Execute server function
        server_function()
        
        # FIXED: Track in global variable
        if (!server_already_initialized) {
          loaded_modules <- c(loaded_modules, module_key)
          assign("LOADED_SERVER_MODULES", loaded_modules, envir = .GlobalEnv)
          
          # Also update reactive for UI consistency
          loaded_server_modules(loaded_modules)
          
          message(sprintf("Server initialized: %s ✓", module_key))
        } else {
          message(sprintf("Server reinitialized: %s ✓", module_key))
        }
        
        return(TRUE)
        
      }, error = function(e) {
        warning(sprintf("Failed to load server module %s: %s", module_key, e$message))
        return(FALSE)
      })
    } else {
      message(sprintf("Server %s already ready - skipping initialization", module_key))
      return(FALSE)
    }
  }
  
  # New function: Check overall section readiness
 # In your app.R file, update the check_section_readiness function:

check_section_readiness <- function(section, year) {
  modules <- modules_ready(section)
  data <- section_data_ready(section, year)
  
  readiness_score <- sum(c(modules$ui, modules$server, data))
  total_possible <- 3
  
  status <- if (readiness_score == total_possible) {
    "fully_ready"
  } else if (readiness_score >= 2) {
    "mostly_ready" 
  } else if (readiness_score >= 1) {
    "partially_ready"
  } else {
    "not_ready"
  }
  
  # Special logging for reports and other special sections
  if (section %in% c("reports", "methodology", "about", "dashboard_map")) {
    message(sprintf("Special section %s readiness: UI=%s, Server=%s (Status: %s)", 
                   section, modules$ui, modules$server, status))
  } else if (section == "explorer") {
    message(sprintf("Explorer readiness: UI=%s, Server=%s, Data=%s (Status: %s)", 
                   modules$ui, modules$server, data, status))
  }
  
  list(
    status = status,
    score = readiness_score,
    total = total_possible,
    details = list(
      ui_ready = modules$ui,
      server_ready = modules$server,
      data_ready = data
    ),
    needs_loading = status != "fully_ready"
  )
}
  
  
geoData <- reactive({
    tryCatch({
      load_geo_data("data/spatial/Distritos_Juarez.shp")
    }, error = function(e) {
      warning(paste("Failed to load geographic data:", e$message))
      return(NULL)
    })
  })
  
  # Store essential reactive functions in session$userData
  session$userData$geoData <- geoData
  session$userData$selectedYear <- selectedYear
  session$userData$data_manager <- data_manager
  
  # ENHANCED: Explorer loading with memory management
  observeEvent(input$navbar, {
    if (input$navbar == "explorer") {
      # Check if explorer modules are already preloaded
      if (exists("explorerUI", envir = .GlobalEnv) && exists("explorerServer", envir = .GlobalEnv)) {
        message("Explorer modules already preloaded - instant activation!")
      } else {
        # Fallback: load modules if not preloaded
        message("Explorer not preloaded - loading now...")
        load_module("R/extras/explorer_ui.R", "explorer")
        load_module("R/extras/explorer_server.R", "explorer")
      }
      
      # FIXED: Survey data loading without reactive context issues
      perSurveyData <- reactive({
        req(selectedYear())
        survey_id <- paste0("PER_", selectedYear())
        
        # First check if background preloader has loaded the data
        cache_key <- paste0("survey_", survey_id)
        
        if (exists(cache_key, envir = .GlobalEnv$GLOBAL_CACHE)) {
          message("Using preloaded PER survey data from background cache")
          return(get(cache_key, envir = .GlobalEnv$GLOBAL_CACHE))
        }
        
        # If not preloaded, trigger priority loading (NON-REACTIVE)
        if (!is.null(background_preloader)) {
          message("PER data not ready - triggering priority load")
          # Use later to avoid reactive context issues
          later::later(function() {
            background_preloader$priority_load("explorer", selectedYear())
          }, delay = 0.1)
        }
        
        # Fallback to regular loading with progress indication
        tryCatch({
          message(sprintf("Loading PER survey data for %s...", selectedYear()))
          showNotification("Loading survey data...", type = "message", duration = 3)
          load_survey_data(survey_id)
        }, error = function(e) {
          showNotification("Failed to load survey data", type = "error")
          warning(paste("Failed to load PER survey data for", selectedYear(), ":", e$message))
          return(NULL)
        })
      })

      parSurveyData <- reactive({
        req(selectedYear())
        survey_id <- paste0("PAR_", selectedYear())
        
        # Check background cache first
        cache_key <- paste0("survey_", survey_id)
        
        if (exists(cache_key, envir = .GlobalEnv$GLOBAL_CACHE)) {
          message("Using preloaded PAR survey data from background cache")
          return(get(cache_key, envir = .GlobalEnv$GLOBAL_CACHE))
        }
        
        # If not preloaded, trigger priority loading (NON-REACTIVE)
        if (!is.null(background_preloader)) {
          message("PAR data not ready - triggering priority load")
          # Use later to avoid reactive context issues
          later::later(function() {
            background_preloader$priority_load("explorer", selectedYear())
          }, delay = 0.1)
        }
        
        # Fallback to regular loading
        tryCatch({
          message(sprintf("Loading PAR survey data for %s...", selectedYear()))
          load_survey_data(survey_id)
        }, error = function(e) {
          warning(paste("Failed to load PAR survey data for", selectedYear(), ":", e$message))
          return(NULL)
        })
      })
      
      # Add to session userData
      session$userData$perSurveyData <- perSurveyData
      session$userData$parSurveyData <- parSurveyData
      
      # Initialize explorer server
      explorerServer("survey_explorer")
      
      message("Explorer initialized with enhanced preload support")
    }
  }, autoDestroy = TRUE)

# ===== ENHANCED YEAR TRACKING =====
  
  observeEvent(input$surveyYear, {
    if (!is.null(input$surveyYear)) {
      new_year <- input$surveyYear
      
      # Enhanced year change tracking
      tryCatch({
        current_section <- isolate({
          navbar_val <- input$navbar
          if (is.null(navbar_val) || length(navbar_val) == 0 || is.na(navbar_val)) "unknown" else navbar_val
        })
        
        if (!is.null(analytics) && analytics$is_enabled()) {
          success <- analytics$track_interaction(
            session_id, 
            "year_change", 
            current_section, 
            paste("Changed to", new_year)
          )
          if (success) {
            message("✅ Year change tracked successfully")
          } else {
            message("⚠️ Year change tracking returned false")
          }
        } else {
          message("⚠️ Analytics not available for year change tracking")
        }
      }, error = function(e) {
        message("❌ Analytics year change tracking error: ", e$message)
      })
      # FIXED: Set both global and reactive flags
      assign("YEAR_CHANGE_IN_PROGRESS", TRUE, envir = .GlobalEnv)
      year_change_in_progress(TRUE)
      
      # Send transition start for year change
      session$sendCustomMessage("year-change-start", list(
        year = new_year,
        message = paste("Actualizando datos para", new_year, "...")
      ))
      
      # Update year immediately
      selectedYearVal(new_year)
      session$sendCustomMessage("updateYearDropdown", new_year)
      
      # Complete transition after data is updated
      later::later(function() {
        # FIXED: Reset both flags
        assign("YEAR_CHANGE_IN_PROGRESS", FALSE, envir = .GlobalEnv)
        year_change_in_progress(FALSE)
        
        session$sendCustomMessage("year-change-complete", list(
          year = new_year
        ))
      }, 1.0)  # Longer delay for year changes
    }
  }, ignoreInit = FALSE)
  
  observe({
    session$sendCustomMessage("setCurrentYear", selectedYear())
  })
  
  # Existing navigation handler (unchanged)
  observeEvent(input$nav_target, {
    nav_value <- input$nav_target
    updateNavbarPage(session, "navbar", selected = nav_value)
  })

  # ===== UI CONTAINER OUTPUTS =====
  
  # BIENESTAR (WELLNESS) SECTION
  output$wellness_ui_container <- renderUI({
    if (exists("wellnessUI", envir = .GlobalEnv)) {
      wellnessUI()
    } else {
      load_ui_module("wellness", "wellness")
      wellnessUI()
    }
  })

  output$economic_ui_container <- renderUI({
    if (exists("economyUI", envir = .GlobalEnv)) {
      economyUI()
    } else {
      load_ui_module("wellness", "economy")
      economyUI()
    }
  })

  output$cultural_ui_container <- renderUI({
    if (exists("culturalUI", envir = .GlobalEnv)) {
      culturalUI()
    } else {
      load_ui_module("wellness", "cultural")
      culturalUI()
    }
  })

  output$identity_ui_container <- renderUI({
    if (exists("identityUI", envir = .GlobalEnv)) {
      identityUI()
    } else {
      load_ui_module("wellness", "identity")
      identityUI()
    }
  })

  output$environment_ui_container <- renderUI({
    if (exists("environmentUI", envir = .GlobalEnv)) {
      environmentUI()
    } else {
      load_ui_module("wellness", "environment")
      environmentUI()
    }
  })

  output$education_ui_container <- renderUI({
    if (exists("educationUI", envir = .GlobalEnv)) {
      educationUI()
    } else {
      load_ui_module("wellness", "education")
      educationUI()
    }
  })

  output$healthcare_ui_container <- renderUI({
    if (exists("healthcareUI", envir = .GlobalEnv)) {
      healthcareUI()
    } else {
      load_ui_module("wellness", "healthcare")
      healthcareUI()
    }
  })

  output$housing_ui_container <- renderUI({
    if (exists("housingUI", envir = .GlobalEnv)) {
      housingUI()
    } else {
      load_ui_module("infrastructure", "housing")
      housingUI()
    }
  })

  # MOVILIDAD (URBAN) SECTION
  output$urban_ui_container <- renderUI({
    if (exists("urbanUI", envir = .GlobalEnv)) {
      urbanUI()
    } else {
      load_ui_module("urban", "urban")
      urbanUI()
    }
  })

  output$mobility_ui_container <- renderUI({
    if (exists("mobilityUI", envir = .GlobalEnv)) {
      mobilityUI()
    } else {
      load_ui_module("urban", "mobility")
      mobilityUI()
    }
  })

  output$transportation_ui_container <- renderUI({
    if (exists("transportationUI", envir = .GlobalEnv)) {
      transportationUI()
    } else {
      load_ui_module("urban", "transportation")
      transportationUI()
    }
  })

  # GOBIERNO (GOVERNMENT) SECTION
  output$government_ui_container <- renderUI({
    if (exists("governmentUI", envir = .GlobalEnv)) {
      governmentUI()
    } else {
      load_ui_module("government", "government")
      governmentUI()
    }
  })

  output$inequality_ui_container <- renderUI({
    if (exists("inequalityUI", envir = .GlobalEnv)) {
      inequalityUI()
    } else {
      load_ui_module("government", "inequality")
      inequalityUI()
    }
  })

  output$accountability_ui_container <- renderUI({
    if (exists("accountabilityUI", envir = .GlobalEnv)) {
      accountabilityUI()
    } else {
      load_ui_module("government", "accountability")
      accountabilityUI()
    }
  })

  output$representation_ui_container <- renderUI({
    if (exists("representationUI", envir = .GlobalEnv)) {
      representationUI()
    } else {
      load_ui_module("government", "representation")
      representationUI()
    }
  })

  output$expectations_ui_container <- renderUI({
    if (exists("expectationsUI", envir = .GlobalEnv)) {
      expectationsUI()
    } else {
      load_ui_module("government", "expectations")
      expectationsUI()
    }
  })

  output$trust_ui_container <- renderUI({
    if (exists("trustUI", envir = .GlobalEnv)) {
      trustUI()
    } else {
      load_ui_module("government", "trust")
      trustUI()
    }
  })

  # INFRAESTRUCTURA (INFRASTRUCTURE) SECTION
  output$infrastructure_ui_container <- renderUI({
    if (exists("infrastructureUI", envir = .GlobalEnv)) {
      infrastructureUI()
    } else {
      load_ui_module("infrastructure", "infrastructure")
      infrastructureUI()
    }
  })

  output$public_services_ui_container <- renderUI({
    if (exists("publicServicesUI", envir = .GlobalEnv)) {
      publicServicesUI()
    } else {
      load_ui_module("infrastructure", "public_services")
      publicServicesUI()
    }
  })

  output$equipment_ui_container <- renderUI({
    if (exists("equipmentUI", envir = .GlobalEnv)) {
      equipmentUI()
    } else {
      load_ui_module("infrastructure", "equipment")
      equipmentUI()
    }
  })

  # PARTICIPACIÓN (PARTICIPATION) SECTION
  output$participation_ui_container <- renderUI({
    if (exists("participationUI", envir = .GlobalEnv)) {
      participationUI()
    } else {
      load_ui_module("participation", "participation")
      participationUI()
    }
  })

  output$civic_ui_container <- renderUI({
    if (exists("civicUI", envir = .GlobalEnv)) {
      civicUI()
    } else {
      load_ui_module("participation", "civic")
      civicUI()
    }
  })

  output$community_ui_container <- renderUI({
    if (exists("communityUI", envir = .GlobalEnv)) {
      communityUI()
    } else {
      load_ui_module("participation", "community")
      communityUI()
    }
  })

  # EXTRAS SECTION
  output$reports_ui_container <- renderUI({
    if (exists("reportsUI", envir = .GlobalEnv)) {
      reportsUI('reports')
    } else {
      load_ui_module("extras", "reports")
      reportsUI('reports')
    }
  })

  output$explorer_ui_container <- renderUI({
    if (exists("explorerUI", envir = .GlobalEnv)) {
      explorerUI('survey_explorer')
    } else {
      message("Explorer UI not preloaded - loading now (this should rarely happen)")
      load_ui_module("extras", "explorer")
      explorerUI('survey_explorer')
    }
  })

  output$methodology_ui_container <- renderUI({
    if (exists("methodologyUI", envir = .GlobalEnv)) {
      methodologyUI()
    } else {
      load_ui_module("extras", "methodology")
      methodologyUI()
    }
  })

  output$about_ui_container <- renderUI({
    if (exists("aboutUI", envir = .GlobalEnv)) {
      aboutUI()
    } else {
      load_ui_module("extras", "about")
      aboutUI()
    }
  })

  output$dashboard_map_ui_container <- renderUI({
    if (exists("dashboardMapUI", envir = .GlobalEnv)) {
      dashboardMapUI('dashboard_map')
    } else {
      load_ui_module("extras", "dashboard_map")
      dashboardMapUI('dashboard_map')
    }
  })
  
   # ===== ENHANCED NAVIGATION TRACKING =====
  
  observe({
    req(input$navbar)
    current_tab <- input$navbar
    current_year <- selectedYear()

    # Enhanced analytics tracking with better error handling
    tryCatch({
      if (!is.null(analytics)) {
        if (analytics$is_enabled()) {
          message("🔍 Tracking navigation: ", current_tab, " (Year: ", current_year, ")")
          success <- analytics$track_navigation(session_id, current_tab, current_year)
          
          if (success) {
            # Track additional insights
            analytics$track_interaction(session_id, "section_visit", current_tab, 
                                       paste("Year:", current_year, "| Time:", format(Sys.time(), "%H:%M")))
            message("✅ Navigation tracked successfully")
          } else {
            message("⚠️ Navigation tracking returned false")
          }
        } else {
          status <- analytics$get_status()
          message("❌ Analytics disabled for navigation tracking - Status: ", status$last_error)
        }
      } else {
        message("❌ Analytics object is NULL - cannot track navigation")
      }
    }, error = function(e) {
      message("❌ Analytics navigation tracking error: ", e$message)
    })

    # Skip transition for overview page
    if (current_tab == "overview") {
      return()
    }
    
    # Force analytics tracking for non-overview pages
    if (!is.null(analytics) && current_tab != "overview") {
      tryCatch({
        message("🔍 Force tracking navigation: ", current_tab, " (Year: ", current_year, ")")
        analytics$track_navigation(session_id, current_tab, current_year)
        message("✅ Force navigation tracked successfully")
      }, error = function(e) {
        message("❌ Force analytics tracking error: ", e$message)
      })
    }
    
    # FIXED: Use global variable to check year change status
    if (exists("YEAR_CHANGE_IN_PROGRESS", envir = .GlobalEnv) && 
        get("YEAR_CHANGE_IN_PROGRESS", envir = .GlobalEnv)) {
      message("Skipping navbar transition - year change in progress")
      return()
    }
    
    # CRITICAL: Capture current year value while we're in reactive context

    # NEW: Check if section is already ready before showing loading
    section_readiness <- modules_ready(current_tab)
    data_readiness <- section_data_ready(current_tab, current_year)
    
    # Only show loading screen if something actually needs to be loaded
    needs_loading <- !section_readiness$both || !data_readiness
    
    if (needs_loading) {
      message(sprintf("Section %s needs loading - UI: %s, Server: %s, Data: %s", 
                     current_tab, section_readiness$ui, section_readiness$server, data_readiness))
      # ONLY show transition start if loading is actually needed
      send_transition_start(session, current_tab)
    } else {
      message(sprintf("Section %s already ready - skipping loading screen", current_tab))
    }
    
    # Add a small delay to ensure smooth transition visual (only if loading needed)
    delay_time <- if (needs_loading) 0.2 else 0.05  # Much shorter delay for cached sections
    
    later::later(function() {
      
      # ===== SECTION NAVIGATION WITH SPECIAL EXPLORER HANDLING =====
      
      if (current_tab == "wellness") {
        load_server_once_with_transition("wellness", "wellness", function() {
          wellnessServer(input, output, session, current_theme)
        }, "wellness", current_year)
        
      } else if (current_tab == "economic") {
        load_server_once_with_transition("wellness", "economy", function() {
          economyServer(input, output, session, current_theme)
        }, "economic", current_year)
        
      } else if (current_tab == "cultural") {
        load_server_once_with_transition("wellness", "cultural", function() {
          culturalServer(input, output, session, current_theme)
        }, "cultural", current_year)
        
      } else if (current_tab == "identity") {
        load_server_once_with_transition("wellness", "identity", function() {
          identityServer(input, output, session, current_theme)
        }, "identity", current_year)
        
      } else if (current_tab == "environment") {
        load_server_once_with_transition("wellness", "environment", function() {
          environmentServer(input, output, session, current_theme)
        }, "environment", current_year)
        
      } else if (current_tab == "education") {
        load_server_once_with_transition("wellness", "education", function() {
          educationServer(input, output, session, current_theme)
        }, "education", current_year)
        
      } else if (current_tab == "healthcare") {
        load_server_once_with_transition("wellness", "healthcare", function() {
          healthcareServer(input, output, session, current_theme)
        }, "healthcare", current_year)
        
      } else if (current_tab == "housing") {
        load_server_once_with_transition("infrastructure", "housing", function() {
          housingServer(input, output, session, current_theme)
        }, "housing", current_year)
      
      # ===== MOVILIDAD (URBAN) SECTIONS =====
      
      } else if (current_tab == "urban") {
        load_server_once_with_transition("urban", "urban", function() {
          urbanServer(input, output, session, current_theme)
        }, "urban", current_year)
        
      } else if (current_tab == "mobility") {
        load_server_once_with_transition("urban", "mobility", function() {
          mobilityServer(input, output, session, current_theme)
        }, "mobility", current_year)
        
      } else if (current_tab == "transportation") {
        load_server_once_with_transition("urban", "transportation", function() {
          transportationServer(input, output, session, current_theme)
        }, "transportation", current_year)
      
      # ===== GOBIERNO (GOVERNMENT) SECTIONS =====
      
      } else if (current_tab == "government") {
        load_server_once_with_transition("government", "government", function() {
          governmentServer(input, output, session, current_theme)
        }, "government", current_year)
        
      } else if (current_tab == "inequality") {
        load_server_once_with_transition("government", "inequality", function() {
          inequalityServer(input, output, session, current_theme)
        }, "inequality", current_year)
        
      } else if (current_tab == "accountability") {
        load_server_once_with_transition("government", "accountability", function() {
          accountabilityServer(input, output, session, current_theme)
        }, "accountability", current_year)
        
      } else if (current_tab == "representation") {
        load_server_once_with_transition("government", "representation", function() {
          representationServer(input, output, session, current_theme)
        }, "representation", current_year)
        
      } else if (current_tab == "expectations") {
        load_server_once_with_transition("government", "expectations", function() {
          expectationsServer(input, output, session, current_theme)
        }, "expectations", current_year)
        
      } else if (current_tab == "trust") {
        load_server_once_with_transition("government", "trust", function() {
          trustServer(input, output, session, current_theme)
        }, "trust", current_year)
      
      # ===== INFRAESTRUCTURA (INFRASTRUCTURE) SECTIONS =====
      
      } else if (current_tab == "infrastructure") {
        load_server_once_with_transition("infrastructure", "infrastructure", function() {
          infrastructureServer(input, output, session, current_theme)
        }, "infrastructure", current_year)
        
      } else if (current_tab == "public_services") {
        load_server_once_with_transition("infrastructure", "public_services", function() {
          publicServicesServer(input, output, session, current_theme)
        }, "public_services", current_year)
        
      } else if (current_tab == "equipment") {
        load_server_once_with_transition("infrastructure", "equipment", function() {
          equipmentServer(input, output, session, current_theme)
        }, "equipment", current_year)
      
      # ===== PARTICIPACIÓN (PARTICIPATION) SECTIONS =====
      
      } else if (current_tab == "participation") {
        load_server_once_with_transition("participation", "participation", function() {
          participationServer(input, output, session, current_theme)
        }, "participation", current_year)
        
      } else if (current_tab == "civic") {
        load_server_once_with_transition("participation", "civic", function() {
          civicServer(input, output, session, current_theme)
        }, "civic", current_year)
        
      } else if (current_tab == "community") {
        load_server_once_with_transition("participation", "community", function() {
          communityServer(input, output, session, current_theme)
        }, "community", current_year)
      
      # ===== EXTRAS SECTIONS =====
      
      } else if (current_tab == "explorer") {
        # Explorer doesn't need special loading - already handled elsewhere
        explorer_readiness <- check_section_readiness("explorer", current_year)
         if (explorer_readiness$needs_loading) {
          message("Explorer needs loading - coordinating with observeEvent")
          # The observeEvent will handle the actual loading
          # We just ensure the modules are ready if needed
          if (!explorer_readiness$details$ui_ready) {
            load_module("R/extras/explorer_ui.R", "explorer")
          }
          if (!explorer_readiness$details$server_ready) {
            load_module("R/extras/explorer_server.R", "explorer")
          }
        } else {
          message("Explorer already fully ready - instant access")
          # Ensure the observeEvent logic still runs but won't trigger loading screens
        }

} else if (current_tab == "reports") {
  message("🚀 REPORTS TAB ACTIVATED - DIRECT APPROACH")
  
  # Bypass the wrapper function entirely - call directly
  tryCatch({
    # Ensure modules are loaded
    if (!exists("reportsUI", envir = .GlobalEnv)) {
      load_module("R/extras/reports_ui.R", "reports")
    }
    if (!exists("reportsServer", envir = .GlobalEnv)) {
      load_module("R/extras/reports_server.R", "reports")
    }
    
    # Call directly without wrapper
    message("🔧 CALLING reportsServer DIRECTLY")
    reportsServer(input, output, session, current_theme)
    message("✅ Reports server called successfully")
    
  }, error = function(e) {
    message("❌ Direct call error: ", e$message)
    print(traceback())
  })
} else if (current_tab == "methodology") {
  message("🚀 METHODOLOGY TAB ACTIVATED - DIRECT APPROACH")
  
  # Capture session in current context to ensure it's available
  current_session <- session
  
  # Debug: Check if session is valid
  message(sprintf("🔍 Session object type: %s", class(current_session)[1]))
  message(sprintf("🔍 Session has sendModal method: %s", "sendModal" %in% names(current_session)))
  
  # Bypass the wrapper function entirely - call directly
  tryCatch({
    # Ensure modules are loaded
    if (!exists("methodologyUI", envir = .GlobalEnv)) {
      load_module("R/extras/methodology_ui.R", "methodology")
    }
    if (!exists("methodologyServer", envir = .GlobalEnv)) {
      load_module("R/extras/methodology_server.R", "methodology")
    }
    
    # Call directly without wrapper - FIXED: Use captured session
    message("🔧 CALLING methodologyServer DIRECTLY")
    methodologyServer(input, output, current_session, current_theme)
    message("✅ Methodology server called successfully")
    
  }, error = function(e) {
    message("❌ Methodology direct call error: ", e$message)
    print(traceback())
  })

} else if (current_tab == "about") {
  message("🚀 ABOUT TAB ACTIVATED - DIRECT APPROACH")
  
  # Bypass the wrapper function entirely - call directly
  tryCatch({
    # Ensure modules are loaded
    if (!exists("aboutUI", envir = .GlobalEnv)) {
      load_module("R/extras/about_ui.R", "about")
    }
    if (!exists("aboutServer", envir = .GlobalEnv)) {
      load_module("R/extras/about_server.R", "about")
    }
    
    # Call directly without wrapper
    message("🔧 CALLING aboutServer DIRECTLY")
    aboutServer(input, output, session, current_theme)
    message("✅ About server called successfully")
    
  }, error = function(e) {
    message("❌ About direct call error: ", e$message)
    print(traceback())
  })

} else if (current_tab == "dashboard_map") {
  message("🚀 DASHBOARD_MAP TAB ACTIVATED - DIRECT APPROACH")
  
  # Bypass the wrapper function entirely - call directly
  tryCatch({
    # Ensure modules are loaded
    if (!exists("dashboardMapUI", envir = .GlobalEnv)) {
      load_module("R/extras/dashboard_map_ui.R", "dashboard_map")
    }
    if (!exists("dashboardMapServer", envir = .GlobalEnv)) {
      load_module("R/extras/dashboard_map_server.R", "dashboard_map")
    }
    
    # Call directly without wrapper
    message("🔧 CALLING dashboardMapServer DIRECTLY")
    dashboardMapServer("dashboard_map")
    message("✅ Dashboard map server called successfully")
    
  }, error = function(e) {
    message("❌ Dashboard map direct call error: ", e$message)
    print(traceback())
  })
}
      
      # ONLY send transition complete if we actually showed a loading screen
      if (needs_loading) {
        later::later(function() {
          send_transition_complete(session, current_tab)
        }, 0.8)  # Increased delay to ensure everything loads
      }
      
}, delay_time)  # Shorter delay for cached sections
    
    # ===== SIMPLIFIED MONITORING =====
    

    if (current_tab != "overview") {
      tryCatch({
        cache_stats <- if(exists("get_cache_stats", envir = data_manager) && 
                         is.function(data_manager[["get_cache_stats"]])) {
          data_manager$get_cache_stats()
        } else {
          list(cache_size_mb = 0, hit_rate = 0, cache_objects = 0)
        }
        
        # Enhanced logging with readiness info
        ready_status <- if(needs_loading) "Loading" else "Cached"
        message(sprintf(
          "Tab: %s | Cache: %.1f MB (%.1f%% hit, %d objects) | Status: %s", 
          current_tab,
          cache_stats$cache_size_mb, 
          cache_stats$hit_rate, 
          cache_stats$cache_objects,
          ready_status
        ))
      }, error = function(e) {
        message(sprintf("Tab: %s | Cache stats unavailable: %s", current_tab, e$message))
      })
    }
  })

  
  # ===== TAB CHANGE TRANSITIONS =====

  # Add transition support for sub-navigation tabs
  observeEvent(input$environment_tabs, {
    if (!is.null(input$environment_tabs)) {
      session$sendCustomMessage("tab-transition", list(
        tab = input$environment_tabs,
        section = "environment"
      ))
    }
  })

  # Add similar observers for other tab-based sections
  observeEvent(input$transportation_tabs, {
    if (!is.null(input$transportation_tabs)) {
      session$sendCustomMessage("tab-transition", list(
        tab = input$transportation_tabs,
        section = "transportation"
      ))
    }
  })
  
  # ===== SECTION CLASS MANAGEMENT =====
  
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
    if (grepl("^wellness|^economic|^cultural|^identity|^environment|^healthcare|^education", current_tab)) {
      shinyjs::addClass(selector = "body", class = "section-bienestar")
    } else if (grepl("^urban|^mobility|^transportation", current_tab)) {
      shinyjs::addClass(selector = "body", class = "section-movilidad")
    } else if (grepl("^government|^inequality|^accountability|^representation|^expectations|^trust", current_tab)) {
      shinyjs::addClass(selector = "body", class = "section-gobierno")
    } else if (grepl("^infrastructure|^public_services|^equipment|^housing", current_tab)) {
      shinyjs::addClass(selector = "body", class = "section-infraestructura")
    } else if (grepl("^participation|^civic|^community", current_tab)) {
      shinyjs::addClass(selector = "body", class = "section-participacion")
    } else if (grepl("^methodology|^about|^reports|^explorer|^dashboard_map", current_tab)) {
      shinyjs::addClass(selector = "body", class = "section-extras")
    }
  })
  
  # ===== ERROR HANDLING FOR TRANSITIONS =====
  
  # Add error handling to prevent stuck transitions
  observeEvent(session$clientData, {
    # If we detect an error state, complete any pending transitions
    if (!is.null(session$clientData$url_search)) {
      later::later(function() {
        session$sendCustomMessage("transition-complete", list(
          section = "error_recovery",
          timestamp = Sys.time()
        ))
      }, 2.0)
    }
  })
  
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

# ===== SIMPLIFIED ANALYTICS MODAL =====

observeEvent(input$secret_analytics_trigger, {
  message("🎯 Analytics modal triggered!")
 analytics_status <- if (!is.null(analytics)) {
      analytics$get_status()
    } else {
      list(enabled = FALSE, last_error = "Analytics object is NULL")
    }
      health_check <- if (!is.null(analytics)) {
      analytics$health_check()
    } else {
      list(status = "null", message = "Analytics object not initialized")
    }
  # Prepare analytics summary (as before)
  analytics_summary <- showAnalyticsModalSafe()
 analytics_summary$system_status <- list(
      analytics_enabled = analytics_status$enabled,
      last_error = analytics_status$last_error,
      health_status = health_check$status,
      health_message = health_check$message,
      retries = analytics_status$retries
    )
  # Assign tables to outputs for modal rendering
  output$analytics_popular_sections <- renderTable({
    if (nrow(analytics_summary$popular_sections) > 0) {
      analytics_summary$popular_sections %>%
        dplyr::mutate(
          avg_time = paste(round(avg_time_seconds/60, 1), "min"),
          total_time = paste(round(total_time_seconds/60, 1), "min"),
          engagement = paste0(round(visits/unique_visitors, 1), "x")
        ) %>%
        dplyr::select(Section = section, Visits = visits, `Unique Visitors` = unique_visitors, 
                      `Avg Time` = avg_time, `Total Time` = total_time, `Engagement` = engagement)
    } else {
      data.frame()
    }
  }, striped = TRUE, bordered = TRUE, align = "c")

  output$analytics_recent_sessions <- renderTable({
    if (nrow(analytics_summary$recent_sessions) > 0) {
      analytics_summary$recent_sessions %>%
        dplyr::mutate(
          start_time = format(as.POSIXct(start_time), "%m/%d %H:%M"),
          sections = paste0(total_sections_visited, " sections"),
          downloads = paste0(total_downloads, " files")
        ) %>%
        dplyr::select(`Session ID` = session_short, `Started` = start_time, 
                      `Sections Visited` = sections, `Downloads` = downloads)
    } else {
      data.frame()
    }
  }, striped = TRUE, bordered = TRUE, align = "c")

  # Download handler for section summary CSV
  output$download_section_summary <- downloadHandler(
    filename = function() {
      paste0("section_summary_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- analytics_summary$popular_sections
      if (nrow(df) > 0) {
        # Only keep relevant columns: section, visits, total_time_seconds
        write.csv(df[, c("section", "visits", "total_time_seconds")], file, row.names = FALSE)
      } else {
        write.csv(data.frame(section=character(0), visits=numeric(0), total_time_seconds=numeric(0)), file, row.names = FALSE)
      }
    }
  )

  # Show the modal with tableOutput placeholders and download button
  showModal(modalDialog(
    title = div(
      icon("chart-line"), 
      "Dashboard Analytics", 
      style = "color: #007bff; font-weight: bold;"
    ),
    size = "l",
    easyClose = TRUE,
    div(
      style = "max-height: 60vh; overflow-y: auto;",
      div(
          class = "analytics-insight mb-4",
          h6("🔧 System Status"),
          fluidRow(
            column(6,
              div(
                class = "text-center",
                div(class = "analytics-stat", 
                    if(analytics_summary$system_status$analytics_enabled) "✅ Enabled" else "❌ Disabled"),
                div(class = "analytics-label", "Analytics Status")
              )
            ),
            column(6,
              div(
                class = "text-center",
                div(class = "analytics-stat", analytics_summary$system_status$health_status),
                div(class = "analytics-label", "Health Status")
              )
            )
          ),
          if (!is.null(analytics_summary$system_status$last_error)) {
            div(
              class = "alert alert-warning mt-2",
              strong("Last Error: "), analytics_summary$system_status$last_error
            )
          }
        ),
      if (!is.null(analytics_summary$error_message)) {
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " ", analytics_summary$error_message
        )
      },
      div(
        class = "analytics-insight mb-4",
        h6("🔍 Key Insights"),
        fluidRow(
          column(4,
            div(
              class = "text-center",
              div(class = "analytics-stat", format(analytics_summary$sessions_7d, big.mark = ",")),
              div(class = "analytics-label", "Total Sessions")
            )
          ),
          column(4,
            div(
              class = "text-center",
              div(class = "analytics-stat", paste(analytics_summary$avg_duration, "min")),
              div(class = "analytics-label", "Avg Session Time")
            )
          ),
          column(4,
            div(
              class = "text-center",
              div(class = "analytics-stat", 
                  if (nrow(analytics_summary$popular_sections) > 0) {
                    analytics_summary$popular_sections$section[1]
                  } else {
                    "N/A"
                  }
              ),
              div(class = "analytics-label", "Most Popular Section")
            )
          )
        )
      ),
      div(
        h5("📊 Popular Sections (7 days)"),
        div(class = "table-responsive", tableOutput("analytics_popular_sections"))
      ),
      div(
        h5("🕒 Recent Sessions"),
        div(class = "table-responsive", tableOutput("analytics_recent_sessions"))
      )
    ),
            if (!is.null(analytics_summary$error_message)) {
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            " ", analytics_summary$error_message
          )
        },
      footer = tagList(
        modalButton("Close"),
        if (!is.null(analytics) && !analytics$is_enabled()) {
          actionButton("retry_analytics", "Retry Analytics", class = "btn-warning")
        },
        downloadButton("download_section_summary", "Download Section Summary (CSV)", 
                      class = "btn-primary", style = "margin-left: 10px;")
      )
    ))
  })


  observeEvent(input$retry_analytics, {
    if (!is.null(analytics)) {
      message("🔄 Manual analytics retry requested...")
      if (analytics$retry_initialization()) {
        showNotification("Analytics re-enabled successfully!", type = "success", duration = 5)
        message("✅ Manual analytics retry successful")
      } else {
        showNotification("Analytics retry failed. Check server logs.", type = "error", duration = 5)
        message("❌ Manual analytics retry failed")
      }
    }
  })
# ===== CONNECTION MONITORING =====

# Monitor analytics health
analytics_monitor <- reactiveTimer(600000)  # Every 10 minutes

observe({
  analytics_monitor()
  
  if (!is.null(analytics)) {
    # Check pool status
    pool_status <- tryCatch({
      get_pool_status()
    }, error = function(e) {
      list(pool_exists = FALSE, error = e$message)
    })
    
    # Log pool status
    if (pool_status$pool_exists) {
      message(sprintf("📊 Pool status: %s", pool_status$status))
    } else {
      message("⚠️ Analytics pool not found")
    }
    
    # Health check
    health <- analytics$health_check()
    if (health$status != "healthy") {
      message(sprintf("⚠️ Analytics health: %s - %s", health$status, health$message))
    }
  }
})
# Handler for checking analytics status
observeEvent(input$check_analytics_status, {
  if (!is.null(analytics)) {
    
    # ✅ NEW SAFE METHOD: Use execute_safe_query() 
    stats_result <- analytics$execute_safe_query(function(conn) {
      # Combine all queries into one safe function
      tryCatch({
        session_count <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as count FROM sessions")
        page_views_count <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as count FROM page_views")  
        interactions_count <- DBI::dbGetQuery(conn, "SELECT COUNT(*) as count FROM interactions")
        
        # Return all results as a list
        return(list(
          sessions = session_count$count[1],
          page_views = page_views_count$count[1], 
          interactions = interactions_count$count[1],
          success = TRUE
        ))
      }, error = function(e) {
        return(list(
          success = FALSE,
          error = e$message
        ))
      })
    }, "check_analytics_status")
    
    # Handle the results
    if (!is.null(stats_result) && stats_result$success) {
      status_msg <- paste(
        "Analytics Status:\n",
        "• Sessions:", stats_result$sessions, "\n",
        "• Page Views:", stats_result$page_views, "\n", 
        "• Interactions:", stats_result$interactions
      )
      showNotification(status_msg, type = "message", duration = 10)
      message("📊 Analytics Status: ", status_msg)
    } else {
      error_msg <- if (!is.null(stats_result) && !is.null(stats_result$error)) {
        stats_result$error
      } else {
        "Failed to get analytics status"
      }
      showNotification(paste("Error checking analytics:", error_msg), type = "error", duration = 5)
      message("❌ Error checking analytics status: ", error_msg)
    }
  } else {
    showNotification("Analytics not available", type = "warning", duration = 5)
  }
})


# Handler for creating test data
observeEvent(input$create_test_data, {
  if (!is.null(analytics)) {
    tryCatch({
      message("🔧 Creating test analytics data...")
      
      # Create some test data with more realistic patterns
      test_sessions <- c("test_session_1", "test_session_2", "test_session_3", "test_session_4", "test_session_5")
      test_sections <- c("wellness", "economic", "cultural", "government", "infrastructure", "participation", "urban", "explorer")
      
      for (i in 1:length(test_sessions)) {
        session_id <- test_sessions[i]
        
        # Start session with realistic timestamp
        start_time <- Sys.time() - runif(1, 0, 86400) # Random time in last 24 hours
        analytics$start_session(session_id, "127.0.0.1", "test_agent")
        
        # Track navigation to multiple sections
        num_sections <- sample(2:5, 1)
        sections_to_visit <- sample(test_sections, num_sections)
        
        for (j in 1:length(sections_to_visit)) {
          section <- sections_to_visit[j]
          
          # Simulate realistic time spent (30-300 seconds)
          time_spent <- sample(30:300, 1)
          
          # Track navigation
          analytics$track_navigation(session_id, section, "2024")
          
          # Track interaction
          analytics$track_interaction(session_id, "section_visit", section, 
                                     paste("Test visit to", section, "- Time spent:", time_spent, "seconds"))
          
          # Simulate some downloads
          if (runif(1) < 0.3) { # 30% chance of download
            analytics$track_interaction(session_id, "download", section, "Test download")
          }
        }
        
        # End session
        analytics$end_session(session_id)
      }
      
      message("✅ Test data created successfully")
      showNotification("Test analytics data created! Refresh the analytics modal to see the data.", 
                      type = "message", duration = 5)
      
    }, error = function(e) {
      message("❌ Error creating test data: ", e$message)
      showNotification(paste("Error creating test data:", e$message), type = "error", duration = 5)
    })
  }
})


showAnalyticsModalSafe <- function() {
  # Initialize default data structure
  analytics_summary <- list(
    sessions_7d = 0,
    avg_duration = 0,
    cache_hit_rate = 0,
    cache_size = 0,
    popular_sections = data.frame(),
    recent_sessions = data.frame(),
    session_insights = list(),
    section_insights = list(),
    error_message = NULL,
    system_status = list()
  )
  
  # Try to get analytics data
  if (!is.null(analytics)) {
    
    # ✅ NEW SAFE METHOD: Use execute_safe_query() for all database operations
    db_results <- analytics$execute_safe_query(function(conn) {
      tryCatch({
        # Check if database exists and has tables
        tables_exist <- tryCatch({
          test_query <- DBI::dbGetQuery(conn, "SHOW TABLES LIKE 'sessions'")
          nrow(test_query) > 0
        }, error = function(e) FALSE)
        
        if (!tables_exist) {
          return(list(
            success = FALSE,
            error = "Analytics database not initialized yet"
          ))
        }
        
        # Simple session count query (MySQL version)
        sessions_result <- DBI::dbGetQuery(conn, "
          SELECT COUNT(*) as total_sessions 
          FROM sessions 
          WHERE start_time IS NOT NULL AND start_time != '' AND start_time >= NOW() - INTERVAL 7 DAY
        ")
        sessions_7d <- if(nrow(sessions_result) > 0) sessions_result$total_sessions[1] else 0
        sessions_7d <- if(is.null(sessions_7d) || is.na(sessions_7d)) 0 else sessions_7d
        
        # Simple duration query (MySQL version)
        duration_result <- DBI::dbGetQuery(conn, "
          SELECT AVG(TIMESTAMPDIFF(MINUTE, start_time, COALESCE(end_time, last_activity))) as avg_minutes
          FROM sessions 
          WHERE start_time IS NOT NULL AND start_time != '' AND start_time >= NOW() - INTERVAL 7 DAY
            AND end_time IS NOT NULL
        ")
        avg_duration_raw <- if(nrow(duration_result) > 0) duration_result$avg_minutes[1] else 0
        avg_duration <- round(if(is.null(avg_duration_raw) || is.na(avg_duration_raw)) 0 else avg_duration_raw, 1)
        
        # Enhanced popular sections with insights (MySQL version)
        popular_result <- tryCatch({
          DBI::dbGetQuery(conn, "
            SELECT 
              pv.section,
              COUNT(*) as visits,
              COUNT(DISTINCT pv.session_id) as unique_visitors,
              AVG(pv.time_spent_seconds) as avg_time_seconds,
              SUM(pv.time_spent_seconds) as total_time_seconds
            FROM page_views pv
            JOIN sessions s ON pv.session_id = s.session_id
            WHERE s.start_time IS NOT NULL AND s.start_time != '' AND s.start_time >= NOW() - INTERVAL 7 DAY
            GROUP BY pv.section
            ORDER BY visits DESC
            LIMIT 10
          ")
        }, error = function(e) {
          data.frame(section = character(0), visits = numeric(0), unique_visitors = numeric(0), 
                    avg_time_seconds = numeric(0), total_time_seconds = numeric(0))
        })
        
        # Recent sessions (MySQL version)
        recent_result <- tryCatch({
          DBI::dbGetQuery(conn, "
            SELECT 
              substr(session_id, 1, 8) as session_short,
              CASE WHEN start_time IS NULL OR start_time = '' THEN 'N/A' ELSE start_time END as start_time,
              total_sections_visited,
              total_downloads
            FROM sessions 
            ORDER BY start_time DESC 
            LIMIT 10
          ")
        }, error = function(e) {
          data.frame(
            session_short = character(0), 
            start_time = character(0),
            total_sections_visited = numeric(0),
            total_downloads = numeric(0)
          )
        })
        
        # Return all results in a single object
        return(list(
          success = TRUE,
          sessions_7d = sessions_7d,
          avg_duration = avg_duration,
          popular_sections = popular_result,
          recent_sessions = recent_result
        ))
        
      }, error = function(e) {
        return(list(
          success = FALSE,
          error = paste("Database error:", e$message)
        ))
      })
    }, "get_analytics_summary")
    
    # Handle the results
    if (!is.null(db_results)) {
      if (db_results$success) {
        # Success - update analytics_summary with results
        analytics_summary$sessions_7d <- db_results$sessions_7d
        analytics_summary$avg_duration <- db_results$avg_duration
        analytics_summary$popular_sections <- db_results$popular_sections
        analytics_summary$recent_sessions <- db_results$recent_sessions
      } else {
        # Database operation failed
        analytics_summary$error_message <- db_results$error
      }
    } else {
      # execute_safe_query returned NULL (analytics disabled)
      analytics_summary$error_message <- "Analytics database operation failed"
    }
    
  } else {
    analytics_summary$error_message <- "Analytics not initialized"
  }
  
  # Get cache stats safely (unchanged - this doesn't use database)
  tryCatch({
    cache_stats <- data_manager$get_cache_stats()
    analytics_summary$cache_hit_rate <- if(is.null(cache_stats$hit_rate) || is.na(cache_stats$hit_rate)) 0 else round(cache_stats$hit_rate, 1)
    analytics_summary$cache_size <- if(is.null(cache_stats$cache_size_mb) || is.na(cache_stats$cache_size_mb)) 0 else round(cache_stats$cache_size_mb, 1)
  }, error = function(e) {
    analytics_summary$cache_hit_rate <- 0
    analytics_summary$cache_size <- 0
  })
  
  # Filter out 'overview' from popular_sections if present
  if (!is.null(analytics_summary$popular_sections) && nrow(analytics_summary$popular_sections) > 0) {
    analytics_summary$popular_sections <- subset(analytics_summary$popular_sections, section != "overview")
  }
  
  return(analytics_summary)
}

# Session cleanup (unchanged - this is fine)
session$onSessionEnded(function() {
  tryCatch({
    if (!is.null(analytics) && analytics$is_enabled()) {
      analytics$end_session(session_id)
      message("✅ Session analytics ended cleanly")
    }
  }, error = function(e) {
    message("❌ Error ending analytics session: ", e$message)
  })
})
}




# Run the application
  shinyApp(ui = ui, server = server)