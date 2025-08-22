# R/extras/reports_ui.R - Fixed version without extra card wrapper

reportsUI <- function(id = "reports") {
  ns <- NS(id)
  
  page_fluid(
    class = "section-extras",
    useShinyjs(),
    init_tooltips(),
    
    # Custom CSS for the reports section with updated styling
    tags$head(
      tags$style(HTML("
        .report-card {
          transition: all 0.3s ease;
          border: 1px solid #e9ecef;
          border-radius: var(--border-radius);
          overflow: hidden;
          height: 100%;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          background: white;
          cursor: pointer;
        }
        
        .report-card:hover {
          transform: translateY(-4px);
          box-shadow: 0 8px 25px rgba(0,0,0,0.15);
        }
        
        .report-card::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 3px;
          background: var(--extras-color);
          z-index: 1;
        }
        
        .report-thumbnail-container {
          position: relative;
          height: 200px;
          overflow: hidden;
          background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
          display: flex;
          align-items: center;
          justify-content: center;
          border-bottom: 1px solid #e9ecef;
        }
        
        .report-thumbnail {
          width: 100%;
          height: 100%;
          object-fit: cover;
          transition: transform 0.3s ease;
        }
        
        .report-card:hover .report-thumbnail {
          transform: scale(1.05);
        }
        
        .report-thumbnail-fallback {
          height: 200px;
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
          color: var(--extras-color);
          border-bottom: 1px solid #e9ecef;
        }
        
        .report-category-badge {
          font-size: 0.75rem;
          font-weight: 600;
          padding: 0.3rem 0.6rem;
          border-radius: 15px;
          display: inline-block;
          margin-bottom: 0.75rem;
          background: var(--extras-color);
          color: white;
          box-shadow: 0 2px 4px rgba(0,0,0,0.15);
          font-family: var(--font-primary);
        }
        
        .report-year-badge {
          background-color: color-mix(in srgb, var(--extras-color) 80%, white);
          color: white;
          font-size: 0.7rem;
          padding: 0.25rem 0.5rem;
          border-radius: 10px;
          font-weight: 600;
          font-family: var(--font-primary);
        }
        
        .report-title {
          font-family: var(--font-display);
          font-size: 1.1rem;
          font-weight: 600;
          line-height: 1.3;
          margin-bottom: 0.75rem;
          color: var(--text-color);
        }
        
        .report-description {
          font-family: var(--font-primary);
          font-size: 0.9rem;
          color: #6c757d;
          line-height: 1.5;
          margin-bottom: 1rem;
          display: -webkit-box;
          -webkit-line-clamp: 3;
          -webkit-box-orient: vertical;
          overflow: hidden;
        }
        
        .report-meta {
          font-family: var(--font-primary);
          font-size: 0.75rem;
          color: #8e9ba3;
          margin-bottom: 1rem;
        }
        
        .reports-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(350px, 1fr));
          gap: var(--spacing-xl);
          margin-top: var(--spacing-xl);
        }
        
        .stats-overview {
          background: linear-gradient(135deg, var(--extras-color) 0%, color-mix(in srgb, var(--extras-color) 80%, black) 100%);
          color: white;
          border-radius: var(--border-radius);
          padding: var(--spacing-xl);
          margin-bottom: var(--spacing-xl);
          box-shadow: 0 4px 15px rgba(45, 45, 45, 0.3);
        }
        
        .stat-item {
          text-align: center;
        }
        
        .stat-number {
          font-family: var(--font-display);
          font-size: 2.5rem;
          font-weight: 700;
          margin-bottom: 0.25rem;
          text-shadow: 0 2px 4px rgba(0,0,0,0.2);
        }
        
        .stat-label {
          font-family: var(--font-primary);
          font-size: 0.9rem;
          opacity: 0.9;
          font-weight: 500;
        }
        
        .empty-state {
          text-align: center;
          padding: 4rem 1rem;
          color: #6c757d;
          background: #f8f9fa;
          border-radius: var(--border-radius);
          border: 2px dashed #dee2e6;
        }
        
        .empty-state-icon {
          font-size: 4rem;
          margin-bottom: 1rem;
          opacity: 0.5;
          color: var(--extras-color);
        }
        
        .empty-state h5 {
          font-family: var(--font-display);
          color: var(--text-color);
          margin-bottom: 1rem;
        }
        
        .timeline-view .report-card {
          margin-left: 2rem;
          border-left: 3px solid var(--extras-color);
          border-radius: 0 var(--border-radius) var(--border-radius) 0;
        }
        
        .timeline-year {
          font-family: var(--font-display);
          font-size: 1.8rem;
          font-weight: 700;
          color: var(--extras-color);
          margin: 2rem 0 1rem 0;
          border-bottom: 3px solid var(--extras-color);
          padding-bottom: 0.5rem;
          display: inline-block;
        }
        
        .download-btn-report {
          background: var(--extras-color) !important;
          border-color: var(--extras-color) !important;
          color: white !important;
          font-family: var(--font-primary);
          font-weight: 600;
          transition: all 0.2s ease;
        }
        
        .download-btn-report:hover {
          background: color-mix(in srgb, var(--extras-color) 90%, black) !important;
          border-color: color-mix(in srgb, var(--extras-color) 90%, black) !important;
          transform: translateY(-1px);
        }
        
        .filters-section {
          background: #f8f9fa;
          border: 2px solid #e9ecef;
          border-radius: var(--border-radius);
          padding: var(--spacing-lg);
          margin-bottom: var(--spacing-xl);
        }
        
        /* Enhanced responsive design */
        @media (max-width: 768px) {
          .reports-grid {
            grid-template-columns: 1fr;
            gap: var(--spacing-lg);
          }
          
          .report-thumbnail-container,
          .report-thumbnail-fallback {
            height: 150px;
          }
          
          .stat-number {
            font-size: 2rem;
          }
          
          .timeline-view .report-card {
            margin-left: 1rem;
          }
        }
        
        /* Loading animation for thumbnails */
        .report-thumbnail[src=''] {
          background: linear-gradient(90deg, #f0f0f0 25%, transparent 37%, #f0f0f0 63%);
          background-size: 400% 100%;
          animation: shimmer 1.5s ease-in-out infinite;
        }
        
        @keyframes shimmer {
          0% { background-position: 100% 50%; }
          100% { background-position: -100% 50%; }
        }
      "))
    ),
    
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
          h1("Biblioteca de Informes", class = "dashboard-header")
        )
      )
    ),
    
    # Statistics Overview
    div(
      class = "stats-overview",
      uiOutput(ns("stats_overview"))
    ),
    
    # Thumbnail Management Section (only show if user is admin)
    conditionalPanel(
      condition = "false", # Change to appropriate admin condition
      div(
        class = "alert",
        style = "background-color: color-mix(in srgb, #ffc107 10%, transparent); border-left: 4px solid #ffc107;",
        fluidRow(
          column(8,
            div(
              style = "font-family: var(--font-primary);",
              icon("info-circle"), 
              " Las miniaturas se generan automáticamente la primera vez. ",
              "Esto puede tomar unos momentos para documentos nuevos."
            )
          ),
          column(4,
            div(
              style = "text-align: right;",
              actionButton(
                ns("regenerate_thumbnails"),
                "Regenerar Miniaturas",
                class = "btn btn-outline-warning btn-sm",
                icon = icon("sync-alt")
              )
            )
          )
        )
      )
    ),
    
    # Filters Section
    div(
      class = "filters-section",
      
      div(
        style = "margin-bottom: var(--spacing-md);",
        h5("Filtros de Búsqueda", style = "font-family: var(--font-display); color: var(--extras-color); margin-bottom: var(--spacing-md);")
      ),
      
      fluidRow(
        column(3,
          selectInput(
            ns("category_filter"),
            "Categoría:",
            choices = NULL,
            selected = "Todos",
            multiple = FALSE
          )
        ),
        column(3,
          selectInput(
            ns("year_filter"),
            "Año:",
            choices = NULL,
            selected = "Todos",
            multiple = FALSE
          )
        ),
        column(3,
          selectInput(
            ns("type_filter"),
            "Tipo de Documento:",
            choices = c("Todos" = "all", "Informes" = "informe", "Presentaciones" = "presentacion"),
            selected = "all"
          )
        ),
        column(3,
          div(
            style = "margin-top: 25px;",
            radioButtons(
              ns("view_mode"),
              "Vista:",
              choices = c("Cuadrícula" = "grid", "Cronológica" = "timeline"),
              selected = "grid",
              inline = TRUE
            )
          )
        )
      ),
      fluidRow(
        column(12,
          div(
            style = "margin-top: var(--spacing-md);",
            textInput(
              ns("search_text"),
              "Buscar en títulos y descripciones:",
              placeholder = "Ejemplo: economía, salud, corrupción..."
            )
          )
        )
      )
    ),
    
    # Results Count and Download
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      div(
        uiOutput(ns("results_count"))
      ),
      div(
        downloadButton(
          ns("download_catalog"),
          "Descargar Catálogo",
          class = "btn btn-outline-primary btn-sm",
          style = "border-color: var(--extras-color); color: var(--extras-color);",
          icon = icon("download")
        )
      )
    ),
    
    # Reports Display
    uiOutput(ns("reports_display")),
    
    # Footer
    create_dashboard_footer()
  )
}