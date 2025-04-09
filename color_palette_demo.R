library(shiny)
library(shinyWidgets)
library(plotly)
library(RColorBrewer)
library(htmltools)

# Proposed color palettes with updates
proposed_palettes <- list(
  # Base colors
  base = list(
    primary = "#0D6EFD",
    secondary = "#6C757D",
    success = "#198754",
    warning = "#FFC107",
    danger = "#DC3545",
    info = "#0DCAF0",
    light = "#F8F9FA",
    dark = "#212529",
    background = "#FFFFFF",
    text = "#212529",
    neutral = "#DEE2E6",
    accent = "#FFD700"
  ),
  
  # District colors (more pastel-like)
  district = c("#94C7E6", "#FFB6C1", "#B7E1A1", "#FDCB85", "#C5B3E6", 
               "#97DCE3", "#F4B8D6", "#D1EC9E", "#F2BDC2", "#B8CFEA"),
  
  # Section-specific palettes
  bienestar = list(
    primary = "#1E88E5",
    secondary = "#90CAF9",
    accent = "#FFA000",
    sequential = c("#BFD3E6", "#9EBCDA", "#8C96C6", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
    categorical = c("#1E88E5", "#42A5F5", "#90CAF9", "#FFA000", "#FFCA28", "#FFE082"),
    diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#1E88E5"))(9)
  ),
  
  movilidad = list(
    primary = "#43A047",
    secondary = "#A5D6A7",
    accent = "#052F5F",
    sequential = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
    categorical =c("#43A047", "#66BB6A", "#A5D6A7", "#052F5F", "#2D4A70", "#5788A0"),
    diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#43A047"))(9)
  ),
  
  gobierno = list(
    primary = "#5E35B1",
    secondary = "#B39DDB",
    accent = "#FB8C00",
    sequential = c("#CAB2D6", "#9E9AC8", "#756BB1", "#5E35B1", "#54278F", "#41236C", "#3A1E61"),
    categorical = c("#5E35B1", "#7E57C2", "#B39DDB", "#FB8C00", "#FFA726", "#FFCC80"),
    diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#5E35B1"))(9)
  ),
  
  infraestructura = list(
    primary = "#F57C00",  # Changed from E64A19 to be more distinct from danger color
    secondary = "#FFCC80",
    accent = "#039BE5",
    sequential = c("#FBB582", "#FA954F", "#F57C00", "#E66D00", "#D45F00", "#B65200", "#8F3F00"),
    categorical = c("#F57C00", "#FB8C00", "#FFCC80", "#039BE5", "#29B6F6", "#81D4FA"),
    diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#F57C00"))(9)
  ),
  
  participacion = list(
    primary = "#8E24AA",
    secondary = "#CE93D8",
    accent = "#00ACC1",
    sequential = c("#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#8E24AA", "#980043", "#67001F"),
    categorical = c("#8E24AA", "#AB47BC", "#CE93D8", "#00ACC1", "#26C6DA", "#80DEEA"),
    diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#8E24AA"))(9)
  )
)

# Define UI
ui <- fluidPage(
  titlePanel("Color Palette Preview for Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("palette_type", "Palette Type:",
                  choices = c("Base Colors", "Core Palettes", "Section Palettes"),
                  selected = "Base Colors"),
      
      conditionalPanel(
        condition = "input.palette_type == 'Section Palettes'",
        radioButtons("section", "Section:",
                    choices = c("Bienestar", "Movilidad", "Gobierno", "Infraestructura", "Participacion"),
                    selected = "Bienestar")
      ),
      
      conditionalPanel(
        condition = "input.palette_type == 'Core Palettes'",
        radioButtons("core_palette", "Core Palette:",
                    choices = c("District Colors", "Gender Colors", "Age Group Colors"),
                    selected = "District Colors")
      ),
      
      conditionalPanel(
        condition = "input.palette_type == 'Section Palettes'",
        radioButtons("section_palette", "Section Palette Type:",
                    choices = c("Main Colors", "Sequential", "Categorical", "Diverging"),
                    selected = "Main Colors")
      ),
      
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(12, 
          h4("Palette Preview"),
          plotlyOutput("palette_plot", height = "200px"),
          hr(),
          h4("Example Visualization"),
          plotlyOutput("example_plot", height = "400px"),
          hr(),
          h4("Color Values"),
          verbatimTextOutput("color_values")
        )
      ),
      width = 9
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Get the currently selected palette
  selected_palette <- reactive({
    if (input$palette_type == "Base Colors") {
      return(proposed_palettes$base)
    } else if (input$palette_type == "Core Palettes") {
      if (input$core_palette == "District Colors") {
        return(proposed_palettes$district)
      } else if (input$core_palette == "Gender Colors") {
        return(c("#FF7BAC", "#007BFF"))  # Pink, Blue
      } else {
        return(c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")) # Age groups
      }
    } else {
      # Section palettes
      section <- tolower(input$section)
      section_palette_type <- tolower(input$section_palette)
      
      if (section_palette_type == "main colors") {
        return(c(
          proposed_palettes[[section]]$primary,
          proposed_palettes[[section]]$secondary,
          proposed_palettes[[section]]$accent
        ))
      } else {
        return(proposed_palettes[[section]][[section_palette_type]])
      }
    }
  })
  
  # Generate palette preview
  output$palette_plot <- renderPlotly({
    palette <- selected_palette()
    
    if (input$palette_type == "Base Colors") {
      # Create a named vector for base colors
      colors <- unlist(palette)
      labels <- names(colors)
      
      # Create data frame for plotting
      df <- data.frame(
        x = 1:length(colors),
        y = rep(1, length(colors)),
        color = colors,
        label = labels
      )
      
      # Create plot
      plot_ly(df, x = ~x, y = ~y, color = I(colors), text = ~paste(label, ":", color)) %>%
        add_bars(width = 0.8) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(
            tickvals = df$x,
            ticktext = df$label,
            tickangle = 45
          ),
          yaxis = list(
            showticklabels = FALSE,
            showgrid = FALSE
          )
        )
    } else {
      # For other palettes, create a simple color bar
      if (length(palette) > 0) {
        df <- data.frame(
          x = 1:length(palette),
          y = rep(1, length(palette)),
          color = palette
        )
        
        plot_ly(df, x = ~x, y = ~y, color = I(palette), 
                text = ~paste("Color:", color)) %>%
          add_bars(width = 0.8) %>%
          layout(
            showlegend = FALSE,
            xaxis = list(
              showticklabels = FALSE,
              showgrid = FALSE
            ),
            yaxis = list(
              showticklabels = FALSE,
              showgrid = FALSE
            )
          )
      } else {
        # Handle empty palette case
        plot_ly() %>%
          layout(
            title = "No colors to display",
            xaxis = list(showticklabels = FALSE, showgrid = FALSE),
            yaxis = list(showticklabels = FALSE, showgrid = FALSE)
          )
      }
    }
  })
  
  # Generate example visualization
  output$example_plot <- renderPlotly({
    palette <- selected_palette()
    
    # Check if palette is empty
    if (length(palette) == 0) {
      return(plot_ly() %>%
        layout(title = "No data to display"))
    }
    
    # Create some example data
    set.seed(123)
    if (input$palette_type == "Base Colors") {
      # Bar chart with base colors
      categories <- names(palette)[1:6]  # First 6 base colors
      values <- round(runif(6, 10, 100))
      colors <- unlist(palette)[1:6]
      
      plot_ly() %>%
        add_bars(x = categories, y = values, marker = list(color = colors),
                 text = ~paste(categories, ":", values),
                 hoverinfo = "text") %>%
        layout(
          title = "Example Bar Chart with Base Colors",
          xaxis = list(title = "Categories"),
          yaxis = list(title = "Values")
        )
      
    } else if (input$palette_type == "Core Palettes") {
      # Group comparison
      if (input$core_palette == "District Colors") {
        categories <- paste("District", 1:length(palette))
        values <- round(runif(length(palette), 10, 100))
        
        plot_ly() %>%
          add_bars(x = categories, y = values, marker = list(color = palette),
                  text = ~paste(categories, ":", values),
                  hoverinfo = "text") %>%
          layout(
            title = "Values by District",
            xaxis = list(title = "District"),
            yaxis = list(title = "Value")
          )
      } else if (input$core_palette == "Gender Colors") {
        # Gender comparison
        plot_ly() %>%
          add_pie(labels = c("Female", "Male"), values = c(55, 45), 
                 marker = list(colors = palette),
                 textinfo = "label+percent") %>%
          layout(title = "Gender Distribution")
      } else {
        # Age groups
        age_groups <- c("18-24", "25-34", "35-44", "45-54", "55+")
        values <- c(15, 25, 30, 20, 10)
        
        plot_ly() %>%
          add_pie(labels = age_groups, values = values, 
                 marker = list(colors = palette),
                 textinfo = "label+percent") %>%
          layout(title = "Age Group Distribution")
      }
    } else {
      # Section visualizations
      section <- tolower(input$section)
      section_palette_type <- tolower(input$section_palette)
      
      if (section_palette_type == "main colors") {
        # Main colors visualization: Primary, Secondary, Accent
        labels <- c("Primary", "Secondary", "Accent")
        values <- c(50, 30, 20)
        
        plot_ly() %>%
          add_pie(labels = labels, values = values,
                 marker = list(colors = palette),
                 textinfo = "label+percent") %>%
          layout(title = paste(input$section, "Main Colors"))
      } else if (section_palette_type == "sequential") {
        # Sequential data visualization
        x <- 1:length(palette)
        y <- seq(10, 100, length.out = length(palette))
        
        plot_ly() %>%
          add_bars(x = x, y = y, marker = list(color = palette),
                  text = ~paste("Value:", y),
                  hoverinfo = "text") %>%
          layout(
            title = paste(input$section, "Sequential Scale"),
            xaxis = list(title = "Categories", showticklabels = FALSE),
            yaxis = list(title = "Values")
          )
      } else if (section_palette_type == "categorical") {
        # Categorical data
        categories <- paste("Category", 1:length(palette))
        values <- round(runif(length(palette), 10, 100))
        
        plot_ly() %>%
          add_bars(x = categories, y = values, marker = list(color = palette),
                  text = ~paste(categories, ":", values),
                  hoverinfo = "text") %>%
          layout(
            title = paste(input$section, "Categorical Colors"),
            xaxis = list(title = "Categories"),
            yaxis = list(title = "Values")
          )
      } else if (section_palette_type == "diverging") {
        # Diverging data
        x <- 1:length(palette)
        # Create data that diverges from a central point
        mid_point <- ceiling(length(palette) / 2)
        y <- c(seq(-50, -10, length.out = mid_point - 1), 0, 
               seq(10, 50, length.out = length(palette) - mid_point))
        
        plot_ly() %>%
          add_bars(x = x, y = y, marker = list(color = palette),
                  text = ~paste("Value:", y),
                  hoverinfo = "text") %>%
          layout(
            title = paste(input$section, "Diverging Scale"),
            xaxis = list(title = "Categories", showticklabels = FALSE),
            yaxis = list(title = "Values")
          )
      }
    }
  })
  
  # Display color values
  output$color_values <- renderPrint({
    palette <- selected_palette()
    
    if (input$palette_type == "Base Colors") {
      # For base colors, show named list
      cat("Base Colors:\n")
      for (name in names(palette)) {
        cat(sprintf("%s: \"%s\"\n", name, palette[[name]]))
      }
    } else {
      # For other palettes, show vector
      if (is.list(palette)) {
        cat("Color List:\n")
        for (name in names(palette)) {
          if (length(palette[[name]]) == 1) {
            cat(sprintf("%s: \"%s\"\n", name, palette[[name]]))
          } else {
            cat(sprintf("%s: c(\"%s\")\n", name, paste(palette[[name]], collapse = "\", \"")))
          }
        }
      } else {
        cat("Color Vector:\n")
        cat(sprintf("c(\"%s\")", paste(palette, collapse = "\", \"")))
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)