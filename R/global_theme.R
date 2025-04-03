# Enhanced theme_config with a more cohesive color palette
theme_config <- list(
  # Base colors
  colors = list(
    primary = "#0D6EFD",       # Primary blue
    secondary = "#6C757D",     # Secondary gray
    success = "#2A9D8F",       # Green for positive values
    warning = "#E9C46A",       # Yellow for warnings
    danger = "#E76F51",        # Red for negative or alerts
    info = "#4CC9F0",          # Light blue for information
    light = "#F8F9FA",         # Light background
    dark = "#212529",          # Dark text
    background = "#FFFFFF",    # White background
    text = "#212529"           # Dark text
  ),
  
 
  # Categorical palettes - coordinated colors
  palettes = list(
    district = c("#0D6EFD", "#4361EE", "#3A0CA3", "#7209B7", "#F72585",
                 "#4CC9F0", "#4895EF", "#560BAD", "#B5179E", "#F15BB5"),
    gender = c("#FF69B4", "#4169E1"),
    age_group = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"),
    sequential = colorRampPalette(c("#F8F9FA", "#212529"))(9),
    diverging = colorRampPalette(c("#E76F51", "#F8F9FA", "#0D6EFD"))(11)
  ),
  
  # Enhanced typography
  typography = list(
    font_family = "'Roboto', 'Segoe UI', Arial, sans-serif",
    sizes = list(
      title = 22,
      subtitle = 18,
      axis = 12,
      text = 14
    )
  ),
  
  # Enhanced layout
  layout = list(
    margin = list(l = 50, r = 20, t = 50, b = 50),
    padding = list(
      small = 8,
      medium = 16,
      large = 24
    ),
    border_radius = "0.375rem"
  )
)

#' Create consistent plotly theme
#' @param p plotly object
#' @param title plot title
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @export
apply_plotly_theme <- function(p, title = "", xlab = "", ylab = "", custom_theme = NULL) {
  # Use provided custom theme or fall back to default theme_config
  active_theme <- if (!is.null(custom_theme)) custom_theme else theme_config
  
  p %>%
    layout(
      title = list(
        text = title,
        font = list(
          family = active_theme$typography$font_family,
          size = active_theme$typography$sizes$title,
          color = active_theme$colors$text
        )
      ),
      xaxis = list(
        title = list(
          text = xlab,
          font = list(
            family = active_theme$typography$font_family,
            size = active_theme$typography$sizes$axis,
            color = active_theme$colors$text
          ),
          standoff = 15
        ),
        tickfont = list(
          family = active_theme$typography$font_family,
          size = active_theme$typography$sizes$text
        ),
        gridcolor = "rgba(0, 0, 0, 0.05)",
        showgrid = TRUE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = list(
          text = ylab,
          font = list(
            family = active_theme$typography$font_family,
            size = active_theme$typography$sizes$axis,
            color = active_theme$colors$text
          ),
          standoff = 15
        ),
        tickfont = list(
          family = active_theme$typography$font_family,
          size = active_theme$typography$sizes$text
        ),
        gridcolor = "rgba(0, 0, 0, 0.05)",
        showgrid = TRUE,
        zeroline = FALSE
      ),
      paper_bgcolor = active_theme$colors$background,
      plot_bgcolor = active_theme$colors$background,
      margin = list(l = 60, r = 30, t = 60, b = 60),
      hoverlabel = list(
        bgcolor = "#FFF",
        bordercolor = active_theme$colors$primary,
        font = list(
          family = active_theme$typography$font_family,
          size = active_theme$typography$sizes$text,
          color = active_theme$colors$text
        )
      ),
      autosize = TRUE
    ) %>%
    config(
      displayModeBar = FALSE,
      responsive = TRUE
    )
}



#' Create color palette function
#' @param palette_name name of the palette from theme_config$palettes
#' @param reverse boolean to reverse the palette
#' @export
get_color_palette <- function(palette_name, reverse = FALSE) {
  palette <- theme_config$palettes[[palette_name]]
  if(reverse) palette <- rev(palette)
  return(palette)
}

plot_functions <- list(
  # Bar plot with dynamic color handling
  bar = function(data, x, y, title = "", xlab = "", ylab = "", 
                orientation = "v", color_by = NULL, group_var = NULL) {
    # Determine colors based on grouping variable
    if (!is.null(color_by) && color_by %in% names(theme_config$palettes)) {
      colors <- theme_config$palettes[[color_by]]
    } else {
      colors <- theme_config$colors$primary
    }
    
    # Create the plot
    if(orientation == "v") {
      if (!is.null(group_var)) {
        p <- plot_ly(
          data = data,
          x = as.formula(paste0("~", x)),
          y = as.formula(paste0("~", y)),
          type = "bar",
          color = as.formula(paste0("~", group_var)),
          colors = colors
        )
      } else {
        p <- plot_ly(
          data = data,
          x = as.formula(paste0("~", x)),
          y = as.formula(paste0("~", y)),
          type = "bar",
          marker = list(color = colors)
        )
      }
    } else {
      if (!is.null(group_var)) {
        p <- plot_ly(
          data = data,
          y = as.formula(paste0("~", x)),
          x = as.formula(paste0("~", y)),
          type = "bar",
          orientation = 'h',
          color = as.formula(paste0("~", group_var)),
          colors = colors
        )
      } else {
        p <- plot_ly(
          data = data,
          y = as.formula(paste0("~", x)),
          x = as.formula(paste0("~", y)),
          type = "bar",
          orientation = 'h',
          marker = list(color = colors)
        )
      }
    }
    
    apply_plotly_theme(p, title = title, xlab = xlab, ylab = ylab)
  },
  
  # Line plot with dynamic color handling
  line = function(data, x, y, title = "", xlab = "", ylab = "", 
                 color_by = NULL, group_var = NULL) {
    # Determine colors based on grouping variable
    if (!is.null(color_by) && color_by %in% names(theme_config$palettes)) {
      colors <- theme_config$palettes[[color_by]]
    } else {
      colors <- theme_config$colors$primary
    }
    
    # Create the plot
    if (!is.null(group_var)) {
      p <- plot_ly(
        data = data,
        x = as.formula(paste0("~", x)),
        y = as.formula(paste0("~", y)),
        type = "scatter",
        mode = "lines",
        color = as.formula(paste0("~", group_var)),
        colors = colors
      )
    } else {
      p <- plot_ly(
        data = data,
        x = as.formula(paste0("~", x)),
        y = as.formula(paste0("~", y)),
        type = "scatter",
        mode = "lines",
        line = list(color = colors)
      )
    }
    
    apply_plotly_theme(p, title = title, xlab = xlab, ylab = ylab)
  },
  
  # Box plot with dynamic color handling
  box = function(data, x, y, title = "", xlab = "", ylab = "", 
                color_by = NULL, group_var = NULL) {
    # Determine colors based on grouping variable
    if (!is.null(color_by) && color_by %in% names(theme_config$palettes)) {
      colors <- theme_config$palettes[[color_by]]
    } else {
      colors <- theme_config$colors$primary
    }
    
    # Create the plot
    if (!is.null(group_var)) {
      p <- plot_ly(
        data = data,
        x = as.formula(paste0("~", x)),
        y = as.formula(paste0("~", y)),
        type = "box",
        color = as.formula(paste0("~", group_var)),
        colors = colors
      )
    } else {
      p <- plot_ly(
        data = data,
        x = as.formula(paste0("~", x)),
        y = as.formula(paste0("~", y)),
        type = "box",
        marker = list(color = colors)
      )
    }
    
    apply_plotly_theme(p, title = title, xlab = xlab, ylab = ylab)
  }
)
# Add to global_theme.R
apply_custom_theme <- function(p, custom_theme, title = "", xlab = "", ylab = "") {
  p %>%
    layout(
      title = list(
        text = title,
        font = list(
          family = custom_theme$typography$font_family,
          size = custom_theme$typography$sizes$title,
          color = custom_theme$colors$text
        )
      ),
      xaxis = list(
        title = xlab,
        titlefont = list(
          family = custom_theme$typography$font_family,
          size = custom_theme$typography$sizes$axis,
          color = custom_theme$colors$text
        ),
        tickfont = list(
          family = custom_theme$typography$font_family,
          size = custom_theme$typography$sizes$text
        ),
        gridcolor = custom_theme$colors$neutral,
        showgrid = TRUE
      ),
      yaxis = list(
        title = ylab,
        titlefont = list(
          family = custom_theme$typography$font_family,
          size = custom_theme$typography$sizes$axis,
          color = custom_theme$colors$text
        ),
        tickfont = list(
          family = custom_theme$typography$font_family,
          size = custom_theme$typography$sizes$text
        ),
        gridcolor = custom_theme$colors$neutral,
        showgrid = TRUE
      ),
      paper_bgcolor = custom_theme$colors$background,
      plot_bgcolor = custom_theme$colors$background,
      margin = custom_theme$layout$margin
    ) %>%
    config(displayModeBar = FALSE)
}

apply_leaflet_theme <- function(map, title = NULL, custom_theme = NULL) {
  # Use provided custom theme or fall back to default theme_config
  active_theme <- if (!is.null(custom_theme)) custom_theme else theme_config
  
  # Add title if provided
  if (!is.null(title)) {
    map <- map %>% 
      addControl(
        html = paste0('<div style="padding: 6px 8px; background: white; 
                      box-shadow: 0 0 15px rgba(0,0,0,0.1); border-radius: 5px; 
                      font-weight: bold; font-size: 14px;">', title, '</div>'),
        position = "topright"
      )
  }
  
  # Add consistent styling to the map
  map %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    # Add fullscreen option
    addFullscreenControl() %>%
    # Add scale bar
    addScaleBar(position = "bottomleft") %>%
    # Set view to Ciudad Juárez (approximate)
    setView(lng = -106.4245, lat = 31.6904, zoom = 11)
}