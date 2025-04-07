# Enhanced theme_config with a more cohesive color palette and section-based themes
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
    text = "#212529",          # Dark text
    neutral = "#DEE2E6"        # Neutral for dividers, borders, etc.
  ),
  
  # Categorical palettes - coordinated colors
  palettes = list(
    district = c("#88BDBC", "#6E9887", "#BECC92", "#FDD692", "#F1BB87", 
                "#F28A80", "#D1A5C6", "#9CADCE", "#B6C5D1", "#D3D9E0"),
    gender = c("#FF69B4", "#4169E1"),
    age_group = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"),
    sequential = colorRampPalette(c("#F8F9FA", "#212529"))(9),
    diverging = colorRampPalette(c("#E76F51", "#F8F9FA", "#0D6EFD"))(11)
  ),
  
  # Enhanced typography
  typography = list(
    font_family = "'Montserrat', 'Segoe UI', Arial, sans-serif",
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
  ),
  
  # Section-specific themes
  sections = list(
    bienestar = list(
      primary = "#006D77",
      secondary = "#83C5BE",
      accent = "#006D77",
      palettes = list(
        sequential = c("#B2D8D8", "#88C1C1", "#5EAAAA", "#349393", "#007C7C"),
        diverging = colorRampPalette(c("#E76F51", "#F8F9FA", "#006D77"))(11),
        categorical = c("#006D77", "#83C5BE", "#EDF6F9", "#FFDDD2", "#E29578", "#006466")
      )
    ),
    movilidad = list(
      primary = "#2A9D8F", 
      secondary = "#80CBC4",
      accent = "#2A9D8F",
      palettes = list(
        sequential = colorRampPalette(c("#F8F9FA", "#2A9D8F"))(9),
        diverging = colorRampPalette(c("#E76F51", "#F8F9FA", "#2A9D8F"))(11),
        categorical= c("#2A9D8F", "#70D6BC", "#B5E2D8", "#F6BD60", "#F7EDE2", "#F5CAC3")
      )
    ),
    gobierno = list(
      primary = "#6969B3",
      secondary = "#B39DDB",
      accent = "#6969B3",
      palettes = list(
        sequential = colorRampPalette(c("#F8F9FA", "#6969B3"))(9),
        diverging = colorRampPalette(c("#E76F51", "#F8F9FA", "#6969B3"))(11),
        categorical = c("#6969B3", "#A06CD5", "#B7C0EE", "#FDA4BA", "#FEE7EC", "#8E94F2")
      )
    ),
    infraestructura = list(
      primary = "#F4A261",
      secondary = "#FFCC80",
      accent = "#F4A261",
      palettes = list(
        sequential = colorRampPalette(c("#F8F9FA", "#F4A261"))(9),
        diverging = colorRampPalette(c("#E76F51", "#F8F9FA", "#F4A261"))(11),
        categorical = c("#F4A261", "#F8BC8B", "#FDCFA4", "#B98B73", "#AACDBE", "#738580")
      )
    ),
    participacion = list(
      primary = "#E76F51",
      secondary = "#FFAB91",
      accent = "#E76F51",
      palettes = list(
        sequential = colorRampPalette(c("#F8F9FA", "#E76F51"))(9),
        diverging = colorRampPalette(c("#6969B3", "#F8F9FA", "#E76F51"))(11),
        categorical =c("#E76F51", "#FFA987", "#F9DCC4", "#C8D5B9", "#8FC0A9", "#68B0AB")
      )
    )
  )
)

#' Function to get the current section theme
#' @param section_name name of the section (bienestar, movilidad, etc.)
#' @return section theme list or default theme if section not found
get_section_theme <- function(section_name = NULL) {
  # If no section provided, return default
  if (is.null(section_name)) {
    return(theme_config)
  }
  
  section_name <- tolower(section_name)
  if (section_name %in% names(theme_config$sections)) {
    # Create a merged theme with section-specific overrides
    section_theme <- theme_config$sections[[section_name]]
    
    # Create a full theme by merging the section theme with the base theme
    full_theme <- theme_config
    full_theme$colors$primary <- section_theme$primary
    full_theme$colors$secondary <- section_theme$secondary
    full_theme$colors$highlight <- section_theme$accent
    
    # Override palettes if present in section theme
    if (!is.null(section_theme$palettes)) {
      if (!is.null(section_theme$palettes$sequential)) {
        full_theme$palettes$sequential <- section_theme$palettes$sequential
      }
      if (!is.null(section_theme$palettes$diverging)) {
        full_theme$palettes$diverging <- section_theme$palettes$diverging
      }
    }
    
    return(full_theme)
  } else {
    return(theme_config)
  }
}

#' Create consistent plotly theme
#' @param p plotly object
#' @param title plot title
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param custom_theme optional custom theme to override defaults
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
        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", 
                                   "zoomIn2d", "zoomOut2d", "autoScale2d", 
                                   "hoverClosestCartesian", "hoverCompareCartesian","hoverClosestPie"),
        modeBarButtonsToAdd = c("resetScale2d", "toImage"),
        displaylogo=FALSE,
        locale = "es",
        responsive = TRUE
    )
}

#' Enhanced color palette function with section awareness
#' @param palette_name name of the palette from theme_config$palettes
#' @param reverse boolean to reverse the palette
#' @param section_name optional section name for section-specific palettes
#' @export
get_color_palette <- function(palette_name, reverse = FALSE, section_name = NULL) {
  # Get the appropriate theme
  theme <- if(!is.null(section_name)) get_section_theme(section_name) else theme_config
  
  # Get the palette
  palette <- theme$palettes[[palette_name]]
  if(is.null(palette)) {
    # Fall back to default theme if not found in section
    palette <- theme_config$palettes[[palette_name]]
  }
  
  if(reverse) palette <- rev(palette)
  return(palette)
}

#' Apply a color strategy based on the data and visualization type
#' @param data data being visualized
#' @param viz_type type of visualization
#' @param section_name section name for section-specific colors
#' @param color_strategy strategy for applying colors ("section", "data", "category")
#' @export
apply_color_strategy <- function(data, viz_type = "bar", section_name = NULL, 
                                color_strategy = "section", group_var = NULL) {
  # Get the appropriate theme
  theme <- if(!is.null(section_name)) get_section_theme(section_name) else theme_config
  
  if (color_strategy == "section") {
    # Use primary color from section
    return(theme$colors$primary)
  } else if (color_strategy == "data") {
    # Different colors based on visualization type
    if (viz_type == "bar") {
      return(theme$colors$primary)
    } else if (viz_type == "line") {
      return(theme$colors$info)
    } else if (viz_type == "scatter") {
      return(theme$colors$success)
    } else {
      return(theme$colors$primary)
    }
  } else if (color_strategy == "category" && !is.null(group_var)) {
    # For categorical data, get the appropriate palette
    if (group_var == "district") {
      return(theme$palettes$district)
    } else if (group_var == "gender") {
      return(theme$palettes$gender)
    } else if (group_var == "age_group") {
      return(theme$palettes$age_group)
    } else {
      # Create a color scale based on unique values in the group variable
      unique_values <- unique(data[[group_var]])
      num_colors <- length(unique_values)
      
      return(colorRampPalette(c(theme$colors$primary, theme$colors$highlight))(num_colors))
    }
  } else {
    # Default to primary color
    return(theme$colors$primary)
  }
}

plot_functions <- list(
  # Bar plot with dynamic color handling
  bar = function(data, x, y, title = "", xlab = "", ylab = "", 
                orientation = "v", color_by = NULL, group_var = NULL, 
                section_name = NULL, custom_theme = NULL) {
    # Determine which theme to use
    active_theme <- if (!is.null(custom_theme)) custom_theme else get_section_theme(section_name)
    
    # Determine colors based on grouping variable
    if (!is.null(color_by) && color_by %in% names(active_theme$palettes)) {
      colors <- active_theme$palettes[[color_by]]
    } else {
      colors <- active_theme$colors$primary
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
    
    apply_plotly_theme(p, title = title, xlab = xlab, ylab = ylab, custom_theme = active_theme)
  },
  
  # Line plot with dynamic color handling
  line = function(data, x, y, title = "", xlab = "", ylab = "", 
                 color_by = NULL, group_var = NULL, section_name = NULL, custom_theme = NULL) {
    # Determine which theme to use
    active_theme <- if (!is.null(custom_theme)) custom_theme else get_section_theme(section_name)
    
    # Determine colors based on grouping variable
    if (!is.null(color_by) && color_by %in% names(active_theme$palettes)) {
      colors <- active_theme$palettes[[color_by]]
    } else {
      colors <- active_theme$colors$primary
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
    
    apply_plotly_theme(p, title = title, xlab = xlab, ylab = ylab, custom_theme = active_theme)
  },
  
  # Box plot with dynamic color handling
  box = function(data, x, y, title = "", xlab = "", ylab = "", 
                color_by = NULL, group_var = NULL, section_name = NULL, custom_theme = NULL) {
    # Determine which theme to use
    active_theme <- if (!is.null(custom_theme)) custom_theme else get_section_theme(section_name)
    
    # Determine colors based on grouping variable
    if (!is.null(color_by) && color_by %in% names(active_theme$palettes)) {
      colors <- active_theme$palettes[[color_by]]
    } else {
      colors <- active_theme$colors$primary
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
    
    apply_plotly_theme(p, title = title, xlab = xlab, ylab = ylab, custom_theme = active_theme)
  }
)

# Enhanced function to apply custom theme to any plotly object
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
      config(
        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", 
                                   "zoomIn2d", "zoomOut2d", "autoScale2d", 
                                   "hoverClosestCartesian", "hoverCompareCartesian","hoverClosestPie"),
        modeBarButtonsToAdd = c("resetScale2d", "toImage"),
        displaylogo=FALSE,
        locale = "es",
        responsive = TRUE
    )
}

# Enhanced function for leaflet maps with theme support
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
    # Set view to Ciudad Juárez (approximate)
    setView(lng = -106.4245, lat = 31.6904, zoom = 11)
}