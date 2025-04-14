
theme_config <- list(
  # Base colors
  colors = list(
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
    neutral = "#DEE2E6"
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
      primary = "#1E88E5",
      secondary = "#90CAF9",
      accent = "#FFA000",
      palettes = list(
        sequential = c("#BFD3E6", "#9EBCDA", "#8C96C6", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
        categorical = c("#1E88E5", "#42A5F5", "#90CAF9", "#FFA000", "#FFCA28", "#FFE082"),
        diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#1E88E5"))(9)
      )
    ),
    movilidad = list(
      primary = "#43A047",
      secondary = "#A5D6A7",
      accent = "#052F5F",
      palettes = list(
        sequential = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
        categorical =c("#43A047", "#66BB6A", "#A5D6A7", "#052F5F", "#2D4A70", "#5788A0"),
        diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#43A047"))(9)
      )
    ),
    gobierno = list(
      primary = "#5E35B1",
      secondary = "#B39DDB",
      accent = "#FB8C00",
      palettes = list(
        sequential = c("#CAB2D6", "#9E9AC8", "#756BB1", "#5E35B1", "#54278F", "#41236C", "#3A1E61"),
        categorical = c("#5E35B1", "#7E57C2", "#B39DDB", "#FB8C00", "#FFA726", "#FFCC80"),
        diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#5E35B1"))(9)
      )
    ),
    infraestructura = list(
      primary = "#F57C00",  # Changed from E64A19 to be more distinct from danger color
      secondary = "#FFCC80",
      accent = "#039BE5",
      palettes = list(
        sequential = c("#FBB582", "#FA954F", "#F57C00", "#E66D00", "#D45F00", "#B65200", "#8F3F00"),
        categorical = c("#F57C00", "#FB8C00", "#FFCC80", "#039BE5", "#29B6F6", "#81D4FA"),
        diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#F57C00"))(9)
      )
    ),
    participacion = list(
      primary = "#8E24AA",
      secondary = "#CE93D8",
      accent = "#00ACC1",
      palettes = list(
        sequential = c("#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#8E24AA", "#980043", "#67001F"),
        categorical = c("#8E24AA", "#AB47BC", "#CE93D8", "#00ACC1", "#26C6DA", "#80DEEA"),
        diverging = colorRampPalette(c("#DC3545", "#DEE2E6", "#8E24AA"))(9)
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
    full_theme$colors$accent <- section_theme$accent
    
    # Override palettes if present in section theme
    if (!is.null(section_theme$palettes)) {
      if (!is.null(section_theme$palettes$sequential)) {
        full_theme$palettes$sequential <- section_theme$palettes$sequential
      }
      if (!is.null(section_theme$palettes$diverging)) {
        full_theme$palettes$diverging <- section_theme$palettes$diverging
      }
      if (!is.null(section_theme$palettes$categorical)) {
        full_theme$palettes$categorical <- section_theme$palettes$categorical
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
                                  "hoverClosestCartesian", "hoverCompareCartesian",
                                  "hoverClosestPie", "toImage"),  # Remove default toImage
        modeBarButtonsToAdd = list(
          list(
            name = "customDownload",
            title = "Descargar Imagen",
            icon = list(
              path ="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708z  M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5",
              width =16,
              height = 16,
              viewBox = "0 0 16 16"
            ),
            click = htmlwidgets::JS("function(gd) { window.customPlotDownload(gd); }")
          )
        ),
        displaylogo= FALSE,
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


