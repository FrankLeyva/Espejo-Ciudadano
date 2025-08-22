# Use rocker/shiny with R 4.5.0 as base image
FROM rocker/shiny:4.5.0

# Set maintainer
LABEL maintainer="f.leyva@planjuarez.org"
LABEL description="Espejo Ciudadano - Ciudad Juárez Dashboard"

# Set environment variables for better container behavior
ENV DEBIAN_FRONTEND=noninteractive
ENV SHINY_LOG_STDERR=1
ENV MYSQL_HOST=srv960.hstgr.io
ENV MYSQL_USER=u550512989_Frank
ENV MYSQL_PASSWORD=o^habiSrSQ7
ENV MYSQL_DB=u550512989_asivemosjuarez

# Install system dependencies required for sf and other geospatial packages
RUN apt-get update && apt-get install -y \
    # Essential system libraries
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    # Geospatial libraries (critical for sf package)
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gdal-bin \
    proj-bin \
    # Additional dependencies for s2 package (sf dependency)
    cmake \
    libabsl-dev \
    # Additional libraries for various R packages
    libv8-dev \
    libjq-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libmagick++-dev \
    libpoppler-cpp-dev \
    # For openxlsx and file operations
    libzip-dev \
    # For htmlwidgets and web technologies
    pandoc \
    # For system fonts and text shaping
    fonts-liberation \
    libharfbuzz-dev \
    libfribidi-dev \
    pkg-config \
    # Additional library for igraph
    libglpk-dev \
    # For health checks
    curl \
    # Clean up
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages directly (no renv complexity)
# FIXED: Added missing jsonlite and other commonly needed packages
RUN R -e "install.packages(c( \
    'tools', 'memoise', 'R6', 'jsonlite', \
    'shiny', 'bslib', 'htmltools', 'htmlwidgets', 'shinyjs', \
    'dplyr', 'tidyr', 'tidyverse', 'stringr', \
    'plotly', 'ggridges', 'RColorBrewer', 'viridisLite', \
    'openxlsx', 'DT', \
    'sf', 'leaflet', \
    'bsicons', 'colourpicker', 'igraph', 'pagedown', 'wordcloud2', 'reshape2', \
    'future', 'promises', 'later', 'magick', \
    'scales', 'lubridate', \
    'DBI', 'RSQLite', 'RMySQL', 'httr' \
), repos = 'https://cran.rstudio.com/', dependencies = TRUE)"

# Verify all critical packages are properly installed
# FIXED: Added jsonlite to verification and additional packages
RUN R -e " \
    critical_packages <- c('jsonlite', 'shiny', 'dplyr', 'plotly','pool', 'leaflet', 'sf', 'htmltools', 'htmlwidgets', 'bslib', 'DT', 'igraph', 'future', 'promises', 'later', 'R6', 'memoise'); \
    cat('Checking packages...\\n'); \
    for (pkg in critical_packages) { \
        if (!requireNamespace(pkg, quietly = TRUE)) { \
            stop(paste('Package', pkg, 'is not available')); \
        } else { \
            cat('✓', pkg, 'OK\\n'); \
        } \
    }; \
    cat('All critical packages verified!\\n'); \
    "

# Create app directory and set working directory
RUN mkdir -p /srv/shiny-server/espejo-ciudadano
WORKDIR /srv/shiny-server/espejo-ciudadano

# Copy application files
COPY app.R ./
COPY R/ ./R/
COPY www/ ./www/
COPY data/ ./data/

# Create necessary directories and set permissions
RUN mkdir -p /var/log/shiny-server \
    && mkdir -p /srv/shiny-server/espejo-ciudadano/logs \
    && chown -R shiny:shiny /srv/shiny-server/espejo-ciudadano \
    && chown -R shiny:shiny /var/log/shiny-server

# Create a custom shiny-server configuration for better performance
RUN echo 'run_as shiny; \
server { \
  listen 3838; \
  location / { \
    app_dir /srv/shiny-server/espejo-ciudadano; \
    log_dir /var/log/shiny-server; \
    app_init_timeout 60; \
    app_idle_timeout 300; \
  } \
}' > /etc/shiny-server/shiny-server.conf

# Health check to ensure the app starts properly
HEALTHCHECK --interval=30s --timeout=10s --start-period=90s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Expose the shiny port
EXPOSE 3838

# Switch to shiny user for security
USER shiny

# Run the Shiny server
CMD ["/usr/bin/shiny-server"]