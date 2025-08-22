# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Espejo Ciudadano** (Citizen Mirror) is a comprehensive R Shiny dashboard for Ciudad Juárez that visualizes citizen perception and participation data. The application presents survey results across multiple thematic areas including government, wellness, infrastructure, participation, and urban planning.

## Application Architecture

### Core Structure
- **Single-page Shiny application** with modular architecture
- **Modular design**: Each thematic area has paired `*_ui.R` and `*_server.R` files
- **Data-driven**: Uses pre-processed survey data stored as `.rds` files and geographic data in GeoJSON format
- **Analytics system**: Tracks user interactions and provides insights via SQLite database

### Main Entry Point
- `app.R`: Main application file that loads all dependencies, modules, and defines the UI/server structure

### Module Organization
```
R/
├── analytics.R              # Analytics system with AnalyticsManager class
├── data_loader.R             # Data loading utilities
├── data_manager.R            # Data management and caching
├── survey_config.R           # Survey configuration and mappings
├── utils.R                   # Utility functions
├── global_theme.R            # UI theme and styling
├── question_classifier.R     # Question type classification
├── background_preloader.R    # Background data preloading
├── themes_metadata.R         # Theme metadata configuration
├── reports_metadata.R        # Report metadata
├── *_module.R               # Data visualization modules by type
├── wellness/                # Wellness-related modules
├── government/              # Government-related modules  
├── infrastructure/          # Infrastructure-related modules
├── participation/           # Participation-related modules
├── urban/                   # Urban planning modules
└── extras/                  # Additional features (reports, explorer, etc.)
```

### Data Structure
- `data/processed/`: Survey response data in CSV format
- `data/plots/`: Pre-generated plot objects stored as `.rds` files
- `data/percentages/`: Calculated percentages stored as `.rds` files
- `data/geo/`: Geographic data (GeoJSON files for maps)
- `data/maps/`: Pre-generated map images
- `data/analytics.db`: SQLite database for analytics tracking

## Development Commands

### Docker Development
```bash
# Build the Docker image
docker-compose build espejo-ciudadano

# Run the application with Docker
docker-compose up -d

# View logs
docker-compose logs -f espejo-ciudadano

# Stop services
docker-compose down
```

### Deployment
```bash
# Full deployment with interactive setup
./deploy.sh

# Update existing deployment
./deploy.sh --update

# Check deployment status
./deploy.sh --status

# View application logs
./deploy.sh --logs

# Clean up Docker resources
./deploy.sh --cleanup
```

### Local Development
```r
# Install dependencies (using renv)
renv::restore()

# Run the application locally
shiny::runApp(port = 3838)

# Test analytics system
source("debug_analytics.R")
```

## Key Configuration Files

### Survey Configuration (`R/survey_config.R`)
Contains mappings for different survey years and types:
- Column mappings for demographics (district, gender, age)
- Value mappings for categorical responses
- Survey-specific configurations for PER_2023, PER_2024, PAR_2023, PAR_2024

### Docker Configuration
- `Dockerfile`: Multi-stage R Shiny application container
- `docker-compose.yml`: Complete stack with Traefik reverse proxy and SSL
- Environment variables configured via `.env` file

## Data Processing Pipeline

1. **Raw Data**: Excel/SPSS files in `data/raw/`
2. **Processing**: Data cleaning and classification (external process)
3. **Processed Data**: Clean CSV files in `data/processed/`
4. **Visualization**: Pre-computed plots and percentages stored as `.rds` files
5. **Presentation**: Modular UI components present the processed data

## Analytics System

The application includes a comprehensive analytics system accessible via multiple hidden triggers:
- **Keyboard**: `Ctrl+Shift+A`
- **UI**: Triple-click "Vista Rápida" title
- **Console**: `showAnalytics()` function
- Database: SQLite at `data/analytics.db`

### Analytics Features
- Session tracking with duration analysis
- Section engagement metrics
- User interaction logging
- Daily usage statistics
- Data export capabilities

## Common Development Patterns

### Adding New Thematic Modules
1. Create `R/theme_name/theme_name_ui.R` for UI components
2. Create `R/theme_name/theme_name_server.R` for server logic
3. Add navigation item to main navbar in `app.R`
4. Source the new modules in `app.R`
5. Add corresponding plot data files in `data/plots/`

### Data Integration
- Survey data follows standardized column naming (Q2 for district, Q101/Q144 for gender)
- Demographic filters are handled centrally via `survey_config.R`
- Geographic integration uses district-level aggregation
- All visualizations use consistent color schemes from `global_theme.R`

## Performance Considerations

- **Caching**: Extensive use of `memoise` for expensive operations
- **Background Loading**: Pre-loads data asynchronously via `background_preloader.R`
- **Data Management**: Efficient data loading with `data_manager.R`
- **Docker Optimization**: Multi-core processing limited to 4 cores in containers

## Security & Privacy

- Analytics data is anonymized (no PII collected)
- Local SQLite storage only
- Docker security headers configured in Traefik
- SSL/TLS termination via Let's Encrypt

## Troubleshooting

### Common Issues
- **Package Dependencies**: Use `renv::restore()` to ensure consistent package versions
- **Data Loading Errors**: Check file paths in `data/` directory and verify `.rds` file integrity
- **Analytics Issues**: Verify SQLite database permissions and RSQLite package installation
- **Docker Issues**: Check environment variables in `.env` file and container logs

### Development Tips
- Use `debug_analytics.R` to test analytics functionality
- Monitor application logs in `logs/` directory
- Check Docker container health with `docker-compose ps`
- For SSL issues, verify domain DNS configuration and Let's Encrypt certificate generation