#!/bin/bash

# Espejo Ciudadano Dashboard Deployment Script
# This script automates the deployment process

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_header() {
    echo -e "${BLUE}================================${NC}"
    echo -e "${BLUE} Espejo Ciudadano Deployment${NC}"
    echo -e "${BLUE}================================${NC}"
}

# Check if required commands exist
check_dependencies() {
    print_status "Checking dependencies..."
    
    if ! command -v docker &> /dev/null; then
        print_error "Docker is not installed. Please install Docker first."
        exit 1
    fi
    
    if ! command -v docker-compose &> /dev/null; then
        print_error "Docker Compose is not installed. Please install Docker Compose first."
        exit 1
    fi
    
    print_status "Dependencies check passed ✓"
}

# Generate secure password hash
generate_password_hash() {
    local password=$1
    if command -v htpasswd &> /dev/null; then
        echo $(htpasswd -nbB admin "$password" | sed -e s/\\$/\\$\\$/g)
    else
        print_warning "htpasswd not found. Using default password hash."
        echo "admin:\$2y\$10\$2lB9Y5I/fSvJb4Q4X9Y5E.JZQ1oQ4Q4Q4Q4Q4Q4Q4Q4Q4Q"
    fi
}

# Setup environment file
setup_environment() {
    print_status "Setting up environment configuration..."
    
    if [ ! -f .env ]; then
        if [ -f .env.template ]; then
            cp .env.template .env
            print_status "Created .env file from template"
        else
            print_error ".env.template not found. Creating basic .env file."
            cat > .env << EOF
DOMAIN=localhost
ACME_EMAIL=admin@localhost
TRAEFIK_LOG_LEVEL=INFO
TRAEFIK_AUTH=admin:\$2y\$10\$2lB9Y5I/fSvJb4Q4X9Y5E.JZQ1oQ4Q4Q4Q4Q4Q4Q4Q4Q4Q
EOF
        fi
    else
        print_status ".env file already exists"
    fi
    
    # Interactive configuration
    read -p "Enter your domain name (e.g., dashboard.yourdomain.com): " domain
    if [ ! -z "$domain" ]; then
        sed -i "s/DOMAIN=.*/DOMAIN=$domain/" .env
    fi
    
    read -p "Enter your email for SSL certificates: " email
    if [ ! -z "$email" ]; then
        sed -i "s/ACME_EMAIL=.*/ACME_EMAIL=$email/" .env
    fi
    
    # Generate secure password
    read -s -p "Enter password for Traefik dashboard (leave empty for default): " traefik_password
    echo
    if [ ! -z "$traefik_password" ]; then
        password_hash=$(generate_password_hash "$traefik_password")
        sed -i "s|TRAEFIK_AUTH=.*|TRAEFIK_AUTH=$password_hash|" .env
        print_status "Generated secure password hash for Traefik dashboard"
    fi
}

# Verify configuration
verify_config() {
    print_status "Verifying configuration..."
    
    if [ ! -f docker-compose.yml ]; then
        print_error "docker-compose.yml not found!"
        exit 1
    fi
    
    if [ ! -f Dockerfile ]; then
        print_error "Dockerfile not found!"
        exit 1
    fi
    
    # Check if data directory exists
    if [ ! -d data ]; then
        print_warning "data directory not found. Creating empty data directory."
        mkdir -p data
    fi
    
    print_status "Configuration verification passed ✓"
}

# Deploy services
deploy_services() {
    print_status "Building and deploying services..."
    
    # Pull latest images
    print_status "Pulling latest base images..."
    docker-compose pull traefik
    
    # Build application
    print_status "Building Espejo Ciudadano application..."
    docker-compose build espejo-ciudadano
    
    # Start services
    print_status "Starting services..."
    docker-compose up -d
    
    # Wait for services to be ready
    print_status "Waiting for services to start..."
    sleep 30
    
    # Check service status
    print_status "Checking service health..."
    docker-compose ps
}

# Verify deployment
verify_deployment() {
    print_status "Verifying deployment..."
    
    # Check if containers are running
    if ! docker-compose ps | grep -q "Up"; then
        print_error "Some services are not running properly!"
        docker-compose logs
        exit 1
    fi
    
    # Try to access the application
    local domain=$(grep DOMAIN .env | cut -d'=' -f2)
    if [ "$domain" = "localhost" ]; then
        if curl -f -s http://localhost/espejo-ciudadano/ > /dev/null; then
            print_status "Application is accessible via HTTP ✓"
        else
            print_warning "Application not yet accessible via HTTP"
        fi
    else
        print_status "Application should be accessible at https://$domain"
        print_status "Please ensure DNS is configured properly"
    fi
}

# Show deployment summary
show_summary() {
    local domain=$(grep DOMAIN .env | cut -d'=' -f2)
    
    echo
    print_header
    print_status "Deployment completed successfully!"
    echo
    echo -e "${BLUE}Access URLs:${NC}"
    echo -e "  Dashboard: ${GREEN}https://$domain${NC}"
    echo -e "  Traefik:   ${GREEN}https://traefik.$domain${NC} (if enabled)"
    echo
    echo -e "${BLUE}Useful Commands:${NC}"
    echo -e "  View logs:     ${YELLOW}docker-compose logs -f${NC}"
    echo -e "  Stop services: ${YELLOW}docker-compose down${NC}"
    echo -e "  Restart:       ${YELLOW}docker-compose restart${NC}"
    echo -e "  Update:        ${YELLOW}./deploy.sh --update${NC}"
    echo
    print_status "For more information, see DEPLOYMENT.md"
}

# Update deployment
update_deployment() {
    print_status "Updating deployment..."
    
    # Pull latest code (if git repo)
    if [ -d .git ]; then
        print_status "Pulling latest code..."
        git pull
    fi
    
    # Rebuild and restart
    docker-compose build espejo-ciudadano
    docker-compose up -d espejo-ciudadano
    
    print_status "Update completed!"
}

# Cleanup function
cleanup() {
    print_status "Cleaning up old containers and images..."
    docker system prune -f
    print_status "Cleanup completed!"
}

# Show help
show_help() {
    echo "Espejo Ciudadano Deployment Script"
    echo
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  --help       Show this help message"
    echo "  --update     Update existing deployment"
    echo "  --cleanup    Clean up old Docker resources"
    echo "  --logs       Show logs"
    echo "  --stop       Stop all services"
    echo "  --status     Show service status"
    echo
    echo "Default: Run full deployment process"
}

# Main function
main() {
    case "${1:-}" in
        --help)
            show_help
            exit 0
            ;;
        --update)
            check_dependencies
            update_deployment
            exit 0
            ;;
        --cleanup)
            cleanup
            exit 0
            ;;
        --logs)
            docker-compose logs -f
            exit 0
            ;;
        --stop)
            print_status "Stopping services..."
            docker-compose down
            exit 0
            ;;
        --status)
            docker-compose ps
            exit 0
            ;;
        --*)
            print_error "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
    
    # Full deployment process
    print_header
    check_dependencies
    setup_environment
    verify_config
    deploy_services
    verify_deployment
    show_summary
}

# Run main function
main "$@"