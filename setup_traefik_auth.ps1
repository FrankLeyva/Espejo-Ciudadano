# Traefik Authentication Setup Script for Windows
Write-Host "=== Traefik Dashboard Authentication Setup ===" -ForegroundColor Green
Write-Host ""

# Get server IP
$SERVER_IP = Read-Host "Enter your server IP address"

# Get username
$USERNAME = Read-Host "Enter username for Traefik dashboard"

# Get password (hidden input)
$PASSWORD = Read-Host "Enter password for Traefik dashboard" -AsSecureString
$PASSWORD_PLAIN = [Runtime.InteropServices.Marshal]::PtrToStringAuto([Runtime.InteropServices.Marshal]::SecureStringToBSTR($PASSWORD))

# Generate password hash using htpasswd (you'll need to install htpasswd or use online generator)
Write-Host "Generating password hash..." -ForegroundColor Yellow
Write-Host "Note: You may need to install htpasswd or use an online generator" -ForegroundColor Yellow
Write-Host "For now, you can use an online htpasswd generator:" -ForegroundColor Yellow
Write-Host "https://hostingcanada.org/htpasswd-generator/" -ForegroundColor Cyan
Write-Host ""

# Create .env file
$ENV_CONTENT = @"
# Traefik Configuration
ACME_EMAIL=your-email@example.com
DOMAIN=your-domain.com

# Traefik Dashboard Authentication
TRAEFIK_USER=$USERNAME
TRAEFIK_PASSWORD_HASH=REPLACE_WITH_GENERATED_HASH
"@

$ENV_CONTENT | Out-File -FilePath ".env" -Encoding UTF8

# Update docker-compose.yml with the correct IP
$DOCKER_COMPOSE_CONTENT = Get-Content "docker-compose.yml" -Raw
$DOCKER_COMPOSE_CONTENT = $DOCKER_COMPOSE_CONTENT -replace "YOUR_SERVER_IP", $SERVER_IP
$DOCKER_COMPOSE_CONTENT | Out-File -FilePath "docker-compose.yml" -Encoding UTF8

Write-Host "=== Setup Complete ===" -ForegroundColor Green
Write-Host "Environment variables have been saved to .env file" -ForegroundColor Yellow
Write-Host "Docker-compose.yml has been updated with your server IP" -ForegroundColor Yellow
Write-Host ""
Write-Host "IMPORTANT: You need to manually update the TRAEFIK_PASSWORD_HASH in .env file" -ForegroundColor Red
Write-Host "Use an online htpasswd generator with username: $USERNAME and your password" -ForegroundColor Red
Write-Host ""
Write-Host "To start the services:" -ForegroundColor Yellow
Write-Host "docker-compose up -d" -ForegroundColor Cyan
Write-Host ""
Write-Host "Access the dashboard at: http://$SERVER_IP`:8080/dashboard/" -ForegroundColor Cyan
Write-Host "Username: $USERNAME" -ForegroundColor Cyan
Write-Host ""
Write-Host "Note: Make sure to update ACME_EMAIL and DOMAIN in the .env file" -ForegroundColor Yellow
Write-Host "if you want to use Let's Encrypt certificates." -ForegroundColor Yellow 