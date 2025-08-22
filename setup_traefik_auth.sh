#!/bin/bash

# Traefik Authentication Setup Script
echo "=== Traefik Dashboard Authentication Setup ==="
echo ""

# Get server IP
read -p "Enter your server IP address: " SERVER_IP

# Get username
read -p "Enter username for Traefik dashboard: " USERNAME

# Get password (hidden input)
read -s -p "Enter password for Traefik dashboard: " PASSWORD
echo ""

# Generate password hash using htpasswd
PASSWORD_HASH=$(htpasswd -nbB $USERNAME $PASSWORD | cut -d ":" -f 2)

# Create .env file
cat > .env << EOF
# Traefik Configuration
ACME_EMAIL=your-email@example.com
DOMAIN=your-domain.com

# Traefik Dashboard Authentication
TRAEFIK_USER=$USERNAME
TRAEFIK_PASSWORD_HASH=$PASSWORD_HASH
EOF

# Update docker-compose.yml with the correct IP
sed -i "s/YOUR_SERVER_IP/$SERVER_IP/g" docker-compose.yml

echo ""
echo "=== Setup Complete ==="
echo "Environment variables have been saved to .env file"
echo "Docker-compose.yml has been updated with your server IP"
echo ""
echo "To start the services:"
echo "docker-compose up -d"
echo ""
echo "Access the dashboard at: http://$SERVER_IP:8080/dashboard/"
echo "Username: $USERNAME"
echo ""
echo "Note: Make sure to update ACME_EMAIL and DOMAIN in the .env file"
echo "if you want to use Let's Encrypt certificates." 