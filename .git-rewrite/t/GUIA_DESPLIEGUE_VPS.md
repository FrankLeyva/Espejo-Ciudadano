# Guía: Desplegar Dashboard en VPS

## Introducción

Esta guía explica paso a paso cómo desplegar el Dashboard Espejo Ciudadano en un VPS (Virtual Private Server). Está dirigida a principiantes que necesitan subir el dashboard a un servidor de producción usando Docker.

## Información del Servidor

- **IP del VPS**: `45.90.208.220`
- **Dominio**: `asiestamosjuarez.org`
- **Puerto HTTP**: `80` (redirige a HTTPS)
- **Puerto HTTPS**: `443`
- **Puerto Dashboard Traefik**: `8080`

## Arquitectura del Despliegue

```
Internet → Traefik (Proxy) → Dashboard Shiny
     ↓
SSL/HTTPS → Certificados Let's Encrypt
```

### Componentes:
- **Traefik**: Proxy reverso con SSL automático
- **Dashboard**: Aplicación R Shiny
- **Docker**: Contenedores para aislamiento
- **Let's Encrypt**: Certificados SSL gratuitos

## Pre-requisitos

### En tu Máquina Local:
- [x] Docker Desktop instalado
- [x] Git (para clonar el repositorio)
- [x] Terminal/PowerShell
- [x] Acceso SSH al VPS

### En el VPS:
- [x] Ubuntu/Debian Linux
- [x] Docker y Docker Compose instalados
- [x] Puerto 80, 443, 8080 abiertos en firewall
- [x] Dominio apuntando a la IP del VPS

## Método 1: Usando Imagen Docker (Recomendado)

### 🚀 **Paso 1: Construir la Imagen Localmente**

#### 1.1. Preparar el proyecto
```bash
# Navegar al directorio del proyecto
cd /path/to/Espejo-Ciudadano

# Verificar que tienes todos los archivos necesarios
ls -la
# Deberías ver: Dockerfile, docker-compose.yml, app.R, R/, data/, www/
```

#### 1.2. Construir la imagen Docker
```bash
# Construir la imagen (esto puede tomar 10-20 minutos)
docker build -t espejo-ciudadano:latest .

# Verificar que la imagen se creó correctamente
docker images | grep espejo-ciudadano
```

#### 1.3. Probar la imagen localmente (opcional)
```bash
# Ejecutar contenedor de prueba
docker run -d -p 3838:3838 --name test-espejo espejo-ciudadano:latest

# Verificar en navegador: http://localhost:3838
# Si funciona, detener el contenedor de prueba
docker stop test-espejo
docker rm test-espejo
```

### 🗜️ **Paso 2: Exportar la Imagen a TAR**

```bash
# Exportar la imagen a un archivo TAR
docker save espejo-ciudadano:latest -o espejo-ciudadano.tar

# Verificar el tamaño del archivo (debería ser ~2-4 GB)
ls -lh espejo-ciudadano.tar
```

### 📤 **Paso 3: Subir al VPS**

#### 3.1. Usando SCP (Secure Copy)
```bash
# Subir el archivo TAR al VPS
scp espejo-ciudadano.tar usuario@45.90.208.220:/home/usuario/

# Ejemplo específico (reemplaza 'usuario' con tu usuario real)
scp espejo-ciudadano.tar root@45.90.208.220:/root/
```

#### 3.2. Usando SFTP (alternativo)
```bash
# Conectar por SFTP
sftp usuario@45.90.208.220

# Una vez conectado:
put espejo-ciudadano.tar
bye
```

#### 3.3. Subir archivos de configuración
```bash
# También subir los archivos de configuración
scp docker-compose.yml usuario@45.90.208.220:/home/usuario/
scp .env usuario@45.90.208.220:/home/usuario/
```

### 🖥️ **Paso 4: Configurar en el VPS**

#### 4.1. Conectarse al VPS
```bash
# Conectar por SSH
ssh usuario@45.90.208.220

# O si usas clave privada:
ssh -i /path/to/private-key usuario@45.90.208.220
```

#### 4.2. Preparar el entorno
```bash
# Actualizar el sistema
sudo apt update && sudo apt upgrade -y

# Instalar Docker si no está instalado
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Instalar Docker Compose
sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# Agregar usuario al grupo docker (opcional, pero recomendado)
sudo usermod -aG docker $USER
```

#### 4.3. Cargar la imagen Docker
```bash
# Cargar la imagen desde el archivo TAR
docker load -i espejo-ciudadano.tar

# Verificar que se cargó correctamente
docker images | grep espejo-ciudadano
```

#### 4.4. Configurar variables de entorno
```bash
# Editar el archivo .env
nano .env
```

Asegúrate de que contenga:
```env
DOMAIN=asiestamosjuarez.org
ACME_EMAIL=f.leyva@planjuarez.org
TRAEFIK_LOG_LEVEL=INFO
TRAEFIK_USER=admin
TRAEFIK_PASSWORD_HASH=$2y$10$2lB9Y5I/fSvJb4Q4X9Y5E.JZQ1oQ4Q4Q4Q4Q4Q4Q4Q4Q4Q
```

### 🚢 **Paso 5: Desplegar con Docker Compose**

#### 5.1. Crear directorios necesarios
```bash
# Crear directorios para logs y datos
mkdir -p logs
chmod 755 logs
```

#### 5.2. Ejecutar el stack completo
```bash
# Iniciar todos los servicios
docker-compose up -d

# Verificar que los contenedores están funcionando
docker-compose ps
```

#### 5.3. Verificar logs
```bash
# Ver logs del dashboard
docker-compose logs -f espejo-ciudadano

# Ver logs de Traefik
docker-compose logs -f traefik

# Ver logs de todos los servicios
docker-compose logs -f
```

### ✅ **Paso 6: Verificar el Despliegue**

#### 6.1. Verificar servicios
```bash
# Verificar que los contenedores están corriendo
docker ps

# Verificar conectividad interna
docker exec espejo-dashboard curl -f http://localhost:3838/
```

#### 6.2. Probar acceso web
- **Dashboard principal**: https://asiestamosjuarez.org
- **Dashboard Traefik**: http://45.90.208.220:8080 (usuario: admin)
- **Verificar SSL**: https://www.ssllabs.com/ssltest/

#### 6.3. Verificar certificados SSL
```bash
# Ver certificados generados
docker exec espejo-traefik ls -la /data/
```

## Método 2: Construir Directamente en el VPS

### 🔄 **Alternativa: Git Clone + Build**

Si prefieres no usar archivos TAR:

#### 2.1. Clonar repositorio en VPS
```bash
# En el VPS, clonar el proyecto
git clone https://github.com/tu-usuario/Espejo-Ciudadano.git
cd Espejo-Ciudadano
```

#### 2.2. Construir en el VPS
```bash
# Construir la imagen directamente en el servidor
docker build -t espejo-ciudadano:latest .

# Desplegar
docker-compose up -d
```

**⚠️ Nota**: Este método requiere más recursos en el VPS y puede ser más lento.

## Configuración de Firewall

### Ubuntu/Debian (ufw)
```bash
# Habilitar firewall
sudo ufw enable

# Permitir puertos necesarios
sudo ufw allow 22    # SSH
sudo ufw allow 80    # HTTP
sudo ufw allow 443   # HTTPS
sudo ufw allow 8080  # Traefik Dashboard

# Verificar reglas
sudo ufw status
```

### CentOS/RHEL (firewalld)
```bash
# Abrir puertos
sudo firewall-cmd --permanent --add-port=80/tcp
sudo firewall-cmd --permanent --add-port=443/tcp
sudo firewall-cmd --permanent --add-port=8080/tcp
sudo firewall-cmd --reload
```

## Configuración DNS

### En tu proveedor de DNS:
```
Tipo    Nombre                  Valor
A       asiestamosjuarez.org   45.90.208.220
CNAME   www                    asiestamosjuarez.org
```

### Verificar propagación DNS:
```bash
# Verificar que el dominio apunta a la IP correcta
nslookup asiestamosjuarez.org

# O usar dig
dig asiestamosjuarez.org
```

## Comandos de Mantenimiento

### 🔄 **Actualizar el Dashboard**

#### Método 1: Con nueva imagen TAR
```bash
# En tu máquina local:
docker build -t espejo-ciudadano:latest .
docker save espejo-ciudadano:latest -o espejo-ciudadano-nueva.tar
scp espejo-ciudadano-nueva.tar usuario@45.90.208.220:/home/usuario/

# En el VPS:
docker-compose down
docker load -i espejo-ciudadano-nueva.tar
docker-compose up -d
```

#### Método 2: Con Git (si usas el método 2)
```bash
# En el VPS:
git pull origin main
docker-compose down
docker build -t espejo-ciudadano:latest .
docker-compose up -d
```

### 📊 **Monitoreo**

#### Ver estado de servicios
```bash
# Estado de contenedores
docker-compose ps

# Recursos utilizados
docker stats

# Espacio en disco
df -h
docker system df
```

#### Ver logs en tiempo real
```bash
# Logs del dashboard
docker-compose logs -f espejo-ciudadano

# Logs de acceso web
docker-compose logs -f traefik | grep "GET"
```

### 🧹 **Limpieza**

#### Limpiar recursos Docker
```bash
# Limpiar imágenes sin usar
docker image prune -f

# Limpiar contenedores detenidos
docker container prune -f

# Limpiar volúmenes no utilizados
docker volume prune -f

# Limpieza completa (cuidado!)
docker system prune -a -f
```

#### Rotar logs
```bash
# Limpiar logs de Docker
sudo sh -c 'echo "" > $(docker inspect --format="{{.LogPath}}" espejo-dashboard)'
sudo sh -c 'echo "" > $(docker inspect --format="{{.LogPath}}" espejo-traefik)'
```

## Backup y Restauración

### 📦 **Crear Backup**

#### Backup de datos
```bash
# Crear directorio de backup
mkdir -p ~/backups/$(date +%Y%m%d)

# Backup de configuración
cp docker-compose.yml ~/backups/$(date +%Y%m%d)/
cp .env ~/backups/$(date +%Y%m%d)/

# Backup de volúmenes Docker
docker run --rm -v espejo-traefik-data:/data -v ~/backups/$(date +%Y%m%d):/backup alpine tar czf /backup/traefik-data.tar.gz -C /data .
docker run --rm -v espejo-shiny-logs:/logs -v ~/backups/$(date +%Y%m%d):/backup alpine tar czf /backup/shiny-logs.tar.gz -C /logs .
```

#### Backup automatizado (script)
```bash
# Crear script de backup
cat > ~/backup-dashboard.sh << 'EOF'
#!/bin/bash
BACKUP_DIR=~/backups/$(date +%Y%m%d_%H%M%S)
mkdir -p $BACKUP_DIR

# Backup archivos de configuración
cp docker-compose.yml $BACKUP_DIR/
cp .env $BACKUP_DIR/

# Backup volúmenes
docker run --rm -v espejo-traefik-data:/data -v $BACKUP_DIR:/backup alpine tar czf /backup/traefik-data.tar.gz -C /data .
docker run --rm -v espejo-shiny-logs:/logs -v $BACKUP_DIR:/backup alpine tar czf /backup/shiny-logs.tar.gz -C /logs .

echo "Backup creado en: $BACKUP_DIR"
EOF

chmod +x ~/backup-dashboard.sh

# Programar backup diario en crontab
echo "0 2 * * * ~/backup-dashboard.sh" | crontab -
```

### 🔄 **Restaurar Backup**

```bash
# Detener servicios
docker-compose down

# Restaurar volúmenes
docker run --rm -v espejo-traefik-data:/data -v ~/backups/20241201:/backup alpine tar xzf /backup/traefik-data.tar.gz -C /data
docker run --rm -v espejo-shiny-logs:/logs -v ~/backups/20241201:/backup alpine tar xzf /backup/shiny-logs.tar.gz -C /logs

# Restaurar configuración
cp ~/backups/20241201/docker-compose.yml .
cp ~/backups/20241201/.env .

# Reiniciar servicios
docker-compose up -d
```

## Troubleshooting

### 🚨 **Problemas Comunes**

#### 1. "Container failed to start"
```bash
# Ver logs detallados
docker-compose logs espejo-ciudadano

# Verificar que la imagen se cargó correctamente
docker images | grep espejo-ciudadano

# Verificar espacio en disco
df -h

# Verificar recursos del sistema
free -h
```

#### 2. "SSL Certificate not working"
```bash
# Verificar logs de Traefik
docker-compose logs traefik | grep -i certificate

# Verificar que el dominio apunta a la IP correcta
nslookup asiestamosjuarez.org

# Forzar renovación de certificado
docker-compose exec traefik rm /data/acme.json
docker-compose restart traefik
```

#### 3. "Dashboard not accessible"
```bash
# Verificar que el contenedor está corriendo
docker ps | grep espejo

# Probar conectividad interna
docker exec espejo-dashboard curl -f http://localhost:3838/

# Verificar configuración de proxy
docker-compose logs traefik | grep -i error
```

#### 4. "Out of disk space"
```bash
# Verificar espacio
df -h

# Limpiar Docker
docker system prune -a -f

# Limpiar logs antiguos
find /var/lib/docker/containers/ -name "*.log" -exec truncate -s 0 {} \;
```

### 🔧 **Comandos de Diagnóstico**

#### Verificación completa del sistema
```bash
# Script de diagnóstico
cat > ~/diagnose-dashboard.sh << 'EOF'
#!/bin/bash
echo "=== DIAGNÓSTICO DASHBOARD ESPEJO CIUDADANO ==="
echo
echo "1. Estado de contenedores:"
docker ps

echo
echo "2. Uso de recursos:"
docker stats --no-stream

echo
echo "3. Espacio en disco:"
df -h

echo
echo "4. Memoria del sistema:"
free -h

echo
echo "5. Verificación de red:"
curl -s -o /dev/null -w "%{http_code}" http://localhost:3838/ || echo "No responde"

echo
echo "6. Certificados SSL:"
docker exec espejo-traefik ls -la /data/ 2>/dev/null || echo "No disponible"

echo
echo "7. Logs recientes:"
docker-compose logs --tail=10 espejo-ciudadano
EOF

chmod +x ~/diagnose-dashboard.sh
./diagnose-dashboard.sh
```

### 📞 **Obtener Ayuda**

#### Información del sistema para soporte
```bash
# Recopilar información del sistema
cat > ~/system-info.txt << EOF
Sistema: $(uname -a)
Docker: $(docker --version)
Docker Compose: $(docker-compose --version)
Espacio en disco: $(df -h /)
Memoria: $(free -h)
Estado de contenedores:
$(docker ps)
EOF

# Enviar esta información cuando solicites ayuda
cat ~/system-info.txt
```

## Scripts de Automatización

### 🚀 **Script de Despliegue Completo**

```bash
# Crear script de despliegue automatizado
cat > ~/deploy-complete.sh << 'EOF'
#!/bin/bash
set -e

echo "🚀 Iniciando despliegue del Dashboard Espejo Ciudadano..."

# Verificar que existe la imagen TAR
if [ ! -f "espejo-ciudadano.tar" ]; then
    echo "❌ Error: No se encuentra espejo-ciudadano.tar"
    exit 1
fi

# Detener servicios existentes
echo "⏹️  Deteniendo servicios existentes..."
docker-compose down 2>/dev/null || true

# Cargar nueva imagen
echo "📦 Cargando imagen Docker..."
docker load -i espejo-ciudadano.tar

# Limpiar imágenes antiguas
echo "🧹 Limpiando imágenes antiguas..."
docker image prune -f

# Iniciar servicios
echo "🚀 Iniciando servicios..."
docker-compose up -d

# Esperar a que los servicios estén listos
echo "⏳ Esperando a que los servicios estén listos..."
sleep 30

# Verificar estado
echo "✅ Verificando estado de servicios..."
docker-compose ps

echo "🎉 ¡Despliegue completado!"
echo "🌐 Dashboard disponible en: https://asiestamosjuarez.org"
echo "🔧 Panel de Traefik: http://45.90.208.220:8080"
EOF

chmod +x ~/deploy-complete.sh
```

### 📊 **Script de Monitoreo**

```bash
# Script de monitoreo continuo
cat > ~/monitor-dashboard.sh << 'EOF'
#!/bin/bash

while true; do
    clear
    echo "📊 MONITOR DASHBOARD ESPEJO CIUDADANO - $(date)"
    echo "=================================================="
    
    echo -e "\n🐳 Estado de Contenedores:"
    docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
    
    echo -e "\n💾 Uso de Recursos:"
    docker stats --no-stream --format "table {{.Name}}\t{{.CPUPerc}}\t{{.MemUsage}}"
    
    echo -e "\n💽 Espacio en Disco:"
    df -h / | grep -v "Filesystem"
    
    echo -e "\n🌐 Verificación de Conectividad:"
    if curl -s -f http://localhost:3838/ > /dev/null; then
        echo "✅ Dashboard: OK"
    else
        echo "❌ Dashboard: ERROR"
    fi
    
    echo -e "\n⏰ Próxima actualización en 30 segundos (Ctrl+C para salir)"
    sleep 30
done
EOF

chmod +x ~/monitor-dashboard.sh
```

---

## Resumen de URLs y Puertos

| Servicio | URL/Puerto | Descripción |
|----------|------------|-------------|
| Dashboard Principal | https://asiestamosjuarez.org | Aplicación principal |
| Dashboard HTTP | http://asiestamosjuarez.org | Redirige a HTTPS |
| Traefik Dashboard | http://45.90.208.220:8080 | Panel de administración |
| SSH | 45.90.208.220:22 | Acceso por terminal |

## Checklist de Despliegue

### ✅ **Pre-despliegue:**
- [ ] Imagen Docker construida localmente
- [ ] Archivo TAR generado y subido al VPS
- [ ] Archivos de configuración (.env, docker-compose.yml) subidos
- [ ] Acceso SSH al VPS funcionando
- [ ] Docker y Docker Compose instalados en VPS
- [ ] Puertos 80, 443, 8080 abiertos en firewall
- [ ] DNS configurado correctamente

### ✅ **Post-despliegue:**
- [ ] Contenedores corriendo sin errores
- [ ] Dashboard accesible vía HTTPS
- [ ] Certificado SSL funcionando
- [ ] Traefik dashboard accesible
- [ ] Logs sin errores críticos
- [ ] Backup inicial creado
- [ ] Monitoreo configurado

---
*Guía de Despliegue VPS - Dashboard Espejo Ciudadano*  
*Versión actualizada para despliegue en 45.90.208.220*