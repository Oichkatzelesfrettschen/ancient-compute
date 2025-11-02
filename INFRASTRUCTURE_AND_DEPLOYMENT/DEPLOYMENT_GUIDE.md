# Ancient Compute - Deployment Guide

**Version**: 1.0
**Date**: November 2, 2025
**Status**: Production Deployment Reference
**Audience**: DevOps Engineers, System Administrators

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Environment Setup](#environment-setup)
3. [Local Development](#local-development)
4. [Docker Deployment](#docker-deployment)
5. [Kubernetes Deployment](#kubernetes-deployment)
6. [Database Setup](#database-setup)
7. [Monitoring & Observability](#monitoring--observability)
8. [Security Hardening](#security-hardening)
9. [Troubleshooting](#troubleshooting)

---

## Prerequisites

### Required Software

- **Docker**: 24.0+ with Docker Compose
- **Kubernetes**: 1.28+ (for production)
- **PostgreSQL**: 15+ (can use Docker)
- **Redis**: 7+ (can use Docker)
- **Node.js**: 18+ LTS (for frontend)
- **Python**: 3.11+ (for backend)

### System Requirements

**Development**:
- 8 GB RAM minimum
- 20 GB disk space
- 4 CPU cores

**Production**:
- 16 GB RAM minimum (32 GB recommended)
- 100 GB disk space
- 8 CPU cores (16 recommended)
- Load balancer (nginx, HAProxy, or cloud LB)

---

## Environment Setup

### 1. Clone Repository

```bash
git clone https://github.com/Oichkatzelesfrettschen/ancient-compute.git
cd ancient-compute
```

### 2. Create Environment Files

**Backend** (`backend/.env`):
```bash
# Application
APP_NAME=Ancient Compute
VERSION=1.0.0
ENVIRONMENT=production
DEBUG=false

# Server
HOST=0.0.0.0
PORT=8000

# Security
SECRET_KEY=your-strong-secret-key-here-min-32-chars
ALLOWED_ORIGINS=https://ancient-compute.com,https://www.ancient-compute.com

# Database
DATABASE_URL=postgresql://ancient:STRONG_PASSWORD@postgres:5432/ancient_compute
DB_POOL_SIZE=20
DB_MAX_OVERFLOW=10

# Redis
REDIS_URL=redis://redis:6379
REDIS_MAX_CONNECTIONS=50

# Language Services
LANGUAGE_SERVICE_TIMEOUT=30
MAX_EXECUTION_TIME=10
MAX_MEMORY_MB=256

# Logging
LOG_LEVEL=INFO
```

**Frontend** (`frontend/.env.production`):
```bash
PUBLIC_API_URL=https://api.ancient-compute.com
PUBLIC_WS_URL=wss://api.ancient-compute.com
NODE_ENV=production
```

### 3. Generate Secrets

```bash
# Generate SECRET_KEY
python3 -c "import secrets; print(secrets.token_urlsafe(32))"

# Generate database password
openssl rand -base64 32
```

---

## Local Development

### Using Docker Compose (Recommended)

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Stop services
docker-compose down

# Rebuild after code changes
docker-compose up -d --build
```

Services accessible at:
- Frontend: http://localhost:3000
- Backend API: http://localhost:8000
- API Docs: http://localhost:8000/docs
- PostgreSQL: localhost:5432
- Redis: localhost:6379

### Manual Setup

**Backend**:
```bash
cd backend
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
pip install -r requirements.txt
uvicorn src.main:app --reload --host 0.0.0.0 --port 8000
```

**Frontend**:
```bash
cd frontend
npm install
npm run dev
```

**Database**:
```bash
# Start PostgreSQL
docker run -d --name postgres \
  -e POSTGRES_USER=ancient \
  -e POSTGRES_PASSWORD=development_password \
  -e POSTGRES_DB=ancient_compute \
  -p 5432:5432 \
  postgres:15

# Run migrations
cd backend
alembic upgrade head
```

**Redis**:
```bash
docker run -d --name redis -p 6379:6379 redis:7-alpine
```

---

## Docker Deployment

### Build Images

```bash
# Backend
docker build -t ancient-compute/backend:1.0.0 -f backend/Dockerfile .

# Frontend
docker build -t ancient-compute/frontend:1.0.0 -f frontend/Dockerfile .

# Language services
docker build -t ancient-compute/c-service:1.0.0 -f services/c/Dockerfile services/c
docker build -t ancient-compute/python-service:1.0.0 -f services/python/Dockerfile services/python
docker build -t ancient-compute/haskell-service:1.0.0 -f services/haskell/Dockerfile services/haskell
```

### Production Docker Compose

**docker-compose.prod.yml**:
```yaml
version: '3.8'

services:
  postgres:
    image: postgres:15-alpine
    environment:
      POSTGRES_USER: ancient
      POSTGRES_PASSWORD_FILE: /run/secrets/db_password
      POSTGRES_DB: ancient_compute
    volumes:
      - postgres_data:/var/lib/postgresql/data
    secrets:
      - db_password
    networks:
      - backend
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U ancient"]
      interval: 10s
      timeout: 5s
      retries: 5

  redis:
    image: redis:7-alpine
    command: redis-server --requirepass ${REDIS_PASSWORD}
    volumes:
      - redis_data:/data
    networks:
      - backend
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 10s
      timeout: 5s
      retries: 5

  backend:
    image: ancient-compute/backend:1.0.0
    env_file:
      - backend/.env.production
    depends_on:
      postgres:
        condition: service_healthy
      redis:
        condition: service_healthy
    networks:
      - backend
      - frontend
    deploy:
      replicas: 3
      resources:
        limits:
          cpus: '2'
          memory: 2G
        reservations:
          cpus: '1'
          memory: 1G
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8000/health"]
      interval: 30s
      timeout: 10s
      retries: 3

  frontend:
    image: ancient-compute/frontend:1.0.0
    depends_on:
      - backend
    networks:
      - frontend
    deploy:
      replicas: 2
      resources:
        limits:
          cpus: '1'
          memory: 512M

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
      - ./ssl:/etc/nginx/ssl:ro
    depends_on:
      - backend
      - frontend
    networks:
      - frontend
    deploy:
      replicas: 1

  # Language services
  c-service:
    image: ancient-compute/c-service:1.0.0
    networks:
      - backend
    deploy:
      replicas: 2
      resources:
        limits:
          cpus: '1'
          memory: 512M

  python-service:
    image: ancient-compute/python-service:1.0.0
    networks:
      - backend
    deploy:
      replicas: 2
      resources:
        limits:
          cpus: '1'
          memory: 512M

  haskell-service:
    image: ancient-compute/haskell-service:1.0.0
    networks:
      - backend
    deploy:
      replicas: 2
      resources:
        limits:
          cpus: '1'
          memory: 1G

networks:
  backend:
    driver: bridge
  frontend:
    driver: bridge

volumes:
  postgres_data:
  redis_data:

secrets:
  db_password:
    file: ./secrets/db_password.txt
```

### Start Production Stack

```bash
docker-compose -f docker-compose.prod.yml up -d
```

---

## Kubernetes Deployment

### 1. Create Namespace

```bash
kubectl create namespace ancient-compute
```

### 2. Create Secrets

```bash
# Database credentials
kubectl create secret generic db-credentials \
  --from-literal=username=ancient \
  --from-literal=password=$(openssl rand -base64 32) \
  -n ancient-compute

# Application secrets
kubectl create secret generic app-secrets \
  --from-literal=secret-key=$(python3 -c "import secrets; print(secrets.token_urlsafe(32))") \
  -n ancient-compute

# TLS certificate
kubectl create secret tls ancient-compute-tls \
  --cert=path/to/tls.crt \
  --key=path/to/tls.key \
  -n ancient-compute
```

### 3. Deploy PostgreSQL

**postgres-deployment.yaml**:
```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: postgres
  namespace: ancient-compute
spec:
  serviceName: postgres
  replicas: 1
  selector:
    matchLabels:
      app: postgres
  template:
    metadata:
      labels:
        app: postgres
    spec:
      containers:
      - name: postgres
        image: postgres:15-alpine
        env:
        - name: POSTGRES_USER
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: username
        - name: POSTGRES_PASSWORD
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: password
        - name: POSTGRES_DB
          value: ancient_compute
        ports:
        - containerPort: 5432
        volumeMounts:
        - name: postgres-storage
          mountPath: /var/lib/postgresql/data
        resources:
          requests:
            memory: "2Gi"
            cpu: "1"
          limits:
            memory: "4Gi"
            cpu: "2"
  volumeClaimTemplates:
  - metadata:
      name: postgres-storage
    spec:
      accessModes: [ "ReadWriteOnce" ]
      resources:
        requests:
          storage: 50Gi
---
apiVersion: v1
kind: Service
metadata:
  name: postgres
  namespace: ancient-compute
spec:
  selector:
    app: postgres
  ports:
  - port: 5432
    targetPort: 5432
  clusterIP: None  # Headless service for StatefulSet
```

### 4. Deploy Backend

**backend-deployment.yaml**:
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: backend
  namespace: ancient-compute
spec:
  replicas: 3
  selector:
    matchLabels:
      app: backend
  template:
    metadata:
      labels:
        app: backend
        version: v1.0.0
    spec:
      containers:
      - name: backend
        image: ancient-compute/backend:1.0.0
        ports:
        - containerPort: 8000
        env:
        - name: ENVIRONMENT
          value: "production"
        - name: DEBUG
          value: "false"
        - name: SECRET_KEY
          valueFrom:
            secretKeyRef:
              name: app-secrets
              key: secret-key
        - name: DATABASE_URL
          value: "postgresql://$(DB_USER):$(DB_PASS)@postgres:5432/ancient_compute"
        - name: DB_USER
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: username
        - name: DB_PASS
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: password
        - name: REDIS_URL
          value: "redis://redis:6379"
        livenessProbe:
          httpGet:
            path: /health
            port: 8000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8000
          initialDelaySeconds: 10
          periodSeconds: 5
        resources:
          requests:
            memory: "1Gi"
            cpu: "500m"
          limits:
            memory: "2Gi"
            cpu: "2"
---
apiVersion: v1
kind: Service
metadata:
  name: backend
  namespace: ancient-compute
spec:
  selector:
    app: backend
  ports:
  - port: 8000
    targetPort: 8000
  type: ClusterIP
```

### 5. Deploy Ingress

**ingress.yaml**:
```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: ancient-compute-ingress
  namespace: ancient-compute
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
    nginx.ingress.kubernetes.io/rate-limit: "100"
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - ancient-compute.com
    - api.ancient-compute.com
    secretName: ancient-compute-tls
  rules:
  - host: ancient-compute.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: frontend
            port:
              number: 3000
  - host: api.ancient-compute.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: backend
            port:
              number: 8000
```

### 6. Apply Configurations

```bash
# Deploy database
kubectl apply -f k8s/postgres-deployment.yaml

# Wait for database to be ready
kubectl wait --for=condition=ready pod -l app=postgres -n ancient-compute --timeout=300s

# Run migrations
kubectl run -it --rm migrations \
  --image=ancient-compute/backend:1.0.0 \
  --restart=Never \
  -n ancient-compute \
  -- alembic upgrade head

# Deploy backend
kubectl apply -f k8s/backend-deployment.yaml

# Deploy frontend
kubectl apply -f k8s/frontend-deployment.yaml

# Deploy ingress
kubectl apply -f k8s/ingress.yaml
```

---

## Database Setup

### Initial Migration

```bash
# Generate initial migration
cd backend
alembic revision --autogenerate -m "Initial schema"

# Review migration file (edit if needed)
vim alembic/versions/*_initial_schema.py

# Apply migration
alembic upgrade head
```

### Backup and Restore

**Backup**:
```bash
# Local backup
pg_dump -h localhost -U ancient ancient_compute > backup_$(date +%Y%m%d).sql

# Kubernetes backup
kubectl exec -n ancient-compute postgres-0 -- \
  pg_dump -U ancient ancient_compute > backup_$(date +%Y%m%d).sql
```

**Restore**:
```bash
# Local restore
psql -h localhost -U ancient ancient_compute < backup_20251102.sql

# Kubernetes restore
kubectl exec -i -n ancient-compute postgres-0 -- \
  psql -U ancient ancient_compute < backup_20251102.sql
```

---

## Monitoring & Observability

### Prometheus Setup

**prometheus.yml**:
```yaml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'ancient-compute-backend'
    static_configs:
      - targets: ['backend:8000']
    metrics_path: '/metrics'
```

### Grafana Dashboards

Import dashboards:
1. **HTTP Metrics**: Request rate, duration, status codes
2. **Code Execution**: Execution count by language, duration histograms
3. **System Metrics**: CPU, memory, active connections
4. **Error Tracking**: Error rates by type and endpoint

### Logging Stack (ELK)

**filebeat.yml**:
```yaml
filebeat.inputs:
  - type: docker
    containers.ids: '*'
    processors:
      - add_docker_metadata: ~

output.elasticsearch:
  hosts: ["elasticsearch:9200"]
```

---

## Security Hardening

### 1. SSL/TLS Configuration

Use Let's Encrypt for free certificates:

```bash
certbot certonly --standalone -d ancient-compute.com -d api.ancient-compute.com
```

### 2. Firewall Rules

```bash
# Allow only necessary ports
ufw default deny incoming
ufw default allow outgoing
ufw allow 22/tcp    # SSH
ufw allow 80/tcp    # HTTP
ufw allow 443/tcp   # HTTPS
ufw enable
```

### 3. Database Security

```sql
-- Create read-only user for monitoring
CREATE USER monitor WITH PASSWORD 'monitoring_password';
GRANT CONNECT ON DATABASE ancient_compute TO monitor;
GRANT USAGE ON SCHEMA public TO monitor;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO monitor;
```

### 4. Regular Updates

```bash
# Update system packages
sudo apt update && sudo apt upgrade -y

# Update Docker images
docker-compose pull
docker-compose up -d

# Kubernetes rolling update
kubectl set image deployment/backend \
  backend=ancient-compute/backend:1.0.1 \
  -n ancient-compute
```

---

## Troubleshooting

### Common Issues

**1. Database Connection Refused**
```bash
# Check PostgreSQL is running
docker ps | grep postgres
kubectl get pods -n ancient-compute | grep postgres

# Check connectivity
telnet postgres 5432

# Check credentials
psql -h postgres -U ancient ancient_compute
```

**2. High Memory Usage**
```bash
# Check container memory
docker stats

# Kubernetes memory usage
kubectl top pods -n ancient-compute

# Adjust resource limits if needed
```

**3. Rate Limit Errors (429)**
```bash
# Check rate limit logs
docker logs backend | grep "Rate limit"

# Adjust limits in backend/src/rate_limiting.py
# Restart service
docker-compose restart backend
```

**4. SSL Certificate Issues**
```bash
# Check certificate validity
openssl x509 -in /path/to/cert.pem -text -noout

# Renew Let's Encrypt certificate
certbot renew
```

### Health Check Commands

```bash
# Backend health
curl http://localhost:8000/health

# Database health
docker exec postgres pg_isready -U ancient

# Redis health
docker exec redis redis-cli ping

# Check all services
docker-compose ps
kubectl get pods -n ancient-compute
```

### Log Access

```bash
# Docker logs
docker-compose logs -f backend
docker-compose logs -f frontend

# Kubernetes logs
kubectl logs -f deployment/backend -n ancient-compute
kubectl logs -f deployment/frontend -n ancient-compute

# All pods logs
kubectl logs -f -l app=backend -n ancient-compute --all-containers
```

---

## Performance Tuning

### Database Optimization

**postgresql.conf**:
```ini
# Connection pooling
max_connections = 200
shared_buffers = 4GB
effective_cache_size = 12GB
maintenance_work_mem = 1GB
checkpoint_completion_target = 0.9
wal_buffers = 16MB
default_statistics_target = 100
random_page_cost = 1.1
effective_io_concurrency = 200
work_mem = 20MB
min_wal_size = 1GB
max_wal_size = 4GB
```

### Redis Optimization

**redis.conf**:
```ini
maxmemory 2gb
maxmemory-policy allkeys-lru
appendonly yes
appendfsync everysec
```

### Nginx Optimization

**nginx.conf**:
```nginx
worker_processes auto;
worker_connections 4096;

http {
    # Enable gzip
    gzip on;
    gzip_types text/plain text/css application/json application/javascript;
    
    # Enable caching
    proxy_cache_path /var/cache/nginx levels=1:2 keys_zone=api_cache:10m max_size=1g;
    
    # Connection pooling
    upstream backend {
        least_conn;
        server backend:8000 max_fails=3 fail_timeout=30s;
        keepalive 32;
    }
}
```

---

**Last Updated**: November 2, 2025
**Version**: 1.0
**Maintainer**: Ancient Compute DevOps Team
