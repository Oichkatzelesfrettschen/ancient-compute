# Ancient Compute Deployment Guide

## Quick Start

This guide covers deploying Ancient Compute to production.

## Prerequisites

- Docker 20.10+
- Docker Compose 2.0+
- PostgreSQL 14+ (or use Docker service)
- Python 3.11+ (for local development only)
- Git

## Environment Setup

### 1. Create Environment Files

```bash
# Backend environment
cat > .env.production << 'EOF'
# Database
DATABASE_URL=postgresql://user:password@postgres:5432/ancient_compute
SQLALCHEMY_ECHO=False

# API Configuration
API_HOST=0.0.0.0
API_PORT=8000
API_WORKERS=4
DEBUG=False

# Security
SECRET_KEY=your-secret-key-here-min-32-chars
ALLOWED_ORIGINS=https://yourdomain.com,https://www.yourdomain.com

# Execution
EXECUTION_CACHE_SIZE=1000
QUERY_CACHE_TTL=300
EXECUTION_TIMEOUT_DEFAULT=10

# Logging
LOG_LEVEL=INFO
EOF

# Frontend environment
cat > frontend/.env.production << 'EOF'
PUBLIC_API_URL=https://api.yourdomain.com
PUBLIC_APP_NAME=Ancient Compute
EOF
```

### 2. Database Setup

```bash
# Run migrations
docker-compose run backend alembic upgrade head

# Create initial data (if needed)
docker-compose run backend python -c "from src.models import *; ..."
```

## Deployment Methods

### Method 1: Docker Compose (Recommended for Small Deployments)

```bash
# Build images
docker-compose build

# Start services
docker-compose up -d

# View logs
docker-compose logs -f backend frontend

# Health checks
docker-compose ps
curl http://localhost:8000/health
```

### Method 2: Kubernetes (Enterprise Deployments)

See `k8s/deployment.yaml` for Kubernetes manifests.

```bash
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/secrets.yaml
kubectl apply -f k8s/deployment.yaml
kubectl apply -f k8s/service.yaml
kubectl apply -f k8s/ingress.yaml

# Scale replicas
kubectl scale deployment ancient-compute-backend --replicas=3
```

### Method 3: Cloud Platforms

#### AWS (ECS + RDS)

```bash
# Push images to ECR
aws ecr get-login-password | docker login --username AWS --password-stdin <account>.dkr.ecr.us-east-1.amazonaws.com
docker tag ancient-compute-backend:latest <account>.dkr.ecr.us-east-1.amazonaws.com/ancient-compute:backend
docker push <account>.dkr.ecr.us-east-1.amazonaws.com/ancient-compute:backend

# Deploy via CloudFormation or Terraform
terraform apply -var="image_uri=<account>.dkr.ecr.us-east-1.amazonaws.com/ancient-compute:backend"
```

#### Google Cloud Run

```bash
gcloud run deploy ancient-compute \
  --image gcr.io/PROJECT_ID/ancient-compute:latest \
  --platform managed \
  --region us-central1 \
  --set-env-vars DATABASE_URL=$DATABASE_URL \
  --memory 2Gi \
  --cpu 2
```

#### Heroku

```bash
heroku login
heroku create ancient-compute
heroku config:set DATABASE_URL=postgresql://...
git push heroku master
heroku logs --tail
```

## Health Checks

### Backend API

```bash
# Startup endpoint
curl http://localhost:8000/health

# Readiness check
curl http://localhost:8000/health?ready=true

# Kubernetes liveness probe
curl --fail http://localhost:8000/health/live || exit 1

# Kubernetes readiness probe
curl --fail http://localhost:8000/health/ready || exit 1
```

### Database Connection

```bash
# Verify database connectivity
docker-compose exec backend python -c "
from src.database import SessionLocal
db = SessionLocal()
result = db.execute('SELECT 1')
print('✓ Database connected')
"
```

### Docker Services

```bash
# Check service status
docker-compose ps

# View logs
docker-compose logs backend
docker-compose logs frontend
docker-compose logs postgres

# Check resource usage
docker stats
```

## Monitoring

### Logging

All containers log to stdout:

```bash
# Aggregate logs
docker-compose logs -f

# View specific service
docker-compose logs -f backend

# With timestamps
docker-compose logs -f --timestamps
```

### Performance Metrics

Monitor via Docker:

```bash
# CPU, memory, network
docker stats
watch docker stats

# Detailed metrics
docker stats --no-stream
```

### Application Metrics

The backend exposes metrics at `/metrics` (Prometheus format):

```bash
curl http://localhost:8000/metrics | grep 'ancient_compute'
```

## Scaling

### Horizontal Scaling

```bash
# Docker Compose (with load balancer)
# Update docker-compose.yml to run multiple backend instances

# Kubernetes
kubectl scale deployment ancient-compute-backend --replicas=3

# AWS ECS
aws ecs update-service --cluster ancient-compute --service backend --desired-count 3
```

### Vertical Scaling

Adjust resource limits in docker-compose.yml:

```yaml
services:
  backend:
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 2G
```

## Backup & Recovery

### Database Backup

```bash
# Automated daily backups (add to crontab)
0 2 * * * docker-compose exec -T postgres pg_dump -U postgres ancient_compute | gzip > /backups/backup_$(date +\%Y\%m\%d).sql.gz

# Manual backup
docker-compose exec postgres pg_dump -U postgres ancient_compute > backup.sql

# Restore from backup
docker-compose exec -T postgres psql -U postgres ancient_compute < backup.sql
```

### Persistent Volumes

Configure backup volumes in docker-compose.yml:

```yaml
volumes:
  postgres_data:
    driver: local
    driver_opts:
      type: nfs
      o: addr=nfs-server,vers=4,soft,timeo=180,bg,tcp,rw
      device: ":/data/ancient-compute"
```

## Security Hardening

### Network Security

```bash
# Disable unnecessary ports
# Update docker-compose to remove exposed ports except 80/443

# Use reverse proxy (nginx/traefik)
# Add SSL/TLS certificates via Let's Encrypt
```

### Secrets Management

```bash
# Use Docker secrets (Docker Swarm)
docker secret create db_password -
docker service create --secret db_password ...

# Or use environment-specific .env files (not in git!)
# Load via docker-compose.yml: env_file: .env.production
```

### Database Security

```bash
# Change default passwords
ALTER ROLE postgres WITH PASSWORD 'strong_password';

# Create restricted database user
CREATE ROLE ancient_compute WITH LOGIN PASSWORD 'strong_password';
GRANT ALL PRIVILEGES ON DATABASE ancient_compute TO ancient_compute;

# Enable SSL connections
# Update PostgreSQL config: ssl = on
```

## Troubleshooting

### Services Won't Start

```bash
# Check logs
docker-compose logs backend frontend postgres

# Validate configuration
docker-compose config

# Check port conflicts
lsof -i :8000
lsof -i :5432

# Full rebuild
docker-compose down --volumes
docker-compose build --no-cache
docker-compose up
```

### Database Connection Issues

```bash
# Test connectivity
docker-compose exec backend psql $DATABASE_URL -c "SELECT 1"

# Check migration status
docker-compose exec backend alembic current
docker-compose exec backend alembic history

# Rollback migrations
docker-compose exec backend alembic downgrade -1
```

### Performance Issues

```bash
# Check resource usage
docker stats

# Check slow queries
docker-compose logs backend | grep "slow"

# Check cache hit rates
curl http://localhost:8000/metrics | grep cache

# Profile application (development only)
docker-compose exec backend python -m cProfile ...
```

### Code Execution Issues

```bash
# Verify language executors
curl -X POST http://localhost:8000/execute \
  -H "Content-Type: application/json" \
  -d '{"language":"python", "code":"print(\"hello\")"}'

# Check Docker images
docker images | grep ancient-compute

# Verify Docker daemon access
docker ps

# Check container security context
docker inspect ancient-compute-backend | grep -i user
```

## Rollback Procedure

If deployment fails:

```bash
# View deployment history
docker-compose ps -a

# Stop current version
docker-compose down

# Checkout previous version
git checkout <previous-commit>

# Redeploy
docker-compose build
docker-compose up -d

# Verify health
curl http://localhost:8000/health
```

## Performance Tuning

### Database Connection Pool

```python
# In database.py
engine = create_engine(
    DATABASE_URL,
    pool_size=20,           # Increase for high concurrency
    max_overflow=40,        # Overflow connections
    pool_recycle=3600,      # Recycle connections hourly
)
```

### Cache Configuration

```python
# In backend config
EXECUTION_CACHE_MAX_ENTRIES = 10000  # Larger cache for common code
QUERY_CACHE_TTL = 600                # Longer TTL for stable data
```

### API Workers

Update in docker-compose.yml:

```yaml
command: >
  uvicorn src.main:app
    --host 0.0.0.0
    --port 8000
    --workers 8                    # Match CPU count
    --worker-class uvicorn.workers.UvicornWorker
```

## Maintenance

### Regular Tasks

Daily:
- Monitor error logs
- Check health endpoints
- Verify backups

Weekly:
- Review resource usage
- Check for updates
- Test backup restoration

Monthly:
- Security patches
- Performance review
- Capacity planning

### Updates

```bash
# Pull latest changes
git pull origin master

# Build updated images
docker-compose build --no-cache

# Run migrations
docker-compose run backend alembic upgrade head

# Zero-downtime deployment (with load balancer)
# 1. Start new containers
docker-compose up -d --scale backend=2

# 2. Wait for health checks
sleep 30

# 3. Remove old containers
docker-compose down

# 4. Verify
curl http://localhost:8000/health
```

## Support

For deployment issues:

- Check logs: `docker-compose logs -f`
- Review this guide
- Check GitHub issues
- Open a GitHub Discussion

## Next Steps

1. Configure monitoring (Prometheus, Grafana)
2. Set up automated backups
3. Configure SSL/TLS certificates
4. Set up CI/CD pipeline (GitHub Actions)
5. Configure alerting (PagerDuty, Slack)
