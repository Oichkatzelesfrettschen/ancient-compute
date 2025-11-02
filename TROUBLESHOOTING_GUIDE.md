# Ancient Compute - Troubleshooting Guide

**Version**: 1.0
**Date**: November 2, 2025
**Last Updated**: November 2, 2025

---

## Table of Contents

1. [Quick Diagnostics](#quick-diagnostics)
2. [Common Issues](#common-issues)
3. [Backend Issues](#backend-issues)
4. [Frontend Issues](#frontend-issues)
5. [Database Issues](#database-issues)
6. [Language Service Issues](#language-service-issues)
7. [Performance Issues](#performance-issues)
8. [Security Issues](#security-issues)
9. [Deployment Issues](#deployment-issues)

---

## Quick Diagnostics

### Health Check Commands

```bash
# Check all services status
docker-compose ps

# Check backend health
curl http://localhost:8000/health

# Check backend readiness (with dependencies)
curl http://localhost:8000/ready

# Check metrics
curl http://localhost:8000/metrics

# Check database
docker exec postgres pg_isready -U ancient

# Check Redis
docker exec redis redis-cli ping
```

### Log Access

```bash
# View all logs
docker-compose logs -f

# Backend only
docker-compose logs -f backend

# Last 100 lines
docker-compose logs --tail=100 backend

# Follow specific service
docker logs -f ancient-compute-backend
```

---

## Common Issues

### Issue: "Connection refused" when accessing API

**Symptoms**:
```
curl: (7) Failed to connect to localhost port 8000: Connection refused
```

**Diagnosis**:
```bash
# Check if backend is running
docker-compose ps backend

# Check logs for errors
docker-compose logs backend | grep -i error
```

**Solutions**:

1. **Service not started**:
   ```bash
   docker-compose up -d backend
   ```

2. **Port conflict**:
   ```bash
   # Check what's using port 8000
   lsof -i :8000
   
   # Change port in docker-compose.yml
   ports:
     - "8001:8000"  # Use 8001 instead
   ```

3. **Container crashed**:
   ```bash
   # Check container status
   docker-compose ps
   
   # Restart container
   docker-compose restart backend
   ```

---

### Issue: Rate limit errors (HTTP 429)

**Symptoms**:
```json
{
  "error": "Rate limit exceeded",
  "retry_after": 45
}
```

**Diagnosis**:
```bash
# Check rate limit logs
docker logs backend | grep "Rate limit"

# Check your IP
curl ipinfo.io
```

**Solutions**:

1. **Wait for retry_after seconds**:
   ```bash
   # Response includes Retry-After header
   sleep 45
   ```

2. **Adjust rate limits** (development only):
   ```python
   # backend/src/rate_limiting.py
   self.route_limits = {
       "/api/v1/execute": (100, 60),  # Increase from 10 to 100
   }
   ```

3. **Use authentication** (higher limits for authenticated users):
   ```bash
   # Login first
   curl -X POST http://localhost:8000/api/v1/auth/login \
     -H "Content-Type: application/json" \
     -d '{"username": "user", "password": "pass"}'
   ```

---

### Issue: Authentication token expired (HTTP 401)

**Symptoms**:
```json
{
  "detail": "Token has expired"
}
```

**Solutions**:

1. **Get new token**:
   ```bash
   curl -X POST http://localhost:8000/api/v1/auth/login \
     -H "Content-Type: application/json" \
     -d '{"username": "user@example.com", "password": "password"}'
   ```

2. **Implement token refresh** (frontend):
   ```javascript
   async function refreshToken() {
     const response = await fetch('/api/v1/auth/refresh', {
       method: 'POST',
       headers: {
         'Authorization': `Bearer ${localStorage.getItem('refresh_token')}`
       }
     });
     const data = await response.json();
     localStorage.setItem('token', data.access_token);
   }
   ```

---

## Backend Issues

### Issue: Module import errors

**Symptoms**:
```
ImportError: No module named 'fastapi'
ModuleNotFoundError: No module named 'pydantic'
```

**Solutions**:

1. **Install dependencies**:
   ```bash
   cd backend
   pip install -r requirements.txt
   ```

2. **Rebuild Docker image**:
   ```bash
   docker-compose build backend
   docker-compose up -d backend
   ```

3. **Virtual environment not activated**:
   ```bash
   source venv/bin/activate  # Linux/Mac
   venv\Scripts\activate     # Windows
   ```

---

### Issue: Database migration errors

**Symptoms**:
```
alembic.util.exc.CommandError: Target database is not up to date.
sqlalchemy.exc.ProgrammingError: relation "users" does not exist
```

**Solutions**:

1. **Run migrations**:
   ```bash
   cd backend
   alembic upgrade head
   ```

2. **Check migration status**:
   ```bash
   alembic current
   alembic history
   ```

3. **Reset database** (development only):
   ```bash
   # WARNING: Deletes all data
   docker-compose down -v
   docker-compose up -d postgres
   alembic upgrade head
   ```

---

### Issue: High memory usage

**Symptoms**:
```
backend container using > 2GB memory
OOMKilled status in docker
```

**Diagnosis**:
```bash
# Check memory usage
docker stats

# Check for memory leaks
docker exec backend python -m memory_profiler
```

**Solutions**:

1. **Increase container memory limit**:
   ```yaml
   # docker-compose.yml
   services:
     backend:
       deploy:
         resources:
           limits:
             memory: 4G
   ```

2. **Check for memory leaks**:
   ```bash
   # Profile specific endpoint
   docker exec -it backend python -m memory_profiler src/api/execute.py
   ```

3. **Implement caching** (reduces repeated work):
   ```python
   # backend/src/services/ir_cache.py
   # Caching already implemented, verify it's enabled
   ```

---

## Frontend Issues

### Issue: "Cannot connect to API"

**Symptoms**:
```
Failed to fetch
Network request failed
CORS error
```

**Diagnosis**:
```bash
# Check if backend is accessible
curl http://localhost:8000/health

# Check frontend environment
cat frontend/.env
```

**Solutions**:

1. **Update API URL**:
   ```bash
   # frontend/.env
   PUBLIC_API_URL=http://localhost:8000
   ```

2. **Fix CORS settings**:
   ```python
   # backend/src/config.py
   ALLOWED_ORIGINS = [
       "http://localhost:3000",
       "http://localhost:5173",  # Vite dev server
   ]
   ```

3. **Restart services**:
   ```bash
   docker-compose restart frontend backend
   ```

---

### Issue: Monaco editor not loading

**Symptoms**:
```
Code editor appears blank
Monaco is not defined
```

**Solutions**:

1. **Clear build cache**:
   ```bash
   cd frontend
   rm -rf .svelte-kit node_modules
   npm install
   npm run dev
   ```

2. **Check dependencies**:
   ```bash
   npm list monaco-editor
   npm install monaco-editor@latest
   ```

---

## Database Issues

### Issue: Connection pool exhausted

**Symptoms**:
```
sqlalchemy.exc.TimeoutError: QueuePool limit of size 5 overflow 10 reached
```

**Diagnosis**:
```bash
# Check active connections
docker exec postgres psql -U ancient -c "SELECT count(*) FROM pg_stat_activity;"

# Check pool settings
docker exec backend env | grep DB_POOL
```

**Solutions**:

1. **Increase pool size**:
   ```python
   # backend/src/config.py
   DB_POOL_SIZE = 20
   DB_MAX_OVERFLOW = 20
   ```

2. **Check for connection leaks**:
   ```python
   # Ensure all sessions are closed
   with Session() as session:
       # Do work
       session.commit()
   # Session auto-closes
   ```

---

### Issue: Slow queries

**Symptoms**:
```
Requests taking > 5 seconds
Database CPU at 100%
```

**Diagnosis**:
```bash
# Check slow queries
docker exec postgres psql -U ancient -d ancient_compute -c \
  "SELECT query, calls, total_time, mean_time 
   FROM pg_stat_statements 
   ORDER BY mean_time DESC 
   LIMIT 10;"

# Enable query logging
docker exec postgres psql -U ancient -c \
  "ALTER SYSTEM SET log_min_duration_statement = 1000;"
```

**Solutions**:

1. **Add indexes**:
   ```sql
   -- Example: Index on frequently queried column
   CREATE INDEX idx_users_email ON users(email);
   CREATE INDEX idx_lessons_module_id ON lessons(module_id);
   ```

2. **Optimize queries** (use JOINs instead of N+1):
   ```python
   # Before (N+1 queries)
   lessons = session.query(Lesson).all()
   for lesson in lessons:
       module = session.query(Module).get(lesson.module_id)
   
   # After (1 query with JOIN)
   lessons = session.query(Lesson).options(
       joinedload(Lesson.module)
   ).all()
   ```

---

## Language Service Issues

### Issue: Language service timeout

**Symptoms**:
```json
{
  "status": "error",
  "error_type": "TimeoutError",
  "message": "Execution timed out after 10 seconds"
}
```

**Diagnosis**:
```bash
# Check language service logs
docker-compose logs c-service
docker-compose logs python-service
docker-compose logs haskell-service

# Check if service is running
docker-compose ps | grep service
```

**Solutions**:

1. **Increase timeout**:
   ```python
   # backend/src/config.py
   MAX_EXECUTION_TIME = 30  # seconds
   ```

2. **Restart language service**:
   ```bash
   docker-compose restart python-service
   ```

3. **Check for infinite loops in code**:
   ```python
   # User's code
   while True:
       pass  # This will timeout
   ```

---

### Issue: Language service unavailable

**Symptoms**:
```
Failed to connect to c-service:8001
ConnectionRefusedError
```

**Solutions**:

1. **Start language service**:
   ```bash
   docker-compose up -d c-service python-service haskell-service
   ```

2. **Check network connectivity**:
   ```bash
   docker network ls
   docker network inspect ancient-compute_default
   ```

3. **Rebuild service**:
   ```bash
   docker-compose build c-service
   docker-compose up -d c-service
   ```

---

## Performance Issues

### Issue: Slow API response times

**Symptoms**:
```
API requests taking > 2 seconds
P95 latency > 1000ms
```

**Diagnosis**:
```bash
# Check metrics
curl http://localhost:8000/metrics | grep http_request_duration

# Profile specific endpoint
curl -w "@curl-format.txt" -o /dev/null -s http://localhost:8000/api/v1/modules
```

**curl-format.txt**:
```
    time_namelookup:  %{time_namelookup}\n
       time_connect:  %{time_connect}\n
    time_appconnect:  %{time_appconnect}\n
   time_pretransfer:  %{time_pretransfer}\n
      time_redirect:  %{time_redirect}\n
 time_starttransfer:  %{time_starttransfer}\n
                    ----------\n
         time_total:  %{time_total}\n
```

**Solutions**:

1. **Enable caching**:
   ```python
   # backend/src/services/cache.py
   from functools import lru_cache
   
   @lru_cache(maxsize=1000)
   def get_module(module_id: int):
       return db.query(Module).get(module_id)
   ```

2. **Add database connection pooling** (already implemented):
   ```python
   # Verify settings
   # backend/src/config.py
   DB_POOL_SIZE = 20
   ```

3. **Implement Redis caching**:
   ```python
   # Cache expensive operations
   result = redis.get(f"module:{module_id}")
   if not result:
       result = expensive_operation()
       redis.setex(f"module:{module_id}", 3600, result)
   ```

---

## Security Issues

### Issue: CORS errors in browser

**Symptoms**:
```
Access to XMLHttpRequest blocked by CORS policy
No 'Access-Control-Allow-Origin' header
```

**Solutions**:

1. **Add frontend origin to ALLOWED_ORIGINS**:
   ```python
   # backend/src/config.py
   ALLOWED_ORIGINS = [
       "http://localhost:3000",
       "http://localhost:5173",
       "https://ancient-compute.com",
   ]
   ```

2. **Restart backend**:
   ```bash
   docker-compose restart backend
   ```

---

### Issue: SSL certificate errors

**Symptoms**:
```
SSL certificate problem: unable to get local issuer certificate
CERT_HAS_EXPIRED
```

**Solutions**:

1. **Renew certificate** (Let's Encrypt):
   ```bash
   certbot renew
   ```

2. **Check certificate validity**:
   ```bash
   openssl x509 -in /path/to/cert.pem -text -noout | grep "Not After"
   ```

3. **Update system CA certificates**:
   ```bash
   sudo update-ca-certificates
   ```

---

## Deployment Issues

### Issue: Kubernetes pod CrashLoopBackOff

**Symptoms**:
```
$ kubectl get pods
NAME                       READY   STATUS             RESTARTS
backend-xyz                0/1     CrashLoopBackOff   5
```

**Diagnosis**:
```bash
# Check pod logs
kubectl logs backend-xyz

# Check previous container logs
kubectl logs backend-xyz --previous

# Describe pod
kubectl describe pod backend-xyz
```

**Solutions**:

1. **Check readiness probe**:
   ```yaml
   # Ensure /ready endpoint returns 200
   readinessProbe:
     httpGet:
       path: /ready
       port: 8000
     initialDelaySeconds: 10
   ```

2. **Check environment variables**:
   ```bash
   kubectl describe pod backend-xyz | grep -A 10 Environment
   ```

3. **Verify secrets**:
   ```bash
   kubectl get secrets
   kubectl describe secret app-secrets
   ```

---

### Issue: Docker build failing

**Symptoms**:
```
ERROR: failed to solve: failed to compute cache key
```

**Solutions**:

1. **Clear Docker cache**:
   ```bash
   docker builder prune -a
   ```

2. **Build without cache**:
   ```bash
   docker build --no-cache -t ancient-compute/backend .
   ```

3. **Check Dockerfile syntax**:
   ```bash
   docker build --check -f Dockerfile .
   ```

---

## Getting Help

### Collect Diagnostic Information

Before reporting an issue, collect:

```bash
# System information
docker version
docker-compose version
uname -a

# Service status
docker-compose ps

# Recent logs
docker-compose logs --tail=100 > logs.txt

# Configuration
cat docker-compose.yml
cat backend/.env (remove secrets!)

# Network information
docker network inspect ancient-compute_default
```

### Report an Issue

Include:
1. Description of the problem
2. Steps to reproduce
3. Expected vs actual behavior
4. System information (from above)
5. Relevant logs
6. Screenshots (if UI issue)

**GitHub Issues**: https://github.com/Oichkatzelesfrettschen/ancient-compute/issues

---

**Last Updated**: November 2, 2025
**Maintained by**: Ancient Compute DevOps Team
