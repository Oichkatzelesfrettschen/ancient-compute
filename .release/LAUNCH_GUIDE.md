# Ancient Compute - Launch and Release Guide

## Overview

This guide provides step-by-step procedures for preparing, testing, and launching Ancient Compute to production. It includes pre-launch validation, database migration procedures, smoke tests, and post-launch monitoring.

**Target**: Production deployment of the complete educational platform with all 8 language services, user progress tracking, and curriculum content.

## Pre-Launch Checklist (T-5 Days)

### Code Readiness

- [ ] All tests passing locally
  ```bash
  cd backend && pytest tests/ -v --tb=short
  cd frontend && npm test -- --run
  ```

- [ ] No compiler warnings
  ```bash
  python3 -m flake8 backend/src --max-line-length=100
  python3 -m black --check backend/src
  npm run lint (frontend)
  ```

- [ ] Coverage above 85%
  ```bash
  pytest backend/tests --cov=backend/src --cov-report=html
  # Coverage report in htmlcov/index.html
  ```

- [ ] Security scanning clean
  ```bash
  docker run --rm -v /var/run/docker.sock:/var/run/docker.sock \
    aquasec/trivy image ancient-compute-backend:latest
  ```

- [ ] Dependencies up to date
  ```bash
  pip list --outdated (Python)
  npm outdated (Node)
  # Review breaking changes before updating
  ```

### Documentation Readiness

- [ ] API documentation complete
  - All endpoints documented
  - Request/response examples provided
  - Error codes enumerated
  - Rate limits defined

- [ ] User guide published
  - How to navigate learning paths
  - How to submit code
  - Troubleshooting common issues
  - Supported languages and features

- [ ] Admin documentation
  - Deployment procedures
  - Backup/recovery procedures
  - Monitoring and alerting setup
  - Database administration

### Infrastructure Readiness

- [ ] Production database provisioned
  ```bash
  # Verify PostgreSQL 14+ running
  psql --version
  # Verify sufficient storage
  SELECT pg_database_size('ancient_compute');
  ```

- [ ] Container registry available
  ```bash
  docker pull ancient-compute-backend:latest
  docker pull ancient-compute-frontend:latest
  ```

- [ ] DNS configured
  - API domain: api.ancient-compute.edu
  - Web domain: learn.ancient-compute.edu
  - Email domain: noreply@ancient-compute.edu

- [ ] SSL/TLS certificates obtained
  ```bash
  # Verify certificate validity
  openssl x509 -in cert.pem -text -noout | grep "Not Before\|Not After"
  ```

## Database Migration (T-1 Day)

### Pre-Migration Backup

```bash
# Full backup of staging database
pg_dump -U postgres ancient_compute_staging | gzip > \
  backups/pre_launch_$(date +%Y%m%d_%H%M%S).sql.gz

# Verify backup
gzip -t backups/pre_launch_*.sql.gz && echo "Backup valid"

# Size check
du -h backups/pre_launch_*.sql.gz
```

### Migration Procedure

```bash
# 1. Connect to production database
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute

# 2. Run migrations
alembic upgrade head

# 3. Verify schema
\dt (list tables)
\di (list indexes)

# 4. Check constraints
SELECT constraint_name, table_name FROM information_schema.table_constraints
WHERE table_name = 'exercises';

# 5. Verify foreign keys
SELECT constraint_name, table_name FROM information_schema.referential_constraints
WHERE table_name = 'submissions';
```

### Post-Migration Validation

```bash
# Count records in each table
SELECT tablename, count(*) FROM pg_tables
WHERE schemaname = 'public';

# Verify indexes are working
EXPLAIN ANALYZE SELECT * FROM exercises WHERE module_id = 1;

# Test queries
SELECT COUNT(*) FROM modules;
SELECT COUNT(*) FROM exercises;
SELECT COUNT(*) FROM users;

# All should return non-zero counts or be empty as expected
```

## Smoke Tests (T-0, 2 Hours Before Launch)

### API Endpoint Verification

```bash
# Health check
curl -v https://api.ancient-compute.edu/health
# Expected: 200 OK

# Language support
curl https://api.ancient-compute.edu/languages
# Expected: All 8 languages listed

# Code execution
curl -X POST https://api.ancient-compute.edu/execute \
  -H "Content-Type: application/json" \
  -d '{"language": "python", "code": "print(\"hello\")"}'
# Expected: 200 OK with stdout containing "hello"

# Multiple languages
for lang in python c haskell idris lisp java assembly systemf; do
  echo "Testing $lang..."
  curl -X POST https://api.ancient-compute.edu/execute \
    -H "Content-Type: application/json" \
    -d "{\"language\": \"$lang\", \"code\": \"# test\"}"
done
```

### Frontend Smoke Tests

```bash
# Load homepage
curl -I https://learn.ancient-compute.edu/
# Expected: 200 OK

# Check CSS/JS loaded
curl https://learn.ancient-compute.edu/ | grep -E "<link|<script"

# Login endpoint
curl -X POST https://learn.ancient-compute.edu/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email": "test@example.com", "password": "test123"}'
# Expected: 401 Unauthorized (test user doesn't exist)

# Browse course
curl https://learn.ancient-compute.edu/courses/1
# Expected: 200 OK with course content
```

### Database Smoke Tests

```bash
# Connection test
psql -h prod-db.ancient-compute.edu -U app_user -d ancient_compute \
  -c "SELECT 1;" && echo "✓ Database connected"

# Data queries
psql -h prod-db.ancient-compute.edu -U app_user -d ancient_compute << EOF
SELECT COUNT(*) as module_count FROM modules;
SELECT COUNT(*) as exercise_count FROM exercises;
SELECT COUNT(*) as user_count FROM users;
SELECT COUNT(*) as submission_count FROM submissions;
EOF
```

### Cache Verification

```bash
# Redis/Memcached connectivity
redis-cli -h cache.ancient-compute.edu ping
# Expected: PONG

# Cache hit test
# Make two identical requests; second should have lower latency
time curl https://api.ancient-compute.edu/modules/1
time curl https://api.ancient-compute.edu/modules/1
```

### Security Verification

```bash
# HTTPS only
curl -I http://api.ancient-compute.edu/health
# Expected: 301 redirect to HTTPS

# HSTS header
curl -I https://api.ancient-compute.edu/health | grep -i "strict-transport"

# CORS headers
curl -I https://api.ancient-compute.edu/health | grep -i "access-control"

# No sensitive headers leaked
curl -I https://api.ancient-compute.edu/health | grep -i "server\|x-powered"
```

## Deployment Process (Launch Day)

### Pre-Deployment (T-1 Hour)

1. **Final Git Tag**
   ```bash
   git tag -a v1.0.0 -m "Production release: Ancient Compute v1.0.0"
   git push origin v1.0.0
   ```

2. **Docker Image Build**
   ```bash
   docker build -t ancient-compute-backend:1.0.0 -f backend/Dockerfile .
   docker build -t ancient-compute-frontend:1.0.0 -f frontend/Dockerfile .
   ```

3. **Image Push to Registry**
   ```bash
   docker tag ancient-compute-backend:1.0.0 \
     registry.ancient-compute.edu/backend:1.0.0
   docker push registry.ancient-compute.edu/backend:1.0.0
   docker push registry.ancient-compute.edu/frontend:1.0.0
   ```

4. **Staging Deployment Test**
   ```bash
   docker-compose -f docker-compose.staging.yml up -d
   # Run smoke tests against staging
   ./scripts/run_smoke_tests.sh https://staging.ancient-compute.edu
   docker-compose -f docker-compose.staging.yml down
   ```

### Deployment (T-0)

**Option 1: Blue-Green Deployment (Recommended)**

```bash
# 1. Ensure green environment is ready
kubectl get deployment ancient-compute-backend-green

# 2. Start new backend pods (green)
kubectl apply -f k8s/deployment-green.yaml

# 3. Wait for readiness
kubectl wait --for=condition=ready pod \
  -l app=ancient-compute-backend,version=green \
  --timeout=300s

# 4. Route traffic to green (0% blue, 100% green)
kubectl patch service ancient-compute-backend \
  -p '{"spec":{"selector":{"version":"green"}}}'

# 5. Monitor green deployment
kubectl logs -f deployment/ancient-compute-backend-green

# 6. Validate with smoke tests
./scripts/run_smoke_tests.sh https://api.ancient-compute.edu

# 7. If successful, scale down blue
kubectl scale deployment ancient-compute-backend-blue --replicas=0

# 8. Keep blue ready for 1 hour for quick rollback
```

**Option 2: Canary Deployment**

```bash
# 1. Deploy new version (10% traffic)
kubectl set image deployment/ancient-compute-backend \
  backend=registry.ancient-compute.edu/backend:1.0.0 \
  --record

# 2. Monitor error rates and latency
kubectl get deployment ancient-compute-backend -o wide
kubectl top pods -l app=ancient-compute-backend

# 3. Gradually increase traffic (10% -> 25% -> 50% -> 100%)
kubectl patch service ancient-compute-backend \
  -p '{"spec":{"trafficPolicy":{"canary":{"weight":25}}}}'

# 4. Each step: verify for 5 minutes
# 5. Rollback on any errors: kubectl rollout undo deployment/ancient-compute-backend
```

### Post-Deployment (T+30 Minutes)

1. **Smoke Test Suite**
   ```bash
   ./scripts/run_comprehensive_smoke_tests.sh
   # Verify all endpoints responding
   # Check all 8 languages executing
   # Validate database connectivity
   # Check cache functionality
   ```

2. **Performance Baseline**
   ```bash
   # Record baseline metrics
   kubectl top pods -l app=ancient-compute-backend
   kubectl get hpa ancient-compute-backend

   # API latency (P50, P95, P99)
   watch -n 5 'curl -w "%{time_total}\n" -o /dev/null https://api.ancient-compute.edu/health'
   ```

3. **Error Tracking**
   ```bash
   # Verify error tracking is working
   curl -X POST https://api.ancient-compute.edu/execute \
     -H "Content-Type: application/json" \
     -d '{"language": "python", "code": "raise ValueError()"}'

   # Check error appears in logs
   kubectl logs -f deployment/ancient-compute-backend | grep ValueError
   ```

4. **Database Verification**
   ```bash
   # Query counts unchanged
   psql -U app_user -d ancient_compute << EOF
   SELECT COUNT(*) FROM users;
   SELECT COUNT(*) FROM exercises;
   SELECT COUNT(*) FROM submissions;
   EOF
   ```

## Rollback Procedures

### Quick Rollback (< 5 minutes)

**Kubernetes Rollback**
```bash
# See deployment history
kubectl rollout history deployment/ancient-compute-backend

# Rollback to previous version
kubectl rollout undo deployment/ancient-compute-backend

# Verify
kubectl get deployment ancient-compute-backend -o wide
./scripts/run_smoke_tests.sh
```

**Docker Compose Rollback**
```bash
# Stop current version
docker-compose down

# Checkout previous version
git checkout v0.9.0

# Redeploy
docker-compose up -d

# Verify
./scripts/run_smoke_tests.sh http://localhost:8000
```

### Database Rollback

**If migration failed**
```bash
# Restore from backup
psql -h prod-db -U postgres -d ancient_compute < backups/pre_launch_*.sql.gz

# Verify data restored
psql -h prod-db -U postgres -d ancient_compute -c "SELECT COUNT(*) FROM users;"

# Redeploy previous API version
kubectl rollout undo deployment/ancient-compute-backend
```

## Post-Launch Monitoring (T+24 Hours)

### Metric Collection

```bash
# API response time
prometheus_query 'histogram_quantile(0.95, http_request_duration_seconds)'

# Error rate
prometheus_query 'rate(http_requests_total{status=~"5.."}[5m])'

# Database connections
prometheus_query 'pg_stat_activity_count'

# Cache hit rate
prometheus_query 'cache_hits / (cache_hits + cache_misses)'
```

### Alert Thresholds

Set up alerts for:

1. **API Latency**: P95 > 500ms
2. **Error Rate**: > 0.5% of requests failing
3. **Database**: Connection pool > 80% full
4. **Memory**: Pod memory > 80% limit
5. **Disk**: Database storage > 80% full

### User Feedback Collection

```bash
# Monitor support tickets
# Check error tracking system
# Review application logs for exceptions
# Track user engagement metrics

# Success indicators:
# - Errors decreasing over time
# - User registrations increasing
# - Course completions occurring
# - All 8 languages being used
```

## Success Criteria

Launch is successful when:

- [ ] All smoke tests passing
- [ ] No critical errors in logs
- [ ] API response time < 500ms (P95)
- [ ] Error rate < 0.5%
- [ ] Database operations normal
- [ ] All 8 language services executing code successfully
- [ ] User registrations flowing
- [ ] Initial users able to complete exercises
- [ ] No data corruption observed
- [ ] Security scanning passing

## Post-Launch (T+7 Days)

### Performance Review

```bash
# Analyze request patterns
SELECT method, path, COUNT(*), AVG(response_time_ms)
FROM api_requests WHERE timestamp > now() - interval '7 days'
GROUP BY method, path
ORDER BY COUNT(*) DESC;

# Check error distribution
SELECT status_code, COUNT(*) FROM api_requests
WHERE timestamp > now() - interval '7 days'
GROUP BY status_code
ORDER BY COUNT(*) DESC;

# Language usage
SELECT language, COUNT(*) FROM code_executions
WHERE timestamp > now() - interval '7 days'
GROUP BY language
ORDER BY COUNT(*) DESC;
```

### Team Retrospective

- What went well?
- What needed improvement?
- Performance: Any bottlenecks?
- User feedback: Any pain points?
- Technical issues: Any surprising bugs?

## Maintenance Schedule

### Daily (Automated)

- [ ] Database backups
- [ ] Log rotation
- [ ] Cache cleanup
- [ ] Performance metrics collection

### Weekly

- [ ] Review error logs
- [ ] Check database size growth
- [ ] Verify backup integrity
- [ ] Performance trend analysis

### Monthly

- [ ] Security patches
- [ ] Dependency updates
- [ ] Capacity planning
- [ ] User support review

## Contact and Escalation

**Launch Commander**: [Name/Role]
**Infrastructure Team**: [Contact]
**Database Administrator**: [Contact]
**On-Call Engineering**: [Rotation Schedule]

**Escalation Path**:
1. Alert sent to on-call
2. If P1: Notify launch commander
3. If unresolved in 15 min: Full team standup
4. If unresolved in 1 hour: Consider rollback

## Launch Success Timeline

| Time | Event | Owner | Status |
|------|-------|-------|--------|
| T-5 Days | Pre-launch checklist | PM | - |
| T-2 Days | Database migration staging | DBA | - |
| T-1 Day | Final code freeze | Engineering Lead | - |
| T-0 | Deploy to production | DevOps | - |
| T+30 min | Smoke test suite | QA | - |
| T+1 hour | Post-deployment monitoring | On-Call | - |
| T+24 hours | Full retrospective | Team | - |

---

**Document Version**: 1.0
**Last Updated**: 2025-01-01
**Next Review**: Post-Launch Retrospective
