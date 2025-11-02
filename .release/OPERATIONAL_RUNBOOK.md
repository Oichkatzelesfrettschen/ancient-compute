# Ancient Compute - Operational Runbook

Quick reference guide for common operational tasks, troubleshooting, and incident response.

## Table of Contents

1. [Service Health Monitoring](#service-health-monitoring)
2. [Scaling Operations](#scaling-operations)
3. [Database Operations](#database-operations)
4. [Performance Troubleshooting](#performance-troubleshooting)
5. [Incident Response](#incident-response)
6. [Backup and Recovery](#backup-and-recovery)

## Service Health Monitoring

### Check Overall System Health

```bash
# Kubernetes health
kubectl get nodes
kubectl get deployments
kubectl get services

# Pod status
kubectl get pods -o wide
kubectl describe pod <pod-name>

# Resource usage
kubectl top nodes
kubectl top pods -l app=ancient-compute-backend

# Recent events
kubectl get events --sort-by='.lastTimestamp' | tail -20
```

### Individual Service Checks

**Backend API**
```bash
# Health endpoint
curl https://api.ancient-compute.edu/health
# Expected: {"status": "healthy"}

# Readiness check
curl https://api.ancient-compute.edu/health?ready=true
# Expected: {"ready": true}

# Logs
kubectl logs -f deployment/ancient-compute-backend --tail=50
```

**Frontend**
```bash
# Check if serving
curl -I https://learn.ancient-compute.edu/
# Expected: 200 OK

# Check assets
curl https://learn.ancient-compute.edu/assets/ -I
# Expected: Directory listing or 200 OK

# Logs
kubectl logs -f deployment/ancient-compute-frontend --tail=50
```

**Database**
```bash
# Connection test
psql -h prod-db.ancient-compute.edu -U app_user -d ancient_compute << EOF
SELECT now();
SELECT count(*) FROM users;
EOF

# Check active connections
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
SELECT usename, count(*) FROM pg_stat_activity GROUP BY usename;
EOF

# Check table sizes
psql -h prod-db.ancient-compute.edu -U app_user -d ancient_compute << EOF
SELECT tablename, pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename))
FROM pg_tables WHERE schemaname = 'public' ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC;
EOF
```

**Cache System**
```bash
# Redis connectivity
redis-cli -h cache.ancient-compute.edu ping
# Expected: PONG

# Memory usage
redis-cli -h cache.ancient-compute.edu info memory

# Key count
redis-cli -h cache.ancient-compute.edu dbsize

# Check for memory warnings
redis-cli -h cache.ancient-compute.edu info stats | grep evicted
```

## Scaling Operations

### Horizontal Scaling (Add/Remove Pods)

**Scale Backend Pods**
```bash
# Current replicas
kubectl get deployment ancient-compute-backend -o wide

# Scale up (add more pods)
kubectl scale deployment ancient-compute-backend --replicas=5

# Scale down (remove pods)
kubectl scale deployment ancient-compute-backend --replicas=3

# Monitor scaling
kubectl rollout status deployment/ancient-compute-backend
kubectl get pods -l app=ancient-compute-backend -w
```

**Scale Frontend**
```bash
# Scale frontend (typically 2-3 replicas sufficient)
kubectl scale deployment ancient-compute-frontend --replicas=3

# Monitor
kubectl get pods -l app=ancient-compute-frontend -w
```

### Auto-Scaling Configuration

**Check HPA Status**
```bash
kubectl get hpa
kubectl describe hpa ancient-compute-backend-hpa

# View scaling events
kubectl get events | grep HorizontalPodAutoscaler
```

**Adjust HPA Thresholds**
```bash
# Edit HPA
kubectl edit hpa ancient-compute-backend-hpa

# Example thresholds:
# - minReplicas: 3
# - maxReplicas: 10
# - targetCPUUtilizationPercentage: 70
```

### Vertical Scaling (Increase Pod Resources)

```bash
# View current resource requests
kubectl get deployment ancient-compute-backend -o yaml | grep -A 10 resources

# Edit deployment
kubectl set resources deployment/ancient-compute-backend \
  -c=backend \
  --limits=cpu=2,memory=4Gi \
  --requests=cpu=1,memory=2Gi

# Monitor impact
kubectl top pods -l app=ancient-compute-backend
```

## Database Operations

### Backup Operations

**Create On-Demand Backup**
```bash
# Full backup
pg_dump -h prod-db.ancient-compute.edu -U postgres ancient_compute | \
  gzip > backups/full_$(date +%Y%m%d_%H%M%S).sql.gz

# Verify backup
gzip -t backups/full_*.sql.gz && echo "✓ Backup valid"

# Size check
du -h backups/full_*.sql.gz

# Store backup
aws s3 cp backups/full_*.sql.gz s3://ancient-compute-backups/postgres/
```

**Partial Backup (Specific Table)**
```bash
pg_dump -h prod-db.ancient-compute.edu -U postgres \
  -t exercises ancient_compute | gzip > backups/exercises_$(date +%Y%m%d).sql.gz
```

### Restore Operations

**Restore Full Database**
```bash
# Stop application
kubectl scale deployment ancient-compute-backend --replicas=0

# Restore from backup
gzip -dc backups/full_20250101_120000.sql.gz | \
  psql -h prod-db.ancient-compute.edu -U postgres ancient_compute

# Verify restore
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
SELECT COUNT(*) FROM users;
SELECT COUNT(*) FROM exercises;
EOF

# Restart application
kubectl scale deployment ancient-compute-backend --replicas=3
```

**Restore Specific Table**
```bash
# Restore single table
gzip -dc backups/exercises_20250101.sql.gz | \
  psql -h prod-db.ancient-compute.edu -U postgres ancient_compute

# Verify
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute -c "SELECT COUNT(*) FROM exercises;"
```

### Database Maintenance

**Vacuum and Analyze**
```bash
# Full maintenance (locks table, run during maintenance window)
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
VACUUM FULL;
ANALYZE;
EOF

# Check index bloat
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
SELECT schemaname, tablename, round(100 * (OTTA - table_len::float) / OTTA) AS table_waste_ratio
FROM pgstattuple_approx('exercises');
EOF
```

**Reindex Tables**
```bash
# List indexes
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute -c "\di"

# Reindex specific index (non-blocking)
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
REINDEX INDEX CONCURRENTLY idx_exercise_module;
EOF
```

## Performance Troubleshooting

### High CPU Usage

**Identify High-CPU Processes**
```bash
# Kubernetes node CPU
kubectl top nodes

# Pod CPU usage
kubectl top pods -l app=ancient-compute-backend --sort-by=cpu

# Get worst offender
kubectl describe pod <pod-name>
kubectl logs <pod-name> --tail=100
```

**Kubernetes Solution**
```bash
# Increase replicas to distribute load
kubectl scale deployment ancient-compute-backend --replicas=5

# Or increase resource allocation
kubectl set resources deployment/ancient-compute-backend \
  -c=backend --requests=cpu=1.5

# Monitor
watch -n 5 'kubectl top pods -l app=ancient-compute-backend'
```

### High Memory Usage

**Identify Memory Leaks**
```bash
# Memory usage by pod
kubectl top pods -l app=ancient-compute-backend --sort-by=memory

# Check pod memory limit
kubectl get pod <pod-name> -o yaml | grep -A 5 memory

# View memory trend (if metrics available)
kubectl get --raw /apis/metrics.k8s.io/v1beta1/namespaces/default/pods | \
  jq '.items[] | select(.metadata.name | contains("backend")) | .containers[].usage.memory'
```

**Solutions**
```bash
# Restart pod to clear memory
kubectl delete pod <pod-name>

# Increase memory limit
kubectl set resources deployment/ancient-compute-backend \
  -c=backend --limits=memory=4Gi

# Check for memory leaks in code
# Review logs for unbounded data structures
# Run memory profiler in staging
```

### Slow Queries

**Find Slow Queries**
```bash
# Enable query logging
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
ALTER SYSTEM SET log_min_duration_statement = 1000; -- Log queries > 1 sec
SELECT pg_reload_conf();
EOF

# View logs
tail -f /var/log/postgresql/postgresql.log | grep "duration:"

# OR query pg_stat_statements (if enabled)
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
SELECT query, calls, mean_time, total_time
FROM pg_stat_statements
ORDER BY mean_time DESC
LIMIT 20;
EOF
```

**Optimize Slow Query**
```bash
# Get query execution plan
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
EXPLAIN ANALYZE SELECT * FROM exercises WHERE module_id = 1 AND difficulty = 'intermediate';
EOF

# Add missing index if needed
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
CREATE INDEX idx_exercise_module_difficulty
ON exercises(module_id, difficulty);
EOF

# Verify index helps
EXPLAIN ANALYZE <same query>;
```

### High Latency

**Measure Latency**
```bash
# API endpoint latency
for i in {1..10}; do
  curl -w "Response time: %{time_total}s\n" -o /dev/null \
    https://api.ancient-compute.edu/exercises/1
done

# Database query latency
psql -h prod-db.ancient-compute.edu -U app_user -d ancient_compute << EOF
\timing
SELECT * FROM exercises LIMIT 10;
EOF

# Network latency
ping -c 5 prod-db.ancient-compute.edu
```

**Solutions**
```bash
# Check network connectivity
kubectl exec -it <pod-name> -- ping prod-db.ancient-compute.edu

# Scale horizontally (more pods closer to client)
kubectl scale deployment ancient-compute-frontend --replicas=5

# Enable caching
curl -H "Cache-Control: max-age=3600" https://api.ancient-compute.edu/exercises

# Use CDN for static assets
# Enable gzip compression
# Optimize database indexes (see slow queries above)
```

## Incident Response

### P1: Service Down (Complete Outage)

**1. Declare Incident** (< 1 min)
```bash
# Page on-call team
# Open incident in PagerDuty/Slack
# Create incident channel: #ancient-compute-incident
```

**2. Triage** (< 5 min)
```bash
# Check all service endpoints
curl https://api.ancient-compute.edu/health
curl https://learn.ancient-compute.edu/
psql -h prod-db.ancient-compute.edu -c "SELECT 1;"

# View recent deployments
kubectl rollout history deployment/ancient-compute-backend | head -5

# Check logs for errors
kubectl logs deployment/ancient-compute-backend -f --tail=100
```

**3. Immediate Recovery** (< 10 min)
```bash
# Option A: Restart pods (clears transient errors)
kubectl rollout restart deployment/ancient-compute-backend

# Option B: Rollback to previous version
kubectl rollout undo deployment/ancient-compute-backend

# Option C: Scale down and up (force restart)
kubectl scale deployment/ancient-compute-backend --replicas=0
sleep 10
kubectl scale deployment/ancient-compute-backend --replicas=3
```

**4. Verify Recovery**
```bash
# Wait for pods to be ready
kubectl get pods -l app=ancient-compute-backend -w
kubectl wait --for=condition=ready pod -l app=ancient-compute-backend --timeout=300s

# Run smoke tests
./scripts/run_smoke_tests.sh https://api.ancient-compute.edu

# Monitor metrics
watch 'kubectl top pods -l app=ancient-compute-backend'
```

### P2: Degraded Performance

**Degradation: API Response Time > 5 seconds**

```bash
# 1. Check if issue is recent
kubectl logs deployment/ancient-compute-backend -f --tail=50

# 2. Check system resources
kubectl top nodes
kubectl top pods -l app=ancient-compute-backend --sort-by=cpu

# 3. Check database
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
SELECT usename, count(*) FROM pg_stat_activity GROUP BY usename;
SELECT * FROM pg_locks WHERE granted = false;
EOF

# 4. Check for recent deployments
kubectl rollout history deployment/ancient-compute-backend

# 5. Scale horizontally (most effective for performance)
kubectl scale deployment/ancient-compute-backend --replicas=5

# 6. Monitor improvement
for i in {1..10}; do
  curl -w "%{time_total}\n" -o /dev/null https://api.ancient-compute.edu/health
done
```

### P3: Data Consistency Issue

**Discrepancy: User submissions not recorded**

```bash
# 1. Check database connectivity
psql -h prod-db.ancient-compute.edu -U app_user -d ancient_compute << EOF
SELECT COUNT(*) FROM submissions WHERE created_at > now() - interval '1 hour';
EOF

# 2. Check application logs for errors
kubectl logs deployment/ancient-compute-backend -f | grep -i "error\|fail"

# 3. Check table integrity
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
SELECT tablename FROM pg_tables WHERE schemaname = 'public';
SELECT constraint_name FROM information_schema.table_constraints WHERE table_name = 'submissions';
EOF

# 4. Verify foreign keys
psql -h prod-db.ancient-compute.edu -U postgres -d ancient_compute << EOF
SELECT * FROM submissions WHERE user_id NOT IN (SELECT id FROM users);
EOF

# 5. If data loss, restore from backup
# See "Restore Full Database" section above
```

## Backup and Recovery

### Verify Backup Process Working

```bash
# Check last backup
ls -lah backups/ | tail -5

# Verify backup is valid
gzip -t backups/full_20250101_120000.sql.gz && echo "✓ Valid"

# Check backup size (should be reasonable)
du -h backups/full_20250101_120000.sql.gz

# Test restore in staging environment (weekly)
# 1. Spin up staging database
# 2. Restore from production backup
# 3. Verify data integrity
```

### Emergency Recovery Checklist

- [ ] Identify what data was lost
- [ ] Determine recovery point (most recent good backup)
- [ ] Get approval from stakeholder
- [ ] Notify affected users
- [ ] Restore from backup to staging first
- [ ] Verify restored data integrity
- [ ] Restore to production (stop app first)
- [ ] Validate all systems working
- [ ] Notify stakeholders of completion
- [ ] Post-incident review within 24 hours

## References

- **Deployment Guide**: [/deployment/DEPLOYMENT_GUIDE.md]
- **Launch Procedures**: [/.release/LAUNCH_GUIDE.md]
- **Architecture**: [/ARCHITECTURE.md]
- **Kubernetes Docs**: https://kubernetes.io/docs/
- **PostgreSQL Docs**: https://www.postgresql.org/docs/
- **On-Call Contacts**: [See wiki]

---

**Document Version**: 1.0
**Last Updated**: 2025-01-01
**Maintenance**: Updated after each incident
