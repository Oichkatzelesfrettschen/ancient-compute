# Ancient Compute: Repository Compatibility Matrix

**Date**: November 2, 2025  
**Version**: 1.0  
**Purpose**: Track version compatibility across repositories

---

## Current Versions

| Repository | Version | Status | Last Updated |
|------------|---------|--------|--------------|
| ancient-compute (core) | 1.0.0 | Stable | 2025-11-02 |
| ancient-compute-frontend | TBD | Not extracted | - |
| ancient-compute-backend | TBD | Not extracted | - |
| ancient-compute-babbage-engine | TBD | Not extracted | - |
| ancient-compute-language-services | TBD | Not extracted | - |
| ancient-compute-curriculum | TBD | Not extracted | - |
| ancient-compute-docs | TBD | Not extracted | - |

---

## Compatibility Matrix

### Frontend ↔ Backend

| Frontend Version | Backend Version | Status | Notes |
|-----------------|-----------------|--------|-------|
| 1.x.x | 1.x.x | ✅ Compatible | Initial release compatibility |
| 2.x.x | 1.x.x | ⚠️ Degraded | Some new features unavailable |
| 1.x.x | 2.x.x | ❌ Incompatible | API breaking changes |

**API Version**: v1  
**Contract**: OpenAPI 3.0 specification in `backend/docs/api-spec.yaml`

### Backend ↔ Language Services

| Backend Version | Services Version | Status | Notes |
|----------------|------------------|--------|-------|
| 1.x.x | 1.x.x | ✅ Compatible | Standardized API contract |
| 2.x.x | 1.x.x | ✅ Compatible | Backward compatible |

**API Contract**: All services expose:
- `POST /execute` - Code execution
- `GET /health` - Health check
- `GET /version` - Service version

### Backend ↔ Babbage Engine

| Backend Version | Babbage Version | Status | Notes |
|----------------|-----------------|--------|-------|
| 1.x.x | 1.x.x | ✅ Compatible | Initial release |

**Integration**: Backend imports Babbage emulator as Python package

### Backend ↔ Curriculum

| Backend Version | Curriculum Version | Status | Notes |
|----------------|-------------------|--------|-------|
| 1.x.x | 1.x.x | ✅ Compatible | Content delivery API |

**Integration**: Backend serves curriculum content via REST API

---

## Dependency Requirements

### Core Repository

```yaml
dependencies:
  git: ">=2.30"
  docker: ">=20.10"
  docker-compose: ">=2.0"
  bazel: ">=6.4.0"
  make: ">=4.0"
```

### Frontend

```yaml
dependencies:
  node: ">=20.0.0"
  pnpm: ">=8.0.0"
runtime:
  backend-api: "v1"
```

**Key Dependencies**:
```json
{
  "svelte": "^4.0.0",
  "sveltekit": "^1.20.0",
  "typescript": "^5.0.0",
  "d3": "^7.8.0",
  "three": "^0.160.0"
}
```

### Backend

```yaml
dependencies:
  python: ">=3.11"
  postgresql: ">=14"
  redis: ">=7.0"
runtime:
  language-services: "v1"
  babbage-engine: "v1"
```

**Key Dependencies**:
```
fastapi>=0.104.0
sqlalchemy>=2.0.0
pydantic>=2.0.0
redis>=5.0.0
```

### Babbage Engine

```yaml
dependencies:
  python: ">=3.11"
runtime: standalone
```

**Key Dependencies**:
```
pytest>=7.4.0
mypy>=1.5.0
```

### Language Services

```yaml
dependencies:
  docker: ">=20.10"
  gvisor: "latest"
runtime: standalone
```

**Per-Service Requirements**:
- C Service: gcc>=11.0
- Python Service: python>=3.11
- Haskell Service: ghc>=9.4
- IDRIS2 Service: idris2>=0.6.0
- LISP Service: sbcl>=2.3
- Java Service: openjdk>=17
- Assembly Service: nasm>=2.15
- System F Service: custom (no external deps)

### Curriculum

```yaml
dependencies: none
runtime: standalone
format: markdown, json
```

### Documentation

```yaml
dependencies:
  texlive: full
  xelatex: latest
  bibtex: latest
runtime: standalone
```

---

## Breaking Change Policy

### Major Version (X.0.0)

**When to increment**:
- Breaking API changes
- Incompatible database schema changes
- Removal of deprecated features

**Migration Required**: Yes  
**Backward Compatible**: No

### Minor Version (x.Y.0)

**When to increment**:
- New features
- New API endpoints (backward compatible)
- New functionality

**Migration Required**: No  
**Backward Compatible**: Yes

### Patch Version (x.y.Z)

**When to increment**:
- Bug fixes
- Security patches
- Performance improvements

**Migration Required**: No  
**Backward Compatible**: Yes

---

## API Versioning Strategy

### REST API (Backend)

- **Current Version**: v1
- **Base Path**: `/api/v1/`
- **Deprecation Policy**: 6 months notice
- **Support Policy**: Current version + 1 previous version

**Version Headers**:
```
Accept: application/vnd.ancient-compute.v1+json
API-Version: 1
```

### WebSocket API (Backend)

- **Current Version**: v1
- **Protocol**: JSON messages with version field
- **Backward Compatibility**: Message type versioning

**Message Format**:
```json
{
  "version": 1,
  "type": "execute_code",
  "data": { ... }
}
```

### Language Service API

- **Current Version**: v1
- **Standard Contract**: All services implement same endpoints
- **Versioning**: Path-based (`/v1/execute`)

---

## Testing Compatibility

### Integration Tests

Location: `ancient-compute/tests/integration/`

**Test Matrix**:
```
Frontend v1.x + Backend v1.x = ✅ Test Suite 1
Frontend v2.x + Backend v1.x = ✅ Test Suite 2 (degraded mode)
Frontend v1.x + Backend v2.x = ❌ Expected to fail

Backend v1.x + Services v1.x = ✅ Test Suite 3
Backend v1.x + Babbage v1.x = ✅ Test Suite 4
```

### Version Discovery

Each repository exposes version information:

**Backend**: `GET /api/v1/version`
```json
{
  "version": "1.2.3",
  "api_version": "v1",
  "build_date": "2025-11-02",
  "dependencies": {
    "babbage-engine": "1.0.0",
    "curriculum": "1.1.0"
  }
}
```

**Frontend**: `window.__APP_VERSION__`
```javascript
{
  version: "1.2.3",
  buildDate: "2025-11-02",
  requiredBackendVersion: "^1.0.0"
}
```

**Services**: `GET /version`
```json
{
  "service": "c-language-service",
  "version": "1.0.0",
  "language_version": "gcc 11.4.0",
  "api_version": "v1"
}
```

---

## Deployment Compatibility

### Docker Images

All images tagged with semantic versions:

```
ghcr.io/oichkatzelesfrettschen/ancient-compute-frontend:1.2.3
ghcr.io/oichkatzelesfrettschen/ancient-compute-backend:1.2.3
ghcr.io/oichkatzelesfrettschen/ancient-compute-service-c:1.0.0
```

**Compatibility Tags**:
- `latest` - Most recent stable release
- `v1` - Latest v1.x.x release
- `v1.2` - Latest v1.2.x release
- `v1.2.3` - Specific version

### Docker Compose

Version pinning in `docker-compose.yml`:

```yaml
services:
  frontend:
    image: ghcr.io/.../ancient-compute-frontend:${FRONTEND_VERSION:-1.2.3}
  
  backend:
    image: ghcr.io/.../ancient-compute-backend:${BACKEND_VERSION:-1.2.3}
```

---

## Upgrade Paths

### Frontend Upgrade

1. Check backend version compatibility
2. Update frontend dependencies
3. Run integration tests
4. Deploy to staging
5. Verify functionality
6. Deploy to production

### Backend Upgrade

1. Check database migration requirements
2. Update backend dependencies
3. Run database migrations
4. Run integration tests
5. Deploy to staging
6. Verify all services compatible
7. Deploy to production

### Breaking Change Upgrade

1. Deploy new backend version alongside old (blue-green)
2. Gradually migrate traffic
3. Monitor for errors
4. Complete migration
5. Deprecate old version
6. Remove old version after deprecation period

---

## Version Support Timeline

| Version | Release Date | Support Ends | Status |
|---------|-------------|--------------|--------|
| 1.0.0 | 2025-11-02 | 2026-05-02 | Current |
| 0.9.x | 2025-09-01 | 2025-12-01 | Deprecated |

**Support Policy**:
- Current major version: Full support
- Previous major version: Security fixes only (6 months)
- Older versions: No support

---

## Changelog and Migration Guides

Each repository maintains:
- `CHANGELOG.md` - Version history
- `MIGRATION.md` - Upgrade guides for breaking changes

Example migration guide structure:

```markdown
# Migrating from v1 to v2

## Breaking Changes

1. **API Endpoint Renamed**
   - Old: `/api/v1/users`
   - New: `/api/v2/accounts`
   - Migration: Update all API calls

2. **Database Schema Change**
   - Run migration: `alembic upgrade head`
   - Backup database before upgrading

## New Features

- Feature 1
- Feature 2

## Deprecations

- Deprecated feature 1 (removed in v3)
```

---

## Monitoring Compatibility

### Health Checks

All services expose health endpoints:

```bash
# Check frontend health
curl http://localhost:3000/health

# Check backend health
curl http://localhost:8000/health

# Check service health
curl http://localhost:8001/health  # C service
curl http://localhost:8002/health  # Python service
```

### Compatibility Alerts

Set up monitoring for:
- Version mismatches between services
- Deprecated API usage
- Failed health checks
- Integration test failures

---

## References

- **Semantic Versioning**: https://semver.org/
- **API Versioning Best Practices**: https://restfulapi.net/versioning/
- **Docker Image Tagging**: https://docs.docker.com/engine/reference/commandline/tag/

---

## Update Schedule

This compatibility matrix is updated:
- On every release
- When breaking changes are introduced
- Monthly review for accuracy

**Last Review**: November 2, 2025  
**Next Review**: December 2, 2025  
**Maintainer**: @Oichkatzelesfrettschen

---

**Document Version**: 1.0  
**Last Updated**: November 2, 2025
