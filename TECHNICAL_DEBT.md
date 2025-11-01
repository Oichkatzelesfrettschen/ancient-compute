# Ancient Compute - Technical Debt Inventory

**Status**: Comprehensive audit completed
**Date**: 2025-10-31
**Phase**: Phase 2 (70% → 85% completion via Option B)

---

## Overview

This document inventories all identified TODO items, placeholders, and implementation gaps across the Ancient Compute codebase. Items are prioritized by implementation phase and impact on Phase 2/3 completion.

---

## Section 1: Critical TODOs (Phase 2 Blockers)

### 1.1 User Authentication Implementation
**Location**: `backend/src/api/code_execution.py:74`
**Issue**: Code submission database saving requires user authentication
**Blocked By**: User model integration, JWT/session management
**Impact**: Code submissions cannot be persisted; learning analytics impossible
**Resolution Path**: 
  1. Implement user authentication middleware (JWT or sessions)
  2. Add user context to ExecutionRequest
  3. Uncomment code submission saving (lines 74-85)
  4. Add CodeSubmission model validation
**Effort**: 2-3 hours
**Dependencies**: User model, database session management
**Phase**: Week 12 Integration Testing (Phase 2 complete)

### 1.2 Database Connection Checks
**Location**: `backend/src/main.py:37`
**Issue**: `/ready` endpoint lacks database and Redis connection validation
**Current State**: Returns static response without actual health checks
**Impact**: Deployment will not detect database/Redis failures
**Resolution Path**:
  1. Create database connection test in get_db()
  2. Create Redis connection test in redis_client initialization
  3. Add try/except blocks with meaningful error messages
  4. Return 503 Service Unavailable if dependencies fail
**Effort**: 1-2 hours
**Dependencies**: database.py, redis configuration
**Phase**: Week 12 Integration Testing

### 1.3 Prometheus Metrics Implementation
**Location**: `backend/src/main.py:40`
**Issue**: `/metrics` endpoint is placeholder; no actual metrics collected
**Current State**: Returns hardcoded zeros for uptime, requests, users, counts
**Impact**: No observability; cannot monitor service health or usage
**Resolution Path**:
  1. Install prometheus_client package
  2. Create Counter for total requests, Gauge for active users, Histogram for latency
  3. Instrument ExecutionRequest endpoint with metrics middleware
  4. Query database for actual module/lesson counts
  5. Implement request counting middleware
**Effort**: 2-3 hours
**Dependencies**: prometheus_client, SQLAlchemy queries
**Phase**: Week 12 Integration Testing

### 1.4 Uptime Tracking
**Location**: `backend/src/main.py:42`
**Issue**: Uptime always returns 0; requires persistent start time tracking
**Current State**: Returns hardcoded 0 in metrics endpoint
**Impact**: No visibility into service reliability or restart events
**Resolution Path**:
  1. Store app start time at module load
  2. Calculate uptime as (current_time - start_time).total_seconds()
  3. Add to metrics endpoint
**Effort**: 30 minutes
**Dependencies**: None
**Phase**: Week 12 Integration Testing

### 1.5 Request Counting Middleware
**Location**: `backend/src/main.py:44`
**Issue**: Request counter not implemented; always returns 0
**Current State**: Placeholder in metrics endpoint
**Impact**: No request volume visibility; cannot analyze usage patterns
**Resolution Path**:
  1. Create FastAPI middleware to count incoming requests
  2. Store count in thread-safe counter
  3. Expose via `/metrics` endpoint
**Effort**: 1 hour
**Dependencies**: FastAPI middleware utilities
**Phase**: Week 12 Integration Testing

### 1.6 Active Users Query
**Location**: `backend/src/main.py:45`
**Issue**: Active users count not queried from database; returns 0
**Current State**: Placeholder; requires User model implementation
**Impact**: Cannot track user engagement or platform usage
**Resolution Path**:
  1. Implement active user query (users with recent activity)
  2. Add query to database layer
  3. Include in metrics endpoint
**Effort**: 1-2 hours
**Dependencies**: User model, activity tracking
**Phase**: Week 12 Integration Testing

### 1.7 Module/Lesson Counts
**Location**: `backend/src/main.py:46-47`
**Issue**: Curriculum content counts not queried from database
**Current State**: Hardcoded zeros; requires Module and Lesson table queries
**Impact**: No curriculum health visibility; cannot track content load
**Resolution Path**:
  1. Query Module table for count
  2. Query Lesson table for count (across all modules)
  3. Include in metrics endpoint
**Effort**: 30 minutes
**Dependencies**: Module and Lesson models
**Phase**: Week 12 Integration Testing

---

## Section 2: Service Implementation Gaps (Phase 2 Implementations)

### 2.1 LISP Language Service (Week 9.1)
**Status**: Not yet implemented
**Files Needed**:
  - `backend/src/compilers/lisp_lexer.py` (400-500 lines)
  - `backend/src/compilers/lisp_parser.py` (550-650 lines)
  - `backend/src/compilers/lisp_ast.py` (150-200 lines)
  - `backend/src/compilers/lisp_compiler.py` (600-700 lines)
  - `backend/src/services/languages/lisp_service.py` (250-300 lines)
  - `backend/src/compilers/test_lisp_compiler.py` (700-800 lines)
**Integration Points**:
  - Register in `services/languages/__init__.py` (add to get_executor factory)
  - Add to `code_execution.py` ExecutionRequest language literal
  - Add language metadata to `/languages` endpoint
  - Update `code_execution.py` health check (3 → 4 languages)
**Effort**: 1,800-2,200 lines, 2-3 days
**Testing Target**: 65+ tests, 100% pass rate

### 2.2 IDRIS2 Language Service (Week 10.1)
**Status**: Not yet implemented
**Files Needed**:
  - `backend/src/compilers/idris_lexer.py` (500-600 lines)
  - `backend/src/compilers/idris_parser.py` (800-900 lines)
  - `backend/src/compilers/idris_ast.py` (250-300 lines)
  - `backend/src/compilers/idris_compiler.py` (800-900 lines)
  - `backend/src/compilers/idris_types.py` (400-500 lines)
  - `backend/src/services/languages/idris_service.py` (300-350 lines)
  - `backend/src/compilers/test_idris_compiler.py` (900-1000 lines)
**Integration Points**:
  - Register in `services/languages/__init__.py`
  - Add to `code_execution.py` ExecutionRequest language literal
  - Add language metadata to `/languages` endpoint
**Effort**: 2,500-3,000 lines, 3-4 days
**Testing Target**: 70+ tests, 100% pass rate
**Complexity**: Dependent type system requires sophisticated type inference

### 2.3 System F Language Service (Week 10.2)
**Status**: Not yet implemented
**Files Needed**:
  - `backend/src/compilers/systemf_lexer.py` (350-400 lines)
  - `backend/src/compilers/systemf_parser.py` (600-700 lines)
  - `backend/src/compilers/systemf_ast.py` (200-250 lines)
  - `backend/src/compilers/systemf_compiler.py` (700-800 lines)
  - `backend/src/compilers/systemf_types.py` (300-400 lines)
  - `backend/src/services/languages/systemf_service.py` (250-300 lines)
  - `backend/src/compilers/test_systemf_compiler.py` (600-700 lines)
**Integration Points**:
  - Register in `services/languages/__init__.py`
  - Add to `code_execution.py` ExecutionRequest language literal
  - Add language metadata to `/languages` endpoint
**Effort**: 2,000-2,500 lines, 2-3 days
**Testing Target**: 60+ tests, 100% pass rate
**Complexity**: Higher-ranked types, rank polymorphism, forall quantification

### 2.4 Java Language Service (Week 11.1)
**Status**: Not yet implemented
**Files Needed**:
  - `backend/src/compilers/java_lexer.py` (500-600 lines)
  - `backend/src/compilers/java_parser.py` (1000-1200 lines)
  - `backend/src/compilers/java_ast.py` (300-400 lines)
  - `backend/src/compilers/java_compiler.py` (800-900 lines)
  - `backend/src/compilers/java_types.py` (350-450 lines)
  - `backend/src/services/languages/java_service.py` (300-350 lines)
  - `backend/src/compilers/test_java_compiler.py` (800-900 lines)
**Integration Points**:
  - Register in `services/languages/__init__.py`
  - Add to `code_execution.py` ExecutionRequest language literal
  - Add language metadata to `/languages` endpoint
**Effort**: 2,200-2,800 lines, 3-4 days
**Testing Target**: 70+ tests, 100% pass rate
**Complexity**: OOP semantics, class hierarchies, method dispatch

---

## Section 3: Service Factory Placeholder
**Location**: `backend/src/services/languages/__init__.py:23`
**Issue**: get_executor() only returns 3 of 8 languages (C, Python, Haskell)
**Current State**: 
```python
executors = {
    "c": CService,
    "python": PythonService,
    "haskell": HaskellService,
    "babbage-assembly": BabbageAssemblyService,
}
```
**Missing**: IDRIS2, LISP, Java, System F
**Resolution Path**:
  1. Add imports for new services as they're implemented
  2. Extend executors dict with all 8 language entries
  3. Verify get_executor returns correct executor for each language
**Effort**: Incremental (done per language service)
**Phase**: As each service is implemented

---

## Section 4: Requirements.txt Gaps
**Location**: `backend/requirements.txt`
**Current Dependencies**: 51 packages (web, DB, cache, testing, code quality)
**Missing for Option B Services**:

### For LISP Service:
- No SBCL/LISP runtime dependency (Docker-based execution)
- No LISP parser libraries (building from scratch)

### For IDRIS2 Service:
- No IDRIS2 compiler (Docker-based execution)
- idris2 (or from containers)

### For System F Service:
- No System F runtime (implementing from scratch)

### For Java Service:
- No Java compiler (Docker-based execution)
- java (JDK, from containers)

### For Code Generation:
- prometheus-client (for metrics) - MISSING
- (other packages available)

**Action Required**:
  1. Add prometheus-client==0.19.0
  2. Document that language runtimes are Docker-based
  3. Add Docker image specifications to Dockerfile or docker-compose.yml
**Effort**: 1 hour

---

## Section 5: Architecture Integration Placeholders

### 5.1 Service Health Check (code_execution.py:116)
**Issue**: Returns hardcoded "3" for languages_available
**Current State**:
```python
return {
    "status": "healthy",
    "service": "code-execution",
    "languages_available": 3,  # C, Python, Haskell currently implemented
}
```
**Resolution**: Update to actual count as services are implemented
**Effort**: 10 minutes per language (trivial)

### 5.2 Language Metadata (code_execution.py:80-115)
**Issue**: All 8 language entries are defined, but only 3 are implemented
**Current State**: Full metadata for all languages, but many not functional
**Resolution**: Verify metadata accuracy as services are implemented
**Effort**: Validation only (30 minutes)

### 5.3 Service Executor Registration (services/languages/__init__.py)
**Issue**: Missing imports and factory entries for IDRIS2, LISP, Java, System F
**Resolution**: Add as services are implemented
**Effort**: 1-2 minutes per service

---

## Section 6: Database Model Gaps

### 6.1 CodeSubmission Model (models/lesson.py)
**Status**: Model defined, but usage disabled due to missing authentication
**Issue**: Cannot save submissions without user context
**Dependency**: User authentication implementation (Section 1.1)
**Resolution**: Enable when user auth is complete
**Effort**: Included in Section 1.1

### 6.2 Activity Tracking
**Status**: Not implemented
**Issue**: No timestamps for active user queries (Section 1.6)
**Required Fields**: Last activity timestamp on User model
**Resolution**: Add updated_at field to User model
**Effort**: 1 hour

---

## Section 7: Code Quality Improvements

### 7.1 Type Hints Completeness
**Status**: Most functions have type hints; some void returns need annotation
**Review**: All public APIs should have complete type hints
**Effort**: 2-3 hours across codebase

### 7.2 Docstring Coverage
**Status**: Compilers have comprehensive docstrings; services need review
**Target**: 100% public API documentation
**Effort**: 3-4 hours

### 7.3 Error Handling Consistency
**Status**: Compilers have good error handling; API endpoints need review
**Target**: All errors should return meaningful messages
**Effort**: 2-3 hours

---

## Section 8: Testing Gaps

### 8.1 Integration Tests (Phase 2 Complete - Week 12)
**Status**: Not yet implemented
**Components to Test**:
  1. Multi-language compilation pipeline
  2. Service factory instantiation
  3. API endpoint execution across all languages
  4. Database persistence of results
  5. Error handling and edge cases
**Files Needed**:
  - `backend/tests/integration/test_multi_language.py`
  - `backend/tests/integration/test_api_endpoints.py`
  - `backend/tests/integration/test_service_factory.py`
**Effort**: 1,500-2,000 lines, 2-3 days
**Target**: 40+ integration tests, 100% pass rate

### 8.2 End-to-End Tests (Phase 2 Complete - Week 12)
**Status**: Not yet implemented
**Scenarios to Test**:
  1. Full compilation pipeline for each language
  2. Error recovery and reporting
  3. Resource limit enforcement
  4. Concurrent execution safety
**Effort**: 800-1,000 lines, 1-2 days

---

## Section 9: Documentation Gaps

### 9.1 CLAUDE.md Updates
**Status**: Project-level CLAUDE.md exists but needs Phase 2 completion info
**Updates Needed**:
  - Option B implementation details
  - Phase 2 completion status (70% → 85%)
  - Language service interface specification
  - Integration testing strategy
**Effort**: 2-3 hours

### 9.2 README.md Updates
**Status**: README exists but needs comprehensive overview
**Updates Needed**:
  - Project vision and scope
  - Architecture overview
  - How to run services
  - Contributing guidelines
**Effort**: 2-3 hours

### 9.3 AGENTS.md Updates
**Status**: AGENTS.md exists but needs multi-agent coordination details
**Updates Needed**:
  - Agent roles and responsibilities
  - Coordination strategy for Week 9-12
  - Knowledge sharing between agents
**Effort**: 1-2 hours

### 9.4 Service Implementation Templates
**Status**: Python/Haskell services exist; need templates for new languages
**Updates Needed**:
  - Create LISP_SERVICE_TEMPLATE.md
  - Create IDRIS2_SERVICE_TEMPLATE.md
  - Create SYSTEMF_SERVICE_TEMPLATE.md
  - Create JAVA_SERVICE_TEMPLATE.md
**Effort**: 2 hours (extract from existing implementations)

---

## Section 10: Configuration Gaps

### 10.1 Docker Compose Services
**Status**: docker-compose.yml exists but needs language service containers
**Updates Needed**:
  - Add lisp-service container
  - Add idris-service container
  - Add systemf-service container
  - Add java-service container
  - Verify resource limits (memory, CPU)
**Effort**: 2-3 hours

### 10.2 Environment Variables
**Status**: .env file exists with database/Redis config
**Updates Needed**:
  - Document all environment variables
  - Add language-specific timeouts
  - Add execution resource limits
**Effort**: 1 hour

### 10.3 CI/CD Pipeline
**Status**: .github/workflows/ci.yml exists but needs language service tests
**Updates Needed**:
  - Add test steps for each language service
  - Add integration test execution
  - Add code coverage reporting
**Effort**: 2-3 hours

---

## Summary: Implementation Roadmap

### Phase 2 Remaining Work (Option B - 6-7 weeks)

**Week 9.1** (2-3 days): LISP Language Service
- Files: 1,800-2,200 lines
- Tests: 65+ tests
- Integration: Register in factory, add to API

**Week 10.1** (3-4 days): IDRIS2 Language Service
- Files: 2,500-3,000 lines
- Tests: 70+ tests
- Integration: Register in factory, add to API

**Week 10.2** (2-3 days): System F Language Service
- Files: 2,000-2,500 lines
- Tests: 60+ tests
- Integration: Register in factory, add to API

**Week 11.1** (3-4 days): Java Language Service
- Files: 2,200-2,800 lines
- Tests: 70+ tests
- Integration: Register in factory, add to API

**Week 12** (3-4 days): Phase 2 Integration Testing + Technical Debt Resolution
- Integration tests: 1,500-2,000 lines
- End-to-end tests: 800-1,000 lines
- Resolve all Section 1-4 TODOs
- Update all documentation
- Final repository audit

**Total Option B Effort**: 10,300-12,700 lines, 6-7 weeks, 100% test coverage

---

## Priority Matrix

| Section | Priority | Effort | Impact | Phase |
|---------|----------|--------|--------|-------|
| 1.1 User Auth | CRITICAL | 2-3h | Enables persistence | Week 12 |
| 1.2-1.7 Metrics/Health | HIGH | 5-8h | Enables observability | Week 12 |
| 2.1-2.4 Language Services | CRITICAL | 10,300-12,700 lines | Core Phase 2 | Week 9-11 |
| 3-5 Integration | HIGH | 2-3h | Enables all services | Week 9-12 |
| 6-7 Quality | MEDIUM | 5-6h | Improves reliability | Week 12 |
| 8 Testing | CRITICAL | 2,300-3,000 lines | Validates Phase 2 | Week 12 |
| 9-10 Documentation | MEDIUM | 10-12h | Improves maintainability | Week 12 |

---

## Sign-Off

**Audit Status**: ✓ COMPLETE
**Total Issues Identified**: 15+ items across 10 sections
**Implementation Path**: Option B (6-7 weeks)
**Quality Target**: 100% test pass rate, no warnings as errors
**Estimated Completion**: Week 12 (early December 2025)

---

*End of Technical Debt Document*
