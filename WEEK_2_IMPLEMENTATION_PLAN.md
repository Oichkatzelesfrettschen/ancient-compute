# Week 2 Implementation Plan
## Ancient Compute Educational Platform - Days 6-10

### Executive Summary
Week 2 focuses on implementing the core execution infrastructure: language services with sandboxing, 5-layer security architecture, content schema with historical data, and interactive timeline visualization. All components integrate with the existing Week 1 foundation (FastAPI backend, SvelteKit frontend, PostgreSQL database).

## Day-by-Day Implementation Schedule

### Day 6 (Monday): Language Services Foundation
**Goal**: Implement base executor and first 3 language services

#### Morning (4 hours)
1. **Base Executor Implementation**
   ```bash
   # Create service directory structure
   mkdir -p backend/services/{languages,security,containers}
   mkdir -p backend/services/containers/{base,c,python,haskell}

   # Implement base_executor.py
   touch backend/services/base_executor.py
   touch backend/services/__init__.py
   ```

2. **Docker Base Image**
   ```bash
   # Build base container
   cd backend/services/containers/base
   docker build -t ancient-compute/base:latest .
   ```

3. **C Language Service**
   - Implement `c_service.py`
   - Create C container Dockerfile
   - Add GCC compilation pipeline
   - Test with hello world

#### Afternoon (4 hours)
4. **Python Service with RestrictedPython**
   ```bash
   pip install RestrictedPython
   ```
   - Implement `python_service.py`
   - Configure safe globals
   - Test restriction enforcement

5. **Haskell Service**
   - Implement `haskell_service.py`
   - Create Haskell container
   - Test GHC compilation

6. **API Integration**
   ```python
   # backend/src/api/code_execution.py
   # Add /api/execute/run endpoint
   # Add /api/execute/languages endpoint
   ```

**Deliverables**:
- [ ] Base executor class working
- [ ] C execution with compilation
- [ ] Python with restrictions
- [ ] Haskell compilation and execution
- [ ] API endpoints responding

### Day 7 (Tuesday): Security Layers 1-3
**Goal**: Implement Docker isolation, gVisor, and Seccomp filtering

#### Morning (4 hours)
1. **Layer 1: Docker Hardening**
   ```bash
   # Configure Docker daemon
   sudo cp docker/daemon.json /etc/docker/daemon.json
   sudo systemctl restart docker
   ```

2. **Layer 2: gVisor Installation**
   ```bash
   # Install gVisor runtime
   ./backend/services/security/install_gvisor.sh

   # Test with runsc
   docker run --runtime=runsc --rm hello-world
   ```

3. **gVisor Configuration per Language**
   - Implement `gvisor_config.py`
   - Configure platform settings
   - Test isolation levels

#### Afternoon (4 hours)
4. **Layer 3: Seccomp Profiles**
   ```bash
   # Deploy seccomp profiles
   sudo mkdir -p /etc/docker/seccomp
   for lang in c python haskell assembly; do
     sudo cp seccomp/${lang}.json /etc/docker/seccomp/
   done
   ```

5. **Seccomp Validation**
   - Test forbidden syscalls
   - Verify filtering works
   - Document blocked operations

6. **Security Test Suite**
   ```bash
   # Run security tests
   pytest backend/services/security/tests/test_security_layers.py -v
   ```

**Deliverables**:
- [ ] Docker daemon hardened
- [ ] gVisor runtime operational
- [ ] Seccomp profiles deployed for 4 languages
- [ ] Security tests passing
- [ ] Penetration tests documented

### Day 8 (Wednesday): Security Layers 4-5 & Remaining Languages
**Goal**: Complete security architecture and implement remaining language services

#### Morning (4 hours)
1. **Layer 4: Cgroups v2 Configuration**
   ```bash
   # Setup cgroups hierarchy
   sudo mkdir -p /sys/fs/cgroup/ancient_compute
   echo "+cpu +memory +pids +io" | sudo tee /sys/fs/cgroup/ancient_compute/cgroup.subtree_control
   ```
   - Implement `cgroups_manager.py`
   - Configure language-specific limits
   - Test resource enforcement

2. **Layer 5: Read-only Filesystem**
   - Implement `filesystem_manager.py`
   - Configure tmpfs mounts
   - Test overlay filesystem

3. **Security Monitoring**
   - Implement `monitor.py`
   - Add Prometheus metrics
   - Create alerting rules

#### Afternoon (4 hours)
4. **Remaining Language Services**
   - IDRIS2 service implementation
   - LISP service (SBCL)
   - Assembly service (NASM)
   - Java service (OpenJDK)
   - System F service

5. **Language Service Testing**
   ```bash
   # Test all 8 languages
   pytest backend/services/tests/test_executors.py::test_all_languages -v
   ```

6. **Bazel Build Integration**
   ```bash
   # Build all services with Bazel
   bazel build //backend/services/...
   ```

**Deliverables**:
- [ ] All 5 security layers operational
- [ ] 8 language services implemented
- [ ] Resource limits enforced
- [ ] Monitoring dashboard active
- [ ] All tests passing

### Day 9 (Thursday): Content Schema & Database Migration
**Goal**: Implement content schema and seed historical data

#### Morning (4 hours)
1. **Database Schema Migration**
   ```bash
   # Create migration
   alembic revision --autogenerate -m "Add content schema tables"

   # Run migration
   alembic upgrade head
   ```

2. **SQLAlchemy Models**
   - Add content models to `backend/src/models/content.py`
   - Era, Civilization, TimelineEvent models
   - LessonContent, QuizQuestion models
   - HistoricalAlgorithm, Artifact models

3. **Seed Historical Data**
   ```python
   # backend/src/content/seed_data.py
   python -m backend.src.content.seed_data
   ```
   - Load Module 0 (Prehistory) content
   - Load Module 1 (Ancient) content
   - Add timeline events

#### Afternoon (4 hours)
4. **Content API Endpoints**
   ```python
   # backend/src/api/content.py
   # GET /api/content/timeline
   # GET /api/content/civilizations
   # GET /api/content/cultural-exchanges/{id}
   # GET /api/content/lesson/{id}/content
   ```

5. **Content Validation**
   - Implement `validators.py`
   - ASCII-only enforcement
   - Markdown validation
   - Test content structure

6. **Type Theory Content Integration**
   - Link type theory modules to timeline
   - Add STLC lesson to Module 7
   - Add System F to Module 8

**Deliverables**:
- [ ] Content schema migrated
- [ ] 2 modules with complete content
- [ ] 50+ timeline events loaded
- [ ] Content API operational
- [ ] Validation framework active

### Day 10 (Friday): Timeline Visualization & Integration
**Goal**: Implement D3.js timeline and integrate all components

#### Morning (4 hours)
1. **Timeline Component Structure**
   ```bash
   # Create timeline components
   mkdir -p frontend/src/lib/components/timeline/{stores,utils,types}
   touch frontend/src/lib/components/timeline/Timeline.svelte
   touch frontend/src/lib/components/timeline/Timeline2D.svelte
   touch frontend/src/lib/components/timeline/Timeline3D.svelte
   ```

2. **D3.js 2D Timeline**
   ```bash
   npm install d3 @types/d3
   ```
   - Implement Timeline2D.svelte
   - Add zoom/pan functionality
   - Category swim lanes
   - Era backgrounds

3. **Timeline Controls**
   - Year range filters
   - Category filters
   - Importance filtering
   - Search functionality

#### Afternoon (4 hours)
4. **Three.js 3D Timeline**
   ```bash
   npm install three @types/three
   ```
   - Implement Timeline3D.svelte
   - Spiral timeline design
   - Event spheres with importance sizing
   - Connection curves

5. **Integration Testing**
   - Test language execution from frontend
   - Test timeline data loading
   - Test security layers with malicious code
   - Performance testing with 1000+ events

6. **Documentation & Demo**
   - Update README with Week 2 features
   - Create demo video
   - Document security features
   - Prepare Week 3 planning

**Deliverables**:
- [ ] 2D timeline with filtering
- [ ] 3D spiral timeline
- [ ] Timeline controls working
- [ ] Full integration tested
- [ ] Demo video recorded

## File Creation Checklist

### Backend Files (25 files)
```
backend/services/
├── __init__.py
├── base_executor.py
├── languages/
│   ├── __init__.py
│   ├── c_service.py
│   ├── python_service.py
│   ├── haskell_service.py
│   ├── idris_service.py
│   ├── lisp_service.py
│   ├── assembly_service.py
│   ├── java_service.py
│   └── systemf_service.py
├── security/
│   ├── __init__.py
│   ├── seccomp_profiles.py
│   ├── resource_limits.py
│   ├── gvisor_config.py
│   ├── cgroups_manager.py
│   ├── filesystem_manager.py
│   ├── seccomp_loader.py
│   ├── monitor.py
│   └── tmpfs_config.py
├── tests/
│   ├── test_executors.py
│   └── test_security_layers.py
└── BUILD.bazel

backend/src/
├── models/
│   └── content.py (new)
├── api/
│   ├── code_execution.py (new)
│   └── content.py (new)
└── content/
    ├── seed_data.py
    ├── validators.py
    └── modules/
        ├── module_0_prehistory.py
        └── module_1_ancient.py
```

### Frontend Files (15 files)
```
frontend/src/lib/components/timeline/
├── Timeline.svelte
├── Timeline2D.svelte
├── Timeline3D.svelte
├── TimelineControls.svelte
├── TimelineTooltip.svelte
├── TimelineMinimap.svelte
├── stores/
│   ├── timelineStore.ts
│   └── filterStore.ts
├── utils/
│   ├── scaleHelpers.ts
│   ├── colorSchemes.ts
│   └── dataTransforms.ts
├── types/
│   └── timeline.ts
└── tests/
    └── Timeline.test.ts

frontend/src/lib/api/
└── timeline.ts (new)
```

### Docker/Security Files (20 files)
```
backend/services/containers/
├── base/Dockerfile
├── c/
│   ├── Dockerfile
│   └── compile.sh
├── python/
│   ├── Dockerfile
│   └── runner.py
├── haskell/
│   ├── Dockerfile
│   └── setup.sh
├── idris/
│   ├── Dockerfile
│   └── setup.sh
├── lisp/
│   ├── Dockerfile
│   └── setup.sh
├── assembly/
│   ├── Dockerfile
│   └── assemble.sh
├── java/
│   ├── Dockerfile
│   └── compile.sh
└── systemf/
    ├── Dockerfile
    └── setup.sh

backend/services/security/
├── seccomp/
│   ├── base.json
│   ├── c.json
│   ├── python.json
│   ├── haskell.json
│   ├── assembly.json
│   ├── idris.json
│   ├── lisp.json
│   ├── java.json
│   └── systemf.json
├── install_gvisor.sh
├── harden_container.sh
├── monitor_cgroups.sh
└── deploy_security.sh

/etc/docker/
└── daemon.json (modified)
```

## Dependencies to Install

### Python Dependencies
```bash
pip install RestrictedPython prometheus-client
```

### JavaScript Dependencies
```bash
npm install d3 @types/d3 three @types/three
```

### System Dependencies
```bash
# Debian/Ubuntu
apt-get install gcc nasm sbcl ghc openjdk-17-jdk

# gVisor
wget https://storage.googleapis.com/gvisor/releases/release/latest/x86_64/runsc
chmod +x runsc && sudo mv runsc /usr/local/bin/
```

## Testing Checkpoints

### Day 6 Tests
```bash
# Test basic execution
curl -X POST http://localhost:8000/api/execute/run \
  -H "Content-Type: application/json" \
  -d '{"language": "c", "code": "#include <stdio.h>\nint main() { printf(\"Hello\"); return 0; }"}'
```

### Day 7 Tests
```bash
# Test security violations
pytest backend/services/security/tests/test_security_layers.py::test_seccomp_restrictions
pytest backend/services/security/tests/test_security_layers.py::test_network_isolation
```

### Day 8 Tests
```bash
# Test resource limits
pytest backend/services/security/tests/test_security_layers.py::test_resource_limits
```

### Day 9 Tests
```bash
# Test content API
curl http://localhost:8000/api/content/timeline?start_year=-3500&end_year=500
```

### Day 10 Tests
```bash
# Full integration test
npm run test:e2e
```

## Risk Assessment

### High Risk Items
1. **gVisor Compatibility**: May have issues on Windows/WSL2
   - Mitigation: Fall back to standard Docker isolation
   - Alternative: Use Firecracker microVMs

2. **Performance with 8 Languages**: Container startup time
   - Mitigation: Pre-warm container pool
   - Alternative: Use WebAssembly for some languages

3. **Timeline Rendering Performance**: 1000+ events
   - Mitigation: Implement clustering and LOD
   - Alternative: Virtual scrolling

### Medium Risk Items
1. **Seccomp Profile Complexity**: May block legitimate operations
   - Mitigation: Iterative refinement based on testing
   - Alternative: Use AppArmor profiles

2. **Content Migration**: Large amount of historical data
   - Mitigation: Incremental loading
   - Alternative: Start with subset

### Low Risk Items
1. **D3.js Learning Curve**: Team may need ramp-up
   - Mitigation: Use examples and documentation
   - Alternative: Use simpler charting library

## Success Criteria

### Functional Requirements
- [ ] All 8 languages execute code successfully
- [ ] Security layers prevent malicious operations
- [ ] Timeline displays 100+ events smoothly
- [ ] Content for 2 modules fully loaded
- [ ] API endpoints return correct data

### Performance Requirements
- [ ] Code execution < 2 seconds for simple programs
- [ ] Timeline renders < 500ms for 1000 events
- [ ] Container startup < 1 second
- [ ] API response time < 100ms

### Security Requirements
- [ ] No container escapes possible
- [ ] Resource limits enforced
- [ ] Network isolation verified
- [ ] Filesystem restrictions working

## Week 3 Preview

Based on Week 2 completion, Week 3 will focus on:
1. User authentication and authorization
2. Progress tracking implementation
3. Interactive exercises and quizzes
4. Advanced timeline features (search, filtering)
5. Content for Modules 2-4
6. WebSocket real-time updates
7. Caching layer with Redis
8. Performance optimizations

## Conclusion

Week 2 establishes the core technical infrastructure for secure code execution and historical content visualization. The multi-layered security architecture ensures safe execution of untrusted code, while the timeline visualization provides an engaging way to explore computational history. All components integrate with the Week 1 foundation and prepare for Week 3's user-facing features.