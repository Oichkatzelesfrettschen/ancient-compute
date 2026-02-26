# Ancient Compute - TODO Tracker & Project Planner

**Document Version**: 3.0
**Date**: February 26, 2026
**Status**: Updated after Phases 0-8 completion (Hypergranular Debt Resolution)
**Purpose**: Comprehensive task tracking, priorities, and execution plan

**Planning Context**:
- Canonical strategy roadmap: `docs/general/MASTER_ROADMAP.md`
- Canonical execution tracker: `docs/general/TODO_TRACKER.md`
- Planning classification and archive policy: `docs/general/PLANNING_CANONICAL_MAP.md`
- Hardware-first sequencing: `docs/general/HARDWARE_LANGUAGE_BRINGUP_PLAN.md`
- Active wave ledger: `docs/general/EXECUTION_RESCOPED_PLAN_2026-02.md`
- Active test gate policy: `docs/general/TEST_GATE_MATRIX.md`

---

## Overview

This document tracks all pending tasks across the Ancient Compute project, organized by priority, phase, and category. It serves as the single source of truth for "what needs to be done" and "in what order."

**Related Documents**:
- MASTER_ROADMAP.md - Strategic vision and phase planning
- TECHNICAL_DEBT.md - Known issues and implementation gaps
- PROJECT_STATUS.md - Current status and metrics

## Planning Hygiene Tasks

- [x] Ensure superseded planning docs are archived with metadata.
- [x] Ensure each archive entry has a canonical successor in `docs/archive/INDEX.md`.
- [x] Merge novel constraints from archived plans before marking them obsolete.

## Last Verified Against Tests

- `pytest -q backend/tests/unit/test_executors_unit.py` (passes locally)
- `pytest -q backend/tests/unit/test_language_registry.py` (passes locally)
- `pytest -q backend/tests/integration/test_cross_language.py` (passes locally)

## Test Status Matrix (2026-02-24)

| Suite | Current Status | Notes |
|------|----------------|-------|
| `backend/tests/unit/test_executors_unit.py` | PASS | Rewritten to current `services.languages` API and no longer module-skipped |
| `backend/tests/unit/test_language_registry.py` | PASS | Verifies canonical IDs, alias normalization, and status-summary integrity |
| `backend/tests/integration/test_cross_language.py` | PASS | Verifies execution API language metadata, alias handling, and health summary consistency |
| Full backend unit suite | NOT RUN IN THIS PASS | Needs scheduled gate run after tracker/docs reconciliation |

## CI Gate Tiers

| Tier | Purpose | Command(s) |
|------|---------|------------|
| Tier A | Fast unit smoke (pre-merge) | `pytest -q backend/tests/unit/test_executors_unit.py backend/tests/unit/test_language_registry.py` |
| Tier B | Service integration focus | `pytest -q backend/tests/integration/test_cross_language.py` |
| Tier C | Full backend regression | `pytest -q backend/tests` |

---

## Table of Contents

1. [Current Sprint (Hardware Bring-up Rescope)](#current-sprint-hardware-bring-up-rescope)
2. [Legacy Sprint Snapshot (Week 9, Historical)](#legacy-sprint-snapshot-week-9-historical)
3. [High Priority (Weeks 9-12)](#high-priority-weeks-9-12)
4. [Medium Priority (Weeks 13-18)](#medium-priority-weeks-13-18)
5. [Low Priority (Weeks 19+)](#low-priority-weeks-19)
6. [Backlog](#backlog)
7. [Completed](#completed)

---

## Current Sprint (Hardware Bring-up Rescope)

**Focus**: Assembly-first stabilization, ABI lock-in, and freestanding C subset bring-up  
**Reference**: `docs/general/HARDWARE_LANGUAGE_BRINGUP_PLAN.md`  
**Execution Window**: Immediate next 2-3 engineering cycles

### Sprint Tasks

- [x] Make language readiness machine-readable in API metadata (`implementation_status`, aliases, execution mode).
- [x] Normalize `/api/v1/execute/run` language IDs/aliases to canonical IDs.
- [x] Add registry/health consistency tests for `/api/v1/execute/languages` and `/api/v1/execute/health`.
- [x] Add Babbage simulation parameter verifier and contract tests.
- [x] Add active test-gate matrix and quarantine register.
- [x] Build assembler golden corpus under `backend/tests` for deterministic machine-code verification.
- [x] Write ABI contract spec doc (calling convention, stack/memory model, register preservation).
- [x] Add ABI conformance tests (positive and intentional-violation cases).
- [x] Define freestanding C subset matrix and explicit unsupported-feature diagnostics.
- [x] Implement Tier B/C Babbage extensions (SHL, SHR, bitwise logic, checksums).
- [x] Implement Lovelace extensions (Music Engine, Symbolic Mode).
- [ ] Promote one partial frontend at a time only after Gate 0/1/2 criteria are green.

### Exit Criteria

- [ ] Gate 0 (assembler stability) is green in CI.
- [ ] Gate 1 (ABI conformance) is green in CI.
- [ ] Gate 2 (freestanding C subset) has deterministic compile/execute examples.

---

## Legacy Sprint Snapshot (Week 9, Historical)

> **ARCHIVED**: This sprint completed November 2025. Tasks below are historical
> reference only. Current work tracked in docs/archive/INDEX.md and
> docs/general/DEFERRED_WORK.md. LISP service reached ~60% completion;
> remainder deferred per user decision (see DEFERRED_WORK.md).

**Focus**: LISP Language Service Implementation
**Duration**: November 2-8, 2025 (7 days)
**Target**: 1,800-2,200 lines, 65+ tests

### Day-by-Day Plan

#### Day 1 (Nov 2): Lexer & Parser Foundation
- [ ] Create `backend/src/compilers/lisp_lexer.py`
  - [ ] Token types: LPAREN, RPAREN, SYMBOL, NUMBER, STRING, QUOTE, etc.
  - [ ] Tokenization of S-expressions
  - [ ] String and number parsing
  - [ ] Line/column tracking
- [ ] Create `backend/src/compilers/lisp_parser.py`
  - [ ] Recursive S-expression parsing
  - [ ] Nested list handling
  - [ ] Quote/unquote special forms
  - [ ] Error recovery
- [ ] Write initial tests (10-15 tests)
  - [ ] Lexer token recognition
  - [ ] Parser S-expression construction
  - [ ] Nested list parsing

**Deliverable**: 900-1,100 lines, 10-15 tests
**Review**: Run pytest, verify all tests pass

#### Day 2 (Nov 3): AST & Semantic Analysis
- [ ] Create `backend/src/compilers/lisp_ast.py`
  - [ ] Node types: SExpr, Symbol, Literal, Lambda, etc.
  - [ ] AST visitor pattern
  - [ ] Pretty-printing for debugging
- [ ] Create `backend/src/compilers/lisp_types.py` (optional, dynamic typing)
  - [ ] Simple type inference (if needed)
  - [ ] Symbol table management
- [ ] Write tests (15-20 tests)
  - [ ] AST construction
  - [ ] Symbol resolution

**Deliverable**: 300-400 lines, 15-20 tests
**Review**: Verify AST structure matches expectations

#### Day 3 (Nov 4): Compiler to Babbage IR
- [ ] Create `backend/src/compilers/lisp_compiler.py`
  - [ ] S-expression to IR translation
  - [ ] cons/car/cdr implementation
  - [ ] lambda translation to IR functions
  - [ ] defun macro expansion and compilation
  - [ ] List operations (append, reverse, map, filter)
- [ ] Write tests (20-25 tests)
  - [ ] Simple expressions (arithmetic)
  - [ ] List operations
  - [ ] Function definitions
  - [ ] Lambda expressions

**Deliverable**: 600-700 lines, 20-25 tests
**Review**: Verify IR generation matches expected patterns

#### Day 4 (Nov 5): Service Integration
- [ ] Create `backend/src/services/languages/lisp_service.py`
  - [ ] FastAPI service wrapper
  - [ ] SBCL Docker container integration
  - [ ] Code execution sandboxing
  - [ ] Error handling and reporting
- [ ] Register in `backend/src/services/languages/__init__.py`
  - [ ] Add "lisp" to get_executor() factory
- [ ] Update `backend/src/api/code_execution.py`
  - [ ] Add "lisp" to language literal type
  - [ ] Update /languages endpoint metadata
  - [ ] Update health check (3 → 4 languages)
- [ ] Write integration tests (10-15 tests)
  - [ ] Service instantiation
  - [ ] Code execution end-to-end
  - [ ] Error handling

**Deliverable**: 250-300 lines, 10-15 tests
**Review**: Test via API endpoint, verify execution

#### Day 5 (Nov 6-7): Testing & Documentation
- [ ] Comprehensive test suite review
  - [ ] Verify all 65+ tests passing
  - [ ] Add edge case tests
  - [ ] Stress testing (large S-expressions)
- [ ] Code quality checks
  - [ ] Run pylint (no warnings)
  - [ ] Run mypy (strict mode)
  - [ ] Run black formatter
- [ ] Documentation
  - [ ] Add docstrings to all public functions
  - [ ] Create LISP_SERVICE_README.md
  - [ ] Update CLAUDE.md with LISP service status

**Deliverable**: Documentation complete, quality verified
**Review**: All tests pass, no warnings, docs complete

### Week 9 Success Criteria

- [ ] 1,800-2,200 lines of production code
- [ ] 65+ tests passing (100% pass rate)
- [ ] Zero pylint/mypy warnings
- [ ] LISP code executes correctly via API
- [ ] Documentation complete and accurate

---

## High Priority (Weeks 9-12)

### Week 10.1: IDRIS2 Language Service (Nov 9-13)

**Complexity**: HIGH (dependent type system)
**Effort**: 2,500-3,000 lines, 70+ tests, 3-4 days

#### Tasks
- [ ] Design dependent type system architecture
  - [ ] Sigma types (dependent pairs)
  - [ ] Refinement types
  - [ ] Type-level computation
  - [ ] Type checking algorithm
- [ ] Implement lexer (500-600 lines)
  - [ ] Dependent type syntax tokens
  - [ ] Type-level expression tokens
- [ ] Implement parser (800-900 lines)
  - [ ] Type annotations
  - [ ] Dependent function types
  - [ ] Pattern matching with types
- [ ] Implement type system (400-500 lines)
  - [ ] Type inference
  - [ ] Type checking
  - [ ] Type erasure before execution
- [ ] Implement compiler (800-900 lines)
  - [ ] Type erasure
  - [ ] Compile-time proof verification
  - [ ] Runtime code generation
- [ ] Create service wrapper (300-350 lines)
  - [ ] IDRIS2 Docker container
  - [ ] Execution pipeline
- [ ] Write comprehensive tests (70+ tests)
  - [ ] Type inference tests
  - [ ] Dependent type validation
  - [ ] Compile-time proof tests
- [ ] Integration and documentation

**Success Criteria**:
- [ ] Dependent types work correctly
- [ ] Type erasure preserves semantics
- [ ] 70+ tests passing
- [ ] Documentation explains type system

### Week 10.2: System F Language Service (Nov 14-16)

**Complexity**: MEDIUM-HIGH (polymorphic lambda calculus)
**Effort**: 2,000-2,500 lines, 60+ tests, 2-3 days

#### Tasks
- [ ] Design rank-2 polymorphism architecture
- [ ] Implement lexer (350-400 lines)
  - [ ] Forall quantification syntax
  - [ ] Type abstraction tokens
- [ ] Implement parser (600-700 lines)
  - [ ] Universal quantification
  - [ ] Type abstraction (Λα)
  - [ ] Type application
- [ ] Implement type system (300-400 lines)
  - [ ] Rank-2 polymorphism
  - [ ] Type unification
  - [ ] Monomorphization strategy
- [ ] Implement compiler (700-800 lines)
  - [ ] Type specialization
  - [ ] Monomorphization
  - [ ] IR generation
- [ ] Create service wrapper (250-300 lines)
  - [ ] Custom interpreter (no standard runtime)
  - [ ] Execution engine
- [ ] Write comprehensive tests (60+ tests)
- [ ] Integration and documentation

**Success Criteria**:
- [ ] Polymorphic functions work correctly
- [ ] Rank-2 types type-check properly
- [ ] 60+ tests passing
- [ ] Documentation explains System F

### Week 11: Java Language Service (Nov 17-21)

**Complexity**: MEDIUM (OOP paradigm)
**Effort**: 2,200-2,800 lines, 70+ tests, 3-4 days

#### Tasks
- [ ] Design OOP semantics translation
- [ ] Implement lexer (500-600 lines)
  - [ ] Java keyword tokenization
  - [ ] Operator precedence
- [ ] Implement parser (1000-1200 lines)
  - [ ] Class declarations
  - [ ] Method definitions
  - [ ] Field declarations
  - [ ] Inheritance syntax
- [ ] Implement type system (350-450 lines)
  - [ ] Subtyping
  - [ ] Method resolution
  - [ ] Overriding validation
- [ ] Implement compiler (800-900 lines)
  - [ ] Class table construction
  - [ ] Method dispatch (virtual vs static)
  - [ ] Constructor handling
  - [ ] Basic exception handling
- [ ] Create service wrapper (300-350 lines)
  - [ ] OpenJDK Docker container
  - [ ] Compilation + execution pipeline
- [ ] Write comprehensive tests (70+ tests)
- [ ] Integration and documentation

**Success Criteria**:
- [ ] Classes and inheritance work
- [ ] Method overriding correct
- [ ] 70+ tests passing
- [ ] Documentation explains OOP translation

### Week 12: Integration & Technical Debt (Nov 22-28)

**Complexity**: MEDIUM (testing and cleanup)
**Effort**: 2,300-3,000 lines, 60+ tests, 5-6 days

#### Day 1-3: Integration Testing
- [ ] Create integration test suite (1,500-2,000 lines)
  - [ ] Multi-language compilation tests
  - [ ] Cross-language comparison (same algorithm, different languages)
  - [ ] Resource limit enforcement
  - [ ] Security sandbox validation
  - [ ] 40+ integration test scenarios
- [ ] Create end-to-end test suite (800-1,000 lines)
  - [ ] Full learning path validation
  - [ ] User journey testing
  - [ ] Performance benchmarking
  - [ ] 20+ E2E test scenarios

#### Day 4-5: Technical Debt Resolution
- [ ] User authentication implementation (2-3 hours)
  - [ ] JWT token handling
  - [ ] User model integration
  - [ ] Uncomment code submission saving
- [ ] Database health checks (1-2 hours)
  - [ ] Test DB connectivity in /ready endpoint
  - [ ] Test Redis connectivity
  - [ ] Return 503 on failure
- [ ] Prometheus metrics (2-3 hours)
  - [ ] Install prometheus_client
  - [ ] Create metrics (Counter, Gauge, Histogram)
  - [ ] Instrument API endpoints
- [ ] Uptime tracking (30 minutes)
  - [ ] Store start time
  - [ ] Calculate uptime in /metrics
- [ ] Request counting middleware (1 hour)
  - [ ] Create FastAPI middleware
  - [ ] Thread-safe counter
- [ ] Active users query (1-2 hours)
  - [ ] Query User model for recent activity
- [ ] Module/lesson counts (30 minutes)
  - [ ] Query Module and Lesson tables

**Total Debt Resolution**: 8-12 hours

#### Day 6: Documentation Updates
- [ ] Update CLAUDE.md
  - [ ] Phase 2 completion status
  - [ ] All 7 language services documented
  - [ ] Integration testing strategy
- [ ] Update README.md
  - [ ] All 7 languages listed
  - [ ] Updated project status
  - [ ] Installation instructions verified
- [ ] Update AGENTS.md
  - [ ] Multi-agent coordination for Phase 2
  - [ ] Agent roles and responsibilities
- [ ] Create language service templates
  - [ ] Extract common patterns
  - [ ] Document 4-phase compiler pipeline
  - [ ] Service wrapper template

**Success Criteria**:
- [ ] All 7 language services complete
- [ ] 300+ total tests passing (100% pass rate)
- [ ] Integration tests validate multi-language pipeline
- [ ] All technical debt from Section 1 resolved
- [ ] Documentation complete and accurate
- [ ] Zero warnings (pylint, mypy, eslint)

---

## Medium Priority (Weeks 13-18)

### Phase 3: Emulator & Tools

#### Week 13: Babbage ISA Emulator
- [ ] RegisterFile implementation (100-150 lines)
- [ ] Memory system (150-200 lines)
- [ ] InstructionDecoder (200-300 lines)
- [ ] ExecutionEngine (300-400 lines)
- [ ] Tests (200-300 lines, 30+ tests)

#### Week 14: I/O System
- [ ] ConsoleIO implementation (300-400 lines)
- [ ] FileSystem (sandboxed) (400-500 lines)
- [ ] SyscallHandler (400-500 lines)
- [ ] Tests (200-300 lines, 25+ tests)

#### Week 15: Debugger
- [ ] BreakpointManager (200-250 lines)
- [ ] DebuggerSession (400-500 lines)
- [ ] Visualization (400-500 lines)
- [ ] Interactive REPL (300-400 lines)
- [ ] Tests (200-300 lines, 25+ tests)

#### Week 16: Performance Analyzer
- [ ] ExecutionProfiler (300-400 lines)
- [ ] BottleneckAnalyzer (300-400 lines)
- [ ] OptimizationAdvisor (200-300 lines)
- [ ] ReportGenerator (200-300 lines)
- [ ] Tests (100-200 lines, 15+ tests)

#### Week 17-18: Phase 3 Integration
- [ ] API endpoint integration
- [ ] Frontend debugger UI
- [ ] Documentation
- [ ] Performance testing

---

## Low Priority (Weeks 19+)

### Phase 4: Frontend & Visualization (Remaining)

#### Week 19: Timeline & Educational Content
- [ ] Interactive timeline component (D3.js)
- [ ] Historical module components
- [ ] Backend content delivery API
- [ ] Database models for modules/lessons

#### Week 20: Language Services Integration
- [ ] Code editor enhancements
- [ ] Execution console UI
- [ ] Language selector
- [ ] Result visualization

#### Week 21: Performance & DevOps
- [ ] Performance optimization (lazy loading, code splitting)
- [ ] Build pipeline improvements
- [ ] Deployment automation
- [ ] Monitoring and logging

#### Week 22: User Testing & Launch Prep
- [ ] User testing sessions
- [ ] Bug fixes and refinements
- [ ] Documentation finalization
- [ ] Launch preparation

### Phase 5: Educational Content Production

- [ ] 7 complete historical volumes (LaTeX)
- [ ] 3 synthesis modules
- [ ] 200+ code examples across all languages
- [ ] 100+ exercises with automated validation
- [ ] TikZ diagrams and visualizations

### Phase 6: Advanced Features

- [ ] Multi-user learning paths
- [ ] Peer code review system
- [ ] Achievements and progress tracking
- [ ] Social features (forums, code sharing)

### Phase 7: Research Extensions

- [ ] Formal verification integration (Coq, Lean)
- [ ] Quantum computing module
- [ ] Additional language services (APL, Prolog, Agda)
- [ ] Advanced type theory curriculum

---

## Backlog

### Infrastructure
- [ ] Kubernetes deployment configuration
- [ ] CI/CD pipeline improvements
- [ ] Automated dependency updates (Dependabot)
- [ ] Security scanning (OWASP, Snyk)
- [ ] Load testing and scalability validation

### Documentation
- [ ] API reference documentation (Swagger/OpenAPI)
- [ ] Developer onboarding guide
- [ ] Contribution guidelines
- [ ] Code of conduct

### Quality
- [ ] Refactor monolithic files (>1000 lines)
- [ ] Extract common utilities to shared modules
- [ ] Improve error messages (user-friendly)
- [ ] Add logging and observability

### Research
- [ ] Performance comparison across languages
- [ ] Type system expressiveness analysis
- [ ] Historical accuracy verification (primary sources)
- [ ] User learning effectiveness studies

---

## Completed

### Phase 1: Foundation ✓
- [x] FastAPI backend structure
- [x] PostgreSQL + Redis setup
- [x] C language service (7 components, 58 tests)
- [x] Python language service (7 components, 58 tests)
- [x] Haskell language service (7 components, 68 tests)
- [x] Code generation pipeline (5 components, 50+ tests)
- [x] Docker Compose orchestration
- [x] Bazel build system configuration
- [x] Frontend skeleton (SvelteKit)

### Phase 2: Partial ✓
- [x] C service complete
- [x] Python service complete
- [x] Haskell service complete

### Phase 4: Partial ✓
- [x] Frontend skeleton (Week 1)
- [x] State management + 3D visualization (Week 2)
- [x] WebSocket + quality adaptation (Week 3)

### Babbage Engineering Capstone ✓
- [x] BILL_OF_MATERIALS_COMPREHENSIVE.md (3 eras, 32.5 KB)
- [x] META_TOOLING_GUIDE.md (tooling infrastructure, 32.9 KB)
- [x] MODERN_MANUFACTURING_COMPARISON.md (2025 vs historical, 29.6 KB)
- [x] Updated Babbage README.md

### Strategic Documentation ✓
- [x] MASTER_ROADMAP.md (consolidation of 50+ phase documents)
- [x] Updated requirements.md (comprehensive specification)

---

## Tracking Metrics

### Current Sprint Progress

| Task | Status | Progress | Tests | Notes |
|------|--------|----------|-------|-------|
| LISP Lexer | 🟢 Complete | 100% | 10/10 | Functional |
| LISP Parser | 🟢 Complete | 100% | 10/10 | Functional |
| LISP AST | 🟢 Complete | 100% | 15/15 | Functional |
| LISP Compiler | 🟡 In Progress | 60% | 7/25 | Core logic (defun, let, if) works |
| LISP Service | ⚪ Not Started | 0% | 0/15 | Next Step |

**Legend**: ⚪ Not Started | 🟡 In Progress | 🟢 Complete

### Overall Project Progress (Feb 2026 Audit)

| Phase | Status | Tests | Notes |
|-------|--------|-------|-------|
| Phase 1 (Foundation) | Complete | 174 | FastAPI, DB, 3 lang services |
| Phase 2 (Languages) | 43% | 184 | C, Python, Haskell only |
| Phase 3 (Emulators) | 80% | 120+ | 13/15 emulators implemented |
| Phase 4 (Frontend) | 80% | 850+ | SvelteKit, timeline, visualizations |
| Physics Model (A-F) | Complete | 400+ | 6 modules, 50+ equations |
| Debt Resolution (0-8) | Complete | -- | 8 phases of cleanup |
| **Total** | **In Progress** | **1409 passing** | **8 skipped, 0 failures** |

**Feb 2026 Metrics**:
- 1409 unit/integration tests passing (up from ~1071 in early Feb)
- 8 tests skipped (DB fixture dependencies)
- 3 collection errors quarantined (test_tools_router, test_cross_language, test_phase4_w1_api)
- Physics: 35/35 dimensional checks, 7/7 Monte Carlo, 24/24 DE2 comparisons
- Note G deck runner: B1-B13 correct from pure card execution
- Opcode-coupled physics: SimulationBridge connects engine to physics
- API: sys.path hacks removed, globals replaced with FastAPI DI

---

## Priority Matrix

| Priority | Focus Area | Effort (weeks) | Impact | Dependencies |
|----------|-----------|----------------|--------|--------------|
| **P0** | LISP Service | 1 | HIGH | None |
| **P0** | IDRIS2 Service | 1 | HIGH | None |
| **P0** | System F Service | 0.5 | HIGH | None |
| **P0** | Java Service | 1 | HIGH | None |
| **P0** | Integration Tests | 1 | CRITICAL | All 4 services |
| **P1** | Tech Debt Resolution | 0.5 | HIGH | User model |
| **P1** | Documentation Updates | 0.5 | MEDIUM | Phase 2 complete |
| **P2** | Emulator Core | 2 | HIGH | Phase 2 complete |
| **P2** | I/O System | 1 | MEDIUM | Emulator core |
| **P2** | Debugger | 1.5 | MEDIUM | Emulator core |
| **P2** | Profiler | 1 | LOW | Emulator core |
| **P3** | Timeline UI | 1 | MEDIUM | Phase 3 complete |
| **P3** | Educational Content | 8 | HIGH | All phases |

---

## Notes and Decisions

### January 14, 2026 (Consolidation Audit)
- Comprehensive repository audit completed
- Fixed Python 3.14 escape sequence errors in haskell_lexer.py, haskell_ast.py
- Fixed import path errors in test_babbage_emulator.py, lisp_service.py
- Discovered Phase 2 was overreported (85% vs actual 43%)
- Documented Phase 3 emulator implementations (13/15 devices at Tier 1+)
- Updated MASTER_ROADMAP.md to v2.0 with accurate status
- Identified async fixture concerns and stale executor unit tests during initial audit

### February 24, 2026 (Reconciliation Refresh)
- Revalidated targeted backend unit tests for Haskell service.
- Prioritized and implemented executor unit test rewrite for current service API.
- Added CI gate tier definitions (A/B/C) to make verification cadence explicit.

### November 2, 2025
- Created comprehensive TODO tracker
- Prioritized Phase 2 completion (Weeks 9-12)
- Detailed day-by-day plan for Week 9 (LISP service)
- Established success criteria for each week

### Future Decisions Needed
- [ ] Select formal verification framework (Coq vs Lean vs Agda)
- [ ] Determine quantum computing module scope
- [ ] Decide on additional language priorities (APL, Prolog, Agda, Coq)

---

## Review Schedule

- **Daily**: Update current sprint progress
- **Weekly**: Review completed tasks, update priorities
- **Monthly**: Strategic review, roadmap adjustments
- **Quarterly**: Major milestone planning

---

**Document Revision**: 3.0
**Last Updated**: February 26, 2026
**Next Review**: After Phase 9-12 completion

**Revision History**:
- v3.0 (Feb 26, 2026): Updated after Phases 0-8 completion (1409 tests, physics model, Note G, API cleanup)
- v2.1 (Feb 24, 2026): Reconciled test status, added gate tiers, and aligned planning hygiene completion
- v2.0 (Jan 14, 2026): Comprehensive audit - corrected Phase 2/3 status, documented fixes and known issues
- v1.0 (Nov 2, 2025): Initial TODO tracker creation

**End of TODO Tracker**
