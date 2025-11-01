# Multi-Agent Coordination Strategy for Ancient Compute

**Document Version**: 1.0
**Last Updated**: 2025-10-31
**Status**: Phase 1 Complete, Phase 2 In Progress

This document defines how multiple AI agents coordinate work on Ancient Compute, a 28,000-line polyglot project with 8 language services, frontend, backend, and documentation subsystems.

---

## Executive Summary

Ancient Compute requires coordinated multi-agent work because:
1. **Scale**: 28,000+ lines across 5+ technology stacks (Python, TypeScript, Haskell, C, LaTeX)
2. **Complexity**: 8 language services with identical 4-phase compiler pipelines
3. **Integration**: Frontend ↔ Backend ↔ Docker services all must work together
4. **Specialization**: Some tasks require domain experts (compiler design, type systems, DevOps)

**Coordination Model**: Sequential phases with parallel work within phases, managed through:
- Explicit task decomposition (TodoWrite)
- Specialized agent routing (via Task tool subagent_type)
- Clear dependency documentation
- Staged integration points

---

## Project Structure & Navigation

- **`backend/`** contains the FastAPI app in `src/` (config, database, API routers, services) plus Alembic migrations and pytest suite. New domains should mirror the existing `src/<domain>/` pattern and document cross-service changes in `CLAUDE.md` (AI assistant guide) and `backend/requirements.md`.

- **`frontend/`** hosts the SvelteKit client; compose UI under `src/lib/components`, routes in `src/routes`, and keep Vitest specs in `frontend/tests/`. Shared state belongs in `src/lib/stores`. See `frontend/requirements.md` for detailed frontend architecture.

- **`services/`** provides per-language sandboxes (C, Python, Haskell, IDRIS2, LISP, System F, Java, Assembly) with Docker isolation. Each follows the same 4-phase compiler pipeline. See `services/requirements.md` for container architecture and resource limits.

- **`docs/`** contains XeLaTeX curriculum (7 volumes, 50+ exercises, 300+ TikZ diagrams). See `docs/requirements.md` for build requirements and structure.

- **Strategic documents**:
  - `TECHNICAL_DEBT.md` - Known issues and quality improvements (15+ items prioritized)
  - `OPTION_B_IMPLEMENTATION_ROADMAP.md` - Phase 2 detailed plan (4 language services, 6-7 weeks)
  - `OPTION_C_PHASE_3_VISION.md` - Phase 3 emulator and tools design
  - `PROJECT_STATUS.md` - Comprehensive metrics and completion tracking
  - `requirements.md` - Project-wide and per-module requirements documentation

## Build, Test & Development Commands
```bash
make setup        # Install backend (pip) + frontend (pnpm) deps and pre-commit
make dev          # Compose stack: FastAPI :8000, SvelteKit :3000, Postgres, Redis
make dev-backend  # Uvicorn live-reload from backend/src/main.py
make dev-frontend # Vite dev server with HMR
make test         # Runs backend pytest then frontend vitest
make lint         # Backend pylint + mypy, frontend eslint
make build        # Bazel backend targets + Svelte production build
make docker-up    # Detached containers for local integration testing
```
Lean on `bazel build //backend:all` and `bazel test //...` for targeted polyglot builds, and prefer Make targets so pre-commit stays consistent.

## Coding Style & Naming Conventions
- Python adheres to Black (100 cols) and strict mypy; keep modules `snake_case`, classes `PascalCase`, functions `snake_case`, and run `make format` (Black + isort) before committing.
- TypeScript/Svelte follows Prettier (tabs, single quotes, 100 cols). Components use `PascalCase.svelte`, stores/utilities `camelCase.ts`, and API clients sit under `frontend/src/lib/api/`.
- Bazel, Make, and script targets stay lowercase with hyphen separators (`build-backend`, `test-frontend`).

## Testing Guidelines
- Backend unit tests live in `backend/tests/` (mirroring package layout); add integration suites under `backend/tests/integration` as they come online per `ARCHITECTURE_AND_DESIGN/PROJECT_STRUCTURE.md`. Use `make test-backend` and `make test-coverage` before PR handoff.
- Frontend Vitest specs belong in `frontend/tests/` or alongside components when snapshots help readability. Use `pnpm test --runInBand` for flaky debugging.
- Curriculum data fixtures ride with the relevant module folders; note any generated data sources in PR descriptions so content reviewers can validate historical timelines.

## Commit & Pull Request Guidelines
- Follow the narrative convention from `git log` (`Week N Day M: <scope>`). Summaries should state the sprint context first, then the functional change.
- Branch naming: `feature/<slug>`, `fix/<slug>`, `docs/<slug>`, `chore/<slug>`; avoid direct pushes to `main`.
- Every PR should provide: change overview, verification commands (`make test`, `make lint`, etc.), relevant doc links (roadmap, architecture), and UI screenshots/GIFs when visual layers change. Request reviewers from both backend and frontend when touching shared contracts or API DTOs.

## Roadmap Awareness for Agents
- `ARCHITECTURE_AND_DESIGN/IMPLEMENTATION_ROADMAP.md` and `DEVELOPMENT_GUIDES/WEEK_2_IMPLEMENTATION_PLAN.md` outline upcoming milestones: completing all eight language executors, layering gVisor/seccomp security, expanding content schemas, and launching the interactive timeline. Reference these when prioritizing tasks or proposing scope changes.
- Escalate deviations that impact the planned sequence (Language Services -> Content CMS -> Interactive Components) so project leads can adjust timelines or staffing.

## Specialized Agent Types and Roles

### 1. Compiler/Language Service Specialist (`chickenos-*` agents)

**Specialization**: Deep expertise in language implementation, compiler design, type systems

**Best For**:
- Implementing new language services (LISP, IDRIS2, System F, Java)
- Debugging compiler pipelines (lexing, parsing, semantic analysis, IR generation)
- Type system design and validation
- Performance optimization of language runtimes

**When to Use**:
- Any language service implementation task
- Cross-language compatibility testing
- Compiler phase optimization

**Example Workflow**:
```
User: "Implement LISP Language Service"
→ Route to: chickenos-compiler-specialist
→ Agent uses standard 4-phase pipeline
→ Outputs: lexer, parser, type system, compiler (1,800-2,200 lines)
→ Deliverable: Service with 65+ tests, 100% pass rate
```

### 2. System Architecture Specialist (`polyglot-systems-architect`)

**Specialization**: Cross-language integration, system design, polyglot orchestration

**Best For**:
- Integrating multiple language services together
- Backend-service communication patterns
- Cross-component dependency resolution
- Performance profiling and optimization

**When to Use**:
- Phase transitions (end of language implementation → integration testing)
- Multi-component issues spanning 2+ subsystems
- Architecture improvements or refactoring

**Example Workflow**:
```
User: "Execute Week 12 integration testing"
→ Route to: polyglot-systems-architect
→ Agent coordinates all 4 language services
→ Validates cross-language compilation
→ Outputs: Integration test suite + performance metrics
```

### 3. Type Theory Specialist (`category-theory-expert`)

**Specialization**: Advanced type theory, formal verification, dependent types

**Best For**:
- IDRIS2 dependent type system design
- System F rank-2 polymorphism implementation
- Type checking algorithm validation
- Proof of correctness for type operations

**When to Use**:
- IDRIS2 or System F language service work
- Type system edge cases or conflicts
- Formal specification of type semantics

### 4. Frontend/UI Specialist

**Specialization**: SvelteKit, TypeScript, visualization libraries, interactive components

**Best For**:
- Frontend component implementation
- D3.js timeline visualization
- Three.js 3D scene development
- API client integration

**When to Use**:
- Frontend feature development
- Interactive visualization work
- UI/UX integration challenges

### 5. DevOps/Infrastructure Specialist (`chickenos-driver-specialist` or general)

**Specialization**: Docker, Kubernetes, build systems, CI/CD, sandbox security

**Best For**:
- Docker container management
- Sandbox security validation
- Build system configuration (Bazel)
- Performance monitoring and profiling

**When to Use**:
- Container or deployment issues
- Security sandbox validation
- Build system debugging
- Performance baseline measurements

---

## Multi-Agent Workflow Patterns

### Pattern 1: Sequential Phase Completion

**Scenario**: Complete Week 9 (LISP Language Service)

1. **Planning Phase** (Human + General Agent)
   - Review `OPTION_B_IMPLEMENTATION_ROADMAP.md` section on LISP
   - Create TodoWrite with 7 subtasks (lexer, parser, type system, compiler, tests, service, integration)
   - Estimated effort: 20-25 hours

2. **Implementation Phase** (Compiler Specialist)
   - Assign subagent: `chickenos-bare-metal-runtime` (LISP compilation)
   - Focus: Implement all 4 phases following proven C/Python/Haskell patterns
   - Checkpoint: Each phase must have 100% test pass rate before proceeding

3. **Integration Phase** (Systems Architect)
   - Integrate LISP service into backend service factory
   - Validate with cross-language test suite
   - Measure compilation times, memory usage

4. **Documentation Phase** (General Agent)
   - Update CLAUDE.md with LISP service status
   - Update requirements.md with LISP-specific requirements
   - Create performance baseline metrics

### Pattern 2: Parallel Work Within Phase

**Scenario**: Week 12 Integration Testing (all 4 language services)

All work can happen in parallel with staged integration:

1. **Language Service Specialists** (4 concurrent agents)
   - Each validates their service individually
   - Creates service-specific test suite
   - No inter-service dependencies yet

2. **Systems Architect** (consolidation)
   - Takes all 4 service test suites
   - Creates cross-language test suite
   - Validates all 4 services work together
   - Measures overall system performance

3. **DevOps Specialist** (final validation)
   - Runs full Docker stack with all 4 services
   - Measures resource limits, sandbox isolation
   - Validates CI/CD pipeline passes

### Pattern 3: Specialized Domain Work

**Scenario**: Type Theory Milestone (IDRIS2 + System F)

1. **Type Theory Specialist**
   - Define IDRIS2 dependent type semantics
   - Specify System F polymorphism constraints
   - Create test matrices for type checking

2. **Compiler Specialists** (2 parallel agents)
   - IDRIS2: Implement dependent type system
   - System F: Implement rank-2 polymorphism
   - Validate against type theory specs

3. **Systems Architect**
   - Integrate both services
   - Validate type system interactions
   - Test cross-language type compatibility

---

## Dependency Management and Critical Paths

### Phase 2 Dependencies (Weeks 9-12)

```
Week 9: LISP Service
  ├─ Blocker: None (independent implementation)
  └─ Gate: 65+ tests, 100% pass rate

Week 10.1: IDRIS2 Service
  ├─ Blocker: None (independent implementation)
  └─ Gate: 70+ tests, type checking validation

Week 10.2: System F Service
  ├─ Blocker: None (independent implementation)
  └─ Gate: 60+ tests, polymorphism validation

Week 11.1: Java Service
  ├─ Blocker: None (independent implementation)
  └─ Gate: 70+ tests, OOP feature coverage

Week 12: Integration Testing
  ├─ Blocker: All 4 services complete (previous weeks)
  ├─ Gate: 100+ integration tests, cross-language validation
  └─ Outputs: Performance metrics, documentation updates
```

### Critical Decision Points

1. **Week 9 Completion**: Can proceed to Week 10 in parallel?
   - YES - LISP, IDRIS2, System F are independent
   - NO - If dependencies discovered → escalate to Systems Architect

2. **Week 12 Go/No-Go**: Can ship Phase 2?
   - YES - All 4 services at 85% completion + integration tests pass
   - NO - Fix blockers, delay ship

---

## Communication and Handoff Protocols

### Information Exchange Between Agents

**When Agent A hands off to Agent B**:

1. **Summary**: 1-2 sentences of what was completed
2. **Metrics**: Lines of code, test count, pass rate
3. **Blockers**: Any unresolved issues or questions
4. **Files Modified**: List of changed files
5. **Next Steps**: What Agent B should focus on

**Example Handoff**:
```
Agent: chickenos-compiler-specialist (LISP implementation)
Completed: LISP lexer, parser, type system, compiler (1,900 lines)
Tests: 55/65 passing (85%), 2 failing on nested list handling
Blockers: Symbol evaluation corner cases need type theory guidance
Files: backend/src/compilers/lisp_*.py, backend/tests/test_lisp.py
Next: category-theory-expert to review symbol binding semantics
```

### TodoWrite as Coordination Tool

**Update TodoWrite when**:
- Agent completes a task (mark as `completed`)
- Agent encounters a blocker (note in description, keep as `in_progress`)
- New task discovered during implementation (add to list)
- Task complexity changes (update estimate in description)

**Keep TodoWrite in sync with reality** - it's the single source of truth for project progress

---

## Agent Workflow Tips

- **Leverage Proven Patterns**: When implementing a new language, copy the proven 4-phase pipeline from C, Python, or Haskell. Don't reinvent from scratch.

- **Test-Driven Development**: Each phase should have tests BEFORE implementation. Use pytest for all language services.

- **Use Make Targets**: `make test`, `make lint`, `make format` keep everything consistent.

- **Check CLAUDE.md First**: Multi-agent coordination norms are documented there. Consult before modifying architecture.

- **Performance Baselines**: When completing a language service, measure:
  - Compilation time per function
  - Memory usage per execution
  - IR size before/after optimization

- **Document as You Go**: Update requirements.md files immediately after implementing a subsystem. Don't batch documentation at the end.

- **Escalate Early**: If a task requires expertise outside your specialization, propose handing off to the appropriate specialist agent.

---

## Example: Coordinating Week 9 LISP Implementation

**Monday 8:00 AM**: User requests "Implement LISP Language Service (Week 9)"

```
1. Create TodoWrite with 7 subtasks (2 min)
   - Implement lisp_lexer.py
   - Implement lisp_parser.py
   - Implement lisp_types.py
   - Implement lisp_compiler.py
   - Implement test_lisp_compiler.py
   - Integrate with service factory
   - Measure performance baselines

2. Route to specialized agent (1 min)
   Task: chickenos-compiler-specialist
   Subagent: chickenos-bare-metal-runtime
   Input: OPTION_B_IMPLEMENTATION_ROADMAP.md (Week 9.1)

3. Agent executes (5-6 hours)
   - Implements lexer with 40+ token types
   - Implements parser for S-expressions
   - Implements type system for dynamic dispatch
   - Implements 4-phase compiler
   - Writes 65+ tests
   - Reports back when complete

4. Handoff to systems architect (30 min)
   - Integrates service factory entry
   - Validates with existing services
   - Creates cross-language test
   - Measures performance vs. baseline

5. Handoff to documentation (15 min)
   - Update CLAUDE.md with LISP service metrics
   - Update requirements.md with LISP specifics
   - Create commit message with detailed summary
```

**Elapsed Time**: 6.5 hours for complete, tested, documented LISP Language Service

---

## References and Cross-Links

- **CLAUDE.md**: Development guide for AI assistants (team coordination norms)
- **README.md**: Project overview and Phase status
- **OPTION_B_IMPLEMENTATION_ROADMAP.md**: Detailed specifications for Phase 2 work
- **OPTION_C_PHASE_3_VISION.md**: Phase 3 emulator and tools architecture
- **requirements.md** files: Module-specific requirements and architecture
- **TECHNICAL_DEBT.md**: Known issues to address during implementation

---

**End of Multi-Agent Coordination Strategy**
