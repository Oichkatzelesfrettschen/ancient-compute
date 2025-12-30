# CLAUDE.md - Ancient Compute Project Guidance

**Last Updated**: 2025-11-19
**Phase Status**: Refer to [docs/general/MASTER_ROADMAP.md](docs/general/MASTER_ROADMAP.md) for current status.
**Codebase Size**: Refer to [docs/general/MASTER_ROADMAP.md](docs/general/MASTER_ROADMAP.md) for current metrics.
**Test Coverage**: Refer to [docs/general/MASTER_ROADMAP.md](docs/general/MASTER_ROADMAP.md) for current metrics.

This file provides comprehensive guidance to Claude Code when working with this repository.

---

## Project Overview

Ancient Compute is a comprehensive educational platform teaching the complete 12,500-year history of computation, logic, and programming paradigms. The project traces computational thinking from prehistoric tally marks through Babylonian algorithms, Greek logic, Islamic mathematics, mechanical calculators, lambda calculus, to modern type theory and quantum computing.

**Target Audience**: Developers who know C, Python, Assembly, Java, Haskell, IDRIS2, and LISP but lack the historical context connecting these paradigms across civilizations and millennia.

**Core Mission**: Transform isolated programming knowledge into deep understanding of computation as humanity's longest continuous intellectual tradition.

**Strategic Vision**: Build the first comprehensive compiler infrastructure that proves all programming paradigms (imperative, functional, dependently-typed, meta-level) compile to a universal intermediate representation (Babbage ISA), unifying centuries of computational diversity under a single architectural vision.

## Architecture Overview

### Multi-Component System

1. **Frontend**: SvelteKit webapp with interactive timeline, code playgrounds, and visualizations
2. **Backend**: FastAPI service orchestrating language execution and content delivery
3. **Language Services**: Isolated Docker containers for C, Python, Haskell, IDRIS2, LISP, Assembly, Java, System F
4. **Documentation System**: LaTeX + pgfplots + TikZ for academic-quality curriculum materials
5. **Build System**: Bazel for hermetic, reproducible polyglot builds

### Key Technical Principles

- **Security First**: All code execution in sandboxed Docker containers with seccomp-bpf, cgroups, namespaces
- **Cross-Platform**: Native support for Debian and Windows 11 via MSYS2/MinGW-w64
- **Progressive Learning**: Content organized by era, concept difficulty, and multiple learning paths
- **Type Safety**: Leverage advanced type systems (System F, dependent types) in educational content
- **No Placeholders**: All implementations must be complete, documented, and integrated into build system

## Project Status

For the most up-to-date information on project phases, status, and metrics, please refer to the [Master Project Roadmap](docs/general/MASTER_ROADMAP.md) and the [TODO Tracker & Project Planner](docs/general/TODO_TRACKER.md).

---

## Curriculum Structure

### Seven Historical Volumes

0. **Volume 0: Prehistory of Counting** (20,000 BC - 3,000 BC)
   - Ishango bone, clay tokens, one-to-one correspondence

1. **Volume 1: Ancient Foundations** (3,000 BC - 500 AD)
   - Mesopotamia: Babylonian algorithms, base-60 arithmetic
   - Egypt: Fraction mathematics, geometry
   - Greece: Euclid, logic, geometric algorithms
   - India: Vedic mathematics, decimal system
   - China: I Ching binary, rod calculations

2. **Volume 2: Medieval Transmission** (500 - 1,500 AD)
   - Islamic Golden Age: Al-Khwarizmi, algebra
   - Scholastic logic: Symbol manipulation
   - Ramon Lull's Ars Magna: Combinatorial logic

3. **Volume 3: Early Modern Symbolic Revolution** (1,500-1,850)
   - Leibniz: Binary arithmetic, calculus
   - Pascal: Pascaline calculator
   - Boolean: Symbolic logic algebra
   - Babbage/Lovelace: Difference Engine, Ada's notes

4. **Volume 4: Foundations Crisis** (1,850-1,940)
   - Frege: Formal logic notation
   - Russell: Paradoxes and type theory
   - Godel: Incompleteness theorems
   - Church: Lambda calculus
   - Turing: Computable numbers and machines

5. **Volume 5: Electronic Age** (1,940-1,980)
   - ENIAC: Vacuum tube computing
   - Von Neumann: Stored-program architecture
   - LISP: Functional programming paradigm
   - ALGOL: Structured programming
   - Transistor and IC evolution

6. **Volume 6: Type Theory Evolution** (1,970-2,000)
   - Curry-Howard isomorphism
   - System F: Polymorphic lambda calculus
   - Hindley-Milner: Practical type inference
   - Martin-Löf: Intuitionistic type theory
   - Dependent types emergence

7. **Volume 7: Paradigm Synthesis** (1,980-2,025)
   - Multi-paradigm language design
   - Logic programming and constraints
   - Modern type systems (Haskell, Rust, Kotlin)
   - Quantum computing foundations
   - Future computational models

### Synthesis Modules

- **Module A**: From Syllogisms to Type Systems (Aristotle → Russell → Church → Haskell)
- **Module B**: From Abacus to Assembly (Physical computation through ages)
- **Module C**: Cross-Cultural Algorithmic Thinking (Global contributions)

## Strategic Planning Documents

For the most up-to-date strategic planning, project status, and task tracking, please refer to the following canonical documents:

-   **[docs/general/MASTER_ROADMAP.md](docs/general/MASTER_ROADMAP.md)** - Comprehensive synthesis of all phases, current status, and strategic vision.
-   **[docs/general/TODO_TRACKER.md](docs/general/TODO_TRACKER.md)** - Active task tracking, priorities, and detailed execution plan.
-   **[docs/requirements/OVERVIEW.md](docs/requirements/OVERVIEW.md)** - Project-wide system and infrastructure requirements overview.
-   **[docs/general/REPOSITORY_GUIDELINES.md](docs/general/REPOSITORY_GUIDELINES.md)** - General guidelines for contributing and development workflow.

This section consolidates all previous planning documents, ensuring a single source of truth for project direction.

---

## Development Commands

### Environment Setup

```bash
# Windows (PowerShell)
.\scripts\setup-windows.ps1

# Linux/Debian
./scripts/setup-debian.sh
```

### Build Commands

```bash
# Build entire project with Bazel
bazel build //...

# Build specific components
bazel build //frontend:app
bazel build //backend:api
bazel build //services/haskell:service
bazel build //docs:curriculum-pdf

# Run tests (warnings as errors)
bazel test //... --test_output=errors
```

### Development Workflow

```bash
# Start all services (Docker Compose)
docker-compose up -d

# Frontend development server
cd frontend && npm run dev

# Backend development server
cd backend && uvicorn src.main:app --reload

# Build LaTeX documentation
cd docs/whitepaper && xelatex main.tex

# Run single test
bazel test //backend/tests:test_language_service --test_output=all
```

### Language Service Testing

```bash
# Test individual language containers
docker-compose run c-service pytest
docker-compose run python-service pytest
docker-compose run haskell-service pytest

# Test backend compiler integration
cd backend && pytest tests/ --cov=src -v

# Security validation
./scripts/validate-sandbox.sh services/*/
```

---

## Language Service Implementation Status

For the most up-to-date information on the status of language service implementation, including completed services, planned services, and detailed metrics, please refer to the [Master Project Roadmap](docs/general/MASTER_ROADMAP.md).

---

## Code Quality Standards

### Warnings As Errors

**Python**:
- pylint with no warnings
- mypy with strict mode
- black formatter required

**TypeScript**:
- eslint with no warnings
- Strict null checks
- No implicit any

**Compilation**:
- `-Wall -Wextra -Werror` for C
- All warnings must be fixed or explicitly allowed

### Testing Requirements

- **Unit Tests**: >90% code coverage
- **Integration Tests**: All compiler phases validated
- **End-to-End Tests**: Code execution through service boundary
- **Sandbox Tests**: Security validations

### Type Hints and Docstrings

- **100% Type Hints**: Every Python function must have type annotations
- **Module-Level Docstrings**: Explain purpose and key classes
- **Function Docstrings**: Describe parameters, return values, exceptions

---

## Key Files by Module

**Backend**:
- `backend/src/main.py` - FastAPI app initialization.
- `backend/src/api/code_execution.py` - Code execution endpoint.
- `backend/src/services/languages/__init__.py` - Service factory.
- `backend/src/compilers/` - Language-specific compilers.
- `backend/src/codegen/` - IR → Assembly pipeline.

**Frontend**:
- `frontend/src/routes/` - SvelteKit pages and layouts.
- `frontend/src/lib/components/` - Reusable UI components.
- `frontend/src/lib/stores/` - Svelte stores for state.
- `frontend/src/lib/api/` - Backend API client.

**Services**:
- `services/docker-compose.yml` - Container orchestration.
- `services/{language}/Dockerfile` - Container definitions.
- `services/{language}/src/execute.py` - Service entrypoint.

**Documentation**:
- `docs/general/` - General project overview, guidelines, reports.
- `docs/babbage_engine/` - Babbage Analytical Engine documentation.
- `docs/requirements/` - Detailed requirements for different modules.
- `docs/specifications/` - Technical specifications.
- `docs/whitepaper/` - LaTeX documentation and related files.
- `content/modules/` - Educational modules.
- `content/synthesis/` - Cross-era synthesis content.

## Code Organization

### Directory Structure

- `backend/`: FastAPI backend application (Python)
- `frontend/`: SvelteKit frontend application (TypeScript)
- `services/`: Language execution microservices (Dockerized)
  - `c/`, `python/`, `haskell/`, `idris/`, `lisp/`, `assembly/`, `java/`, `systemf/`
- `docs/`: Comprehensive project documentation
  - `general/`: General project overview, guidelines, agent docs, reports.
  - `babbage_engine/`: Documentation specific to Babbage Analytical Engine.
  - `requirements/`: Detailed requirements for different modules.
  - `specifications/`: Technical specifications (e.g., language service architecture).
  - `minix/`: MINIX-related documentation.
  - `whitepaper/`: LaTeX documentation and related files.
  - `archive/`: Historical/superseded documents and migration reports.
- `content/`: Educational curriculum content (modules, synthesis).
- `scripts/`: Utility scripts for development, build, and deployment.
- `config/`: Configuration files for various environments.
- `tools/`: Development tools (e.g., todo_report.py).
- `shared/`: Common utilities and libraries (e.g., common data models).

### Content Management

Content files follow strict hierarchical organization:

```
content/
  modules/
    module-0-prehistory/
      lessons/
      exercises/
      code-examples/
    module-1-ancient/
      mesopotamia/
      egypt/
      greece/
      india/
      china/
  synthesis/
    syllogisms-to-types/
    abacus-to-assembly/
    cross-cultural/
```

Each lesson includes:
- Historical context and primary sources
- Conceptual bridges to modern programming
- Interactive code examples in multiple languages
- Exercises with automated validation
- References to LaTeX documentation sections

## Key Architectural Patterns

### Language Service Interface

All language services expose standardized REST/WebSocket APIs:

```
POST /execute
  - Sandboxed code execution with resource limits
  - Returns: stdout, stderr, execution time, memory usage

POST /validate
  - Static analysis and type checking
  - Returns: errors, warnings, type information

GET /capabilities
  - Service metadata and feature support
```

### Security Layers

1. **Docker Isolation**: Each language in separate container
2. **Seccomp-bpf**: Syscall filtering (no network, restricted filesystem)
3. **Cgroups v2**: CPU/memory limits per execution
4. **Linux Namespaces**: PID, mount, network isolation
5. **Read-only Filesystem**: Except tmpfs work directories

### Documentation Generation Pipeline

```
content/*.md -> Jinja2 templates -> LaTeX source -> XeLaTeX -> PDF
                                 -> TikZ diagrams
                                 -> pgfplots visualizations
```

## Type System Pedagogy

This project heavily emphasizes type theory evolution:

- **Untyped**: Assembly, early LISP examples
- **Weakly Typed**: C pointer arithmetic demonstrations
- **Strongly Typed**: Java, Python (with type hints)
- **Parametric Polymorphism**: System F, Haskell generics
- **Dependent Types**: IDRIS2 with compile-time proofs

When implementing educational examples:
- Show historical progression within single concept
- Demonstrate same algorithm across type system spectrum
- Use IDRIS2 for formal correctness proofs of ancient algorithms
- Connect type errors to historical mathematical paradoxes (Russell's paradox)

## Historical Accuracy Requirements

All historical claims must be:
- Sourced from primary documents or peer-reviewed scholarship
- Presented with appropriate epistemic humility
- Cross-referenced with multiple cultural perspectives
- Linked to modern implementations with clear conceptual bridges

Do not oversimplify or create false teleological narratives. Computation's history is not linear progress but complex cultural exchange.

## Testing Strategy

### Unit Tests
- Backend API endpoints: pytest with coverage >90%
- Frontend components: Vitest + Testing Library
- Language services: Service-specific frameworks

### Integration Tests
- End-to-end learning path validation
- Multi-language code example execution
- LaTeX document generation pipeline
- Security sandbox validation

### Educational Content Tests
- Code examples must execute successfully in target language
- Exercises must have computable validation criteria
- Historical timelines must be chronologically consistent
- Cross-references between content must resolve

## Build System Philosophy

Using Bazel for:
- **Hermetic Builds**: Reproducible across Windows and Debian
- **Polyglot Support**: Single build graph for 8+ languages
- **Incremental Compilation**: Only rebuild changed components
- **Remote Caching**: Share build artifacts across team

All builds run with warnings as errors. No compilation warnings are acceptable.

## Common Development Pitfalls

### 1. Language Service Sandboxing
**Problem**: Forgetting resource limits leads to DOS vulnerabilities
**Solution**: Always validate cgroup configs in `services/*/sandbox-config.json`

### 2. Cross-Platform Path Handling
**Problem**: Hardcoded Unix paths break on Windows
**Solution**: Use `pathlib` (Python) or Bazel's `$(location)` syntax

### 3. LaTeX Compilation Dependencies
**Problem**: Missing TikZ libraries cause silent PDF generation failures
**Solution**: Run `./scripts/validate-latex-deps.sh` before building docs

### 4. Type System Examples
**Problem**: System F and IDRIS2 examples too advanced without scaffolding
**Solution**: Follow progressive disclosure pattern: simple types -> generics -> dependent types

### 5. Historical Timeline Accuracy
**Problem**: Conflating invention dates across cultures
**Solution**: Consult `docs/timeline-sources.bib` for all date claims

## Future Extension Points

The architecture supports future additions:

- **More Languages**: Add services for APL, Prolog, Agda, Coq following template in `services/_template/`
- **Interactive Visualizations**: Three.js scenes for mechanical computers, circuit simulators
- **Collaborative Features**: Multi-user learning paths, peer code review
- **Formal Verification**: Integration with Coq/Lean for proving historical algorithms correct
- **Quantum Module**: Extension to quantum logic and quantum programming (Qiskit, Cirq)

## References for Development

- **Curriculum Design**: Refer to [docs/general/EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md](docs/general/EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md) for the comprehensive curriculum outline.
- **Technical Architecture**: See [docs/general/MASTER_ROADMAP.md](docs/general/MASTER_ROADMAP.md) for the current master roadmap and architectural overview, and [docs/specifications/LANGUAGE_SERVICE_ARCHITECTURE.md](docs/specifications/LANGUAGE_SERVICE_ARCHITECTURE.md) for detailed language service architecture.
- **Project Planning**: See [docs/general/MASTER_ROADMAP.md](docs/general/MASTER_ROADMAP.md) for the consolidated project roadmap and [docs/general/TODO_TRACKER.md](docs/general/TODO_TRACKER.md) for active task tracking.
- **Language Service Specifications**: Refer to [docs/specifications/LANGUAGE_SERVICE_ARCHITECTURE.md](docs/specifications/LANGUAGE_SERVICE_ARCHITECTURE.md) for standardized interface definitions and security requirements.
- **Repository Guidelines**: See [docs/general/REPOSITORY_GUIDELINES.md](docs/general/REPOSITORY_GUIDELINES.md) for general guidelines and development workflow.

## Development Mindset

When working on this project:

1. **Think Across Millennia**: Every feature connects ancient wisdom to modern code
2. **Respect Cultural Contributions**: Avoid Eurocentric narratives in computational history
3. **Maintain Rigor**: Historical accuracy AND technical correctness are non-negotiable
4. **Progressive Pedagogy**: Design for complete novices who will become experts
5. **Security Paranoia**: User code execution is always untrusted
6. **Cross-Platform Native**: Not "works on Windows", but "designed for Windows and Debian equally"

This is not just a programming tutorial. It's a bridge across 12,500 years of human thought, connecting prehistoric tally marks to dependent type theory, showing that every line of code participates in humanity's ancient dialogue between mind and mechanism.
