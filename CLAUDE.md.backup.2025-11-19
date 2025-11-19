# CLAUDE.md - Ancient Compute Project Guidance

**Last Updated**: 2025-10-31
**Phase Status**: Phase 1 ✓ Complete | Phase 2 → 85% (Languages) | Phase 3 → Designed (Emulator)
**Codebase Size**: ~28,000 lines (7,070 lines Phase 1; 10,270 lines Phase 2+; 10,660 lines Phase 3)
**Test Coverage**: 500+ tests, 100% pass rate, >90% code coverage target

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

## Project Phases and Status

### Phase 1: Foundation ✓ COMPLETE (100%)

**Duration**: Week 1-8 (8 weeks)
**Status**: All components implemented and tested
**Deliverables**:
- Backend API (FastAPI) with database models and service orchestration
- Frontend scaffold (SvelteKit) with basic routing
- C language service (lexer, parser, type system, compiler, tests)
- Python language service (lexer, parser, type system, compiler, tests)
- Haskell language service (lexer, parser, type system, compiler, tests)
- Docker infrastructure (compose file, sandbox configuration)
- Build system (Bazel configuration)

**Codebase**: ~7,070 lines
**Tests**: 174 tests (58 per language service + backend/frontend)
**Pass Rate**: 100%

### Phase 2: Languages → 85% (CURRENT PHASE)

**Duration**: Week 9-12 (4 weeks)
**Current Progress**: 70% → targeting 85% (3 of 4 language services complete)
**Completed**:
- Week 8.1: C Language Service ✓
- Week 8.2: Python Language Service ✓
- Week 8.3: Haskell Language Service ✓

**Planned** (Weeks 9-11):
- Week 9.1: LISP Language Service (meta-programming paradigm)
  - Effort: 1,800-2,200 lines of code
  - Tests: 65+ test cases
  - Unique: S-expressions, symbol manipulation, homoiconicity

- Week 10.1: IDRIS2 Language Service (dependent types)
  - Effort: 2,500-3,000 lines of code
  - Tests: 70+ test cases
  - Unique: Type-level computation, compile-time proofs

- Week 10.2: System F Language Service (polymorphic lambda calculus)
  - Effort: 2,000-2,500 lines of code
  - Tests: 60+ test cases
  - Unique: Rank-2 polymorphism, type application

- Week 11.1: Java Language Service (OOP paradigm)
  - Effort: 2,200-2,800 lines of code
  - Tests: 70+ test cases
  - Unique: Class hierarchy, method resolution, exceptions

**Week 12**: Integration Testing
- Cross-language compilation tests
- End-to-end learning path validation
- Performance benchmarking
- Documentation updates

**Target**: 10,270+ lines, 300+ tests, 100% pass rate

### Phase 3: Emulator & Tools (Designed)

**Duration**: Weeks 13-18 (6 weeks)
**Status**: Architecture designed, ready for implementation
**Components**:
- Babbage ISA Emulator (2,000-2,500 lines)
  - RegisterFile, Memory, InstructionDecoder, ExecutionState
  - 25+ instruction execution, cycle counting
  - 30+ tests

- I/O System (1,500-2,000 lines)
  - ConsoleIO, FileSystem, SyscallHandler
  - 20+ syscalls, sandboxed file access
  - 25+ tests

- Debugger (1,500-2,000 lines)
  - Breakpoints, stepping, state inspection
  - Interactive REPL interface
  - 25+ tests

- Performance Analyzer (1,000-1,500 lines)
  - ExecutionProfiler, BottleneckAnalyzer
  - Instruction/address/cycle analysis
  - 15+ tests

**Target**: 6,000-8,000 lines, 120+ tests, 100% pass rate

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

**Created during Phase 2 Planning (Week 8)**:

1. **TECHNICAL_DEBT.md** (464 lines)
   - Complete inventory of 15+ identified issues and TODO items
   - Priority matrix with effort vs. impact analysis
   - Critical blockers (user auth, DB checks, metrics)
   - Quality improvements (type hints, docstrings, testing)
   - Configuration and documentation gaps

2. **OPTION_B_IMPLEMENTATION_ROADMAP.md** (1,149 lines)
   - Detailed 6-7 week plan for Phase 2 completion (70% → 85%)
   - Complete specifications for 4 new language services
   - Line-of-code estimates and testing requirements
   - Dependency mapping and integration points
   - Week-by-week breakdown with delivery milestones

3. **OPTION_C_PHASE_3_VISION.md** (1,323 lines)
   - Strategic vision for Babbage ISA emulator system
   - Detailed component specifications (Emulator, I/O, Debugger, Profiler)
   - API design and integration architecture
   - Testing strategy and implementation timeline
   - Performance targets and quality metrics

4. **PROJECT_STATUS.md** (574 lines)
   - Comprehensive project overview and strategic direction
   - Phase-by-phase completion status
   - Repository analysis findings
   - Architecture validation results
   - Document dependencies and references

5. **requirements.md files** (12,200 lines across 5 files)
   - Project-wide requirements (requirements.md)
   - Backend module requirements (backend/requirements.md)
   - Frontend module requirements (frontend/requirements.md)
   - Services and Docker requirements (services/requirements.md)
   - Documentation build requirements (docs/requirements.md)

**Key Planning Insights**:
- Identified 7 critical TODOs in main.py (user auth, metrics, health checks)
- Service factory pattern only registers 3 of 8 languages (gap identified)
- Code execution API supports 8 languages but only 3 implemented
- Requirements documentation is complete for Phase 1 and planned features
- All 4 language services follow identical 4-phase compiler pipeline

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
cd backend && uvicorn main:app --reload

# Build LaTeX documentation
cd docs && xelatex main.tex

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

### Completed ✓

**C Language Service** (Phase 1)
- Lines: 350 (lexer) + 500 (parser) + 180 (type system) + 550 (compiler) = 1,580 total
- Tests: 58 tests, 100% pass rate
- Features: Functions, operators, pointers, arrays, control flow
- Compilation time: ~60ms per function

**Python Language Service** (Phase 1)
- Lines: 400 (lexer) + 550 (parser) + 200 (type system) + 500 (compiler) = 1,650 total
- Tests: 58 tests, 100% pass rate
- Features: Functions, loops with break/continue, lists, exception handling (basic)
- Dynamic type inference implemented

**Haskell Language Service** (Phase 1)
- Lines: 400 (lexer) + 550 (parser) + 250 (type system) + 600 (compiler) = 1,800 total
- Tests: 68 tests, 100% pass rate
- Features: Pattern matching, lambdas, let/in, case expressions, guards
- Polymorphic type system with unification algorithm

### Planned (Phase 2)

**LISP Language Service** (Week 9)
- Effort: 1,800-2,200 lines total
- Tests: 65+ tests
- Features: S-expressions, symbol manipulation, built-in functions
- Meta-programming with homoiconicity

**IDRIS2 Language Service** (Week 10.1)
- Effort: 2,500-3,000 lines total
- Tests: 70+ tests
- Features: Dependent types, type-level computation, proof checking
- Type erasure before execution

**System F Language Service** (Week 10.2)
- Effort: 2,000-2,500 lines total
- Tests: 60+ tests
- Features: Rank-2 polymorphism, type abstraction, lambda-forall
- Monomorphization strategy

**Java Language Service** (Week 11.1)
- Effort: 2,200-2,800 lines total
- Tests: 70+ tests
- Features: Classes, methods, inheritance, exception handling
- Object-oriented paradigm support

### Compiler Pipeline (Universal)

Every language service implements the same 4-phase pipeline:

1. **Lexing** (tokenization with line/column tracking)
   - Language-specific keywords and operators
   - String/number/comment handling
   - Indentation or bracket-based syntax

2. **Parsing** (AST construction with operator precedence)
   - Recursive descent parser
   - Error recovery and reporting
   - Location information preservation

3. **Semantic Analysis** (symbol table and type checking)
   - First-pass declaration collection
   - Type environment construction
   - Scope management with parent pointers

4. **IR Generation** (Babbage ISA target)
   - Direct AST-to-IR translation
   - Control flow graph construction
   - Function and global variable handling

All services output identical Babbage IR representation, enabling cross-language compilation validation.

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
- `backend/src/main.py` - FastAPI app initialization (7 TODOs to resolve)
- `backend/src/api/code_execution.py` - Code execution endpoint
- `backend/src/services/languages/__init__.py` - Service factory (needs LISP/IDRIS/SysF/Java)
- `backend/src/compilers/` - Language-specific compilers
- `backend/src/codegen/` - IR → Assembly pipeline

**Frontend**:
- `frontend/src/routes/` - SvelteKit pages and layouts
- `frontend/src/lib/components/` - Reusable UI components
- `frontend/src/lib/stores/` - Svelte stores for state
- `frontend/src/lib/api/` - Backend API client

**Services**:
- `services/docker-compose.yml` - Container orchestration
- `services/{language}/Dockerfile` - Container definitions
- `services/{language}/src/execute.py` - Service entrypoint

**Documentation**:
- `docs/main.tex` - LaTeX master document
- `docs/volumes/` - 7 historical volumes
- `docs/diagrams/` - TikZ illustrations
- `docs/exercises/` - Programming exercises

## Code Organization

### Directory Structure

- `frontend/`: SvelteKit webapp (TypeScript, Monaco Editor, D3.js, Three.js)
- `backend/`: FastAPI orchestration layer (Python, async, WebSockets)
- `services/`: Language-specific execution services (Docker containers)
  - `c/`, `python/`, `haskell/`, `idris/`, `lisp/`, `assembly/`, `java/`, `systemf/`
- `docs/`: LaTeX curriculum materials (XeLaTeX, TikZ, pgfplots)
  - `volumes/`: Seven-volume historical series
  - `exercises/`: Hands-on coding challenges
  - `diagrams/`: TikZ visualizations of computational evolution
- `content/`: Educational content organized by module and era
- `shared/`: Common utilities and type definitions
- `scripts/`: Build and deployment automation

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

- **Curriculum Design**: See comprehensive curriculum outline from logic-computation-historian agent
- **Technical Architecture**: See detailed system design from polyglot-systems-architect agent
- **Implementation Roadmap**: 52-week phased rollout plan with milestones
- **Language Service Spec**: Standardized interface definitions and security requirements

## Development Mindset

When working on this project:

1. **Think Across Millennia**: Every feature connects ancient wisdom to modern code
2. **Respect Cultural Contributions**: Avoid Eurocentric narratives in computational history
3. **Maintain Rigor**: Historical accuracy AND technical correctness are non-negotiable
4. **Progressive Pedagogy**: Design for complete novices who will become experts
5. **Security Paranoia**: User code execution is always untrusted
6. **Cross-Platform Native**: Not "works on Windows", but "designed for Windows and Debian equally"

This is not just a programming tutorial. It's a bridge across 12,500 years of human thought, connecting prehistoric tally marks to dependent type theory, showing that every line of code participates in humanity's ancient dialogue between mind and mechanism.
