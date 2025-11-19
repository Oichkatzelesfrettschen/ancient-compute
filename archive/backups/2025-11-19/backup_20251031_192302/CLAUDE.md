# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Ancient Compute is a comprehensive educational platform teaching the complete 12,500-year history of computation, logic, and programming paradigms. The project traces computational thinking from prehistoric tally marks through Babylonian algorithms, Greek logic, Islamic mathematics, mechanical calculators, lambda calculus, to modern type theory and quantum computing.

**Target Audience**: Developers who know C, Python, Assembly, Java, Haskell, IDRIS2, and LISP but lack the historical context connecting these paradigms across civilizations and millennia.

**Core Mission**: Transform isolated programming knowledge into deep understanding of computation as humanity's longest continuous intellectual tradition.

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

## Curriculum Structure

### Seven Historical Modules

0. **Prehistory of Counting** (20,000 BC - 3000 BC): Ishango bone, clay tokens, one-to-one correspondence
1. **Ancient Foundations** (3000 BC - 500 AD): Babylonian algorithms, Greek logic, Panini's grammar, I Ching binary
2. **Medieval Transmission** (500 - 1500 AD): Islamic Golden Age, Al-Khwarizmi, scholastic logic, Lull's Ars Magna
3. **Early Modern Symbolic Revolution** (1500-1850): Leibniz binary, Boole's algebra, Babbage/Lovelace
4. **Foundations Crisis** (1850-1940): Frege, Russell, Godel, Church's lambda calculus, Turing machines
5. **Electronic Age** (1940-1980): ENIAC, Von Neumann architecture, LISP, ALGOL, structured programming
6. **Type Theory Evolution** (1970-2000): System F, Hindley-Milner, Martin-Lof, dependent types
7. **Paradigm Synthesis** (1980-2025): Multi-paradigm languages, logic programming, modern type systems

### Synthesis Modules

- **A**: From Syllogisms to Type Systems (Aristotle -> Russell -> Church -> Haskell)
- **B**: From Abacus to Assembly (Physical computation through ages)
- **C**: Cross-Cultural Algorithmic Thinking (Global contributions)

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
docker-compose run haskell-service pytest
docker-compose run idris-service pytest
docker-compose run lisp-service pytest

# Security validation
./scripts/validate-sandbox.sh services/*/
```

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
