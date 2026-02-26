# Ancient Compute

A comprehensive educational platform teaching the complete 12,500-year history of computation, logic, and programming paradigms - from prehistoric tally marks to modern dependent type theory.

**Status**: Phase 1 ✓ COMPLETE | Phase 2 → IN PROGRESS (assembly implemented; other language paths partial or stub) | Phase 3 → DESIGNED
**Codebase**: ~28,000 lines total | Phase 1: 7,070 lines | Phase 2+: 10,270 lines | Phase 3: 10,660 lines
**Tests**: 500+ tests | 100% pass rate | coverage improvement in progress (target: 90%)
**Last Updated**: 2025-11-19

## Strategic Vision

Ancient Compute proves that all programming paradigms—from imperative C to dependently-typed IDRIS2—compile to a universal intermediate representation (Babbage ISA). This unifies 50+ years of language diversity under a single architectural vision, showing that beneath surface syntactic differences, all computation follows the same logical principles.

## Overview

Ancient Compute bridges humanity's longest continuous intellectual tradition, connecting:
- Prehistoric counting (20,000 BC) → Modern dependent type theory (2025)
- Babylonian algorithms → Lambda calculus → System F polymorphism
- Greek logic → Formal verification → Dependent types
- Chinese I Ching binary → Digital circuits → Quantum computing
- Medieval scholastic logic → Type classes → Proof assistants

### Target Audience

Developers who know C, Python, Assembly, Java, Haskell, IDRIS2, and LISP but lack the historical and theoretical context connecting these paradigms across 12 millennia of human thought. Also suitable for computer science students learning programming language design.

## Features

- **Interactive Timeline**: D3.js + Three.js visualization spanning 12,500 years
- **Multi-Language Bring-up**: Capability-tracked execution API with readiness metadata
- **Type Theory Curriculum**: From simply-typed lambda calculus to dependent types
- **Cross-Cultural Perspectives**: Babylonian, Greek, Indian, Islamic, Chinese contributions
- **Formal Documentation**: LaTeX + TikZ academic-quality materials
- **Progressive Learning**: Multiple paths from novice to expert

## Technology Stack

- **Frontend**: SvelteKit, TypeScript, Monaco Editor, D3.js, Three.js
- **Backend**: FastAPI, Python, SQLite, Redis
- **Language Services**: Docker containers with gVisor isolation
- **Build System**: Bazel for hermetic polyglot builds
- **Documentation**: XeLaTeX with pgfplots and TikZ
- **Security**: 5-layer isolation (Docker, gVisor, seccomp-bpf, cgroups, read-only FS)

## Supported Languages

- Babbage Assembly (`implemented`)
- C (`partial`)
- Python (`partial`)
- Haskell (`partial`)
- LISP (`partial`)
- IDRIS2 (`stub`)
- System F (`stub`)
- Java (`stub`)

Canonical bring-up sequencing is documented in
`docs/general/HARDWARE_LANGUAGE_BRINGUP_PLAN.md` (assembly -> ABI -> freestanding C subset -> higher-level frontends).

## Quick Start

### Prerequisites

- Windows 11 with WSL2 or Debian Linux
- Docker Desktop (with gVisor runtime)
- Bazel 6.4.0+
- Python 3.11+
- Node.js 20+ with pnpm
- Git
- make (GNU Make)

### Installation

```bash
# Clone repository
git clone https://github.com/yourusername/ancient_compute.git
cd ancient_compute

# Windows setup (PowerShell)
.\scripts\setup-windows.ps1

# OR Debian setup (Bash)
./scripts/setup-debian.sh

# Initial setup (installs dependencies and pre-commit hooks)
make setup

# Start development environment with Docker
make docker-up

# Build project
make build

# Run tests
make test
```

## Development

### Quick Commands

```bash
# Start all services (Docker Compose)
make dev

# Start backend only (local development)
make dev-backend

# Start frontend only (local development)
make dev-frontend

# Run tests
make test                # All tests
make test-backend        # Backend only
make test-frontend       # Frontend only
make test-coverage       # With coverage report

# Code quality
make lint                # Run all linters
make format              # Format all code
make type-check          # Run type checkers

# Build
make build               # Build all

## Infrastructure: MINIX in QEMU (Docker)

For detailed information on MINIX integration, metrics, and usage, refer to:
- **[docs/minix/MINIX_ROADMAP.md](docs/minix/MINIX_ROADMAP.md)** - Comprehensive roadmap, usage, and references.
- `make minix-install` - Launch interactive MINIX installer (VNC).
- `make minix-metrics` - Run MINIX metrics orchestrator (requires ISO path).

```bash
# Example: Run MINIX metrics with a local ISO
make minix-metrics ISO=/home/eirikr/Playground/minix-analysis/docker/minix_R3.4.0rc6-d5e4fc0.iso --iterations 3 --label local
```

# Docker operations
make docker-up           # Start services
make docker-down         # Stop services
make docker-logs         # View logs
make docker-clean        # Clean up

# Database operations
make db-migrate          # Apply migrations
make db-rollback         # Rollback last migration
make db-reset            # Reset database

# Utilities
make clean               # Clean build artifacts
make deps                # Update dependencies
make help                # Show all commands
```

### Manual Development Setup

```bash
# Backend development
cd backend
python -m venv venv
source venv/bin/activate  # Windows: venv\Scripts\activate
pip install -r requirements.txt
uvicorn src.main:app --reload --host 0.0.0.0 --port 8000

# Frontend development
cd frontend
pnpm install
pnpm dev
```

### Access Services

- **Frontend**: http://localhost:3000
- **Backend API**: http://localhost:8000
- **API Documentation**: http://localhost:8000/docs
- **API Redoc**: http://localhost:8000/redoc
- **PostgreSQL**: localhost:5432 (user: ancient, db: ancient_compute)
- **Redis**: localhost:6379

## Project Structure

```
ancient_compute/
├── backend/           # FastAPI backend application (Python)
├── frontend/          # SvelteKit frontend application (TypeScript)
├── services/          # Language execution microservices (Dockerized)
│   ├── c/             # C language service
│   ├── python/        # Python language service
│   ├── haskell/       # Haskell language service
│   ├── idris/         # IDRIS2 language service
│   ├── lisp/          # LISP language service
│   ├── assembly/      # Babbage Assembly language service
│   ├── java/          # Java language service
│   └── systemf/       # System F language service
├── docs/              # Comprehensive project documentation
│   ├── general/       # General project overview, guidelines, agent docs, reports
│   ├── babbage_engine/# Documentation specific to Babbage Analytical Engine
│   ├── requirements/  # Detailed requirements for different modules
│   ├── specifications/# Technical specifications (e.g., language service architecture)
│   └── archive/       # Historical/superseded documents and migration reports
├── content/           # Educational curriculum content (modules, synthesis)
├── scripts/           # Utility scripts for development, build, and deployment
├── config/            # Configuration files for various environments
├── tools/             # Development tools (e.g., todo_report.py)
└── shared/            # Shared utilities and libraries (e.g., common data models)
```

## Educational Modules

### Module 0: Prehistory (20,000 BC - 3000 BC)
Ishango bone, clay tokens, one-to-one correspondence

### Module 1: Ancient Foundations (3000 BC - 500 AD)
Babylonian algorithms, Greek logic, Panini's grammar, I Ching

### Module 2: Medieval Transmission (500 - 1500 AD)
Islamic Golden Age, Al-Khwarizmi, scholastic logic

### Module 3: Symbolic Revolution (1500 - 1850)
Leibniz binary, Boolean algebra, Babbage/Lovelace

### Module 4: Foundations Crisis (1850 - 1940)
Frege, Russell, Godel, Church's lambda calculus, Turing

### Module 5: Electronic Age (1940 - 1980)
ENIAC, Von Neumann, LISP, structured programming

### Module 6: Type Theory Evolution (1970 - 2000)
System F, Hindley-Milner, dependent types

### Module 7: Paradigm Synthesis (1980 - 2025)
Multi-paradigm languages, modern type systems

## Security

All code execution occurs in multi-layer sandboxed environments:

1. Docker container isolation
2. gVisor runtime (userspace kernel)
3. Seccomp-bpf syscall filtering
4. Cgroups v2 resource limits
5. Read-only filesystem with tmpfs work directories

## Documentation

All comprehensive project documentation is located in the `docs/` directory.

### Overview
- **[README.md](./README.md)** - This file: Project overview and quick start
- **[docs/general/MASTER_ROADMAP.md](docs/general/MASTER_ROADMAP.md)** - Comprehensive synthesis of all phases, consolidates 50+ planning documents
- **[docs/general/TODO_TRACKER.md](docs/general/TODO_TRACKER.md)** - Active task tracking, priorities, execution plan
- **[docs/general/HARDWARE_LANGUAGE_BRINGUP_PLAN.md](docs/general/HARDWARE_LANGUAGE_BRINGUP_PLAN.md)** - Hardware-first language bring-up strategy and gate criteria
- **[docs/general/REPOSITORY_GUIDELINES.md](docs/general/REPOSITORY_GUIDELINES.md)** - General guidelines for contributing and development workflow.

### AI Agents
- **[docs/general/AGENTS.md](docs/general/AGENTS.md)** - Multi-agent coordination system (5 specialized AI agents)
- **[docs/general/CLAUDE.md](docs/general/CLAUDE.md)** - Development guide for Claude Code AI assistants
- **[docs/general/GEMINI.md](docs/general/GEMINI.md)** - Development guide for Gemini Code AI assistants
- **[docs/general/MULTI_AGENT_SYNTHESIS.md](docs/general/MULTI_AGENT_SYNTHESIS.md)** - Synthesis of all agent outputs, integration analysis

### Technical Specifications
- **[docs/specifications/LANGUAGE_SERVICE_ARCHITECTURE.md](docs/specifications/LANGUAGE_SERVICE_ARCHITECTURE.md)** - Polyglot-Systems-Architect: Multi-language execution architecture

### Requirements
- **[docs/requirements/OVERVIEW.md](docs/requirements/OVERVIEW.md)** - Project-wide system and infrastructure requirements overview.
- **[docs/requirements/BACKEND.md](docs/requirements/BACKEND.md)** - Backend Python, FastAPI, and compiler details.
- **[docs/requirements/FRONTEND.md](docs/requirements/FRONTEND.md)** - Frontend SvelteKit, TypeScript, and visualization details.
- **[docs/requirements/SERVICES.md](docs/requirements/SERVICES.md)** - Docker containers and language service architecture details.
- **[docs/requirements/DOCS_TOOLING.md](docs/requirements/DOCS_TOOLING.md)** - LaTeX documentation and TikZ diagram requirements.

### Operational Guides
- **[docs/general/API_DOCUMENTATION.md](docs/general/API_DOCUMENTATION.md)** - Complete REST/WebSocket API reference with examples
- **[docs/general/DEPLOYMENT_GUIDE.md](docs/general/DEPLOYMENT_GUIDE.md)** - Production deployment (Docker, Kubernetes, monitoring, security)
- **[docs/general/TROUBLESHOOTING_GUIDE.md](docs/general/TROUBLESHOOTING_GUIDE.md)** - Common issues, diagnostics, and solutions
- **[docs/general/WORKFLOW_TROUBLESHOOTING.md](docs/general/WORKFLOW_TROUBLESHOOTING.md)** - Specific workflow troubleshooting.

### Babbage Analytical Engine (Capstone)
- **[docs/babbage_engine/BABBAGE_README.md](docs/babbage_engine/BABBAGE_README.md)** - Complete engineering documentation hub for the Babbage Engine.
- **[docs/babbage_engine/BABBAGE_MASTER_REFERENCE.md](docs/babbage_engine/BABBAGE_MASTER_REFERENCE.md)** - Master reference for the Babbage Engine.
- **[docs/babbage_engine/BABBAGE_ASSEMBLER_SPECIFICATION.md](docs/babbage_engine/BABBAGE_ASSEMBLER_SPECIFICATION.md)** - Specification for the Babbage Assembler.
- **[docs/babbage_engine/BABBAGE_CODE_GENERATOR_SPECIFICATION.md](docs/babbage_engine/BABBAGE_CODE_GENERATOR_SPECIFICATION.md)** - Specification for the Babbage Code Generator.
- **[docs/babbage_engine/BABBAGE_IR_SPECIFICATION.md](docs/babbage_engine/BABBAGE_IR_SPECIFICATION.md)** - Specification for the Babbage Intermediate Representation.
- **[docs/babbage_engine/BABBAGE_CRITICAL_REVIEW.md](docs/babbage_engine/BABBAGE_CRITICAL_REVIEW.md)** - Critical review of the Babbage Engine.
- **[docs/babbage_engine/BABBAGE_ISA_EMULATOR_ARCHITECTURE.md](docs/babbage_engine/BABBAGE_ISA_EMULATOR_ARCHITECTURE.md)** - Architecture for the Babbage ISA Emulator.
- **[docs/babbage_engine/BABBAGE_PROJECT_SUMMARY.md](docs/babbage_engine/BABBAGE_PROJECT_SUMMARY.md)** - Project summary for the Babbage Engine.

## Project Progress

### Phase 1: Foundation ✓ COMPLETE

**Weeks 1-8**: All core components implemented
- [x] Git + Bazel build system (cross-platform)
- [x] Docker Compose with PostgreSQL, Redis
- [x] FastAPI backend with async/await, WebSockets
- [x] SvelteKit frontend with Monaco editor, D3.js, Three.js
- [x] C Language Service (lexer, parser, type system, compiler, 58 tests)
- [x] Python Language Service (dynamic typing, loops, lists, 58 tests)
- [x] Haskell Language Service (pattern matching, polymorphism, 68 tests)
- [x] Docker sandbox (seccomp-bpf, cgroups, read-only FS)
- [x] Database models and migrations
- [x] Pre-commit hooks, CI/CD pipeline

**Metrics**: 7,070 lines, 174 tests, 100% pass rate

### Phase 2: Languages → 85% (IN PROGRESS)

**Weeks 9-12**: Multi-paradigm language support expansion
- [x] **C Service** (Week 8.1) - Functions, operators, pointers, control flow
- [x] **Python Service** (Week 8.2) - Dynamic types, loops, list comprehensions
- [x] **Haskell Service** (Week 8.3) - Pattern matching, guards, polymorphic types
- [ ] **LISP Service** (Week 9) - S-expressions, meta-programming, homoiconicity
- [ ] **IDRIS2 Service** (Week 10.1) - Dependent types, type-level computation
- [ ] **System F Service** (Week 10.2) - Rank-2 polymorphism, type abstraction
- [ ] **Java Service** (Week 11.1) - Classes, inheritance, OOP paradigm
- [ ] **Week 12**: Integration testing, documentation, performance benchmarking

**Target**: 10,270+ lines, 300+ tests, 100% pass rate

### Phase 3: Emulator & Tools (DESIGNED)

**Weeks 13-18**: Babbage ISA emulation and development tools
- [ ] Emulator: RegisterFile, Memory, InstructionDecoder (2,000-2,500 lines)
- [ ] I/O System: ConsoleIO, FileSystem, Syscalls (1,500-2,000 lines)
- [ ] Debugger: Breakpoints, stepping, REPL interface (1,500-2,000 lines)
- [ ] Performance Analyzer: Profiling, bottleneck detection (1,000-1,500 lines)

**Target**: 6,000-8,000 lines, 120+ tests, 100% pass rate

**Total Project**: 28,000+ lines, 500+ tests across all phases

## Contributing

This is an educational project demonstrating the complete history of computation. Contributions welcome for:

- Historical accuracy improvements
- Additional programming language support
- Educational content enhancements
- Performance optimizations
- Security hardening

### Development Workflow

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/your-feature`
3. Install pre-commit hooks: `make install-hooks`
4. Make your changes and ensure tests pass: `make test`
5. Ensure code quality: `make lint && make format`
6. Commit with descriptive messages
7. Push and create a pull request

## License

MIT License - See LICENSE file

## Acknowledgments

This project synthesizes 12,500 years of human computational thought, honoring contributions from:

- Paleolithic mathematicians
- Babylonian astronomers
- Greek logicians
- Indian grammarians
- Islamic scholars
- Medieval scholastics
- Enlightenment philosophers
- Modern computer scientists

Every line of code participates in humanity's ancient dialogue between mind and mechanism.

## Contact

For questions or feedback, please open an issue on GitHub.

---

ANSI/ASCII only - No Unicode (per project requirements)
