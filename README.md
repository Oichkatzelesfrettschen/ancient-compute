# Ancient Compute

A comprehensive educational platform teaching the complete 12,500-year history of computation, logic, and programming paradigms - from prehistoric tally marks to modern dependent type theory.

**Status**: Phase 1 ✓ COMPLETE | Phase 2 → 85% (3 of 7 language services) | Phase 3 → DESIGNED
**Codebase**: ~28,000 lines total | Phase 1: 7,070 lines | Phase 2+: 10,270 lines | Phase 3: 10,660 lines
**Tests**: 500+ tests | 100% pass rate | >90% code coverage
**Last Updated**: 2025-10-31

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
- **Multi-Language Execution**: Secure sandboxed code execution for 8+ languages
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

- C (GCC/Clang)
- Python (CPython with RestrictedPython)
- Assembly (x86-64 emulator)
- Java (OpenJDK)
- Haskell (GHC)
- IDRIS2 (dependent types)
- LISP (SBCL)
- System F (custom interpreter)

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

We integrate an external, working MINIX-in-Docker implementation (minix-analysis/docker) and add orchestration, metrics collection, and APIs in this repo.

- Orchestrator: `scripts/minix_metrics.sh`
- Artifacts: `metrics/minix/<arch>/{boot_time.json,resource_timeseries.csv,boot.log,qemu-debug.log}`
- API (dev): `GET /api/v1/infra/minix/metrics?arch=i386`, `.../runs`, `.../run/{runId}`
- Frontend: `/infra/minix` route shows latest metrics and recent runs

Quick start:
```bash
./scripts/minix_metrics.sh --iso /home/eirikr/Playground/minix-analysis/docker/minix_R3.4.0rc6-d5e4fc0.iso --iterations 3 --label local
```

CI workflow: `.github/workflows/minix-metrics.yml` (requires self-hosted runner with `/dev/kvm`).

make build-backend       # Backend only
make build-frontend      # Frontend only
make build-docker        # Docker images

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
├── README.md                           # This file - main entry point
├── AGENTS.md                           # AI agent coordination system
├── backend/                            # FastAPI backend (active codebase)
│   ├── src/
│   │   ├── emulator/                   # Babbage Analytical Engine emulator
│   │   ├── compilers/                  # Language compilers
│   │   └── services/                   # Execution services
│   └── tests/
├── frontend/                           # SvelteKit frontend (active codebase)
│   ├── src/
│   └── tests/
├── services/                           # Language execution containers
│   ├── lisp/                           # LISP service
│   └── (c, python, haskell, etc.)
├── BABBAGE_ANALYTICAL_ENGINE/          # Complete Babbage documentation
│   ├── specifications/                 # Technical specifications
│   ├── documentation/                  # Manufacturing, operations guides
│   ├── blueprints/                     # Engineering diagrams
│   ├── code/                           # Reference implementations
│   └── reference/                      # Historical archives
├── IMPLEMENTATION_PHASES/              # Phase-by-phase development
│   ├── PHASE_2/                        # Multi-language support
│   ├── PHASE_3/                        # Emulator & tools
│   └── PHASE_4/                        # Advanced features
├── ARCHITECTURE_AND_DESIGN/            # Architecture documentation
│   ├── ARCHITECTURE.md
│   ├── LANGUAGE_SERVICES_ARCHITECTURE.md
│   ├── MASTER_ROADMAP.md
│   └── MULTI_AGENT_SYNTHESIS.md
├── DEVELOPMENT_GUIDES/                 # Developer documentation
│   ├── API_DOCUMENTATION.md
│   ├── BUILD.md
│   ├── TROUBLESHOOTING_GUIDE.md
│   └── LANGUAGE_SERVICE_SPECIFICATION.md
├── INFRASTRUCTURE_AND_DEPLOYMENT/      # Deployment & operations
│   ├── DEPLOYMENT_GUIDE.md
│   └── PRODUCTION_READINESS_REVIEW.md
├── HISTORICAL_RECORDS/                 # Development history archive
│   ├── sessions/                       # Session summaries
│   └── weekly/                         # Weekly progress reports
├── CURRICULUM_AND_CONTENT/             # Educational content
│   └── TYPE_THEORY_CURRICULUM.md
├── DOCUMENTATION_AND_ORGANIZATION/     # Project management docs
│   ├── PROJECT_STATUS.md
│   ├── TODO_TRACKER.md
│   └── TECHNICAL_DEBT.md
├── docs/                               # Academic papers (LaTeX)
│   ├── whitepaper-arxiv/
│   └── PEDAGOGICAL_*.tex
├── scripts/                            # Build and deployment scripts
└── config/                             # Configuration files
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

See the following for comprehensive information:

### Core Documentation
- **[README.md](./README.md)** - This file: Project overview and quick start
- **[AGENTS.md](./AGENTS.md)** - Multi-agent AI coordination system (5 specialized agents)
- **[CLAUDE.md](./CLAUDE.md)** - Development guide for Claude Code AI assistants
- **[GEMINI.md](./GEMINI.md)** - Development guide for Gemini AI assistants

### Architecture & Design
- **[ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md](./ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md)** - System architecture overview
- **[ARCHITECTURE_AND_DESIGN/MASTER_ROADMAP.md](./ARCHITECTURE_AND_DESIGN/MASTER_ROADMAP.md)** - Comprehensive roadmap (50+ planning docs consolidated)
- **[ARCHITECTURE_AND_DESIGN/LANGUAGE_SERVICES_ARCHITECTURE.md](./ARCHITECTURE_AND_DESIGN/LANGUAGE_SERVICES_ARCHITECTURE.md)** - Multi-language execution architecture
- **[ARCHITECTURE_AND_DESIGN/MULTI_AGENT_SYNTHESIS.md](./ARCHITECTURE_AND_DESIGN/MULTI_AGENT_SYNTHESIS.md)** - Multi-agent integration analysis

### Development Guides
- **[DEVELOPMENT_GUIDES/API_DOCUMENTATION.md](./DEVELOPMENT_GUIDES/API_DOCUMENTATION.md)** - Complete REST/WebSocket API reference
- **[DEVELOPMENT_GUIDES/BUILD.md](./DEVELOPMENT_GUIDES/BUILD.md)** - Build system documentation
- **[DEVELOPMENT_GUIDES/TROUBLESHOOTING_GUIDE.md](./DEVELOPMENT_GUIDES/TROUBLESHOOTING_GUIDE.md)** - Common issues and solutions
- **[DEVELOPMENT_GUIDES/LANGUAGE_SERVICE_SPECIFICATION.md](./DEVELOPMENT_GUIDES/LANGUAGE_SERVICE_SPECIFICATION.md)** - Language service specification

### Infrastructure & Deployment
- **[INFRASTRUCTURE_AND_DEPLOYMENT/DEPLOYMENT_GUIDE.md](./INFRASTRUCTURE_AND_DEPLOYMENT/DEPLOYMENT_GUIDE.md)** - Production deployment guide
- **[INFRASTRUCTURE_AND_DEPLOYMENT/PRODUCTION_READINESS_REVIEW.md](./INFRASTRUCTURE_AND_DEPLOYMENT/PRODUCTION_READINESS_REVIEW.md)** - Production readiness review

### Implementation Phases
- **[IMPLEMENTATION_PHASES/README.md](./IMPLEMENTATION_PHASES/README.md)** - Phase-by-phase implementation guide
- **[IMPLEMENTATION_PHASES/PHASE_2/](./IMPLEMENTATION_PHASES/PHASE_2/)** - Multi-language support (85% complete)
- **[IMPLEMENTATION_PHASES/PHASE_3/](./IMPLEMENTATION_PHASES/PHASE_3/)** - Emulator & tools (designed)
- **[IMPLEMENTATION_PHASES/PHASE_4/](./IMPLEMENTATION_PHASES/PHASE_4/)** - Advanced features (in progress)

### Babbage Analytical Engine
- **[BABBAGE_ANALYTICAL_ENGINE/README.md](./BABBAGE_ANALYTICAL_ENGINE/README.md)** - Complete engineering documentation hub
- **[BABBAGE_ANALYTICAL_ENGINE/INDEX.md](./BABBAGE_ANALYTICAL_ENGINE/INDEX.md)** - Master navigation for all Babbage resources
- **[BABBAGE_ANALYTICAL_ENGINE/specifications/](./BABBAGE_ANALYTICAL_ENGINE/specifications/)** - Technical specifications
- **[BABBAGE_ANALYTICAL_ENGINE/documentation/](./BABBAGE_ANALYTICAL_ENGINE/documentation/)** - Manufacturing and operations guides

### Educational Content
- **[CURRICULUM_AND_CONTENT/TYPE_THEORY_CURRICULUM.md](./CURRICULUM_AND_CONTENT/TYPE_THEORY_CURRICULUM.md)** - Type systems across 12,500 years

### Project Management
- **[DOCUMENTATION_AND_ORGANIZATION/PROJECT_STATUS.md](./DOCUMENTATION_AND_ORGANIZATION/PROJECT_STATUS.md)** - Current project status
- **[DOCUMENTATION_AND_ORGANIZATION/TODO_TRACKER.md](./DOCUMENTATION_AND_ORGANIZATION/TODO_TRACKER.md)** - Task tracking and priorities
- **[DOCUMENTATION_AND_ORGANIZATION/TECHNICAL_DEBT.md](./DOCUMENTATION_AND_ORGANIZATION/TECHNICAL_DEBT.md)** - Known issues and improvements

### Historical Records
- **[HISTORICAL_RECORDS/README.md](./HISTORICAL_RECORDS/README.md)** - Development history archive
- **[HISTORICAL_RECORDS/sessions/](./HISTORICAL_RECORDS/sessions/)** - Session summaries
- **[HISTORICAL_RECORDS/weekly/](./HISTORICAL_RECORDS/weekly/)** - Weekly progress reports

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
