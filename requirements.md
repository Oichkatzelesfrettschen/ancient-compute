# Ancient Compute - Project Requirements

**Document Version**: 1.0
**Last Updated**: 2025-10-31
**Status**: Project Architecture Phase (70% → 85%)

---

## Overview

Ancient Compute is a comprehensive educational platform teaching the 12,500-year history of computation through interactive code examples, curriculum materials, and multi-language execution services. This document defines all system, language, and infrastructure requirements across all project components.

**Project Scope**:
- **Frontend**: SvelteKit webapp with D3.js visualizations and Monaco code editor
- **Backend**: FastAPI service orchestration with async/await patterns
- **Language Services**: Sandboxed Docker containers for C, Python, Haskell, IDRIS2, LISP, System F, Java, Assembly
- **Documentation**: LaTeX curriculum with TikZ diagrams and pgfplots visualizations
- **Build System**: Bazel for hermetic, reproducible builds

---

## System Requirements

### Minimum Hardware

- **CPU**: Multi-core processor (4+ cores recommended)
- **RAM**: 16 GB minimum (32 GB recommended for parallel compilation)
- **Storage**: 50 GB free (faster with NVMe SSD)
- **Network**: 100 Mbps for Docker image pulls

### Operating Systems

**Primary (Tested)**:
- Debian 12 (bookworm) - x86_64
- CachyOS (Arch-based) - x86_64

**Secondary (Supported)**:
- Windows 11 with WSL2 (Ubuntu 22.04 guest)
- macOS 12+ (Intel/Apple Silicon via Rosetta2)

**Not Supported**:
- 32-bit systems
- ARM32 (Raspberry Pi 3/4) - requires separate cross-compilation setup
- End-of-life OS versions

### Network Requirements

- **Outbound**: HTTP/HTTPS for package downloads (pip, npm, Docker Hub)
- **Inbound**: Localhost ports 3000 (frontend), 8000 (backend), 5432 (PostgreSQL), 6379 (Redis)
- **Docker Registry**: Access to docker.io for base images

---

## Language Toolchain Requirements

All language-specific requirements are isolated in Docker containers. The host system needs only:

### Required (Core)

- **Python**: 3.11+ (for backend and code analysis)
  - Packages: FastAPI, SQLAlchemy, pytest (see backend/requirements.txt)
  - Virtual environment: `python -m venv venv`

- **Node.js**: 18.0+ (for frontend)
  - Package manager: pnpm 8.0+ (required, not npm)
  - Packages: SvelteKit, Vite, TypeScript (see frontend/package.json)

- **Docker**: 24.0+ (for language services)
  - Docker Compose: 2.20+
  - Storage: 30+ GB for base images (C, Python, Haskell, Java, LLVM)
  - Runtime: Linux (native) or WSL2 (Windows)

### Optional (Development)

- **Bazel**: 6.0+ (for hermetic builds)
  - Replaces Make, CMake, Maven for polyglot coordination
  - Install: See backend/BUILD docs

- **LaTeX**: XeLaTeX with TikZ packages (for documentation)
  - Required packages: pgfplots, tikz, fontspec, hyperref
  - Test: `xelatex --version` && `kpsewhich tikz.sty`

- **Git**: 2.40+ (for version control)
  - Hooks: pre-commit framework for CI/CD gates

---

## Backend Requirements

### Location
`backend/` directory with subdirectories:
- `src/` - Application source code
- `tests/` - pytest test suite
- `alembic/` - Database migrations
- `venv/` - Python virtual environment (generated)

### Python Dependencies

**Web Framework** (3 packages, 1.2 MB):
- fastapi==0.104.1 - REST/WebSocket API framework
- uvicorn[standard]==0.24.0 - ASGI web server
- python-multipart==0.0.6 - Form data parsing

**Database** (3 packages, 8.5 MB):
- sqlalchemy==2.0.23 - ORM and schema definition
- alembic==1.12.1 - Database migration tool
- psycopg2-binary==2.9.9 - PostgreSQL driver

**Caching** (2 packages, 2.1 MB):
- redis==5.0.1 - Redis client
- hiredis==2.2.3 - C parser for faster Redis protocol

**Authentication** (2 packages, 1.8 MB):
- python-jose[cryptography]==3.3.0 - JWT token handling
- passlib[bcrypt]==1.7.4 - Password hashing

**Configuration & Validation** (3 packages, 3.2 MB):
- pydantic==2.5.0 - Data validation with type hints
- pydantic-settings==2.1.0 - Environment variable management
- email-validator==2.1.0 - Email format validation
- python-dotenv==1.0.0 - .env file loading

**HTTP Clients** (2 packages, 1.5 MB):
- httpx==0.25.1 - Async HTTP client
- aiohttp==3.9.0 - Async HTTP requests

**Testing** (4 packages, 2.8 MB):
- pytest==7.4.3 - Test framework
- pytest-asyncio==0.21.1 - Async test support
- pytest-cov==4.1.0 - Coverage reporting
- pytest-mock==3.12.0 - Mocking framework

**Code Quality** (4 packages, 45 MB):
- black==23.11.0 - Code formatter
- pylint==3.0.2 - Linter
- mypy==1.7.1 - Type checker
- ruff==0.1.6 - Fast Python linter

**Development** (2 packages, 15 MB):
- watchfiles==0.21.0 - File watching for auto-reload
- ipython==8.17.2 - Enhanced Python REPL

**Language Services** (3 packages, 8.5 MB):
- docker==7.0.0 - Docker API client
- RestrictedPython==6.2 - Sandboxed code execution
- prometheus-client==0.19.0 - Metrics collection

**Total**: 51 packages, ~110 MB installed size

### Python Version

- **Minimum**: 3.11.0
- **Recommended**: 3.12.x
- **Maximum**: Latest (3.13+) - may need dependency updates

### Database

- **PostgreSQL**: 15.0+ (primary)
  - Connection: postgresql://user:pass@localhost:5432/ancient_compute
  - Encoding: UTF-8
  - Initialization: Automatic via alembic migrations

- **Redis**: 7.0+ (caching, sessions)
  - Connection: redis://localhost:6379/0
  - Memory: 512 MB minimum
  - Persistence: Optional (snapshot mode acceptable for dev)

### Setup Instructions

```bash
# 1. Create virtual environment
python3.11 -m venv venv
source venv/bin/activate  # Linux/macOS
# or: venv\Scripts\activate  (Windows)

# 2. Install dependencies
pip install -r requirements.txt

# 3. Configure environment
cp .env.example .env
# Edit .env with local database/redis URLs

# 4. Initialize database
alembic upgrade head

# 5. Run backend
uvicorn src.main:app --reload
```

**Result**: FastAPI running on http://localhost:8000

---

## Frontend Requirements

### Location
`frontend/` directory with subdirectories:
- `src/` - SvelteKit application code
- `tests/` - Vitest test suite
- `node_modules/` - npm packages (generated)

### Node.js Dependencies

**Core Framework** (6 packages, 25 MB):
- @sveltejs/kit@1.27.6 - Meta-framework for Svelte
- @sveltejs/adapter-node@1.3.1 - Node.js deployment adapter
- svelte@4.2.7 - UI framework
- vite@4.5.0 - Build tool and dev server
- typescript@5.3.2 - Type safety for JavaScript
- tslib@2.6.2 - TypeScript helper library

**Visualization & UI** (3 packages, 45 MB):
- d3@7.8.5 - Data visualization (timeline, graphs)
- three@0.158.0 - 3D graphics (historical visualizations)
- monaco-editor@0.44.0 - Code editor with syntax highlighting

**Development Tools** (10 packages, 120 MB):
- @typescript-eslint/eslint-plugin@6.12.0 - TypeScript linting
- @typescript-eslint/parser@6.12.0 - TypeScript parser for ESLint
- eslint@8.54.0 - Code quality tool
- eslint-config-prettier@9.0.0 - Prettier integration
- eslint-plugin-svelte@2.35.1 - Svelte-specific linting
- prettier@3.1.0 - Code formatter
- prettier-plugin-svelte@3.1.2 - Svelte formatting
- svelte-check@3.6.2 - Svelte type checking
- vitest@0.34.6 - Unit test framework
- @sveltejs/vite-plugin-svelte@2.5.3 - Svelte/Vite integration

**Type Definitions** (2 packages, 8 MB):
- @types/d3@7.4.3 - D3.js TypeScript definitions
- @types/three@0.158.3 - Three.js TypeScript definitions

**Total**: 21 packages, ~200 MB installed size

### Node.js Version

- **Minimum**: 18.0.0
- **Recommended**: 20.x LTS
- **Package Manager**: pnpm 8.0+ (required, not npm/yarn)

### Setup Instructions

```bash
# 1. Install pnpm (if not present)
npm install -g pnpm

# 2. Install dependencies
pnpm install

# 3. Development server
pnpm run dev              # http://localhost:5173

# 4. Build for production
pnpm run build
pnpm run preview

# 5. Type checking
pnpm run check

# 6. Testing
pnpm run test
```

**Result**: SvelteKit dev server with hot reload

---

## Docker Service Requirements

### Base Images

Each language service runs in an isolated Docker container with specific base images:

| Service | Base Image | Size | Compiler | Runtime |
|---------|-----------|------|----------|---------|
| C | gcc:13-bullseye | 1.4 GB | GCC 13 | glibc 2.31 |
| Python | python:3.12-slim | 450 MB | N/A | CPython 3.12 |
| Haskell | haskell:9.6 | 3.2 GB | GHC 9.6 | RTS |
| IDRIS2 | idris/idris2:latest | 2.1 GB | IDRIS2 | Scheme (CHEZ) |
| LISP | debian:bookworm-slim | 90 MB | SBCL 2.3 | SBCL runtime |
| System F | haskell:9.6 | 3.2 GB | GHC | RTS (for lambda calc) |
| Java | eclipse-temurin:21-jdk | 800 MB | javac 21 | JVM 21 |
| Assembly | debian:bookworm-slim | 90 MB | nasm/gas | None (direct execution) |

**Total Image Size**: ~12 GB (compressed: ~3.5 GB)

### Docker Compose

**File**: `docker-compose.yml`

**Services Defined**:
- PostgreSQL (persistence)
- Redis (caching)
- FastAPI backend
- 8 language containers (on-demand)

**Network**: Internal bridge network, no external exposure by default

**Storage Volumes**:
- `/var/lib/postgresql/data` - Database persistence
- `/tmp/code-execution/` - Temporary code files

### Security Sandbox

Each service execution runs with:
- **seccomp-bpf**: Syscall filtering (no network, restricted I/O)
- **cgroups v2**: Resource limits
  - CPU: 1000m (1 core equivalent)
  - Memory: 512 MB
  - Storage: 100 MB temp filesystem
- **Linux namespaces**: PID, mount, network isolation
- **Read-only rootfs**: Except `/tmp/work` (tmpfs)

**Resource Limits** (enforced at container start):
```yaml
cpus: 1.0              # 1 CPU core
memory: 512m           # 512 MB RAM
memswap_limit: 512m    # No swap
ulimits:
  nofile: 1024         # Max open files
  nproc: 256           # Max processes
```

---

## Build System Requirements

### Bazel

**Purpose**: Hermetic, reproducible builds across all components

**Installation**:
```bash
# Linux/macOS
brew install bazelisk

# Windows
choco install bazelisk
# OR: Download from github.com/bazelbuild/bazelisk
```

**Key Bazel Rules**:
- `py_binary`, `py_test` - Python targets
- `nodejs_binary`, `nodejs_test` - Node.js targets
- `cc_binary`, `cc_library`, `cc_test` - C/C++ targets
- `docker_build` - Docker image building

**Build Graph**:
```
ancient-compute/
  frontend/BUILD          → SvelteKit bundle
  backend/BUILD           → FastAPI executable
  services/c/BUILD        → C language service container
  services/python/BUILD   → Python language service container
  docs/BUILD              → LaTeX/PDF curriculum
```

**Common Commands**:
```bash
bazel build //...              # Build all targets
bazel test //...               # Run all tests (warnings=errors)
bazel build //frontend:app     # Build frontend only
bazel test //backend/tests:... # Run backend tests
```

---

## Documentation Requirements

### LaTeX Tools

**Required**:
- XeLaTeX 3.14+ - Main compiler (Unicode support)
- TikZ 3.1.4+ - Diagram drawing
- pgfplots 1.18+ - Function plotting
- hyperref - Cross-references and PDF features

**Installation**:

**Linux (Debian)**:
```bash
apt-get install texlive-full
# Or selective: texlive-latex-base texlive-latex-extra texlive-fonts-recommended
```

**macOS**:
```bash
brew install mactex
```

**Windows**:
```
Download: https://miktex.org/download
Install: MiKTeX Console → Install missing packages automatically
```

**Verification**:
```bash
xelatex --version
kpsewhich tikz.sty
kpsewhich pgfplots.sty
```

### Documentation Structure

**Location**: `docs/` directory

**Files**:
- `main.tex` - Master document (imports all volumes)
- `volumes/` - 7 historical volumes
  - `volume-0-prehistory/`
  - `volume-1-ancient/`
  - ... (through volume-6-paradigm-synthesis)
- `diagrams/` - TikZ illustrations
- `exercises/` - Code examples and solutions

**Build**:
```bash
cd docs
xelatex -interaction=nonstopmode main.tex  # Run 2-3 times for references
pdfinfo main.pdf                           # Verify output
```

**Output**: `main.pdf` (3,000-5,000 pages of curriculum)

---

## Configuration Requirements

### Environment Variables

**Backend** (`.env` file):
```bash
# FastAPI
APP_NAME=AncientCompute
ENVIRONMENT=development
HOST=127.0.0.1
PORT=8000
SECRET_KEY=your-secret-key-here (generate: openssl rand -hex 32)

# Database
DATABASE_URL=postgresql://postgres:password@localhost:5432/ancient_compute
DB_POOL_SIZE=20
DB_MAX_OVERFLOW=10

# Redis
REDIS_URL=redis://localhost:6379/0
REDIS_MAX_CONNECTIONS=10

# Language Services
LANGUAGE_SERVICE_TIMEOUT=30
MAX_EXECUTION_TIME=10
MAX_MEMORY_MB=512

# Logging
LOG_LEVEL=INFO
```

**Frontend** (`.env` file):
```bash
VITE_API_URL=http://localhost:8000
VITE_WS_URL=ws://localhost:8000
VITE_APP_NAME=AncientCompute
```

### Database Migrations

**Location**: `backend/alembic/versions/`

**Workflow**:
```bash
# Create new migration
alembic revision --autogenerate -m "Add lessons table"

# Review generated migration
cat backend/alembic/versions/xxxx_add_lessons_table.py

# Apply migration
alembic upgrade head

# Rollback if needed
alembic downgrade -1
```

---

## Testing Requirements

### Unit Tests

**Backend** (pytest):
- Location: `backend/tests/`
- Command: `pytest backend/tests/ --cov=backend/src -v`
- Coverage target: > 90%
- Format: One test file per module

**Frontend** (Vitest):
- Location: `frontend/tests/`
- Command: `pnpm run test`
- Coverage target: > 85% for components

### Integration Tests

**Language Services**:
- Validate each service compiles/runs code correctly
- Test resource limits (CPU, memory, time)
- Verify sandbox security

**API Endpoints**:
- Test code execution flow
- Validate error handling
- Check WebSocket connections

### End-to-End Tests

**Full Learning Path**:
- User completes lesson
- Submits code exercise
- Service executes code
- Results displayed in frontend

---

## Quality Gates

### Pre-Commit Hooks

**Enabled**:
- Black formatter (Python)
- Prettier formatter (TypeScript)
- pylint/mypy (Python type checking)
- ESLint (TypeScript linting)
- shellcheck (Bash scripts)
- namcap (Arch packaging validation)

**Invocation**:
```bash
git commit  # Hooks run automatically, blocks if failures
```

### Build Warnings

**Policy**: Warnings are errors

- C: `-Wall -Wextra -Werror`
- Python: No pylint warnings
- TypeScript: No ESLint warnings
- LaTeX: Clean compilation (no undefined references)

---

## Development Workflow

### Local Setup (Quick Start)

```bash
# 1. Clone and enter repo
git clone <repo-url>
cd ancient_compute

# 2. Initialize services
./scripts/setup-debian.sh        # Linux/WSL2
# or: .\scripts\setup-windows.ps1  (Windows)

# 3. Start all services
docker-compose up -d

# 4. Install backend dependencies
cd backend && python -m venv venv
source venv/bin/activate
pip install -r requirements.txt

# 5. Install frontend dependencies
cd ../frontend && pnpm install

# 6. Run development servers
# Terminal 1: Backend
cd backend && uvicorn src.main:app --reload

# Terminal 2: Frontend
cd frontend && pnpm run dev

# 7. Access application
# Frontend: http://localhost:5173
# Backend API: http://localhost:8000
# API Docs: http://localhost:8000/docs
```

**Time**: ~15 minutes (excluding Docker image pulls)

### Continuous Integration

**Triggers**: Each git push

**Stages**:
1. Lint (pylint, eslint, black --check)
2. Type checking (mypy, TypeScript)
3. Unit tests (pytest, vitest)
4. Integration tests (docker-compose + API tests)
5. Build documentation (LaTeX)
6. Security scan (OWASP, dependency audit)

**Configuration**: `.github/workflows/ci.yml`

---

## Implementation Status

### Phase 1: Foundation ✓ Complete

- [x] Backend API structure
- [x] Database schema
- [x] Frontend scaffold
- [x] Docker configuration
- [x] C language service
- [x] Python language service
- [x] Haskell language service

### Phase 2: Languages (→ 85%)

- [x] C service (complete)
- [x] Python service (complete)
- [x] Haskell service (complete)
- [ ] IDRIS2 service (planned)
- [ ] LISP service (planned)
- [ ] System F service (planned)
- [ ] Java service (planned)
- [ ] Assembly service (planned)

### Phase 3: Emulator & Tools (planned)

- [ ] Babbage ISA emulator
- [ ] I/O system (console, file)
- [ ] Debugger with REPL
- [ ] Performance profiler

---

## Dependencies Graph

```
ancient-compute (root)
├── backend/
│   ├── Python 3.11+ (51 packages)
│   ├── PostgreSQL 15+
│   ├── Redis 7+
│   └── Docker 24+
├── frontend/
│   ├── Node.js 18+ (pnpm 8+)
│   └── 21 npm packages
├── services/ (Docker containers)
│   ├── C (gcc base)
│   ├── Python (python:3.12)
│   ├── Haskell (haskell:9.6)
│   ├── IDRIS2 (idris/idris2)
│   ├── LISP (debian + SBCL)
│   ├── System F (haskell:9.6)
│   ├── Java (eclipse-temurin:21)
│   └── Assembly (debian + nasm/gas)
├── docs/
│   ├── XeLaTeX 3.14+
│   ├── TikZ 3.1+
│   └── pgfplots 1.18+
└── build/
    └── Bazel 6.0+ (optional)
```

---

## Maintenance Schedule

### Weekly
- Dependency updates check (pip, npm)
- Database backup verification
- Test suite execution (full)

### Monthly
- Security vulnerability scan
- Performance baseline comparison
- Documentation review and updates

### Quarterly
- Major version updates (Python, Node.js, base images)
- Architecture review
- Curriculum content refresh

---

## Support and References

- **Backend API**: http://localhost:8000/docs (Swagger UI)
- **Database**: PostgreSQL documentation
- **Frontend Framework**: https://kit.svelte.dev
- **Language Specifications**:
  - C: C99 standard (ISO/IEC 9899:1999)
  - Haskell: Haskell 2010 Report
  - IDRIS2: https://idris2.readthedocs.io
  - LISP: Scheme R7RS specification

---

**End of Requirements Document**
