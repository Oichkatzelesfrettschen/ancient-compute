# Repository Split Guide for Ancient Compute

**Date**: November 2, 2025  
**Version**: 1.0  
**Purpose**: Step-by-step guide for splitting the monorepo into specialized repositories

---

## Overview

This guide documents how to split the Ancient Compute monorepo into multiple specialized repositories while preserving git history. Each component (Babbage Engine, Frontend, Backend, etc.) will become its own repository within the GitHub Project.

## Prerequisites

- Git 2.30+ installed
- `git-filter-repo` installed: `pip install git-filter-repo`
- GitHub CLI installed: `brew install gh` or `apt install gh`
- Admin access to the Oichkatzelesfrettschen organization
- Local clone of the ancient-compute repository

## Architecture Overview

### Before: Monorepo Structure

```
ancient-compute/
├── frontend/
├── backend/
├── services/
├── BABBAGE_ANALYTICAL_ENGINE/
├── CURRICULUM_AND_CONTENT/
├── docs/
├── scripts/
└── [other files]
```

### After: Multi-Repo Structure

```
GitHub Organization: Oichkatzelesfrettschen
├── ancient-compute (core orchestration)
├── ancient-compute-frontend
├── ancient-compute-backend
├── ancient-compute-babbage-engine
├── ancient-compute-language-services
├── ancient-compute-curriculum
└── ancient-compute-docs
```

---

## Repository Extraction Plan

### 1. Babbage Analytical Engine Repository

**New Repo**: `ancient-compute-babbage-engine`

#### Files to Extract

```
BABBAGE_ANALYTICAL_ENGINE/
BABBAGE_*.md (all Babbage-related markdown files)
- BABBAGE_MASTER_REFERENCE.md
- BABBAGE_ISA_EMULATOR_ARCHITECTURE.md
- BABBAGE_IR_SPECIFICATION.md
- BABBAGE_ASSEMBLER_SPECIFICATION.md
- BABBAGE_CODE_GENERATOR_SPECIFICATION.md
- BABBAGE_COMPLETE_WHITEPAPER.tex
```

#### Extraction Steps

```bash
# 1. Create working directory
mkdir -p /tmp/ancient-compute-split
cd /tmp/ancient-compute-split

# 2. Clone the original repo
git clone https://github.com/Oichkatzelesfrettschen/ancient-compute.git babbage-engine
cd babbage-engine

# 3. Extract Babbage files with history
git filter-repo --path BABBAGE_ANALYTICAL_ENGINE/ \
                --path-regex '^BABBAGE_.*\.md$' \
                --path BABBAGE_COMPLETE_WHITEPAPER.tex \
                --path-rename BABBAGE_ANALYTICAL_ENGINE/:

# 4. Clean up and prepare
# Create new directory structure
mkdir -p src/{emulator,assembler,codegen}
mkdir -p docs/{manufacturing,operations,whitepapers}
mkdir -p tests

# 5. Move files to appropriate locations
mv BABBAGE_README.md README.md
mv BABBAGE_*.md docs/
mv babbage_emulator.py src/emulator/
mv whitepaper/ docs/whitepapers/

# 6. Create new README
cat > README.md << 'EOF'
# Ancient Compute: Babbage Analytical Engine

Complete specification, emulator, and documentation for the Babbage Analytical Engine.

Part of the [Ancient Compute](https://github.com/Oichkatzelesfrettschen/ancient-compute) project.

## Overview

This repository contains:
- Babbage ISA specification
- Python-based emulator implementation
- Assembler and code generator
- Manufacturing documentation
- Historical research and whitepapers

## Quick Start

```bash
# Install dependencies
pip install -r requirements.txt

# Run emulator
python src/emulator/babbage_emulator.py

# Run tests
pytest tests/
```

## Documentation

See `docs/` directory for comprehensive documentation:
- [Master Reference](docs/BABBAGE_MASTER_REFERENCE.md)
- [ISA Specification](docs/BABBAGE_IR_SPECIFICATION.md)
- [Emulator Architecture](docs/BABBAGE_ISA_EMULATOR_ARCHITECTURE.md)
- [Manufacturing Guide](docs/BILL_OF_MATERIALS_COMPREHENSIVE.md)

## License

MIT - See [LICENSE](../ancient-compute/LICENSE)
EOF

# 7. Create requirements.txt
cat > requirements.txt << 'EOF'
# Babbage Analytical Engine Emulator Requirements
pytest>=7.4.0
pytest-cov>=4.1.0
black>=23.7.0
mypy>=1.5.0
ruff>=0.0.287
EOF

# 8. Create .gitignore
cat > .gitignore << 'EOF'
__pycache__/
*.py[cod]
*$py.class
.pytest_cache/
.coverage
htmlcov/
*.egg-info/
dist/
build/
.mypy_cache/
.ruff_cache/
EOF

# 9. Commit cleanup
git add .
git commit -m "chore: reorganize repository structure for standalone repo"

# 10. Create new repository on GitHub
gh repo create Oichkatzelesfrettschen/ancient-compute-babbage-engine \
    --public \
    --description "Babbage Analytical Engine emulator and documentation" \
    --source=.

# 11. Push to new repository
git remote rename origin old-origin
git remote add origin https://github.com/Oichkatzelesfrettschen/ancient-compute-babbage-engine.git
git push -u origin main
```

#### Post-Extraction

```bash
# Tag initial release
git tag v1.0.0
git push origin v1.0.0

# Create GitHub release
gh release create v1.0.0 \
    --title "Babbage Engine v1.0.0" \
    --notes "Initial standalone repository extracted from ancient-compute monorepo"
```

---

### 2. Frontend Repository

**New Repo**: `ancient-compute-frontend`

#### Files to Extract

```
frontend/
playwright.config.ts
```

#### Extraction Steps

```bash
cd /tmp/ancient-compute-split
git clone https://github.com/Oichkatzelesfrettschen/ancient-compute.git frontend
cd frontend

# Extract frontend with history
git filter-repo --path frontend/ \
                --path playwright.config.ts \
                --path-rename frontend/:

# Create README
cat > README.md << 'EOF'
# Ancient Compute: Frontend

SvelteKit-based interactive frontend for Ancient Compute.

Part of the [Ancient Compute](https://github.com/Oichkatzelesfrettschen/ancient-compute) project.

## Features

- Interactive 12,500-year timeline (D3.js)
- 3D Babbage Engine visualization (Three.js)
- Monaco code editor with multi-language support
- WebSocket real-time updates

## Development

```bash
pnpm install
pnpm dev
```

## Testing

```bash
pnpm test          # Unit tests
pnpm test:e2e      # E2E tests with Playwright
```

## License

MIT
EOF

# Create repository and push
gh repo create Oichkatzelesfrettschen/ancient-compute-frontend \
    --public \
    --description "SvelteKit frontend for Ancient Compute" \
    --source=.

git remote add origin https://github.com/Oichkatzelesfrettschen/ancient-compute-frontend.git
git push -u origin main
```

---

### 3. Backend Repository

**New Repo**: `ancient-compute-backend`

#### Files to Extract

```
backend/
```

#### Extraction Steps

```bash
cd /tmp/ancient-compute-split
git clone https://github.com/Oichkatzelesfrettschen/ancient-compute.git backend
cd backend

# Extract backend with history
git filter-repo --path backend/ --path-rename backend/:

# Create README
cat > README.md << 'EOF'
# Ancient Compute: Backend

FastAPI backend providing REST and WebSocket APIs.

Part of the [Ancient Compute](https://github.com/Oichkatzelesfrettschen/ancient-compute) project.

## Features

- REST API for content delivery
- WebSocket API for real-time code execution
- PostgreSQL database with SQLAlchemy ORM
- Redis caching
- JWT authentication

## Development

```bash
# Create virtual environment
python -m venv venv
source venv/bin/activate  # Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt

# Run migrations
alembic upgrade head

# Start server
uvicorn src.main:app --reload
```

## API Documentation

- Swagger UI: http://localhost:8000/docs
- ReDoc: http://localhost:8000/redoc

## Testing

```bash
pytest tests/
pytest --cov  # With coverage
```

## License

MIT
EOF

gh repo create Oichkatzelesfrettschen/ancient-compute-backend \
    --public \
    --description "FastAPI backend for Ancient Compute" \
    --source=.

git remote add origin https://github.com/Oichkatzelesfrettschen/ancient-compute-backend.git
git push -u origin main
```

---

### 4. Language Services Repository

**New Repo**: `ancient-compute-language-services`

#### Files to Extract

```
services/
```

#### Extraction Steps

```bash
cd /tmp/ancient-compute-split
git clone https://github.com/Oichkatzelesfrettschen/ancient-compute.git language-services
cd language-services

# Extract services with history
git filter-repo --path services/ --path-rename services/:

# Create top-level README
cat > README.md << 'EOF'
# Ancient Compute: Language Execution Services

Dockerized language execution environments for 8+ programming languages.

Part of the [Ancient Compute](https://github.com/Oichkatzelesfrettschen/ancient-compute) project.

## Supported Languages

- C (GCC/Clang)
- Python (CPython with RestrictedPython)
- Haskell (GHC)
- IDRIS2 (dependent types)
- LISP (SBCL)
- Java (OpenJDK)
- Assembly (x86-64 emulator)
- System F (lambda calculus interpreter)

## Architecture

Each language service is a Docker container with:
- Standardized REST API
- Security sandboxing (gVisor, seccomp-bpf)
- Resource limits (CPU, memory, timeout)
- Health monitoring

## Development

```bash
# Build all services
docker-compose build

# Start all services
docker-compose up

# Test specific service
cd c/
docker build -t ancient-compute-c .
docker run -p 8001:8000 ancient-compute-c
```

## API Contract

All services expose:
- `POST /execute` - Execute code
- `GET /health` - Service health
- `GET /version` - Language version

## License

MIT
EOF

gh repo create Oichkatzelesfrettschen/ancient-compute-language-services \
    --public \
    --description "Docker language execution services for Ancient Compute" \
    --source=.

git remote add origin https://github.com/Oichkatzelesfrettschen/ancient-compute-language-services.git
git push -u origin main
```

---

### 5. Curriculum Repository

**New Repo**: `ancient-compute-curriculum`

#### Files to Extract

```
CURRICULUM_AND_CONTENT/
HISTORICAL_CONTEXT/
TYPE_THEORY_CURRICULUM.md
```

#### Extraction Steps

```bash
cd /tmp/ancient-compute-split
git clone https://github.com/Oichkatzelesfrettschen/ancient-compute.git curriculum
cd curriculum

# Extract curriculum with history
git filter-repo --path CURRICULUM_AND_CONTENT/ \
                --path HISTORICAL_CONTEXT/ \
                --path TYPE_THEORY_CURRICULUM.md \
                --path-rename CURRICULUM_AND_CONTENT/:modules/

# Rename HISTORICAL_CONTEXT/ to historical/
git filter-repo --path modules/ \
                --path HISTORICAL_CONTEXT/ \
                --path TYPE_THEORY_CURRICULUM.md \
                --path-rename HISTORICAL_CONTEXT/:historical/

# Create README
cat > README.md << 'EOF'
# Ancient Compute: Curriculum

Educational content covering 12,500 years of computational history.

Part of the [Ancient Compute](https://github.com/Oichkatzelesfrettschen/ancient-compute) project.

## Modules

### Historical Modules (7)

0. **Prehistory** (20,000 BC - 3000 BC)
1. **Ancient Foundations** (3000 BC - 500 AD)
2. **Medieval Transmission** (500 - 1500 AD)
3. **Symbolic Revolution** (1500 - 1850)
4. **Foundations Crisis** (1850 - 1940)
5. **Electronic Age** (1940 - 1980)
6. **Type Theory Evolution** (1970 - 2000)
7. **Paradigm Synthesis** (1980 - 2025)

### Synthesis Modules (3)

- Module A: Cross-cultural connections
- Module B: Type theory evolution
- Module C: Modern applications

## Structure

Each module contains:
- Historical context and primary sources
- Code examples across multiple languages
- Exercises and solutions
- Cross-references to other modules

## License

MIT
EOF

gh repo create Oichkatzelesfrettschen/ancient-compute-curriculum \
    --public \
    --description "Educational curriculum for Ancient Compute" \
    --source=.

git remote add origin https://github.com/Oichkatzelesfrettschen/ancient-compute-curriculum.git
git push -u origin main
```

---

### 6. Documentation Repository

**New Repo**: `ancient-compute-docs`

#### Files to Extract

```
docs/
whitepaper/
*.tex (LaTeX files)
Makefile.whitepaper
PEDAGOGICAL_*.tex
```

#### Extraction Steps

```bash
cd /tmp/ancient-compute-split
git clone https://github.com/Oichkatzelesfrettschen/ancient-compute.git docs
cd docs

# Extract documentation with history
git filter-repo --path docs/ \
                --path whitepaper/ \
                --path-regex '.*\.tex$' \
                --path Makefile.whitepaper \
                --path-rename docs/:

# Create README
cat > README.md << 'EOF'
# Ancient Compute: Documentation

LaTeX academic documentation and whitepapers.

Part of the [Ancient Compute](https://github.com/Oichkatzelesfrettschen/ancient-compute) project.

## Contents

- Academic whitepapers
- TikZ diagrams and visualizations
- ArXiv submission materials
- Pedagogical documentation

## Building

```bash
# Install LaTeX (Ubuntu)
sudo apt install texlive-full

# Build whitepaper
make -f Makefile.whitepaper

# Build all PDFs
make all
```

## Requirements

- XeLaTeX
- pgfplots
- TikZ
- BibTeX

## License

MIT
EOF

gh repo create Oichkatzelesfrettschen/ancient-compute-docs \
    --public \
    --description "LaTeX documentation for Ancient Compute" \
    --source=.

git remote add origin https://github.com/Oichkatzelesfrettschen/ancient-compute-docs.git
git push -u origin main
```

---

### 7. Core Repository (Orchestration)

**Existing Repo**: `ancient-compute` (becomes core orchestration)

#### Files to Keep

```
README.md
MASTER_ROADMAP.md
AGENTS.md
PROJECT_STRUCTURE.md
CONTRIBUTING.md
docker-compose.yml (orchestrates all services)
MODULE.bazel
WORKSPACE.bazel
scripts/ (cross-repo automation)
.github/ (workflows, templates, project config)
```

#### Steps to Update Core Repo

```bash
cd /path/to/ancient-compute

# Add submodules for all extracted repos
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-frontend.git frontend
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-backend.git backend
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-babbage-engine.git babbage
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-language-services.git services
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-curriculum.git curriculum
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-docs.git docs

# Update .gitmodules with proper configuration
cat > .gitmodules << 'EOF'
[submodule "frontend"]
    path = frontend
    url = https://github.com/Oichkatzelesfrettschen/ancient-compute-frontend.git
    branch = main
[submodule "backend"]
    path = backend
    url = https://github.com/Oichkatzelesfrettschen/ancient-compute-backend.git
    branch = main
[submodule "babbage"]
    path = babbage
    url = https://github.com/Oichkatzelesfrettschen/ancient-compute-babbage-engine.git
    branch = main
[submodule "services"]
    path = services
    url = https://github.com/Oichkatzelesfrettschen/ancient-compute-language-services.git
    branch = main
[submodule "curriculum"]
    path = curriculum
    url = https://github.com/Oichkatzelesfrettschen/ancient-compute-curriculum.git
    branch = main
[submodule "docs"]
    path = docs
    url = https://github.com/Oichkatzelesfrettschen/ancient-compute-docs.git
    branch = main
EOF

# Update README to reflect new structure
# (See updated README template in next section)

# Commit changes
git add .
git commit -m "refactor: migrate to multi-repo structure with submodules"
git push origin main
```

---

## Updated Core README

Update the main `README.md` in the core repository:

```markdown
# Ancient Compute (Core)

**Multi-Repository Project Orchestration**

Ancient Compute is a comprehensive educational platform teaching 12,500 years of computational history, organized as a GitHub Project with multiple specialized repositories.

## Project Structure

This is the core orchestration repository. The project consists of:

1. **[ancient-compute](https://github.com/Oichkatzelesfrettschen/ancient-compute)** (this repo) - Project coordination
2. **[ancient-compute-frontend](https://github.com/Oichkatzelesfrettschen/ancient-compute-frontend)** - SvelteKit UI
3. **[ancient-compute-backend](https://github.com/Oichkatzelesfrettschen/ancient-compute-backend)** - FastAPI service
4. **[ancient-compute-babbage-engine](https://github.com/Oichkatzelesfrettschen/ancient-compute-babbage-engine)** - Babbage emulator
5. **[ancient-compute-language-services](https://github.com/Oichkatzelesfrettschen/ancient-compute-language-services)** - Language containers
6. **[ancient-compute-curriculum](https://github.com/Oichkatzelesfrettschen/ancient-compute-curriculum)** - Educational content
7. **[ancient-compute-docs](https://github.com/Oichkatzelesfrettschen/ancient-compute-docs)** - LaTeX documentation

See [PROJECT_STRUCTURE.md](./PROJECT_STRUCTURE.md) for detailed information.

## Quick Start

```bash
# Clone with all submodules
git clone --recurse-submodules https://github.com/Oichkatzelesfrettschen/ancient-compute.git
cd ancient-compute

# Or initialize submodules after cloning
git submodule update --init --recursive

# Start all services
docker-compose up
```

## Documentation

- **[PROJECT_STRUCTURE.md](./PROJECT_STRUCTURE.md)** - Multi-repo organization
- **[CONTRIBUTING.md](./CONTRIBUTING.md)** - Contribution guidelines
- **[MASTER_ROADMAP.md](./MASTER_ROADMAP.md)** - Project roadmap
- **[AGENTS.md](./AGENTS.md)** - AI agent coordination

## License

MIT - See LICENSE
```

---

## Verification Checklist

After completing the split:

### For Each New Repository

- [ ] README.md created with proper description
- [ ] LICENSE file added (or linked to core)
- [ ] .gitignore configured appropriately
- [ ] requirements.txt or package.json present
- [ ] CI/CD workflow added (.github/workflows/ci.yml)
- [ ] CODEOWNERS configured
- [ ] Repository settings configured:
  - [ ] Branch protection enabled on main
  - [ ] Require PR reviews before merging
  - [ ] Require status checks to pass
  - [ ] Require up-to-date branches
- [ ] Repository topics added for discoverability
- [ ] Initial release tagged (v1.0.0)

### For Core Repository

- [ ] All submodules added correctly
- [ ] .gitmodules configured
- [ ] README updated with links to all repos
- [ ] docker-compose.yml updated to use submodules
- [ ] GitHub Project created and linked
- [ ] All repos linked to GitHub Project
- [ ] Labels synced across all repositories
- [ ] Milestones created in each repository

### Integration Testing

- [ ] Clone core repo with submodules: `git clone --recurse-submodules`
- [ ] Verify all submodules initialized
- [ ] Run `docker-compose up` - all services start
- [ ] Run integration tests
- [ ] Verify cross-repo links in documentation

---

## Rollback Plan

If issues arise, rollback procedure:

1. **Preserve old monorepo**: Create backup tag before splitting
   ```bash
   cd ancient-compute
   git tag monorepo-backup
   git push origin monorepo-backup
   ```

2. **Rollback steps**:
   ```bash
   # Delete new repositories (if needed)
   gh repo delete Oichkatzelesfrettschen/ancient-compute-frontend --confirm
   # ... repeat for all new repos
   
   # Restore core repo to backup
   cd ancient-compute
   git reset --hard monorepo-backup
   git push --force origin main
   ```

---

## Post-Migration Tasks

### Update External References

- [ ] Update documentation links
- [ ] Update CI/CD configurations
- [ ] Update deployment scripts
- [ ] Notify contributors of new structure
- [ ] Update issue/PR references

### Archive Old Monorepo

If creating entirely new repos (not converting existing):

```bash
# Archive the original monorepo
gh repo archive Oichkatzelesfrettschen/ancient-compute-old
```

---

## Timeline

**Estimated Time**: 2-3 days

- **Day 1**: Extract and create repositories (6-8 hours)
- **Day 2**: Configure GitHub Project, links, automation (4-6 hours)
- **Day 3**: Integration testing, documentation updates (4-6 hours)

---

## References

- **Git Filter Repo**: https://github.com/newren/git-filter-repo
- **Git Submodules**: https://git-scm.com/book/en/v2/Git-Tools-Submodules
- **GitHub Projects**: https://docs.github.com/en/issues/planning-and-tracking-with-projects

---

**Document Version**: 1.0  
**Last Updated**: November 2, 2025  
**Author**: Ancient Compute Team
