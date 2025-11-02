# Ancient Compute: Multi-Repository Project Structure

**Date**: November 2, 2025  
**Version**: 1.0  
**Status**: Migration Guide

---

## Overview

Ancient Compute is organized as a GitHub Project containing multiple specialized repositories. This document describes the project structure, repository relationships, and best practices for cross-repository development.

## GitHub Project Organization

### Project Board Structure

The Ancient Compute GitHub Project uses the following structure:

```
Ancient Compute (GitHub Project)
├── Repositories (7 specialized repos)
├── Project Board (Kanban-style tracking)
├── Milestones (Cross-repo coordination)
└── Labels (Standardized across repos)
```

### Project Views

1. **Overview Board**: High-level project status across all repos
2. **By Repository**: Issues grouped by repository
3. **By Component**: Issues grouped by architectural component
4. **Roadmap**: Timeline view of milestones
5. **Priority Matrix**: Issues by priority and effort

---

## Repository Structure

### 1. ancient-compute-core (Orchestration & Documentation)

**Purpose**: Main repository for project-wide coordination, documentation, and infrastructure

**Contents**:
- Project-wide documentation (README, ROADMAP, ARCHITECTURE)
- Multi-agent coordination system (AGENTS.md)
- Build orchestration (Bazel WORKSPACE, MODULE.bazel)
- Docker Compose orchestration
- CI/CD workflows for integration
- Issue templates and project management
- Contributing guidelines
- Security policies

**Key Files**:
```
ancient-compute-core/
├── README.md (Project overview)
├── MASTER_ROADMAP.md
├── AGENTS.md
├── PROJECT_STRUCTURE.md (this file)
├── CONTRIBUTING.md
├── docker-compose.yml (orchestrates all services)
├── MODULE.bazel (Bazel workspace)
├── .github/
│   ├── workflows/ (Integration CI)
│   ├── ISSUE_TEMPLATE/
│   ├── PULL_REQUEST_TEMPLATE/
│   └── CODEOWNERS
└── scripts/ (Cross-repo automation)
```

**Dependencies**: References all other repos as submodules or dependencies

---

### 2. ancient-compute-frontend (User Interface)

**Purpose**: SvelteKit-based interactive frontend

**Contents**:
- SvelteKit application
- Three.js 3D visualizations
- D3.js timeline components
- Monaco code editor integration
- WebSocket clients
- Frontend tests (Playwright, Vitest)

**Key Files**:
```
ancient-compute-frontend/
├── README.md
├── package.json
├── svelte.config.js
├── playwright.config.ts
├── src/
│   ├── routes/
│   ├── lib/
│   └── components/
├── tests/
└── e2e/
```

**API Dependencies**: ancient-compute-backend (REST/WebSocket)

---

### 3. ancient-compute-backend (API Service)

**Purpose**: FastAPI backend serving REST and WebSocket APIs

**Contents**:
- FastAPI application
- Database models (SQLAlchemy)
- Authentication and authorization
- Content delivery APIs
- Language service orchestration
- WebSocket handlers
- Backend tests (pytest)

**Key Files**:
```
ancient-compute-backend/
├── README.md
├── requirements.txt
├── pyproject.toml
├── alembic.ini
├── src/
│   ├── main.py
│   ├── api/
│   ├── models/
│   ├── services/
│   └── db/
├── tests/
└── alembic/ (migrations)
```

**Dependencies**: 
- ancient-compute-language-services (code execution)
- PostgreSQL database
- Redis cache

---

### 4. ancient-compute-babbage-engine (Babbage Analytical Engine)

**Purpose**: Complete Babbage Analytical Engine specification, emulator, and documentation

**Contents**:
- Babbage ISA specification
- Emulator implementation (Python)
- Assembler and code generator
- Manufacturing documentation
- Bill of materials
- Historical research and whitepapers
- Emulator tests

**Key Files**:
```
ancient-compute-babbage-engine/
├── README.md
├── BABBAGE_MASTER_REFERENCE.md
├── OPTIMAL_BABBAGE_SPECIFICATION.md
├── BILL_OF_MATERIALS_COMPREHENSIVE.md
├── src/
│   ├── emulator/
│   ├── assembler/
│   └── codegen/
├── docs/
│   ├── manufacturing/
│   ├── operations/
│   └── whitepapers/
└── tests/
```

**Used By**: ancient-compute-backend (emulator API)

---

### 5. ancient-compute-language-services (Code Execution)

**Purpose**: Dockerized language execution environments

**Contents**:
- Docker containers for 8+ languages (C, Python, Haskell, IDRIS2, LISP, Java, Assembly, System F)
- Standardized REST APIs for code execution
- Security sandboxing (gVisor, seccomp-bpf)
- Language-specific compilers/interpreters
- Service tests

**Key Files**:
```
ancient-compute-language-services/
├── README.md
├── docker-compose.yml (all services)
├── c/
│   ├── Dockerfile
│   ├── src/ (lexer, parser, compiler)
│   └── tests/
├── python/
├── haskell/
├── idris2/
├── lisp/
├── java/
├── assembly/
└── systemf/
```

**API Contract**: Standardized across all languages
- POST /execute (run code)
- GET /health (service status)
- GET /version (language version)

---

### 6. ancient-compute-curriculum (Educational Content)

**Purpose**: Historical modules, lessons, and learning materials

**Contents**:
- 7 historical modules (Prehistory → Modern)
- 3 synthesis modules (cross-era connections)
- Type theory curriculum
- Code examples across languages
- Exercises and solutions
- Historical context and primary sources

**Key Files**:
```
ancient-compute-curriculum/
├── README.md
├── modules/
│   ├── module_0_prehistory/
│   ├── module_1_ancient/
│   ├── module_2_medieval/
│   ├── module_3_symbolic/
│   ├── module_4_foundations/
│   ├── module_5_electronic/
│   ├── module_6_type_theory/
│   └── module_7_synthesis/
├── synthesis/
│   ├── synthesis_a/
│   ├── synthesis_b/
│   └── synthesis_c/
├── exercises/
└── examples/
```

**Used By**: ancient-compute-backend (content delivery)

---

### 7. ancient-compute-docs (Academic Documentation)

**Purpose**: LaTeX academic documentation and whitepapers

**Contents**:
- LaTeX documentation sources
- TikZ diagrams
- Academic whitepapers
- ArXiv submissions
- PDF build infrastructure
- Documentation tests

**Key Files**:
```
ancient-compute-docs/
├── README.md
├── Makefile.whitepaper
├── whitepapers/
│   ├── pedagogical/
│   ├── babbage/
│   └── arxiv/
├── diagrams/
│   └── tikz/
├── bibliography/
└── build/
```

**Build System**: XeLaTeX, pgfplots, TikZ

---

## Cross-Repository Coordination

### Git Submodules Strategy

The core repository references other repositories as git submodules:

```bash
# In ancient-compute-core
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-frontend.git frontend
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-backend.git backend
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-babbage-engine.git babbage
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-language-services.git services
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-curriculum.git curriculum
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-docs.git docs
```

### Dependency Management

```
ancient-compute-core (orchestration)
├── frontend (depends on backend API)
├── backend (depends on services, babbage, curriculum)
├── services (independent language containers)
├── babbage (independent emulator)
├── curriculum (content only)
└── docs (documentation only)
```

### Version Synchronization

Each repository uses semantic versioning (SemVer):
- **Major**: Breaking API changes
- **Minor**: New features, backward compatible
- **Patch**: Bug fixes

Cross-repo compatibility matrix maintained in `ancient-compute-core/COMPATIBILITY.md`

---

## GitHub Project Configuration

### Labels (Standardized Across All Repos)

**Priority**:
- `priority: critical` (P0 - blocking)
- `priority: high` (P1 - important)
- `priority: medium` (P2 - normal)
- `priority: low` (P3 - nice-to-have)

**Type**:
- `type: bug` (defects)
- `type: feature` (new functionality)
- `type: enhancement` (improvements)
- `type: documentation` (docs only)
- `type: security` (security issues)
- `type: performance` (optimization)

**Component**:
- `component: frontend`
- `component: backend`
- `component: services`
- `component: babbage`
- `component: curriculum`
- `component: docs`
- `component: infrastructure`

**Phase**:
- `phase: 1-foundation`
- `phase: 2-languages`
- `phase: 3-emulator`
- `phase: 4-frontend`

**Status** (automatic via GitHub Projects):
- `status: backlog`
- `status: todo`
- `status: in-progress`
- `status: review`
- `status: done`

### Milestones

Milestones span multiple repositories:
- **Phase 1: Foundation** (Complete)
- **Phase 2: Language Services** (85%)
- **Phase 3: Emulator & Tools** (Designed)
- **Phase 4: Frontend & Visualization** (80%)

Each milestone references issues across all relevant repos.

---

## Development Workflow

### 1. Issue Creation

Issues are created in the most relevant repository but tracked in the GitHub Project.

**Example**: Frontend bug
1. Create issue in `ancient-compute-frontend`
2. Issue automatically appears in GitHub Project
3. Add labels: `type: bug`, `component: frontend`, `priority: high`
4. Assign to milestone and project column

### 2. Cross-Repository Pull Requests

When changes span multiple repositories:

1. Create feature branch in each affected repo
2. Reference related PRs in PR description
3. Use consistent branch naming: `feature/description`, `fix/issue-123`
4. Link PRs to GitHub Project issues
5. Coordinate merge order (dependencies first)

### 3. CI/CD Pipeline

**Per-Repository CI** (`.github/workflows/ci.yml`):
- Lint
- Type check
- Unit tests
- Build

**Integration CI** (`ancient-compute-core`):
- Build all submodules
- Integration tests
- E2E tests
- Deploy to staging

### 4. Release Process

**Per-Repository Release**:
1. Update version in package.json/pyproject.toml
2. Update CHANGELOG.md
3. Tag release: `git tag v1.2.3`
4. Create GitHub release
5. Publish artifacts (npm, PyPI, Docker Hub)

**Project-Wide Release**:
1. Update all submodule references in core
2. Update COMPATIBILITY.md
3. Tag core repository
4. Create project-wide release notes

---

## Best Practices

### Communication

1. **Cross-Repo Issues**: Use issue references (`org/repo#123`)
2. **Breaking Changes**: Announce in all affected repos
3. **API Changes**: Document in OpenAPI/AsyncAPI specs
4. **Security Issues**: Private disclosure, coordinated fix

### Code Quality

1. **Linting**: Standardized configs (`.eslintrc`, `.pylintrc`)
2. **Testing**: >80% coverage per repo
3. **Documentation**: README, API docs, inline comments
4. **Code Review**: Required for all PRs

### Security

1. **Dependency Scanning**: Dependabot enabled on all repos
2. **Secret Scanning**: GitHub secret scanning enabled
3. **SAST**: CodeQL on all repos
4. **Supply Chain**: SBOM generation for releases

### Monitoring

1. **Service Health**: Prometheus metrics per service
2. **Logging**: Centralized logging (ELK stack)
3. **Tracing**: Distributed tracing (Jaeger)
4. **Alerts**: PagerDuty integration

---

## Migration Guide

### From Monorepo to Multi-Repo

**Phase 1: Preparation**
1. Identify component boundaries
2. Map dependencies between components
3. Create new repositories
4. Set up CI/CD for each repo

**Phase 2: Code Split**
1. Use `git filter-branch` or `git-filter-repo` to preserve history
2. Extract each component to its repository
3. Clean up extracted history (remove unrelated files)
4. Set up submodules in core repo

**Phase 3: Integration**
1. Update import paths
2. Fix cross-repo dependencies
3. Update CI/CD pipelines
4. Test integration builds

**Phase 4: Documentation**
1. Update all READMEs
2. Create cross-repo navigation
3. Update contribution guides
4. Archive monorepo

### Preserving Git History

```bash
# Extract frontend preserving history
git clone https://github.com/Oichkatzelesfrettschen/ancient-compute.git ancient-compute-frontend
cd ancient-compute-frontend
git filter-repo --path frontend/ --path-rename frontend/:

# Extract backend preserving history
git clone https://github.com/Oichkatzelesfrettschen/ancient-compute.git ancient-compute-backend
cd ancient-compute-backend
git filter-repo --path backend/ --path-rename backend/:
```

---

## Repository Templates

Each repository includes:

### README.md Template

```markdown
# [Repository Name]

Part of the [Ancient Compute](https://github.com/Oichkatzelesfrettschen/ancient-compute) project.

## Overview
[Description]

## Installation
[Setup instructions]

## Development
[Development workflow]

## Testing
[How to run tests]

## API Documentation
[Link to API docs]

## Contributing
See [CONTRIBUTING.md](../ancient-compute-core/CONTRIBUTING.md)

## License
MIT - See [LICENSE](../ancient-compute-core/LICENSE)
```

### CODEOWNERS

```
# Default owners for everything
* @team-leads

# Component-specific owners
/src/api/ @backend-team
/src/frontend/ @frontend-team
/services/ @devops-team
```

---

## Tools and Automation

### Recommended Tools

1. **Git Submodule Helper**: `git-submodule-helper` (manage submodules)
2. **Monorepo Migration**: `git-filter-repo` (split with history)
3. **Cross-Repo Search**: GitHub Code Search
4. **Dependency Tracking**: `renovate` or `dependabot`
5. **Release Automation**: `semantic-release`

### Automation Scripts

Located in `ancient-compute-core/scripts/`:
- `sync-submodules.sh` - Update all submodules to latest
- `check-compatibility.sh` - Verify version compatibility
- `cross-repo-search.sh` - Search across all repos
- `release.sh` - Coordinated release process

---

## FAQ

**Q: Why split into multiple repositories?**
A: Better separation of concerns, independent versioning, clearer ownership, faster CI, easier contribution.

**Q: How do I work across multiple repos?**
A: Use git submodules in the core repo, or check out repos side-by-side and use relative paths.

**Q: What happens to the original monorepo?**
A: It's archived with a README pointing to the new structure.

**Q: How are dependencies managed across repos?**
A: Each repo publishes versioned artifacts. Consuming repos depend on specific versions.

**Q: What about integration testing?**
A: Integration tests live in `ancient-compute-core` and test all components together.

---

## References

- **GitHub Projects Documentation**: https://docs.github.com/en/issues/planning-and-tracking-with-projects
- **Git Submodules Guide**: https://git-scm.com/book/en/v2/Git-Tools-Submodules
- **Monorepo to Multi-Repo**: https://github.com/newren/git-filter-repo
- **Semantic Versioning**: https://semver.org/

---

**Document Version**: 1.0  
**Last Updated**: November 2, 2025  
**Maintainer**: Ancient Compute Team
