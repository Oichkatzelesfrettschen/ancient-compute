# Ancient Compute: Multi-Repo Quick Reference

**Quick access guide for the multi-repository project structure**

---

## 📚 Documentation Index

### Getting Started
- **[README.md](./README.md)** - Project overview
- **[CONTRIBUTING.md](./CONTRIBUTING.md)** - How to contribute
- **[PROJECT_STRUCTURE.md](./PROJECT_STRUCTURE.md)** - Architecture and organization

### Migration & Planning
- **[MIGRATION_PLAN.md](./MIGRATION_PLAN.md)** - 3-day migration timeline
- **[REPOSITORY_SPLIT_GUIDE.md](./REPOSITORY_SPLIT_GUIDE.md)** - Step-by-step extraction
- **[COMPATIBILITY_MATRIX.md](./COMPATIBILITY_MATRIX.md)** - Version compatibility

### Project Management
- **[MASTER_ROADMAP.md](./MASTER_ROADMAP.md)** - Overall project roadmap
- **[AGENTS.md](./AGENTS.md)** - AI agent coordination
- **[.github/PROJECT_TEMPLATES/](./github/PROJECT_TEMPLATES/)** - GitHub Project setup

---

## 🏗️ Repository Structure

```
Ancient Compute GitHub Project
│
├─ ancient-compute (core)
│  ├─ Documentation and orchestration
│  └─ Submodules to all other repos
│
├─ ancient-compute-frontend
│  └─ SvelteKit + TypeScript + Three.js
│
├─ ancient-compute-backend
│  └─ FastAPI + Python + PostgreSQL
│
├─ ancient-compute-babbage-engine
│  └─ Babbage ISA emulator + docs
│
├─ ancient-compute-language-services
│  └─ Docker containers (C, Python, Haskell, etc.)
│
├─ ancient-compute-curriculum
│  └─ Educational modules (7 historical + 3 synthesis)
│
└─ ancient-compute-docs
   └─ LaTeX documentation + whitepapers
```

---

## 🚀 Quick Commands

### Clone All Repositories
```bash
git clone --recurse-submodules https://github.com/Oichkatzelesfrettschen/ancient-compute.git
cd ancient-compute
```

### Update All Submodules
```bash
./scripts/multi-repo/sync-submodules.sh
```

### Check Compatibility
```bash
./scripts/multi-repo/check-compatibility.sh
```

### Search Across All Repos
```bash
./scripts/multi-repo/cross-repo-search.sh "pattern"
```

### Start All Services
```bash
docker-compose up
```

---

## 📋 Common Tasks

### Working on Frontend
```bash
cd frontend
pnpm install
pnpm dev
```

### Working on Backend
```bash
cd backend
python -m venv venv
source venv/bin/activate
pip install -r requirements.txt
uvicorn src.main:app --reload
```

### Working on a Language Service
```bash
cd services/c
docker build -t ancient-compute-c .
docker run -p 8001:8000 ancient-compute-c
```

### Working on Babbage Engine
```bash
cd babbage
pip install -r requirements.txt
python src/emulator/babbage_emulator.py
```

### Running Tests
```bash
# All tests
make test

# Specific component
cd frontend && pnpm test
cd backend && pytest
```

---

## 🔗 Important Links

### Repositories
- [Core (Orchestration)](https://github.com/Oichkatzelesfrettschen/ancient-compute)
- [Frontend](https://github.com/Oichkatzelesfrettschen/ancient-compute-frontend) (TBD)
- [Backend](https://github.com/Oichkatzelesfrettschen/ancient-compute-backend) (TBD)
- [Babbage Engine](https://github.com/Oichkatzelesfrettschen/ancient-compute-babbage-engine) (TBD)
- [Language Services](https://github.com/Oichkatzelesfrettschen/ancient-compute-language-services) (TBD)
- [Curriculum](https://github.com/Oichkatzelesfrettschen/ancient-compute-curriculum) (TBD)
- [Documentation](https://github.com/Oichkatzelesfrettschen/ancient-compute-docs) (TBD)

### Project Management
- [GitHub Project Board](https://github.com/orgs/Oichkatzelesfrettschen/projects) (TBD)
- [Issues (All Repos)](https://github.com/Oichkatzelesfrettschen/ancient-compute/issues)
- [Pull Requests (All Repos)](https://github.com/Oichkatzelesfrettschen/ancient-compute/pulls)

---

## 🏷️ Labels

### Priority
- `priority: critical` - P0 blocking issues
- `priority: high` - P1 important
- `priority: medium` - P2 normal
- `priority: low` - P3 nice-to-have

### Type
- `type: bug` - Something broken
- `type: feature` - New functionality
- `type: enhancement` - Improvement
- `type: documentation` - Docs only
- `type: security` - Security related

### Component
- `component: frontend`
- `component: backend`
- `component: services`
- `component: babbage`
- `component: curriculum`
- `component: docs`
- `component: infrastructure`

---

## 📝 Issue Templates

When creating issues, use the appropriate template:
- **Bug Report** - For defects and errors
- **Feature Request** - For new features
- **Documentation** - For documentation issues

---

## 🔄 Workflow

### Creating a Feature

1. **Create Issue** in relevant repository
2. **Create Branch**: `feature/description`
3. **Make Changes** following coding standards
4. **Write Tests** (>80% coverage)
5. **Update Docs** if needed
6. **Create PR** using template
7. **Code Review** required
8. **Merge** after approval

### Cross-Repo Changes

1. **Plan Dependencies** - Identify which repos need changes
2. **Create Issues** in each relevant repo
3. **Link Issues** in descriptions
4. **Coordinate PRs** - Merge in dependency order
5. **Update Core** - Update submodule references
6. **Integration Test** - Verify everything works together

---

## 🛠️ Automation Scripts

Located in `scripts/multi-repo/`:

- `sync-submodules.sh` - Update all submodules to latest
- `check-compatibility.sh` - Verify version compatibility
- `cross-repo-search.sh` - Search pattern across repos

See [scripts/multi-repo/README.md](./scripts/multi-repo/README.md) for details.

---

## 📊 Current Status

| Repository | Status | Version | Tests |
|------------|--------|---------|-------|
| Core | ✅ Active | 1.0.0 | - |
| Frontend | 📝 To Extract | TBD | - |
| Backend | 📝 To Extract | TBD | - |
| Babbage | 📝 To Extract | TBD | - |
| Services | 📝 To Extract | TBD | - |
| Curriculum | 📝 To Extract | TBD | - |
| Docs | 📝 To Extract | TBD | - |

**Legend**: ✅ Active | 📝 Planned | 🚧 In Progress | ⚠️ Issues

---

## 🆘 Getting Help

### Documentation
1. Check [PROJECT_STRUCTURE.md](./PROJECT_STRUCTURE.md)
2. Check [CONTRIBUTING.md](./CONTRIBUTING.md)
3. Search existing issues

### Ask Questions
1. Open a [GitHub Discussion](https://github.com/Oichkatzelesfrettschen/ancient-compute/discussions)
2. Create an issue with `question` label
3. Tag with relevant component label

### Report Issues
1. Use [Bug Report template](.github/ISSUE_TEMPLATE/bug_report.yml)
2. Include reproduction steps
3. Add relevant labels

---

## 📚 Learning Resources

### Multi-Repo Development
- [Git Submodules Tutorial](https://git-scm.com/book/en/v2/Git-Tools-Submodules)
- [GitHub Projects Guide](https://docs.github.com/en/issues/planning-and-tracking-with-projects)
- [Semantic Versioning](https://semver.org/)

### Ancient Compute Specific
- [MASTER_ROADMAP.md](./MASTER_ROADMAP.md) - Project overview
- [TYPE_THEORY_CURRICULUM.md](./TYPE_THEORY_CURRICULUM.md) - Educational content
- [AGENTS.md](./AGENTS.md) - AI agent system

---

## ✅ Checklist for New Contributors

- [ ] Read [README.md](./README.md)
- [ ] Read [CONTRIBUTING.md](./CONTRIBUTING.md)
- [ ] Read [PROJECT_STRUCTURE.md](./PROJECT_STRUCTURE.md)
- [ ] Clone with submodules
- [ ] Run compatibility check
- [ ] Set up development environment
- [ ] Run tests locally
- [ ] Introduce yourself in Discussions

---

**Last Updated**: November 2, 2025  
**Version**: 1.0

**Quick Links**: [Docs](#-documentation-index) | [Commands](#-quick-commands) | [Tasks](#-common-tasks) | [Help](#-getting-help)
