# Week 1 Implementation Checklist

## Objective
Initialize the Ancient Compute project with foundational infrastructure, build system, and development environment setup for cross-platform compatibility (Windows 11 + Debian).

## Prerequisites Check
- [ ] Windows 11 with WSL2 enabled
- [ ] Docker Desktop installed and configured
- [ ] Git configured with proper line ending settings
- [ ] Bazel 6.0+ installed
- [ ] Python 3.11+ available
- [ ] Node.js 18+ and pnpm installed
- [ ] Visual Studio Code with recommended extensions

## Day 1: Repository and Build System Setup

### Morning (4 hours)
- [ ] Initialize Git repository
  ```bash
  git init ancient_compute
  cd ancient_compute
  git config core.autocrlf input  # Important for Windows
  ```

- [ ] Create .gitignore
  ```bash
  # Copy from MVP_FILE_STRUCTURE.md .gitignore section
  ```

- [ ] Create .gitattributes for cross-platform compatibility
  ```
  * text=auto eol=lf
  *.bat text eol=crlf
  *.ps1 text eol=crlf
  ```

- [ ] Initialize Bazel workspace
  ```bash
  touch WORKSPACE.bazel
  touch MODULE.bazel
  touch BUILD.bazel
  touch .bazelrc
  echo "6.4.0" > .bazelversion
  ```

### Afternoon (4 hours)
- [ ] Configure Bazel for polyglot support
  - [ ] Add rules_python to WORKSPACE.bazel
  - [ ] Add rules_nodejs to WORKSPACE.bazel
  - [ ] Add rules_docker to WORKSPACE.bazel
  - [ ] Configure .bazelrc with platform-specific settings

- [ ] Create initial directory structure
  ```bash
  mkdir -p backend/src backend/tests
  mkdir -p frontend/src frontend/tests
  mkdir -p services/{c,python,haskell,idris,lisp,assembly,java,systemf}
  mkdir -p docs/chapters docs/diagrams
  mkdir -p content/modules content/synthesis
  mkdir -p scripts config shared tools
  mkdir -p .github/workflows
  ```

## Day 2: Docker Environment Setup

### Morning (4 hours)
- [ ] Create base docker-compose.yml
  - [ ] Copy from DOCKER_INFRASTRUCTURE.md
  - [ ] Adjust paths for Windows compatibility

- [ ] Set up Docker networking
  ```yaml
  # Create networks as specified in DOCKER_INFRASTRUCTURE.md
  ```

- [ ] Configure WSL2 integration
  ```powershell
  # Run setup-wsl2.ps1 from DOCKER_INFRASTRUCTURE.md
  ```

- [ ] Install gVisor runtime for enhanced isolation
  ```bash
  # Follow gVisor installation from DOCKER_INFRASTRUCTURE.md
  ```

### Afternoon (4 hours)
- [ ] Create security configurations
  - [ ] config/seccomp/default.json
  - [ ] services/*/sandbox-config.json templates

- [ ] Set up base Docker images
  - [ ] Create Dockerfile.base for common dependencies
  - [ ] Build and test base image

- [ ] Configure Docker registry (local)
  ```bash
  docker run -d -p 5000:5000 --name registry registry:2
  ```

## Day 3: Backend Foundation

### Morning (4 hours)
- [ ] Initialize Python backend
  ```bash
  cd backend
  python -m venv venv
  source venv/bin/activate  # or venv\Scripts\activate on Windows
  pip install fastapi uvicorn sqlalchemy alembic redis pydantic
  pip freeze > requirements.txt
  ```

- [ ] Create FastAPI skeleton
  - [ ] backend/src/main.py
  - [ ] backend/src/config.py
  - [ ] backend/src/database.py
  - [ ] backend/src/api/router.py

- [ ] Set up database models
  - [ ] backend/src/models/user.py
  - [ ] backend/src/models/module.py
  - [ ] backend/src/models/lesson.py

### Afternoon (4 hours)
- [ ] Configure Alembic for migrations
  ```bash
  alembic init alembic
  # Configure alembic.ini with database URL
  ```

- [ ] Create initial migration
  ```bash
  alembic revision --autogenerate -m "Initial schema"
  alembic upgrade head
  ```

- [ ] Implement health check endpoints
  - [ ] GET /health
  - [ ] GET /ready
  - [ ] GET /metrics

- [ ] Write initial backend tests
  ```bash
  pytest backend/tests/test_health.py
  ```

## Day 4: Frontend Foundation

### Morning (4 hours)
- [ ] Initialize SvelteKit project
  ```bash
  cd frontend
  npm create svelte@latest .
  # Choose: Skeleton project, TypeScript, ESLint, Prettier
  pnpm install
  ```

- [ ] Configure TypeScript strictly
  ```json
  // tsconfig.json
  {
    "compilerOptions": {
      "strict": true,
      "noUncheckedIndexedAccess": true,
      "noImplicitOverride": true
    }
  }
  ```

- [ ] Install core dependencies
  ```bash
  pnpm add -D @sveltejs/adapter-node
  pnpm add d3 three monaco-editor
  pnpm add -D @types/d3 @types/three
  ```

### Afternoon (4 hours)
- [ ] Create base layout structure
  - [ ] frontend/src/routes/+layout.svelte
  - [ ] frontend/src/lib/components/common/Header.svelte
  - [ ] frontend/src/lib/components/common/Navigation.svelte

- [ ] Set up API client
  - [ ] frontend/src/lib/api/client.ts
  - [ ] Configure environment variables

- [ ] Create initial routes
  - [ ] frontend/src/routes/+page.svelte (landing)
  - [ ] frontend/src/routes/modules/+page.svelte
  - [ ] frontend/src/routes/timeline/+page.svelte

- [ ] Configure Vite for development
  ```js
  // vite.config.js - Add proxy for backend API
  ```

## Day 5: Development Tooling and CI/CD

### Morning (4 hours)
- [ ] Set up pre-commit hooks
  ```bash
  pip install pre-commit
  # Create .pre-commit-config.yaml from template
  pre-commit install
  ```

- [ ] Configure linters and formatters
  - [ ] Python: black, pylint, mypy
  - [ ] TypeScript: eslint, prettier
  - [ ] Docker: hadolint

- [ ] Create Makefile for common tasks
  ```makefile
  # Essential targets
  .PHONY: setup dev test build clean
  ```

### Afternoon (4 hours)
- [ ] Set up GitHub Actions workflow
  - [ ] .github/workflows/ci.yml
  - [ ] Configure for Windows and Linux runners
  - [ ] Add security scanning with trivy

- [ ] Create development scripts
  - [ ] scripts/setup-windows.ps1
  - [ ] scripts/setup-debian.sh
  - [ ] scripts/validate-sandbox.sh

- [ ] Configure VS Code workspace
  ```json
  // .vscode/settings.json
  {
    "files.eol": "\n",
    "python.linting.enabled": true,
    "typescript.tsdk": "node_modules/typescript/lib"
  }
  ```

- [ ] Document development setup
  - [ ] Update README.md with quick start
  - [ ] Create CONTRIBUTING.md

## Validation Checklist

### Build System
- [ ] `bazel build //...` runs successfully
- [ ] `bazel test //...` passes (even with minimal tests)
- [ ] Cross-platform build works (test on WSL2)

### Docker Environment
- [ ] `docker-compose up -d` starts all services
- [ ] Services are accessible at expected ports
- [ ] Networks are properly isolated
- [ ] Security configurations are applied

### Backend
- [ ] FastAPI server starts: `uvicorn main:app --reload`
- [ ] Health endpoints respond: `curl localhost:8000/health`
- [ ] Database migrations apply cleanly
- [ ] Basic tests pass: `pytest`

### Frontend
- [ ] Development server starts: `pnpm dev`
- [ ] Pages load without errors
- [ ] API proxy works to backend
- [ ] TypeScript compilation succeeds

### Development Tools
- [ ] Pre-commit hooks run on commit
- [ ] Linters catch issues appropriately
- [ ] Make targets work as expected
- [ ] CI pipeline passes on push

## Success Metrics for Week 1

1. **Infrastructure Ready**: Full development environment operational on Windows 11
2. **Build System Functional**: Bazel successfully building all components
3. **Docker Orchestration**: All services running in isolated containers
4. **Basic Application**: Minimal but functional backend API and frontend UI
5. **CI/CD Pipeline**: Automated testing on every push
6. **Documentation**: Clear setup instructions for new developers

## Next Week Preview (Week 2)

- Implement C language service with GCC and sandboxing
- Create Python service with RestrictedPython
- Design and implement security layers
- Begin content schema design
- Start timeline visualization component

## Common Issues and Solutions

### Issue: WSL2 Performance
**Solution**: Ensure project files are in WSL2 filesystem, not Windows mount

### Issue: Bazel Cache Permissions
**Solution**: Configure Bazel to use WSL2-native cache directory

### Issue: Docker Desktop Memory
**Solution**: Allocate at least 8GB RAM to Docker Desktop in settings

### Issue: Line Ending Conflicts
**Solution**: Ensure .gitattributes is committed and all developers run `git config core.autocrlf input`

### Issue: Port Conflicts
**Solution**: Check for existing services on ports 80, 443, 5432, 6379, 8000, 3000

## Repository Structure After Week 1

```
ancient_compute/
├── .bazelrc                    ✓
├── .bazelversion               ✓
├── .gitignore                  ✓
├── .gitattributes             ✓
├── .pre-commit-config.yaml    ✓
├── BUILD.bazel                ✓
├── WORKSPACE.bazel            ✓
├── MODULE.bazel               ✓
├── docker-compose.yml         ✓
├── Makefile                   ✓
├── README.md                  ✓
├── CONTRIBUTING.md            ✓
├── backend/
│   ├── BUILD.bazel            ✓
│   ├── Dockerfile             ✓
│   ├── requirements.txt       ✓
│   ├── src/
│   │   ├── main.py           ✓
│   │   ├── config.py         ✓
│   │   ├── database.py       ✓
│   │   └── api/              ✓
│   ├── tests/                ✓
│   └── alembic/              ✓
├── frontend/
│   ├── BUILD.bazel           ✓
│   ├── package.json          ✓
│   ├── svelte.config.js      ✓
│   ├── vite.config.js        ✓
│   ├── tsconfig.json         ✓
│   ├── src/
│   │   ├── app.html         ✓
│   │   ├── routes/          ✓
│   │   └── lib/             ✓
│   └── tests/               ✓
├── services/                ✓ (structure only)
├── docs/                    ✓ (structure only)
├── content/                 ✓ (structure only)
├── scripts/
│   ├── setup-windows.ps1    ✓
│   └── setup-debian.sh      ✓
├── config/
│   └── seccomp/
│       └── default.json     ✓
└── .github/
    └── workflows/
        └── ci.yml           ✓
```

## Final Verification

Run this command sequence to verify everything works:

```bash
# 1. Build everything
make setup
bazel build //...

# 2. Start services
docker-compose up -d

# 3. Run tests
bazel test //...

# 4. Check endpoints
curl http://localhost:8000/health
curl http://localhost:3000/

# 5. Verify security
docker-compose run backend python -c "import os; print(os.getuid())"  # Should not be 0

# 6. Stop services
docker-compose down
```

If all checks pass, Week 1 is complete! The Ancient Compute foundation is ready for language services and content development.