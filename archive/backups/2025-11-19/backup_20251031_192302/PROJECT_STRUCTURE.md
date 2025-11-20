# Ancient Compute Project Structure

## Directory Organization

```
ancient_compute/
├── .github/                      # GitHub specific configuration
│   ├── workflows/               # CI/CD workflows
│   │   ├── ci.yml              # Continuous integration
│   │   ├── deploy.yml          # Deployment pipeline
│   │   ├── security.yml        # Security scanning
│   │   └── docs.yml            # Documentation generation
│   ├── ISSUE_TEMPLATE/         # Issue templates
│   └── PULL_REQUEST_TEMPLATE/  # PR templates
│
├── backend/                     # FastAPI backend application
│   ├── app/
│   │   ├── api/                # API endpoints
│   │   │   ├── v1/
│   │   │   │   ├── languages/ # Language service endpoints
│   │   │   │   ├── content/   # Content management
│   │   │   │   ├── users/     # User management
│   │   │   │   └── ws/        # WebSocket handlers
│   │   ├── core/              # Core functionality
│   │   │   ├── config.py      # Configuration
│   │   │   ├── security.py    # Security utilities
│   │   │   └── database.py    # Database connection
│   │   ├── models/            # SQLAlchemy models
│   │   │   ├── content.py     # Content models
│   │   │   ├── user.py        # User models
│   │   │   └── execution.py   # Execution history
│   │   ├── schemas/           # Pydantic schemas
│   │   │   ├── language.py    # Language service schemas
│   │   │   ├── content.py     # Content schemas
│   │   │   └── execution.py   # Execution schemas
│   │   ├── services/          # Business logic
│   │   │   ├── compiler.py    # Compilation service
│   │   │   ├── executor.py    # Execution service
│   │   │   ├── sandbox.py     # Sandboxing logic
│   │   │   └── cache.py       # Caching service
│   │   ├── utils/             # Utility functions
│   │   └── main.py            # Application entry point
│   ├── migrations/             # Alembic migrations
│   ├── tests/                  # Backend tests
│   │   ├── unit/
│   │   ├── integration/
│   │   └── fixtures/
│   ├── requirements.txt        # Python dependencies
│   ├── pyproject.toml         # Poetry configuration
│   └── Dockerfile             # Backend container
│
├── frontend/                    # SvelteKit frontend
│   ├── src/
│   │   ├── routes/            # Page routes
│   │   │   ├── +layout.svelte # Root layout
│   │   │   ├── +page.svelte   # Home page
│   │   │   ├── learn/         # Learning modules
│   │   │   │   ├── [era]/     # Era-based content
│   │   │   │   └── [module]/  # Individual modules
│   │   │   ├── practice/      # Practice exercises
│   │   │   ├── reference/     # Language reference
│   │   │   └── api/           # API routes
│   │   ├── lib/               # Shared components
│   │   │   ├── components/    # UI components
│   │   │   │   ├── Editor/    # Code editor
│   │   │   │   ├── Terminal/  # Terminal emulator
│   │   │   │   ├── Visualizer/# Visualizations
│   │   │   │   └── Exercise/  # Exercise components
│   │   │   ├── stores/        # Svelte stores
│   │   │   ├── utils/         # Utility functions
│   │   │   └── api/           # API client
│   │   ├── app.html           # HTML template
│   │   └── app.css            # Global styles
│   ├── static/                 # Static assets
│   │   ├── fonts/
│   │   ├── images/
│   │   └── icons/
│   ├── tests/                  # Frontend tests
│   │   ├── unit/
│   │   └── e2e/
│   ├── package.json           # Node dependencies
│   ├── svelte.config.js       # SvelteKit config
│   ├── vite.config.js         # Vite config
│   └── Dockerfile             # Frontend container
│
├── services/                    # Language service containers
│   ├── lang-c/
│   │   ├── src/
│   │   │   ├── compiler.c     # Compilation wrapper
│   │   │   ├── sandbox.c      # Sandboxing implementation
│   │   │   └── server.py      # Service API
│   │   ├── tests/
│   │   ├── Dockerfile
│   │   └── BUILD.bazel
│   ├── lang-python/
│   │   ├── src/
│   │   │   ├── executor.py    # Python executor
│   │   │   ├── restrictor.py  # RestrictedPython
│   │   │   └── server.py      # Service API
│   │   ├── tests/
│   │   ├── Dockerfile
│   │   └── BUILD.bazel
│   ├── lang-haskell/
│   │   ├── src/
│   │   │   ├── Compiler.hs    # GHC wrapper
│   │   │   ├── TypeChecker.hs # Type information
│   │   │   └── Server.hs      # Service API
│   │   ├── tests/
│   │   ├── stack.yaml
│   │   ├── Dockerfile
│   │   └── BUILD.bazel
│   ├── lang-idris2/
│   │   ├── src/
│   │   │   ├── Compiler.idr   # Idris compiler wrapper
│   │   │   ├── ProofChecker.idr
│   │   │   └── server.py      # Service API
│   │   ├── tests/
│   │   ├── Dockerfile
│   │   └── BUILD.bazel
│   ├── lang-lisp/
│   │   ├── src/
│   │   │   ├── evaluator.lisp # SBCL evaluator
│   │   │   ├── macro-expander.lisp
│   │   │   └── server.py      # Service API
│   │   ├── tests/
│   │   ├── Dockerfile
│   │   └── BUILD.bazel
│   ├── lang-asm/
│   │   ├── src/
│   │   │   ├── assembler.c    # Multi-arch assembler
│   │   │   ├── emulator.c     # QEMU wrapper
│   │   │   └── server.py      # Service API
│   │   ├── tests/
│   │   ├── Dockerfile
│   │   └── BUILD.bazel
│   ├── lang-java/
│   │   ├── src/
│   │   │   └── main/
│   │   │       └── java/
│   │   │           └── com/ancientcompute/
│   │   │               ├── Compiler.java
│   │   │               ├── Executor.java
│   │   │               └── Server.java
│   │   ├── tests/
│   │   ├── build.gradle
│   │   ├── Dockerfile
│   │   └── BUILD.bazel
│   └── lang-systemf/
│       ├── src/
│       │   ├── Syntax.hs      # AST definition
│       │   ├── TypeChecker.hs # Type system
│       │   ├── Evaluator.hs   # Reduction strategies
│       │   └── Server.hs      # Service API
│       ├── tests/
│       ├── Dockerfile
│       └── BUILD.bazel
│
├── content/                     # Educational content
│   ├── eras/
│   │   ├── prehistoric/
│   │   │   ├── 01-counting-systems/
│   │   │   │   ├── lesson.md
│   │   │   │   ├── exercises/
│   │   │   │   ├── examples/
│   │   │   │   └── assets/
│   │   │   └── ...
│   │   ├── ancient/
│   │   ├── medieval/
│   │   ├── modern/
│   │   └── contemporary/
│   ├── concepts/
│   │   ├── logic/
│   │   │   ├── boolean-logic/
│   │   │   ├── predicate-logic/
│   │   │   └── modal-logic/
│   │   ├── computation/
│   │   ├── types/
│   │   └── paradigms/
│   ├── languages/
│   │   ├── c/
│   │   │   ├── tutorial/
│   │   │   ├── reference/
│   │   │   └── examples/
│   │   └── ...
│   ├── projects/
│   │   ├── turing-machine/
│   │   ├── lambda-calculus/
│   │   └── compiler/
│   └── metadata.yaml           # Content metadata
│
├── docs/                        # Documentation source
│   ├── latex/
│   │   ├── templates/
│   │   │   ├── ancientcompute.cls
│   │   │   ├── whitepaper.tex
│   │   │   └── tutorial.tex
│   │   ├── figures/            # TikZ diagrams
│   │   │   ├── automata.tex
│   │   │   ├── circuits.tex
│   │   │   └── trees.tex
│   │   └── bibliography.bib
│   ├── api/                    # API documentation
│   ├── architecture/           # Architecture docs
│   ├── guides/                 # User guides
│   │   ├── getting-started.md
│   │   ├── educator-guide.md
│   │   └── contributor-guide.md
│   └── Makefile                # Documentation build
│
├── infrastructure/              # Infrastructure as code
│   ├── docker/
│   │   ├── docker-compose.yml  # Development setup
│   │   ├── docker-compose.prod.yml
│   │   └── .env.example
│   ├── kubernetes/
│   │   ├── charts/             # Helm charts
│   │   │   └── ancient-compute/
│   │   ├── manifests/          # K8s manifests
│   │   └── secrets/            # Secret templates
│   ├── terraform/              # Cloud infrastructure
│   │   ├── modules/
│   │   ├── environments/
│   │   └── variables.tf
│   └── ansible/                # Configuration management
│       ├── playbooks/
│       └── roles/
│
├── scripts/                     # Utility scripts
│   ├── setup/
│   │   ├── setup-dev.sh        # Development setup
│   │   ├── setup-dev.ps1       # Windows setup
│   │   └── install-deps.sh     # Dependency installation
│   ├── build/
│   │   ├── build-all.sh        # Build all components
│   │   └── build-docs.sh       # Build documentation
│   ├── test/
│   │   ├── run-tests.sh        # Run all tests
│   │   └── validate-examples.sh # Validate code examples
│   └── deploy/
│       ├── deploy-staging.sh   # Deploy to staging
│       └── deploy-prod.sh      # Deploy to production
│
├── tests/                       # System-wide tests
│   ├── integration/            # Integration tests
│   ├── e2e/                    # End-to-end tests
│   ├── load/                   # Load tests
│   └── security/               # Security tests
│
├── tools/                       # Development tools
│   ├── linters/                # Custom linters
│   ├── generators/             # Code generators
│   └── analyzers/              # Static analyzers
│
├── examples/                    # Standalone examples
│   ├── algorithms/             # Algorithm implementations
│   │   ├── sorting/
│   │   ├── searching/
│   │   └── graphs/
│   ├── data-structures/        # Data structure examples
│   ├── paradigms/              # Programming paradigm demos
│   └── historical/             # Historical recreations
│
├── benchmarks/                  # Performance benchmarks
│   ├── compilation/            # Compilation speed
│   ├── execution/              # Execution performance
│   └── memory/                 # Memory usage
│
├── .bazelrc                    # Bazel configuration
├── .bazelversion               # Bazel version
├── BUILD.bazel                 # Root build file
├── WORKSPACE.bazel             # Bazel workspace
├── .dockerignore               # Docker ignore file
├── .gitignore                  # Git ignore file
├── .gitattributes              # Git attributes
├── .editorconfig               # Editor configuration
├── .prettierrc                 # Prettier configuration
├── .eslintrc.json              # ESLint configuration
├── Makefile                    # Root makefile
├── README.md                   # Project readme
├── ARCHITECTURE.md             # Architecture documentation
├── CONTRIBUTING.md             # Contribution guidelines
├── LICENSE                     # License file
└── SECURITY.md                 # Security policy
```

## Key Directory Purposes

### `/backend`
FastAPI application serving as the main API backend. Handles user management, content delivery, and orchestrates language services.

### `/frontend`
SvelteKit application providing the web interface. Includes code editor, visualization components, and educational UI.

### `/services`
Individual language service containers. Each service is self-contained with its own Dockerfile and implements the standard language service interface.

### `/content`
Educational content organized by era, concept, and language. Includes lessons, exercises, and examples in Markdown format with metadata.

### `/docs`
Documentation source files including LaTeX templates, API documentation, and user guides. Generates PDF and HTML documentation.

### `/infrastructure`
Infrastructure as code for deployment. Includes Docker Compose for development, Kubernetes manifests for production, and Terraform for cloud resources.

### `/scripts`
Utility scripts for setup, building, testing, and deployment. Supports both Linux (bash) and Windows (PowerShell).

### `/tests`
System-wide integration and end-to-end tests that span multiple components.

### `/tools`
Custom development tools including linters, generators, and analyzers specific to the project.

### `/examples`
Standalone code examples demonstrating algorithms, data structures, and programming paradigms across all supported languages.

### `/benchmarks`
Performance benchmarking suite for measuring compilation speed, execution performance, and resource usage.

## Build System Organization

### Bazel Build Structure
```
- Root WORKSPACE defines external dependencies
- Each service has its own BUILD.bazel file
- Shared libraries in //lib:... targets
- Language-specific rules for each service
- Test targets with //tests:... prefix
```

### Docker Build Strategy
```
- Multi-stage builds for optimization
- Base images with common dependencies
- Language-specific images extend base
- Production images minimize size
- Development images include debugging tools
```

## Configuration Management

### Environment Variables
```
- .env files for local development
- ConfigMaps for Kubernetes
- AWS Systems Manager for cloud
- Separate configs per environment
```

### Secrets Management
```
- Local: .env files (git-ignored)
- Development: Docker secrets
- Production: Kubernetes secrets
- Cloud: AWS Secrets Manager / Azure Key Vault
```

## Testing Organization

### Test Hierarchy
```
1. Unit tests: In each component's tests/ directory
2. Integration tests: In /tests/integration
3. E2E tests: In /tests/e2e
4. Load tests: In /tests/load
5. Security tests: In /tests/security
```

### Test Naming Convention
```
- Unit: test_*.py, *_test.go, *.test.ts
- Integration: integration_test_*.py
- E2E: e2e_*.spec.ts
- Benchmarks: bench_*.py
```

## Documentation Structure

### Documentation Types
```
1. API: OpenAPI/Swagger specifications
2. Architecture: Design documents and diagrams
3. User Guides: Tutorials and how-tos
4. Developer: Setup and contribution guides
5. Academic: Whitepapers and research
```

### Documentation Generation
```
- API docs from OpenAPI specs
- LaTeX to PDF for whitepapers
- Markdown to HTML for web docs
- Inline code documentation extracted
```

## Version Control Strategy

### Branch Structure
```
main          - Production-ready code
develop       - Development integration
feature/*     - Feature branches
release/*     - Release preparation
hotfix/*      - Production fixes
docs/*        - Documentation updates
```

### Commit Convention
```
feat: New feature
fix: Bug fix
docs: Documentation
style: Formatting
refactor: Code restructuring
test: Test additions
chore: Maintenance
```

## Security Considerations

### File Permissions
```
- Executable scripts: 755
- Configuration files: 644
- Secret files: 600
- Directories: 755
```

### Sensitive Files
```
- Never commit: .env, *.key, *.pem
- Use git-secret for encrypted files
- Rotate secrets regularly
- Audit access logs
```

## Development Workflow

### Local Development
```bash
# Initial setup
make setup-dev

# Start services
docker-compose up

# Run tests
make test

# Build documentation
make docs
```

### CI/CD Pipeline
```
1. Lint and format check
2. Build all components
3. Run unit tests
4. Run integration tests
5. Security scanning
6. Deploy to staging
7. Run E2E tests
8. Deploy to production
```

This structure provides clear separation of concerns, supports polyglot development, and scales from local development to production deployment.
