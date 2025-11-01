# Ancient Compute MVP File Structure

## Project Root Structure

```
ancient_compute/
├── .bazelrc                    # Bazel configuration
├── .bazelversion               # Pinned Bazel version
├── .gitignore                  # Git ignore rules
├── .gitattributes             # Git attributes (CRLF handling)
├── BUILD.bazel                # Root build file
├── WORKSPACE.bazel            # External dependencies
├── MODULE.bazel               # Bazel module configuration
├── docker-compose.yml         # Development environment
├── docker-compose.prod.yml    # Production environment
├── Makefile                   # Developer convenience commands
├── README.md                  # Project overview
├── LICENSE                    # MIT License
├── requirements.txt           # Python dependencies
├── package.json               # Node.js workspace root
├── pnpm-workspace.yaml        # PNPM monorepo configuration
├── .env.example               # Environment variables template
├── .dockerignore             # Docker ignore rules
├── .editorconfig             # Editor configuration
├── .pre-commit-config.yaml   # Pre-commit hooks
└── .vscode/                  # VS Code settings
    ├── settings.json
    ├── launch.json
    └── extensions.json

## Backend Structure

backend/
├── BUILD.bazel               # Backend build configuration
├── pyproject.toml           # Python project configuration
├── setup.py                 # Python package setup
├── requirements.txt         # Backend dependencies
├── requirements-dev.txt     # Development dependencies
├── Dockerfile               # Backend container
├── src/
│   ├── __init__.py
│   ├── main.py             # FastAPI application entry
│   ├── config.py           # Configuration management
│   ├── database.py         # Database connection
│   ├── redis_client.py     # Redis connection
│   ├── api/
│   │   ├── __init__.py
│   │   ├── router.py       # Main API router
│   │   ├── deps.py         # Dependency injection
│   │   ├── endpoints/
│   │   │   ├── __init__.py
│   │   │   ├── auth.py
│   │   │   ├── users.py
│   │   │   ├── modules.py
│   │   │   ├── lessons.py
│   │   │   ├── exercises.py
│   │   │   ├── execution.py
│   │   │   └── progress.py
│   │   └── websockets/
│   │       ├── __init__.py
│   │       ├── manager.py
│   │       └── handlers.py
│   ├── core/
│   │   ├── __init__.py
│   │   ├── security.py
│   │   ├── exceptions.py
│   │   ├── logging.py
│   │   └── middleware.py
│   ├── models/
│   │   ├── __init__.py
│   │   ├── user.py
│   │   ├── module.py
│   │   ├── lesson.py
│   │   ├── exercise.py
│   │   └── progress.py
│   ├── schemas/
│   │   ├── __init__.py
│   │   ├── user.py
│   │   ├── module.py
│   │   ├── lesson.py
│   │   ├── exercise.py
│   │   └── execution.py
│   ├── services/
│   │   ├── __init__.py
│   │   ├── auth_service.py
│   │   ├── content_service.py
│   │   ├── execution_service.py
│   │   ├── grading_service.py
│   │   └── progress_service.py
│   └── utils/
│       ├── __init__.py
│       ├── validators.py
│       ├── formatters.py
│       └── cache.py
├── tests/
│   ├── __init__.py
│   ├── conftest.py
│   ├── test_api/
│   ├── test_models/
│   ├── test_services/
│   └── test_integration/
└── alembic/
    ├── alembic.ini
    ├── env.py
    ├── script.py.mako
    └── versions/

## Frontend Structure

frontend/
├── BUILD.bazel
├── package.json
├── pnpm-lock.yaml
├── svelte.config.js
├── vite.config.js
├── tsconfig.json
├── postcss.config.js
├── tailwind.config.js
├── Dockerfile
├── .env.example
├── src/
│   ├── app.html
│   ├── app.css
│   ├── app.d.ts
│   ├── lib/
│   │   ├── components/
│   │   │   ├── common/
│   │   │   │   ├── Header.svelte
│   │   │   │   ├── Footer.svelte
│   │   │   │   ├── Navigation.svelte
│   │   │   │   └── LoadingSpinner.svelte
│   │   │   ├── editor/
│   │   │   │   ├── CodeEditor.svelte
│   │   │   │   ├── EditorToolbar.svelte
│   │   │   │   ├── LanguageSelector.svelte
│   │   │   │   └── ThemeSelector.svelte
│   │   │   ├── timeline/
│   │   │   │   ├── Timeline.svelte
│   │   │   │   ├── TimelineEvent.svelte
│   │   │   │   └── TimelineControls.svelte
│   │   │   ├── lesson/
│   │   │   │   ├── LessonViewer.svelte
│   │   │   │   ├── CodeExample.svelte
│   │   │   │   └── ExerciseRunner.svelte
│   │   │   └── visualization/
│   │   │       ├── TuringMachine.svelte
│   │   │       ├── LambdaReducer.svelte
│   │   │       └── MemoryDiagram.svelte
│   │   ├── stores/
│   │   │   ├── auth.ts
│   │   │   ├── progress.ts
│   │   │   ├── editor.ts
│   │   │   └── websocket.ts
│   │   ├── api/
│   │   │   ├── client.ts
│   │   │   ├── auth.ts
│   │   │   ├── content.ts
│   │   │   └── execution.ts
│   │   ├── utils/
│   │   │   ├── monaco.ts
│   │   │   ├── validation.ts
│   │   │   └── formatting.ts
│   │   └── types/
│   │       ├── index.ts
│   │       ├── api.ts
│   │       └── content.ts
│   ├── routes/
│   │   ├── +layout.svelte
│   │   ├── +layout.ts
│   │   ├── +page.svelte
│   │   ├── +error.svelte
│   │   ├── auth/
│   │   │   ├── login/+page.svelte
│   │   │   └── register/+page.svelte
│   │   ├── modules/
│   │   │   ├── +page.svelte
│   │   │   └── [moduleId]/
│   │   │       ├── +page.svelte
│   │   │       └── lessons/
│   │   │           └── [lessonId]/+page.svelte
│   │   ├── playground/
│   │   │   └── +page.svelte
│   │   └── timeline/
│   │       └── +page.svelte
│   └── tests/
│       ├── unit/
│       └── e2e/
├── static/
│   ├── favicon.png
│   └── robots.txt
└── public/
    └── assets/
        ├── images/
        └── fonts/

## Language Services Structure

services/
├── BUILD.bazel
├── _template/              # Template for new services
│   ├── Dockerfile
│   ├── requirements.txt
│   ├── sandbox-config.json
│   ├── server.py
│   └── tests/
├── c/
│   ├── BUILD.bazel
│   ├── Dockerfile
│   ├── requirements.txt
│   ├── sandbox-config.json
│   ├── server.py
│   ├── compiler.py
│   ├── sandbox.py
│   └── tests/
├── python/
│   ├── BUILD.bazel
│   ├── Dockerfile
│   ├── requirements.txt
│   ├── sandbox-config.json
│   ├── server.py
│   ├── executor.py
│   ├── type_checker.py
│   └── tests/
├── haskell/
│   ├── BUILD.bazel
│   ├── Dockerfile
│   ├── package.yaml
│   ├── stack.yaml
│   ├── sandbox-config.json
│   ├── app/
│   │   └── Main.hs
│   ├── src/
│   │   ├── Server.hs
│   │   ├── Compiler.hs
│   │   └── Sandbox.hs
│   └── test/
├── idris/
│   ├── BUILD.bazel
│   ├── Dockerfile
│   ├── idris.ipkg
│   ├── sandbox-config.json
│   ├── src/
│   └── test/
├── lisp/
│   ├── BUILD.bazel
│   ├── Dockerfile
│   ├── server.lisp
│   ├── sandbox-config.json
│   └── tests/
├── assembly/
│   ├── BUILD.bazel
│   ├── Dockerfile
│   ├── server.py
│   ├── emulator.py
│   ├── sandbox-config.json
│   └── tests/
├── java/
│   ├── BUILD.bazel
│   ├── Dockerfile
│   ├── pom.xml
│   ├── sandbox-config.json
│   ├── src/main/java/
│   └── src/test/java/
└── systemf/
    ├── BUILD.bazel
    ├── Dockerfile
    ├── requirements.txt
    ├── interpreter.py
    ├── type_checker.py
    ├── sandbox-config.json
    └── tests/

## Documentation Structure

docs/
├── BUILD.bazel
├── Dockerfile.latex
├── Makefile
├── main.tex                # Master document
├── ancientcompute.cls      # Custom LaTeX class
├── bibliography.bib        # References
├── timeline-sources.bib    # Historical sources
├── chapters/
│   ├── introduction.tex
│   ├── prehistory.tex
│   ├── ancient.tex
│   ├── medieval.tex
│   ├── modern.tex
│   └── contemporary.tex
├── diagrams/
│   ├── BUILD.bazel
│   ├── turing-machine.tex
│   ├── lambda-reduction.tex
│   └── type-hierarchy.tex
├── exercises/
│   └── solutions/
└── generated/             # Auto-generated from content

## Content Structure

content/
├── BUILD.bazel
├── manifest.yaml          # Content registry
├── modules/
│   ├── module-0-prehistory/
│   │   ├── metadata.yaml
│   │   ├── overview.md
│   │   ├── lessons/
│   │   │   ├── 01-tally-marks.md
│   │   │   ├── 02-clay-tokens.md
│   │   │   └── 03-proto-writing.md
│   │   ├── exercises/
│   │   │   ├── 01-counting.yaml
│   │   │   └── 02-representation.yaml
│   │   └── code-examples/
│   │       ├── tally.c
│   │       ├── tally.py
│   │       └── tally.hs
│   ├── module-1-ancient/
│   │   ├── metadata.yaml
│   │   ├── mesopotamia/
│   │   ├── egypt/
│   │   ├── greece/
│   │   ├── india/
│   │   └── china/
│   └── [other modules...]
└── synthesis/
    ├── syllogisms-to-types/
    ├── abacus-to-assembly/
    └── cross-cultural/

## Scripts and Configuration

scripts/
├── BUILD.bazel
├── setup-windows.ps1       # Windows setup script
├── setup-debian.sh         # Debian setup script
├── validate-sandbox.sh     # Security validation
├── validate-latex-deps.sh  # LaTeX dependencies
├── build-all.sh           # Full build script
├── test-all.sh            # Full test suite
├── deploy-staging.sh      # Staging deployment
└── deploy-production.sh   # Production deployment

config/
├── BUILD.bazel
├── nginx/
│   ├── nginx.conf
│   └── sites-available/
├── prometheus/
│   └── prometheus.yml
├── grafana/
│   └── dashboards/
└── kubernetes/
    ├── namespace.yaml
    ├── deployments/
    ├── services/
    └── ingress.yaml

## Shared Components

shared/
├── BUILD.bazel
├── types/
│   ├── execution.proto    # Protocol buffers
│   ├── content.proto
│   └── progress.proto
├── utils/
│   ├── python/
│   ├── typescript/
│   └── go/
└── contracts/
    └── openapi.yaml       # API specification

## Testing Infrastructure

tests/
├── BUILD.bazel
├── integration/
│   ├── test_full_flow.py
│   ├── test_language_services.py
│   └── test_security.py
├── performance/
│   ├── k6/
│   │   ├── load-test.js
│   │   └── stress-test.js
│   └── benchmarks/
└── security/
    ├── sandbox-escape/
    └── vulnerability-scan/

## CI/CD Configuration

.github/
├── workflows/
│   ├── ci.yml             # Continuous Integration
│   ├── cd-staging.yml     # Deploy to staging
│   ├── cd-production.yml  # Deploy to production
│   ├── security-scan.yml  # Security scanning
│   └── documentation.yml  # Build docs
├── dependabot.yml
└── CODEOWNERS

## Development Tools

tools/
├── BUILD.bazel
├── linters/
│   ├── .pylintrc
│   ├── .eslintrc.json
│   └── .hlint.yaml
├── formatters/
│   ├── .prettierrc
│   ├── .black.toml
│   └── .stylish-haskell.yaml
└── hooks/
    ├── pre-commit
    └── pre-push

## Database Migrations

migrations/
├── BUILD.bazel
├── alembic.ini
└── versions/
    ├── 001_initial_schema.py
    ├── 002_add_progress_tracking.py
    └── 003_add_exercises.py

## Deployment Artifacts

deploy/
├── BUILD.bazel
├── docker/
│   ├── base/
│   │   └── Dockerfile.base
│   └── production/
├── terraform/
│   ├── main.tf
│   ├── variables.tf
│   └── modules/
└── ansible/
    ├── playbooks/
    └── inventory/

## Build System Notes

### Bazel BUILD Files Structure

Each BUILD.bazel file follows this pattern:

```python
load("@rules_python//python:defs.bzl", "py_library", "py_test")
load("@npm//:defs.bzl", "npm_package")
load("@io_bazel_rules_docker//python3:image.bzl", "py3_image")

# Library definitions
py_library(
    name = "lib",
    srcs = glob(["src/**/*.py"]),
    deps = [
        "//shared/types:execution_py",
        "@pip//fastapi",
    ],
)

# Test definitions
py_test(
    name = "test",
    srcs = glob(["tests/**/*.py"]),
    deps = [":lib"],
)

# Docker image
py3_image(
    name = "image",
    srcs = [":lib"],
    main = "src/main.py",
    base = "//deploy/docker/base:python_base",
)
```

### Cross-Platform Considerations

1. All paths use forward slashes in BUILD files
2. Python code uses pathlib for OS-agnostic paths
3. Shell scripts have Windows PowerShell equivalents
4. Docker volumes mount consistently across platforms

This structure ensures:
- Clear separation of concerns
- Reproducible builds with Bazel
- Easy testing at all levels
- Secure language service isolation
- Cross-platform compatibility
- Scalable architecture
