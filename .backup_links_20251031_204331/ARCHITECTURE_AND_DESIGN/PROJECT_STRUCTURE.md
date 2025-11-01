# Ancient Compute Project Structure

## Directory Organization

```
ancient_compute/
├── GETTING_STARTED/              # Entry points & navigation
│   ├── README.md                 # Main welcome & orientation
│   ├── QUICK_START_5_MINUTES.md  # Quick paths by role
│   ├── DOCUMENT_FINDER.md        # Searchable document index
│   └── SITE_MAP.md               # Visual site hierarchy
│
├── ARCHITECTURE_AND_DESIGN/      # System specifications
│   ├── ARCHITECTURE.md           # Complete technical architecture
│   ├── PROJECT_STRUCTURE.md      # This file
│   ├── BACKEND_ARCHITECTURE.md   # FastAPI backend specifics
│   ├── FRONTEND_ARCHITECTURE.md  # SvelteKit frontend specifics
│   ├── DATABASE_SCHEMA.md        # Data model
│   ├── API_SPECIFICATION.md      # REST/WebSocket endpoints
│   ├── SECURITY_ARCHITECTURE.md  # Security & sandboxing
│   ├── IMPLEMENTATION_ROADMAP.md # 52-week development plan
│   ├── WEEK_1_COMPLETION_STATUS.md
│   ├── STRATEGIC_ROADMAP.md      # Next phases & priorities
│   ├── RISK_MANAGEMENT.md        # Risk assessment
│   └── PROJECT_SUMMARY_FOR_STAKEHOLDERS.md
│
├── DEVELOPMENT_GUIDES/           # How-to guides
│   ├── 01_SETUP_AND_INSTALLATION.md
│   ├── 02_CODE_STANDARDS_AND_STYLE.md
│   ├── 03_BUILD_SYSTEM_AND_TOOLS.md
│   ├── 04_TESTING_AND_QA.md
│   ├── 05_PERFORMANCE_AND_OPTIMIZATION.md
│   └── 06_CONTRIBUTION_GUIDELINES.md
│
├── CURRICULUM_AND_CONTENT/       # 12,500 years of computation
│   ├── 00_PLATFORM_OVERVIEW.md
│   ├── module-0-prehistory/
│   ├── module-1-ancient/
│   ├── module-2-medieval/
│   ├── module-3-early-modern/
│   ├── module-4-foundations/
│   ├── module-5-electronic/
│   ├── module-6-type-theory/
│   ├── module-7-synthesis/
│   ├── synthesis-a-*/
│   ├── synthesis-b-*/
│   ├── synthesis-c-*/
│   ├── code-examples/
│   ├── exercises/
│   └── references.md
│
├── BABBAGE_ENGINE_SPECIFICATION/ # Manufacturing specs
│   ├── 00_QUICK_FACTS_AND_OVERVIEW.md
│   ├── 01_COMPLETE_TECHNICAL_SPECIFICATION.md
│   ├── 02_MANUFACTURING_PROCEDURES.md
│   ├── 03_HISTORICAL_AUDIT_AND_CORRECTIONS.md
│   └── 04_REGIONAL_IMPLEMENTATIONS.md
│
├── IMPLEMENTATION_PHASES/        # Phase 0-4 details
│   ├── phase0/
│   ├── phase1/
│   ├── phase2/
│   ├── phase3/
│   └── phase4/
│
├── DEPLOYMENT_AND_DEVOPS/        # Production infrastructure
│   ├── DEPLOYMENT_GUIDE.md
│   ├── INFRASTRUCTURE_SETUP.md
│   ├── KUBERNETES_CONFIGURATION.md
│   ├── MONITORING_AND_LOGGING.md
│   └── DISASTER_RECOVERY.md
│
├── REFERENCE_MATERIALS/          # Standards & best practices
│   ├── PYTHON_STANDARDS.md
│   ├── TYPESCRIPT_STANDARDS.md
│   ├── SQL_BEST_PRACTICES.md
│   ├── REST_API_STANDARDS.md
│   ├── TESTING_BEST_PRACTICES.md
│   ├── GIT_WORKFLOW.md
│   ├── DOCUMENTATION_STANDARDS.md
│   ├── LICENSE_AND_ATTRIBUTION.md
│   └── ISSUE_REPORTING.md
│
├── ARCHIVE/                      # Historical documents
│
├── .github/                      # GitHub workflows
├── backend/                      # FastAPI backend
├── frontend/                     # SvelteKit frontend
├── services/                     # Language containers
├── content/                      # Educational content
├── docs/                         # LaTeX & documentation
├── infrastructure/               # Infrastructure as code
├── scripts/                      # Utility scripts
├── tests/                        # Integration tests
├── tools/                        # Development tools
├── examples/                     # Standalone examples
├── benchmarks/                   # Performance benchmarks
│
├── BUILD.bazel                   # Bazel root build
├── WORKSPACE.bazel               # Bazel workspace
├── Makefile                      # Development tasks
├── docker-compose.yml            # Development environment
├── .bazelrc                      # Bazel config
└── [config files]
```

## Documentation Directory Structure (Complete)

```
GETTING_STARTED/
├── README.md                     # Welcome & main entry point
├── QUICK_START_5_MINUTES.md     # Fast paths for different roles
├── DOCUMENT_FINDER.md           # Searchable document index
└── SITE_MAP.md                  # Visual site hierarchy

ARCHITECTURE_AND_DESIGN/
├── ARCHITECTURE.md              # Complete technical architecture
├── PROJECT_STRUCTURE.md         # This file - directory organization
├── BACKEND_ARCHITECTURE.md      # FastAPI, SQLAlchemy, async
├── FRONTEND_ARCHITECTURE.md     # SvelteKit, Monaco Editor, D3.js
├── DATABASE_SCHEMA.md           # Tables, relationships, migrations
├── API_SPECIFICATION.md         # REST endpoints, WebSocket events
├── SECURITY_ARCHITECTURE.md     # Sandboxing, isolation, threat model
├── IMPLEMENTATION_ROADMAP.md    # 52-week development plan
├── WEEK_1_COMPLETION_STATUS.md  # Current progress & assessment
├── STRATEGIC_ROADMAP.md         # Next phases & long-term vision
├── RISK_MANAGEMENT.md           # Identified risks & mitigation
└── PROJECT_SUMMARY_FOR_STAKEHOLDERS.md

DEVELOPMENT_GUIDES/
├── 01_SETUP_AND_INSTALLATION.md
├── 02_CODE_STANDARDS_AND_STYLE.md
├── 03_BUILD_SYSTEM_AND_TOOLS.md
├── 04_TESTING_AND_QA.md
├── 05_PERFORMANCE_AND_OPTIMIZATION.md
└── 06_CONTRIBUTION_GUIDELINES.md

CURRICULUM_AND_CONTENT/
├── 00_PLATFORM_OVERVIEW.md
├── module-0-prehistory/
│   ├── README.md
│   ├── lessons/
│   ├── exercises/
│   ├── code-examples/
│   └── references.md
├── module-1-ancient/
├── module-2-medieval/
├── module-3-early-modern/
├── module-4-foundations/
├── module-5-electronic/
├── module-6-type-theory/
├── module-7-synthesis/
├── synthesis-a-syllogisms-to-types/
├── synthesis-b-abacus-to-assembly/
├── synthesis-c-cross-cultural/
├── code-examples/
├── exercises/
└── references.md

BABBAGE_ENGINE_SPECIFICATION/
├── 00_QUICK_FACTS_AND_OVERVIEW.md
├── 01_COMPLETE_TECHNICAL_SPECIFICATION.md
├── 02_MANUFACTURING_PROCEDURES.md
├── 03_HISTORICAL_AUDIT_AND_CORRECTIONS.md
└── 04_REGIONAL_IMPLEMENTATIONS.md

IMPLEMENTATION_PHASES/
├── phase0/
│   ├── PHASE0_COMPLETION_INDEX.md
│   ├── design-review/
│   └── team-setup/
├── phase1/
│   ├── PHASE1_COMPLETION_INDEX.md
│   ├── procedures/
│   ├── procurement/
│   └── timeline/
├── phase2/
│   ├── PHASE2_COMPLETION_INDEX.md
│   ├── procedures/
│   └── diagrams/
├── phase3/
│   ├── PHASE3_COMPLETION_INDEX.md
│   ├── testing/
│   └── procedures/
└── phase4/
    ├── PHASE4_COMPLETION_INDEX.md
    ├── validation/
    └── documentation/

DEPLOYMENT_AND_DEVOPS/
├── DEPLOYMENT_GUIDE.md
├── INFRASTRUCTURE_SETUP.md
├── KUBERNETES_CONFIGURATION.md
├── MONITORING_AND_LOGGING.md
└── DISASTER_RECOVERY.md

REFERENCE_MATERIALS/
├── PYTHON_STANDARDS.md
├── TYPESCRIPT_STANDARDS.md
├── SQL_BEST_PRACTICES.md
├── REST_API_STANDARDS.md
├── TESTING_BEST_PRACTICES.md
├── GIT_WORKFLOW.md
├── DOCUMENTATION_STANDARDS.md
├── LICENSE_AND_ATTRIBUTION.md
└── ISSUE_REPORTING.md

ARCHIVE/
└── [Historical & deprecated documents]
```

## Source Code Directory Structure

### Backend (`/backend`)
```
backend/
├── src/
│   ├── api/
│   │   ├── __init__.py
│   │   ├── code_execution.py       # Code execution endpoints
│   │   └── router.py               # API route registration
│   ├── models/
│   │   ├── __init__.py
│   │   ├── lesson.py               # Lesson models
│   │   ├── module.py               # Module models
│   │   └── user.py                 # User models
│   ├── config.py                   # Configuration
│   ├── database.py                 # Database connection
│   ├── main.py                     # Application entry point
│   └── services/
│       └── [Business logic services]
├── tests/
│   ├── conftest.py
│   └── unit/
├── requirements.txt                # Dependencies
├── alembic.ini                    # Migration config
└── alembic/
    └── env.py                      # Migration environment
```

### Frontend (`/frontend`)
```
frontend/
├── src/
│   ├── routes/
│   │   ├── +layout.svelte
│   │   ├── +page.svelte
│   │   └── [dynamic routes]
│   ├── lib/
│   │   ├── components/
│   │   ├── stores/
│   │   └── utils/
│   ├── app.html
│   └── app.css
├── static/
│   ├── fonts/
│   ├── images/
│   └── icons/
├── tests/
├── package.json
├── svelte.config.js
├── vite.config.js
└── tsconfig.json
```

### Language Services (`/services`)
```
services/
├── lang-c/
│   ├── src/
│   ├── tests/
│   ├── Dockerfile
│   └── BUILD.bazel
├── lang-python/
├── lang-haskell/
├── lang-idris2/
├── lang-lisp/
├── lang-asm/
├── lang-java/
└── lang-systemf/
```

## Key Principles

### Separation of Concerns
- Documentation organized by audience and purpose
- Source code organized by component and layer
- Test files colocated with source code
- Configuration files at appropriate hierarchy levels

### Documentation Accessibility
- Multiple entry points for different roles
- Searchable index with cross-references
- Visual site map for navigation
- Quick-start guides for immediate action

### Build System
- Bazel for polyglot builds
- Make for common development tasks
- Docker for containerization
- Separate configs per environment

### Testing Organization
- Unit tests: With source code
- Integration tests: In `/tests` directory
- E2E tests: In `frontend/tests/e2e`
- Test naming: Descriptive filenames

### Documentation Standards
- Markdown for content documents
- LaTeX for academic whitepapers
- Frontmatter metadata for categorization
- Consistent heading hierarchy
- Cross-references with links

## Navigation Guide

### For Learning
→ Start in `CURRICULUM_AND_CONTENT/00_PLATFORM_OVERVIEW.md`

### For Development
→ Start in `DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md`

### For Understanding Architecture
→ Start in `ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md`

### For Manufacturing Babbage
→ Start in `BABBAGE_ENGINE_SPECIFICATION/00_QUICK_FACTS_AND_OVERVIEW.md`

### For Project Management
→ Start in `ARCHITECTURE_AND_DESIGN/PROJECT_SUMMARY_FOR_STAKEHOLDERS.md`

---

**Related Documents**:
- [ARCHITECTURE.md](ARCHITECTURE.md) - Technical architecture details
- [IMPLEMENTATION_ROADMAP.md](IMPLEMENTATION_ROADMAP.md) - Development timeline
- [README.md](../GETTING_STARTED/README.md) - Main entry point

**Last Updated**: October 31, 2025
