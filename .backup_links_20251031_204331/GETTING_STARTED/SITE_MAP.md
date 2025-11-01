# Site Map: Visual Navigation Guide

This document provides a visual hierarchy of all documentation in the Ancient Compute project.

---

## Main Site Structure

```
ANCIENT COMPUTE (Root)
│
├── GETTING_STARTED/              ← You are here
│   ├── README.md                 ← Start here for orientation
│   ├── QUICK_START_5_MINUTES.md  ← Fast paths by role
│   ├── DOCUMENT_FINDER.md        ← Search all documents
│   └── SITE_MAP.md               ← This file
│
├── ARCHITECTURE_AND_DESIGN/      ← System design & specifications
├── DEVELOPMENT_GUIDES/           ← How-to guides & practices
├── CURRICULUM_AND_CONTENT/       ← 12,500 years of computation
├── BABBAGE_ENGINE_SPECIFICATION/ ← Manufacturing the Analytical Engine
├── IMPLEMENTATION_PHASES/        ← Phase 0-4 detailed planning
├── DEPLOYMENT_AND_DEVOPS/        ← Production deployment
├── REFERENCE_MATERIALS/          ← Standards & best practices
├── ARCHIVE/                      ← Historical documents
│
└── [Source Code & Build Files]
    ├── frontend/                 ← SvelteKit web app
    ├── backend/                  ← FastAPI API server
    ├── services/                 ← Language execution services
    ├── docs/                     ← LaTeX curriculum
    ├── whitepaper/               ← Babbage whitepaper PDFs
    ├── Makefile                  ← Development tasks
    ├── docker-compose.yml        ← Local environment
    └── [config files]
```

---

## GETTING_STARTED (Entry Points)

```
GETTING_STARTED/
├── README.md
│   ├── Welcome message
│   ├── Quick orientation (5 min)
│   ├── What are you here for? (4 paths)
│   ├── Project at a glance
│   ├── Technology stack
│   ├── Quick links by role
│   ├── Directory structure
│   ├── Most important documents
│   └── Finding what you need
│
├── QUICK_START_5_MINUTES.md
│   ├── Learning computation history (2 min setup)
│   ├── Manufacturing Babbage (2 min setup)
│   ├── Development environment (2 min setup)
│   ├── Understanding architecture (2 min setup)
│   └── Common tasks (quick reference)
│
├── DOCUMENT_FINDER.md
│   ├── Search by topic (with read times)
│   ├── Search by role/audience
│   ├── Search by document type
│   ├── Search by keyword
│   ├── All documents at a glance
│   └── Quick navigation tips
│
└── SITE_MAP.md (this file)
    ├── Main site structure
    ├── Detailed section navigation
    ├── Content type structure
    ├── Learning paths
    └── Cross-references
```

---

## ARCHITECTURE_AND_DESIGN (System Specifications)

```
ARCHITECTURE_AND_DESIGN/
│
├── PROJECT_STRUCTURE.md
│   └── Detailed directory organization & rationale
│
├── ARCHITECTURE.md (PRIMARY)
│   ├── Tech stack overview
│   ├── System layers & integration
│   ├── Multi-language support
│   ├── Database architecture
│   ├── Security model
│   ├── Scalability & deployment
│   ├── Content management
│   ├── Interactive components
│   ├── Testing strategy
│   ├── Performance optimization
│   ├── Monitoring & observability
│   ├── Accessibility
│   └── Extensibility
│
├── BACKEND_ARCHITECTURE.md
│   ├── FastAPI framework & routing
│   ├── SQLAlchemy ORM & models
│   ├── Service layer & business logic
│   ├── Error handling & logging
│   ├── Async/await patterns
│   ├── Dependency injection
│   └── Testing strategy (backend)
│
├── FRONTEND_ARCHITECTURE.md
│   ├── SvelteKit & routing
│   ├── Component structure
│   ├── State management
│   ├── API client & WebSocket
│   ├── Monaco editor integration
│   ├── D3.js visualizations
│   ├── Three.js 3D models
│   └── Testing strategy (frontend)
│
├── DATABASE_SCHEMA.md
│   ├── Entity relationship diagram
│   ├── Table definitions & relationships
│   ├── Indexing strategy
│   ├── Migration procedures
│   └── Query patterns
│
├── API_SPECIFICATION.md
│   ├── REST endpoint documentation
│   ├── Request/response formats
│   ├── Authentication & authorization
│   ├── Error responses
│   ├── Rate limiting
│   ├── WebSocket events
│   └── API versioning strategy
│
├── SECURITY_ARCHITECTURE.md
│   ├── Threat model
│   ├── Sandbox isolation (Docker)
│   ├── Resource limits & cgroups
│   ├── Authentication/authorization
│   ├── Data encryption
│   ├── Security headers
│   ├── Vulnerability scanning
│   └── Incident response
│
├── IMPLEMENTATION_ROADMAP.md (PRIMARY)
│   ├── 52-week phased plan
│   ├── Foundation phase (Weeks 1-4)
│   ├── Language services (Weeks 5-8)
│   ├── Content management (Weeks 9-12)
│   ├── Interactive components (Weeks 13-16)
│   ├── Documentation system (Weeks 17-20)
│   ├── Testing & QA (Weeks 21-24)
│   ├── Optimization (Weeks 25-28)
│   ├── Deployment (Weeks 29-32)
│   ├── Content development (Weeks 33-40)
│   ├── Advanced features (Weeks 41-44)
│   ├── Beta testing (Weeks 45-48)
│   ├── Launch (Weeks 49-52)
│   ├── Milestones & deliverables
│   ├── Resource allocation
│   └── Success metrics
│
├── WEEK_1_COMPLETION_STATUS.md
│   ├── Current progress summary
│   ├── Completed items
│   ├── Blockers & risks
│   ├── Next week priorities
│   └── Assessment ratings
│
├── STRATEGIC_ROADMAP.md
│   ├── Vision & long-term goals
│   ├── Next phase priorities
│   ├── Architectural evolution
│   ├── New feature pipeline
│   ├── Technology upgrades
│   └── Long-term research directions
│
├── RISK_MANAGEMENT.md
│   ├── Identified risks
│   ├── Risk ratings (probability × impact)
│   ├── Mitigation strategies
│   ├── Contingency plans
│   ├── Monitoring & alerts
│   └── Risk review schedule
│
└── PROJECT_SUMMARY_FOR_STAKEHOLDERS.md
    ├── Executive summary
    ├── Key achievements
    ├── Current metrics
    ├── Budget & resources
    ├── Timeline & milestones
    ├── Risk summary
    └── Next phases
```

---

## DEVELOPMENT_GUIDES (How-To Guides)

```
DEVELOPMENT_GUIDES/
│
├── 01_SETUP_AND_INSTALLATION.md (PRIMARY)
│   ├── System requirements
│   ├── Prerequisites (Docker, Python, Node, Make)
│   ├── Environment setup (all OSes)
│   ├── Database initialization
│   ├── Dependency installation
│   ├── Development server startup
│   ├── Troubleshooting setup issues
│   └── IDE configuration (VS Code, PyCharm, WebStorm)
│
├── 02_CODE_STANDARDS_AND_STYLE.md
│   ├── Python conventions (PEP 8, type hints)
│   ├── TypeScript conventions (strict mode)
│   ├── Naming conventions
│   ├── Code organization (modules, layers)
│   ├── Error handling patterns
│   ├── Logging standards
│   ├── Comments & documentation
│   ├── Code review checklist
│   └── Automated formatting (Black, Prettier)
│
├── 03_BUILD_SYSTEM_AND_TOOLS.md
│   ├── Bazel overview & configuration
│   ├── Make targets & development tasks
│   ├── Docker image building
│   ├── Docker Compose for local development
│   ├── Build optimization
│   ├── CI/CD pipeline (GitHub Actions)
│   ├── Build troubleshooting
│   └── Performance profiling
│
├── 04_TESTING_AND_QA.md
│   ├── Testing strategy (unit, integration, e2e)
│   ├── pytest configuration (backend)
│   ├── Vitest configuration (frontend)
│   ├── Mock & fixture patterns
│   ├── Coverage measurement
│   ├── Load testing procedures
│   ├── Security testing
│   ├── Regression testing
│   └── Test data management
│
├── 05_PERFORMANCE_AND_OPTIMIZATION.md
│   ├── Profiling tools & techniques
│   ├── Database query optimization
│   ├── API response time optimization
│   ├── Frontend bundle optimization
│   ├── Caching strategies
│   ├── Memory leak detection
│   ├── Benchmarking procedures
│   └── Performance monitoring
│
└── 06_CONTRIBUTION_GUIDELINES.md
    ├── Community guidelines
    ├── Code of conduct
    ├── How to contribute
    ├── Branch naming conventions
    ├── Commit message style
    ├── Pull request process
    ├── Code review expectations
    ├── Issue reporting
    └── Contact & communication channels
```

---

## CURRICULUM_AND_CONTENT (Learning Materials)

```
CURRICULUM_AND_CONTENT/
│
├── 00_PLATFORM_OVERVIEW.md
│   └── Curriculum structure, learning paths, module organization
│
├── module-0-prehistory/
│   ├── README.md (module overview)
│   ├── lessons/
│   │   ├── 01_ishango_bone.md
│   │   ├── 02_clay_tokens.md
│   │   ├── 03_one_to_one_correspondence.md
│   │   └── ...
│   ├── exercises/
│   │   ├── 01_counting_systems.md
│   │   └── ...
│   ├── code-examples/
│   │   └── ...
│   └── references.md
│
├── module-1-ancient/
│   ├── README.md
│   ├── mesopotamia/
│   │   ├── lessons/
│   │   │   ├── babylonian_algorithms.md
│   │   │   ├── multiplication.md
│   │   │   └── ...
│   │   ├── exercises/
│   │   └── code-examples/
│   ├── greece/
│   │   ├── lessons/
│   │   │   ├── aristotelian_logic.md
│   │   │   ├── syllogisms.md
│   │   │   └── ...
│   │   ├── exercises/
│   │   └── code-examples/
│   ├── india/
│   │   └── [same structure]
│   ├── china/
│   │   └── [same structure]
│   └── references.md
│
├── module-2-medieval/
│   ├── README.md
│   ├── islamic_golden_age/
│   │   ├── al_khwarizmi.md
│   │   ├── algebra.md
│   │   └── ...
│   ├── scholastic/
│   │   └── [similar structure]
│   └── references.md
│
├── module-3-early-modern/
│   ├── README.md
│   ├── leibniz_binary.md
│   ├── boole_algebra.md
│   ├── babbage_lovelace.md
│   └── references.md
│
├── module-4-foundations/
│   ├── README.md
│   ├── frege_russell.md
│   ├── godel_incompleteness.md
│   ├── church_lambda.md
│   ├── turing_machines.md
│   └── references.md
│
├── module-5-electronic/
│   ├── README.md
│   ├── eniac_vonneumann.md
│   ├── lisp_history.md
│   ├── algol_structured.md
│   └── references.md
│
├── module-6-type-theory/
│   ├── README.md
│   ├── system_f.md
│   ├── hindley_milner.md
│   ├── martin_lof_types.md
│   ├── dependent_types.md
│   └── references.md
│
├── module-7-synthesis/
│   ├── README.md
│   ├── multiparadigm.md
│   ├── quantum_computing.md
│   └── references.md
│
├── synthesis-a-syllogisms-to-types/
│   ├── README.md
│   ├── lessons/
│   └── exercises/
│
├── synthesis-b-abacus-to-assembly/
│   ├── README.md
│   ├── lessons/
│   └── exercises/
│
├── synthesis-c-cross-cultural/
│   ├── README.md
│   ├── lessons/
│   └── exercises/
│
├── code-examples/
│   ├── README.md (how to use examples)
│   ├── by-language/
│   │   ├── c/
│   │   ├── python/
│   │   ├── haskell/
│   │   ├── idris2/
│   │   ├── lisp/
│   │   ├── assembly/
│   │   ├── java/
│   │   └── systemf/
│   └── by-concept/
│       ├── sorting/
│       ├── searching/
│       ├── arithmetic/
│       ├── logic/
│       └── ...
│
├── exercises/
│   ├── README.md (difficulty levels & grading)
│   ├── beginner/
│   ├── intermediate/
│   ├── advanced/
│   └── expert/
│
└── references.md (primary & secondary sources)
```

---

## BABBAGE_ENGINE_SPECIFICATION (Manufacturing Docs)

```
BABBAGE_ENGINE_SPECIFICATION/
│
├── 00_QUICK_FACTS_AND_OVERVIEW.md
│   ├── Executive summary
│   ├── Feasibility verdict
│   ├── Key metrics (size, weight, cost, timeline)
│   ├── Regional options
│   ├── Historical context
│   └── Next steps
│
├── 01_COMPLETE_TECHNICAL_SPECIFICATION.md (PRIMARY)
│   ├── System architecture
│   ├── Arithmetic unit (50-digit decimal)
│   ├── Memory subsystem (2,000 entries)
│   ├── Control unit & sequencing
│   ├── 32-instruction set specification
│   ├── Mill assembly specifications
│   ├── Store assembly specifications
│   ├── I/O mechanisms
│   ├── Bill of materials (Tier 1/2/3)
│   ├── Tolerance specifications
│   ├── Manufacturing procedures overview
│   ├── Quality gates & verification
│   └── Cost analysis
│
├── 02_MANUFACTURING_PROCEDURES.md
│   ├── Phase 1: Facility setup
│   ├── Phase 2: Procurement
│   ├── Phase 3: Component manufacturing
│   ├── Phase 4: Subassembly
│   ├── Phase 5: Integration & testing
│   ├── Quality verification procedures
│   ├── Rework & scrap procedures
│   ├── Documentation requirements
│   └── Timeline & critical path
│
├── 03_HISTORICAL_AUDIT_AND_CORRECTIONS.md
│   ├── Verification of historical claims
│   ├── Supplier documentation & dates
│   ├── Technology availability by era
│   ├── Anachronisms identified & corrected
│   ├── Regional industrialization analysis
│   ├── Primary sources cited
│   └── Confidence assessment
│
└── 04_REGIONAL_IMPLEMENTATIONS.md
    ├── India scenario (optimal case)
    │   ├── Cost breakdown
    │   ├── Timeline
    │   ├── Supplier sourcing
    │   ├── Labor availability
    │   └── Feasibility assessment
    ├── Brazil scenario
    ├── Argentina scenario
    └── China scenario
```

---

## IMPLEMENTATION_PHASES (Phase-by-Phase Details)

```
IMPLEMENTATION_PHASES/
│
├── phase0/
│   ├── PHASE0_COMPLETION_INDEX.md
│   ├── design-review/
│   │   ├── PHASE0_DESIGN_REVIEW.md
│   │   ├── Bill of Materials Review
│   │   ├── Manufacturing Procedures Review
│   │   └── Risk Assessment
│   └── team-setup/
│       └── Team Structure & Roles
│
├── phase1/
│   ├── PHASE1_COMPLETION_INDEX.md
│   ├── procedures/
│   │   ├── PHASE1_MANUFACTURING_PROCEDURES.md
│   │   ├── Facility Setup Procedures
│   │   ├── Equipment Calibration
│   │   └── Quality Control Setup
│   ├── procurement/
│   │   └── Supplier Contacts & Lead Times
│   └── timeline/
│       └── Critical Path Analysis
│
├── phase2/
│   ├── PHASE2_COMPLETION_INDEX.md
│   ├── procedures/
│   │   ├── PHASE2_ASSEMBLY_PROCEDURES_WITH_DIAGRAMS.md
│   │   ├── Mill Assembly
│   │   ├── Store Assembly
│   │   ├── Barrel Assembly
│   │   ├── I/O Assembly
│   │   └── Interconnection Procedures
│   └── diagrams/
│       ├── Assembly drawings (TikZ)
│       ├── Part layouts
│       └── Connection diagrams
│
├── phase3/
│   ├── PHASE3_COMPLETION_INDEX.md
│   ├── testing/
│   │   ├── PHASE3_COMPONENT_TEST_SPECIFICATIONS.md
│   │   ├── Unit Tests for Mill
│   │   ├── Unit Tests for Store
│   │   ├── Unit Tests for Barrel
│   │   ├── Unit Tests for I/O
│   │   └── Integration Tests
│   └── procedures/
│       └── Test Execution Procedures
│
└── phase4/
    ├── PHASE4_COMPLETION_INDEX.md
    ├── validation/
    │   ├── PHASE4_SYSTEM_OPERATIONAL_VALIDATION.md
    │   ├── Functional Validation Tests
    │   ├── Performance Benchmarks
    │   ├── Accuracy Verification
    │   ├── Reliability Testing
    │   └── Long-term Stability Tests
    └── documentation/
        └── System Handover Documentation
```

---

## DEPLOYMENT_AND_DEVOPS (Infrastructure)

```
DEPLOYMENT_AND_DEVOPS/
│
├── DEPLOYMENT_GUIDE.md (PRIMARY)
│   ├── Pre-deployment checklist
│   ├── Docker image building
│   ├── Docker registry setup
│   ├── Kubernetes deployment
│   ├── ConfigMaps & Secrets
│   ├── Service discovery
│   ├── Ingress configuration
│   ├── SSL/TLS setup
│   ├── Deployment verification
│   └── Rollback procedures
│
├── INFRASTRUCTURE_SETUP.md
│   ├── Cloud provider selection (AWS, GCP, Azure)
│   ├── VPC & networking setup
│   ├── Database provisioning
│   ├── Cache/Redis setup
│   ├── Storage configuration
│   ├── DNS setup
│   └── CDN configuration
│
├── KUBERNETES_CONFIGURATION.md
│   ├── Cluster setup
│   ├── Namespace configuration
│   ├── Deployment manifests
│   ├── StatefulSets for databases
│   ├── DaemonSets for logging
│   ├── ConfigMaps examples
│   ├── Secrets management
│   ├── Resource limits & requests
│   ├── Auto-scaling policies
│   └── Network policies
│
├── MONITORING_AND_LOGGING.md
│   ├── Prometheus metrics setup
│   ├── Grafana dashboard examples
│   ├── ELK stack configuration (Elasticsearch, Logstash, Kibana)
│   ├── Alerting rules
│   ├── Custom metrics instrumentation
│   ├── Log aggregation
│   ├── Performance monitoring
│   └── Incident response runbooks
│
└── DISASTER_RECOVERY.md
    ├── Backup strategy
    ├── Backup verification
    ├── Recovery procedures
    ├── Data retention policy
    ├── RTO & RPO targets
    ├── Failover procedures
    ├── High availability setup
    └── Disaster recovery drills
```

---

## REFERENCE_MATERIALS (Standards & Best Practices)

```
REFERENCE_MATERIALS/
│
├── PYTHON_STANDARDS.md
│   ├── PEP 8 style guide
│   ├── Type hints (mypy)
│   ├── FastAPI patterns
│   ├── SQLAlchemy patterns
│   ├── Testing (pytest)
│   ├── Logging configuration
│   └── Common pitfalls
│
├── TYPESCRIPT_STANDARDS.md
│   ├── Strict mode requirements
│   ├── SvelteKit patterns
│   ├── Component best practices
│   ├── State management
│   ├── Type safety in API calls
│   ├── Testing (Vitest)
│   └── Common pitfalls
│
├── SQL_BEST_PRACTICES.md
│   ├── Query optimization
│   ├── Indexing strategy
│   ├── N+1 query prevention
│   ├── Transaction patterns
│   ├── Migration procedures
│   ├── Schema design
│   └── Performance monitoring
│
├── REST_API_STANDARDS.md
│   ├── Endpoint naming conventions
│   ├── HTTP method usage
│   ├── Status code usage
│   ├── Error response format
│   ├── Pagination patterns
│   ├── Filtering & sorting
│   ├── API versioning
│   └── Rate limiting
│
├── TESTING_BEST_PRACTICES.md
│   ├── Test pyramid strategy
│   ├── Mocking & fixtures
│   ├── Test data management
│   ├── Parametrized tests
│   ├── Integration test patterns
│   ├── E2E test approach
│   ├── Test organization
│   └── Coverage measurement
│
├── GIT_WORKFLOW.md
│   ├── Branch naming conventions
│   ├── Commit message style
│   ├── Interactive rebasing
│   ├── Merge vs rebase strategy
│   ├── Pull request process
│   ├── Code review expectations
│   ├── Handling merge conflicts
│   └── Undoing mistakes safely
│
├── DOCUMENTATION_STANDARDS.md
│   ├── Writing style guidelines
│   ├── Markdown formatting
│   ├── Code block examples
│   ├── Cross-references
│   ├── Table of contents
│   ├── Diagram conventions
│   ├── Update procedures
│   └── Deprecation notices
│
├── LICENSE_AND_ATTRIBUTION.md
│   ├── Project license (choice of license)
│   ├── Third-party attributions
│   ├── Open source dependencies
│   ├── Fair use & citations
│   └── Contributor attribution
│
└── ISSUE_REPORTING.md
    ├── Bug report template
    ├── Feature request template
    ├── Documentation issue template
    ├── Triage process
    ├── Priority assessment
    └── Resolution tracking
```

---

## ARCHIVE (Historical Documents)

```
ARCHIVE/
│
├── superseded-documentation/
│   └── [Old versions of documents]
│
├── exploration-notes/
│   └── [Research & investigation notes]
│
├── deprecated-features/
│   └── [Documentation for removed features]
│
└── historical-records/
    └── [Historical context & decisions]
```

---

## Learning Paths (Recommended Reading Order)

### Path 1: Learning Computation History
```
GETTING_STARTED/README.md
  ↓
CURRICULUM_AND_CONTENT/00_PLATFORM_OVERVIEW.md
  ↓
CURRICULUM_AND_CONTENT/module-0-prehistory/README.md
  ↓
CURRICULUM_AND_CONTENT/module-1-ancient/README.md
  ↓
[Continue through modules 2-7]
  ↓
CURRICULUM_AND_CONTENT/synthesis-a-*/README.md
```

### Path 2: Development Setup & Contribution
```
GETTING_STARTED/QUICK_START_5_MINUTES.md
  ↓
DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md
  ↓
ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md
  ↓
DEVELOPMENT_GUIDES/02_CODE_STANDARDS_AND_STYLE.md
  ↓
DEVELOPMENT_GUIDES/03_BUILD_SYSTEM_AND_TOOLS.md
  ↓
DEVELOPMENT_GUIDES/06_CONTRIBUTION_GUIDELINES.md
```

### Path 3: Manufacturing Babbage Engine
```
GETTING_STARTED/QUICK_START_5_MINUTES.md
  ↓
BABBAGE_ENGINE_SPECIFICATION/00_QUICK_FACTS_AND_OVERVIEW.md
  ↓
BABBAGE_ENGINE_SPECIFICATION/01_COMPLETE_TECHNICAL_SPECIFICATION.md
  ↓
BABBAGE_ENGINE_SPECIFICATION/04_REGIONAL_IMPLEMENTATIONS.md
  ↓
BABBAGE_ENGINE_SPECIFICATION/02_MANUFACTURING_PROCEDURES.md
  ↓
IMPLEMENTATION_PHASES/phase0/
  ↓
[Continue through phases 1-4]
```

### Path 4: Project Management & Stakeholder View
```
GETTING_STARTED/README.md
  ↓
ARCHITECTURE_AND_DESIGN/PROJECT_SUMMARY_FOR_STAKEHOLDERS.md
  ↓
ARCHITECTURE_AND_DESIGN/IMPLEMENTATION_ROADMAP.md
  ↓
ARCHITECTURE_AND_DESIGN/WEEK_1_COMPLETION_STATUS.md
  ↓
ARCHITECTURE_AND_DESIGN/RISK_MANAGEMENT.md
  ↓
ARCHITECTURE_AND_DESIGN/STRATEGIC_ROADMAP.md
```

---

## Cross-Reference Map

**Architecture Documentation** ↔ **Implementation Phases**
- ARCHITECTURE.md references phase-specific details
- Each phase document references relevant architecture sections

**Curriculum** ↔ **Code Examples**
- Each lesson links to related code examples
- Code examples reference lesson materials

**Development Guides** ↔ **Reference Materials**
- Guides link to reference standards
- Standards provide detailed specifications

**Babbage Specification** ↔ **Implementation Phases**
- Phase 0 = design review of specification
- Phases 1-4 = step-by-step implementation
- Each phase validates specification sections

---

**Last Updated**: October 31, 2025  
**Structure**: 9 main sections, 64 documents, multiple learning paths
