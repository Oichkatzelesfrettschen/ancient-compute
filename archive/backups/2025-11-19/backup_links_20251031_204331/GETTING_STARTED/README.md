# Ancient Compute: Getting Started

Welcome to **Ancient Compute**, a comprehensive educational platform tracing 12,500 years of computational history, logic, and programming paradigms—from prehistoric tally marks through quantum computing.

This is also the home of the **Babbage Analytical Engine Specification**, a complete engineering blueprint for mechanically manufacturing a fully-functional Analytical Engine using 1930s-1960s technology.

---

## I'm New Here—Where Do I Start?

### Quick Orientation (5 minutes)
- **First time?** → Read [QUICK_START_5_MINUTES.md](QUICK_START_5_MINUTES.md)
- **Need a specific document?** → Use [DOCUMENT_FINDER.md](DOCUMENT_FINDER.md) to search by topic
- **Visual learner?** → Start with the [Project Structure Overview](../ARCHITECTURE_AND_DESIGN/PROJECT_STRUCTURE.md)

### What Are You Here For?

**Learning the History of Computation**
- Start: [Ancient Compute Platform Overview](../CURRICULUM_AND_CONTENT/00_PLATFORM_OVERVIEW.md)
- Path: Prehistory → Ancient Foundations → Medieval → Early Modern → Foundations Crisis → Electronic Age → Type Theory → Modern Synthesis
- Next: [Module 0: Prehistory of Counting](../CURRICULUM_AND_CONTENT/module-0-prehistory/README.md)

**Building/Developing the Platform**
- Start: [Development Environment Setup](../DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md)
- Read: [Architecture Overview](../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md)
- Deep Dive: [Backend Architecture](../ARCHITECTURE_AND_DESIGN/BACKEND_ARCHITECTURE.md) and [Frontend Architecture](../ARCHITECTURE_AND_DESIGN/FRONTEND_ARCHITECTURE.md)
- Build: [Build System Guide](../DEVELOPMENT_GUIDES/03_BUILD_SYSTEM_AND_TOOLS.md)
- Test: [Testing Strategy](../DEVELOPMENT_GUIDES/04_TESTING_AND_QA.md)

**Implementing/Manufacturing a Babbage Engine**
- Start: [Babbage Engine Quick Facts](../BABBAGE_ENGINE_SPECIFICATION/00_QUICK_FACTS_AND_OVERVIEW.md)
- Read: [Complete Technical Specification](../BABBAGE_ENGINE_SPECIFICATION/01_COMPLETE_TECHNICAL_SPECIFICATION.md)
- Deep Dive: [Manufacturing Procedures](../BABBAGE_ENGINE_SPECIFICATION/02_MANUFACTURING_PROCEDURES.md)
- Verify: [Historical Audit and Corrections](../BABBAGE_ENGINE_SPECIFICATION/03_HISTORICAL_AUDIT_AND_CORRECTIONS.md)
- Phases: [Implementation Phases 0-4](../IMPLEMENTATION_PHASES/)

**Understanding the Roadmap**
- Read: [52-Week Implementation Roadmap](../ARCHITECTURE_AND_DESIGN/IMPLEMENTATION_ROADMAP.md)
- See: [Week 1-2 Completion Status](../ARCHITECTURE_AND_DESIGN/WEEK_1_COMPLETION_STATUS.md)
- Next Steps: [Strategic Roadmap and Next Phases](../ARCHITECTURE_AND_DESIGN/STRATEGIC_ROADMAP.md)

---

## Project at a Glance

### Two Major Components

**1. Ancient Compute Platform**
- Educational platform covering 12,500 years of computational history
- 7 main historical modules + 3 synthesis modules
- 8 supported programming languages (C, Python, Haskell, IDRIS2, Lisp, Assembly, Java, System F)
- Interactive code playgrounds, visualizations, exercises
- **Status**: Week 1-2 foundation complete; Phase 1-4 implementation in progress

**2. Babbage Analytical Engine Specification**
- Complete engineering specification for mechanical computation
- Proven feasible with 1930s-1960s technology
- Covers sourcing, manufacturing, assembly, validation
- 4 regional implementations analyzed (India, Brazil, Argentina, China)
- **Status**: Phase 0-4 complete; ready for implementation

### Technology Stack

**Backend**: FastAPI, PostgreSQL, Redis, Docker, Kubernetes (planned)
**Frontend**: SvelteKit, TypeScript, Monaco Editor, D3.js, Three.js
**Languages**: C, Python, Haskell, IDRIS2, Lisp via isolated Docker services
**Build**: Bazel (cross-platform), Make (development), Docker Compose (local)
**Docs**: LaTeX, TikZ, pgfplots (publication-quality PDFs)

---

## Quick Links by Role

### Students/Learners
- [Quick Start (5 min)](QUICK_START_5_MINUTES.md)
- [Curriculum Overview](../CURRICULUM_AND_CONTENT/00_PLATFORM_OVERVIEW.md)
- [Interactive Lessons](../CURRICULUM_AND_CONTENT/) (organized by historical era)
- [Code Examples](../CURRICULUM_AND_CONTENT/code-examples/)
- [Exercises with Solutions](../CURRICULUM_AND_CONTENT/exercises/)

### Developers/Engineers
- [Setup and Installation](../DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md)
- [Architecture Deep Dive](../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md)
- [Build System Guide](../DEVELOPMENT_GUIDES/03_BUILD_SYSTEM_AND_TOOLS.md)
- [Code Quality Standards](../DEVELOPMENT_GUIDES/02_CODE_STANDARDS_AND_STYLE.md)
- [Testing and QA](../DEVELOPMENT_GUIDES/04_TESTING_AND_QA.md)
- [Deployment Guide](../DEPLOYMENT_AND_DEVOPS/DEPLOYMENT_GUIDE.md)

### Manufacturing Engineers / Babbage Researchers
- [Babbage Overview & Quick Facts](../BABBAGE_ENGINE_SPECIFICATION/00_QUICK_FACTS_AND_OVERVIEW.md)
- [Complete Technical Specification](../BABBAGE_ENGINE_SPECIFICATION/01_COMPLETE_TECHNICAL_SPECIFICATION.md)
- [Manufacturing Procedures](../BABBAGE_ENGINE_SPECIFICATION/02_MANUFACTURING_PROCEDURES.md)
- [Historical Verification](../BABBAGE_ENGINE_SPECIFICATION/03_HISTORICAL_AUDIT_AND_CORRECTIONS.md)
- [Regional Implementation Plans](../BABBAGE_ENGINE_SPECIFICATION/04_REGIONAL_IMPLEMENTATIONS.md)
- [Phase 0-4 Implementation Details](../IMPLEMENTATION_PHASES/)

### Project Managers / Stakeholders
- [Executive Summary](../ARCHITECTURE_AND_DESIGN/PROJECT_SUMMARY_FOR_STAKEHOLDERS.md)
- [52-Week Roadmap](../ARCHITECTURE_AND_DESIGN/IMPLEMENTATION_ROADMAP.md)
- [Week 1-2 Completion Report](../ARCHITECTURE_AND_DESIGN/WEEK_1_COMPLETION_STATUS.md)
- [Risk Management & Mitigation](../ARCHITECTURE_AND_DESIGN/RISK_MANAGEMENT.md)
- [Strategic Roadmap (Next Phases)](../ARCHITECTURE_AND_DESIGN/STRATEGIC_ROADMAP.md)

---

## Directory Structure

```
ancient_compute/
├── GETTING_STARTED/                    # You are here - entry points & navigation
├── ARCHITECTURE_AND_DESIGN/            # Technical specifications & design docs
├── DEVELOPMENT_GUIDES/                 # How-to guides & development practices
├── CURRICULUM_AND_CONTENT/             # Lessons, exercises, historical materials
├── BABBAGE_ENGINE_SPECIFICATION/       # Complete Babbage engine technical docs
├── IMPLEMENTATION_PHASES/              # Phase 0-4 implementation details
├── DEPLOYMENT_AND_DEVOPS/              # Deployment, DevOps, infrastructure
├── REFERENCE_MATERIALS/                # Standards, best practices, references
├── ARCHIVE/                            # Historical documents, deprecated docs
├── frontend/                           # SvelteKit application source
├── backend/                            # FastAPI application source
├── services/                           # Language-specific Docker services
├── docs/                               # LaTeX curriculum & whitepaper
├── whitepaper/                         # Babbage engine whitepaper PDFs
└── [build files & config]              # Makefile, docker-compose, Bazel, etc.
```

---

## Most Important Documents to Read First

1. **[QUICK_START_5_MINUTES.md](QUICK_START_5_MINUTES.md)** - Orientation & first steps
2. **[ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md](../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md)** - System design overview
3. **[BABBAGE_ENGINE_SPECIFICATION/01_COMPLETE_TECHNICAL_SPECIFICATION.md](../BABBAGE_ENGINE_SPECIFICATION/01_COMPLETE_TECHNICAL_SPECIFICATION.md)** - If manufacturing Babbage
4. **[DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md](../DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md)** - If setting up locally
5. **[CURRICULUM_AND_CONTENT/00_PLATFORM_OVERVIEW.md](../CURRICULUM_AND_CONTENT/00_PLATFORM_OVERVIEW.md)** - If learning history of computation

---

## Finding What You Need

### By Topic
- **Computation History**: [CURRICULUM_AND_CONTENT/](../CURRICULUM_AND_CONTENT/)
- **System Architecture**: [ARCHITECTURE_AND_DESIGN/](../ARCHITECTURE_AND_DESIGN/)
- **Development Setup**: [DEVELOPMENT_GUIDES/](../DEVELOPMENT_GUIDES/)
- **Babbage Engine**: [BABBAGE_ENGINE_SPECIFICATION/](../BABBAGE_ENGINE_SPECIFICATION/)
- **Implementation Details**: [IMPLEMENTATION_PHASES/](../IMPLEMENTATION_PHASES/)
- **DevOps/Deployment**: [DEPLOYMENT_AND_DEVOPS/](../DEPLOYMENT_AND_DEVOPS/)

### By Document Type
- **Quick References**: See [DOCUMENT_FINDER.md](DOCUMENT_FINDER.md)
- **Detailed Specifications**: See [REFERENCE_MATERIALS/](../REFERENCE_MATERIALS/)
- **How-To Guides**: See [DEVELOPMENT_GUIDES/](../DEVELOPMENT_GUIDES/)
- **Visual Diagrams**: See individual module README files

### By Role/Audience
- **For Students**: [QUICK_START_5_MINUTES.md](QUICK_START_5_MINUTES.md) → [CURRICULUM_AND_CONTENT/](../CURRICULUM_AND_CONTENT/)
- **For Developers**: [DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md](../DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md)
- **For Engineers**: [BABBAGE_ENGINE_SPECIFICATION/](../BABBAGE_ENGINE_SPECIFICATION/)
- **For Project Managers**: [ARCHITECTURE_AND_DESIGN/PROJECT_SUMMARY_FOR_STAKEHOLDERS.md](../ARCHITECTURE_AND_DESIGN/PROJECT_SUMMARY_FOR_STAKEHOLDERS.md)

---

## Getting Help

- **Can't find a document?** → Use [DOCUMENT_FINDER.md](DOCUMENT_FINDER.md)
- **Questions about development?** → Check [DEVELOPMENT_GUIDES/](../DEVELOPMENT_GUIDES/)
- **Questions about Babbage?** → Check [BABBAGE_ENGINE_SPECIFICATION/](../BABBAGE_ENGINE_SPECIFICATION/)
- **Questions about history?** → Check [CURRICULUM_AND_CONTENT/](../CURRICULUM_AND_CONTENT/)
- **Questions about architecture?** → Check [ARCHITECTURE_AND_DESIGN/](../ARCHITECTURE_AND_DESIGN/)

---

## Community & Contribution

- **Want to contribute?** → See [DEVELOPMENT_GUIDES/06_CONTRIBUTION_GUIDELINES.md](../DEVELOPMENT_GUIDES/06_CONTRIBUTION_GUIDELINES.md)
- **Found an issue?** → Check [REFERENCE_MATERIALS/ISSUE_REPORTING.md](../REFERENCE_MATERIALS/ISSUE_REPORTING.md)
- **Want to propose changes?** → See [DEVELOPMENT_GUIDES/](../DEVELOPMENT_GUIDES/) for workflow

---

## License & Attribution

This project combines:
- **Ancient Compute Platform**: Educational materials spanning 12,500 years of computational history
- **Babbage Analytical Engine Specification**: Engineering specification and manufacturing feasibility analysis

See [REFERENCE_MATERIALS/LICENSE_AND_ATTRIBUTION.md](../REFERENCE_MATERIALS/LICENSE_AND_ATTRIBUTION.md) for full attribution and licensing.

---

**Last Updated**: October 31, 2025  
**Status**: Active Development (Week 1-2 Complete, Phase 1+ In Progress)  
**Next**: See [STRATEGIC_ROADMAP.md](../ARCHITECTURE_AND_DESIGN/STRATEGIC_ROADMAP.md) for upcoming phases.
