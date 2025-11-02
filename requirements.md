Document Version: 1.1
Last Updated: 2025-11-02
Status: Comprehensive Requirements Specification (All Phases)
Consolidates: Backend, frontend, services, docs, infrastructure requirements

Module Requirements

Authoritative pointers and compatibility notes for each module.

Backend: backend/requirements.txt (FastAPI, SQLAlchemy, Redis, testing)

Python: 3.11–3.12 verified. Python 3.13 currently incompatible with pinned sqlalchemy==2.0.23.

Action: Upgrade to sqlalchemy>=2.0.35,<2.1 and align Alembic for Python 3.13 support.

Docs: backend/requirements.md

Frontend: frontend/requirements.md (Node/Pnpm/Vite/Svelte)

Services: services/requirements.md (Docker, per-language containers)

Docs tooling: docs/requirements.md (LaTeX, TikZ, arXiv build)

Project Scope

Frontend: SvelteKit webapp with D3.js visualizations and Monaco code editor

Backend: FastAPI service orchestration with async/await patterns

Language Services: Sandboxed Docker containers for 7+ languages (C, Python, Haskell, IDRIS2, LISP, System F, Java, Assembly)

Compiler Pipeline: Universal IR targeting Babbage ISA

Emulator: Babbage ISA virtual machine

Documentation: LaTeX curriculum with TikZ diagrams and pgfplots visualizations

Build System: Bazel for hermetic, reproducible builds

Babbage Engine: Complete engineering specifications for historical (1910-1970) and modern (2025) manufacturing

Current Status (November 2, 2025)

Phase 1: ✓ COMPLETE (Foundation, 7,070 LOC)

Phase 2: 85% (3 of 7 language services, 10,270 LOC)

Phase 3: DESIGNED (Emulator & Tools, 6,000-8,000 LOC)

Phase 4: 80% (Frontend visualization, 6,000 LOC)

Total Codebase: ~28,000 lines

Test Coverage: 500+ tests, 100% pass rate, >90% coverage

Tooling & Validation

Repo validation: scripts/validate_repo.sh

TODO audit: tools/todo_report.py

Minix metrics orchestration: scripts/minix_metrics.sh

Minix metrics merge: scripts/minix_merge_metrics.py

Minix Metrics Tooling (host requirements)

Docker with access to /dev/kvm for KVM acceleration (optional; falls back to TCG)

QEMU and Python are in the container; host only needs Docker

ISO path provided via --iso (bind-mounted read-only)

Reproducibility

Treat warnings as errors in all steps.

Prefer pinned versions. When upgrading for compatibility, document rationale and testing evidence in the corresponding requirements.md.