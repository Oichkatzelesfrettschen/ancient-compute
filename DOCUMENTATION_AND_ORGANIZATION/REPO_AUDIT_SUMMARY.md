# Repository Audit Summary — Phase: Stabilize & Synthesize

Date: 2025-11-02
Owner: Engineering

## Highlights
- Tests: Environment-incompatibility uncovered (Python 3.13 vs SQLAlchemy 2.0.23).
- Quality: Enforced warnings-as-errors in pytest; fixed missing FastAPI import.
- Resilience: Test fixtures no longer hard-crash on incompatible deps.
- Tooling: Added TODO report generator + validation script.
- Infra Metrics: Added MINIX-in-QEMU-in-Docker metrics plan and scripts.
- Requirements: Updated backend dependency pins with Python 3.13 note.

## Changes Implemented
- backend/src/main.py: add `HTTPException` import (readiness path correctness).
- backend/tests/conftest.py: catch broad exceptions to allow optional API tests.
- backend/src/services/query_cache.py: lazy DB imports; module usable without SQLAlchemy.
- backend/src/services/optimized_executor.py: optional SQLAlchemy typing to avoid import failures.
- backend/pytest.ini: treat warnings as errors (`-W error`), remove `--disable-warnings`.
- backend/requirements.txt: bump `sqlalchemy` to 2.0.35, `alembic` to 1.13.2.
- backend/requirements.md: document compatibility and version changes.
- requirements.md: new root index tying module requirement docs.
- tools/todo_report.py: scans repo and writes `TODO_REPORT.md`.
- scripts/validate_repo.sh: runs backend tests, TODO scan, optional lint.
- INFRASTRUCTURE_AND_DEPLOYMENT/MINIX_QEMU_DOCKER_METRICS.md + scripts/minix_metrics.sh: metrics plan.

## Current Test Status
- Backend pytest fails under Python 3.13 due to SQLAlchemy import assertion (prior to bump). After bump, a reinstall is required.
- Action: Recreate backend venv and reinstall (`pip install -r backend/requirements.txt`).

## Next Steps (High-Value)
1. Rebuild backend venv on Python 3.13 and rerun tests.
2. Add GitHub Actions CI matrix (3.11, 3.12, 3.13) with `warnings-as-errors`.
3. Create docs index: `docs/INDEX.md` listing whitepaper and related assets.
4. Consolidate top-level .md reports into `DOCUMENTATION_AND_ORGANIZATION/` with an index (non-destructive symlinks or references first).
5. Add ruff/black pre-commit; enforce in CI (no format drift).
6. Expand `scripts/minix_metrics.sh` to produce CSV/JSON artifacts, include sample runs in CI (self-hosted runner only).

## Risk & Mitigation
- Risk: Broad try/except can mask genuine issues in fixtures.
  - Mitigation: Emit a warning when skipping; add env flag `STRICT_TEST_DEPS=1` to force hard-fail in CI.
- Risk: Bumping DB libs without migration testing.
  - Mitigation: Run alembic upgrade/downgrade in CI on PostgreSQL service.

## Open Questions
- Preferred baseline Python (3.12 vs 3.13) for prod?
- Which subset of backend tests should run without Docker/Redis/PG in CI?

