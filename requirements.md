# Installation Requirements Index

Authoritative pointers and compatibility notes for each module. Treat warnings as errors in all steps.

- Backend: `backend/requirements.txt` (FastAPI, SQLAlchemy, Redis, testing)
  - Python: 3.11–3.12 verified. Python 3.13 currently incompatible with pinned `sqlalchemy==2.0.23`.
  - Action: Upgrade to `sqlalchemy>=2.0.35,<2.1` and align Alembic for Python 3.13 support.
  - Docs: `backend/requirements.md`

- Frontend: `frontend/requirements.md` (Node/Pnpm/Vite/Svelte)

- Services: `services/requirements.md` (Docker, per-language containers)

- Docs tooling: `docs/requirements.md` (LaTeX, TikZ, arXiv build)

Validation scripts
- Repo validation: `scripts/validate_repo.sh`
- TODO audit: `tools/todo_report.py`
- Minix metrics orchestration: `scripts/minix_metrics.sh`
- Minix metrics merge: `scripts/minix_merge_metrics.py`

Reproducibility
- Prefer pinned versions. When upgrading for compatibility, document rationale and testing evidence in the corresponding `requirements.md`.

Minix Metrics Tooling (host requirements)
- Docker with access to `/dev/kvm` for KVM acceleration (optional; falls back to TCG)
- QEMU and Python are in the container; host only needs Docker
- ISO path provided via `--iso` (bind-mounted read-only)

