# Repository Guidelines

## Project Structure & Navigation
- `backend/` contains the FastAPI app in `src/` (config, database, API routers, services) plus Alembic migrations and pytest suite. New domains should mirror the existing `src/<domain>/` pattern and document cross-service changes in `ARCHITECTURE_AND_DESIGN/LANGUAGE_SERVICES_ARCHITECTURE.md`.
- `frontend/` hosts the SvelteKit client; compose UI under `src/lib/components`, routes in `src/routes`, and keep Vitest specs in `frontend/tests/`. Shared state belongs in `src/lib/stores` (create the directory when adding stores to match the planned layout).
- `services/` provides per-language sandboxes (C, Python, Haskell, etc.) that align with the architecture diagram in `ARCHITECTURE_AND_DESIGN/PROJECT_STRUCTURE.md`. Extend each runtime folder with Dockerfiles, entrypoints, and smoke tests.
- `content/modules/` and `content/synthesis/` store instructional material that maps to the curriculum outlined in `ARCHITECTURE_AND_DESIGN/IMPLEMENTATION_ROADMAP.md`. Track any new taxonomy or metadata fields in `CURRICULUM_AND_CONTENT/CONTENT_SCHEMA_DESIGN.md`.
- Operational assets live under `scripts/` (automation), `config/` (environment templates), and `tools/`. Use `ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md` as the source of truth for service boundaries before moving or renaming modules.

## Build, Test & Development Commands
```bash
make setup        # Install backend (pip) + frontend (pnpm) deps and pre-commit
make dev          # Compose stack: FastAPI :8000, SvelteKit :3000, Postgres, Redis
make dev-backend  # Uvicorn live-reload from backend/src/main.py
make dev-frontend # Vite dev server with HMR
make test         # Runs backend pytest then frontend vitest
make lint         # Backend pylint + mypy, frontend eslint
make build        # Bazel backend targets + Svelte production build
make docker-up    # Detached containers for local integration testing
```
Lean on `bazel build //backend:all` and `bazel test //...` for targeted polyglot builds, and prefer Make targets so pre-commit stays consistent.

## Coding Style & Naming Conventions
- Python adheres to Black (100 cols) and strict mypy; keep modules `snake_case`, classes `PascalCase`, functions `snake_case`, and run `make format` (Black + isort) before committing.
- TypeScript/Svelte follows Prettier (tabs, single quotes, 100 cols). Components use `PascalCase.svelte`, stores/utilities `camelCase.ts`, and API clients sit under `frontend/src/lib/api/`.
- Bazel, Make, and script targets stay lowercase with hyphen separators (`build-backend`, `test-frontend`).

## Testing Guidelines
- Backend unit tests live in `backend/tests/` (mirroring package layout); add integration suites under `backend/tests/integration` as they come online per `ARCHITECTURE_AND_DESIGN/PROJECT_STRUCTURE.md`. Use `make test-backend` and `make test-coverage` before PR handoff.
- Frontend Vitest specs belong in `frontend/tests/` or alongside components when snapshots help readability. Use `pnpm test --runInBand` for flaky debugging.
- Curriculum data fixtures ride with the relevant module folders; note any generated data sources in PR descriptions so content reviewers can validate historical timelines.

## Commit & Pull Request Guidelines
- Follow the narrative convention from `git log` (`Week N Day M: <scope>`). Summaries should state the sprint context first, then the functional change.
- Branch naming: `feature/<slug>`, `fix/<slug>`, `docs/<slug>`, `chore/<slug>`; avoid direct pushes to `main`.
- Every PR should provide: change overview, verification commands (`make test`, `make lint`, etc.), relevant doc links (roadmap, architecture), and UI screenshots/GIFs when visual layers change. Request reviewers from both backend and frontend when touching shared contracts or API DTOs.

## Roadmap Awareness for Agents
- `ARCHITECTURE_AND_DESIGN/IMPLEMENTATION_ROADMAP.md` and `DEVELOPMENT_GUIDES/WEEK_2_IMPLEMENTATION_PLAN.md` outline upcoming milestones: completing all eight language executors, layering gVisor/seccomp security, expanding content schemas, and launching the interactive timeline. Reference these when prioritizing tasks or proposing scope changes.
- Escalate deviations that impact the planned sequence (Language Services -> Content CMS -> Interactive Components) so project leads can adjust timelines or staffing.

## Agent Workflow Tips
- Use Make targets or `pre-commit run --all-files` to keep formatting and linting aligned with hooks defined in `.pre-commit-config.yaml`.
- Consult `CLAUDE.md` for multi-agent coordination norms and `DEVELOPMENT_GUIDES/SECURITY_LAYERS_IMPLEMENTATION.md` before modifying sandbox boundaries.
- Document exploratory scripts under `tools/` or `scripts/` with a cleanup plan; flag security-sensitive changes early to the team.
