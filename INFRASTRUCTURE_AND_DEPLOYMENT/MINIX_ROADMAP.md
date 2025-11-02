# MINIX Metrics Roadmap — QEMU in Docker

Status: Phase 0–1 implemented; Phase 2–4 scoped.

## Phases

- Phase 0 — Wire-in Existing Implementation [DONE]
  - Orchestrator script calls minix-analysis image with ISO + metrics bind.
  - Artifact contract and docs updated.

- Phase 1 — Metrics Enrichment [DONE]
  - CPU/RSS/IO timeseries (per-second deltas) via /proc.
  - Boot markers via boot-profiler; canonical boot_time.json merger.
  - Iterations + run history + summary aggregator.

- Phase 2 — CI Integration [IN PROGRESS]
  - Self-hosted workflow with inputs: iso_path, arch, iterations, baseline_ms.
  - Regression gate (scripts/minix_regression_check.py) — optional step.
  - Next: add baseline JSON artifact and auto-compare averages from summary.json.

- Phase 3 — API/Frontend [IN PROGRESS]
  - Backend endpoints to serve latest metrics and run history.
  - Frontend route `/infra/minix` to visualize current metrics and runs.
  - Next: add charts for CSV timeseries (client-side D3 or Chart.js) and trend over runs.

- Phase 4 — ARM Variant + Docs [PENDING]
  - Validate arm flow using Dockerfile.arm (requires ARM ISO and runner support).
  - Document env flags and performance differences.

## Open Items and Next Steps

1. CI Baseline Handling
   - Add job step to persist a baseline JSON per arch; compare new average against baseline with tolerance.
2. Frontend Visualization
   - Render resource_timeseries.csv as live charts; add toggle for CPU/RSS/IO.
   - Add run selector to preview any run’s metrics.
3. Security & Hardening
   - Add ShellCheck lint CI for scripts; mypy/ruff for Python tooling.
   - Ensure no network from guest; restrict Docker caps to minimal (currently only KVM device).
4. ARM Testing
   - Acquire/confirm ARM ISO and expected boot flow; update scripts.

## Usage Cheatsheet

- Single run (i386)
  - `./scripts/minix_metrics.sh --iso /path/to/minix_R3.4.0rc6.iso`
- Multiple iterations, labeled
  - `./scripts/minix_metrics.sh --iso /path/to/iso --iterations 5 --label nightly`
- Regression gate
  - `python3 scripts/minix_regression_check.py --metrics metrics/minix/i386/boot_time.json --baseline-ms 65000 --tolerance 20`

## References
- Orchestrator: `scripts/minix_metrics.sh`
- Merge: `scripts/minix_merge_metrics.py`
- Aggregate: `scripts/minix_aggregate.py`
- Regression: `scripts/minix_regression_check.py`
- Docs: `INFRASTRUCTURE_AND_DEPLOYMENT/MINIX_QEMU_DOCKER_METRICS.md`

