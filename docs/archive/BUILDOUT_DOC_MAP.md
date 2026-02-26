# Build-Out Documentation Map

Date: 2026-02-24
Status: Active
Purpose: Direct implementation work to active docs while preserving archive provenance.

## Canonical By Work Type

| Work Type | Canonical Docs | Notes |
|---|---|---|
| Project planning and sequencing | `docs/general/MASTER_ROADMAP.md`, `docs/general/TODO_TRACKER.md` | Primary source for scope, priorities, and execution order. |
| Planning policy and supersedence rules | `docs/general/PLANNING_CANONICAL_MAP.md`, `docs/archive/INDEX.md` | Explains why older plans are archived and where their novel content moved. |
| Architecture and technical design | `docs/general/ARCHITECTURE.md`, `docs/specifications/` | Use for current design decisions and service architecture. |
| Babbage engine implementation/spec details | `docs/babbage_engine/BABBAGE_MASTER_REFERENCE.md`, `docs/babbage_engine/BABBAGE_IR_SPECIFICATION.md`, `docs/babbage_engine/BABBAGE_ISA_EMULATOR_ARCHITECTURE.md` | Canonical subsystem references for compiler and emulator work. |
| Simulation and model behavior | `docs/simulation/EMULATOR_INTERFACE.md`, `docs/simulation/EMULATOR_COVERAGE_MATRIX.md`, `docs/simulation/specs/` | Current simulation contracts and coverage. |
| Build, deployment, and troubleshooting | `docs/general/BUILD.md`, `docs/general/DEPLOYMENT_GUIDE.md`, `docs/general/TROUBLESHOOTING_GUIDE.md` | Operational runbooks for code build-out and runtime support. |

## Archive Audit Snapshot

Derived from `docs/archive/AUDIT_LEDGER.md` and `docs/archive/audit_ledger.csv`.
Class totals use deterministic filename heuristics plus link-health checks; treat them as triage-first, then refine manually when needed.

- Legacy archive files audited: 97
- `functional_planning`: 23
- `functional_design_code`: 15
- `historical_only`: 59
- Files with archive metadata blocks: 24
- Files referenced by active docs: 4
- Files with broken internal links: 6
- Quarantine set size: 2 (`docs/archive/QUARANTINE.md`)

## Planning Inventory Counts

Filename-based planning scan (`ROADMAP|PLAN|PLANNING|TODO|TRACKER`):

- Active docs: 14
- Archived docs: 13

Interpretation:

1. Active planning is intentionally concentrated in `docs/general` plus domain-specific execution plans.
2. Archive planning files remain as evidence snapshots and migration provenance, not execution authority.

## Practical Usage Rules

1. Start every implementation cycle from `MASTER_ROADMAP` plus `TODO_TRACKER`.
2. Use archive docs only when you need historical rationale or provenance.
3. If an archive file conflicts with active docs, active docs win.
4. Do not use files listed in `docs/archive/QUARANTINE.md` for design or scheduling decisions.

## Next-Step Queue

1. Expand metadata coverage from planning-class docs to `functional_design_code` docs.
2. Repair or normalize links in quarantined files, then re-run the ledger audit.
3. Add a lightweight repo check to prevent new stale links in archive markdown files.
