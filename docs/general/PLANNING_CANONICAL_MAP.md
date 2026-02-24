# Planning Canonical Map

**Date**: 2026-02-24  
**Status**: Active  
**Purpose**: Deterministic classification for roadmap/plan/tracker documentation and archive decisions.

## Canonical Planning Sources

These two files are the default planning sources for project-wide execution:

1. `docs/general/MASTER_ROADMAP.md` (strategy and phase-level status)
2. `docs/general/TODO_TRACKER.md` (execution order and task tracking)

Supporting canonical context:

1. `docs/general/ROADMAP_RECONCILIATION_JAN_2026.md` (audit and reconciliation baseline)

## Planning Taxonomy Snapshot

### Active General Planning Docs (6)

1. `docs/general/MASTER_ROADMAP.md`
2. `docs/general/TODO_TRACKER.md`
3. `docs/general/ROADMAP_RECONCILIATION_JAN_2026.md`
4. `docs/general/HYPERGRANULAR_ROADMAP.md`
5. `docs/general/DOCUMENTATION_SYNTHESIS_AND_REORGANIZATION_PLAN.md`
6. `docs/general/LOGIC_COMPUTATION_HISTORIAN_PLAN.md`

### Active Domain Planning Docs (6)

1. `docs/history/HISTORIAN_ROADMAP.md`
2. `docs/history/MEASUREMENT_ROADMAP.md`
3. `docs/history/DOCUMENTATION_ARCHITECT_EXECUTION_PLAN.md`
4. `docs/minix/MINIX_ROADMAP.md`
5. `docs/simulation/EXTRACTION_PLAN.md`
6. `docs/sources/IMPLEMENTATION_PLAN.md`

### Archived Planning Docs (14)

See `docs/archive/INDEX.md` for full reason classes and successor mappings.

## Supersedence Reason Model

Each archived planning document is assigned one primary reason class:

1. `superseded_by_newer_canonical`: newer active docs capture and maintain the same intent.
2. `phase_completed_snapshot`: phase/week execution snapshot retained as historical evidence.
3. `migration_or_audit_artifact`: migration/link/audit logs preserved for provenance.
4. `obsolete_process_or_tooling`: process no longer represents current operating model.
5. `historical_context_only`: retained for research context without active execution value.

## Why Plans Become Superseded

Recurring supersedence causes observed in this repository:

1. Reality changed faster than plan assumptions (for example Jan 2026 reconciliation against code/tests).
2. Phase plans completed and transitioned from execution docs to historical evidence.
3. Parallel planning docs diverged and required a single canonical source.
4. Repository restructuring produced transient migration plans and link-update playbooks.
5. Tooling and architecture evolved, making prior process docs obsolete as runbooks.

## Integrated Insights From Archived Planning Docs

Archived docs are not discarded. Novel content has been incorporated into active planning guidance:

1. Gate-based phase milestones and sequencing patterns integrated into `MASTER_ROADMAP.md`.
2. Day-by-day execution granularity and acceptance criteria integrated into `TODO_TRACKER.md`.
3. Documentation migration verification checklists and cross-reference repair patterns integrated into this file and `docs/archive/INDEX.md`.
4. Phase 4 educational/timeline delivery decomposition integrated into `HYPERGRANULAR_ROADMAP.md`.

## Maintenance Rules

When archiving a planning document:

1. Add or update `Archive Metadata` block in the archived file.
2. Add entry to `docs/archive/INDEX.md` with reason class, successor, and merge target.
3. Ensure canonical docs contain any novel constraints, decisions, or checks first.
4. Rewrite stale links to canonical successors where practical.
5. Preserve artifact history; do not delete archival evidence unless duplicate and content-identical.
