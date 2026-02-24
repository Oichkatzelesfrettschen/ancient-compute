# Archive Quarantine

Date: 2026-02-24
Status: Active
Purpose: Identify archived files with degraded internal link integrity so they are not used as build/planning design inputs.

## Criteria

A file is quarantined when either condition is true:

1. `broken_link_count >= 5` in `docs/archive/audit_ledger.csv`.
2. Path patterns are malformed enough to mislead navigation during active implementation work.

## Current Quarantine Set

| File | Risk | Why Quarantined | Use Instead |
|---|---|---|---|
| `docs/archive/WEEK_3_TASK_3_LINK_MAPPING_AND_UPDATE_PLAN.md` | high | 27 broken internal links and malformed path examples from migration-time rewrites | `docs/general/PLANNING_CANONICAL_MAP.md`, `docs/archive/INDEX.md` |
| `docs/archive/STRATEGIC_ROADMAP.md` | medium | 8 stale links to pre-restructure doc paths | `docs/general/MASTER_ROADMAP.md`, `docs/general/TODO_TRACKER.md` |

## Handling Rules

1. Do not cite quarantined files as canonical implementation guidance.
2. Keep quarantined files for historical provenance only.
3. When practical, repair links and remove from quarantine after re-audit.

## Exit Criteria

A quarantined file can be removed from this list when:

1. Broken internal links are reduced below 5.
2. Replacement pointers remain explicit in the archive metadata.
3. `docs/archive/audit_ledger.csv` and `docs/archive/AUDIT_LEDGER.md` are regenerated.
