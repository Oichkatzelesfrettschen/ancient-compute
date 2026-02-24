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
| _none_ | low | no files exceed threshold | n/a |

## Handling Rules

1. Do not cite quarantined files as canonical implementation guidance.
2. Keep quarantined files for historical provenance only.
3. When practical, repair links and remove from quarantine after re-audit.

## Exit Criteria

A quarantined file can be removed from this list when:

1. Broken internal links are reduced below 5.
2. Replacement pointers remain explicit in the archive metadata.
3. `docs/archive/audit_ledger.csv` and `docs/archive/AUDIT_LEDGER.md` are regenerated.
