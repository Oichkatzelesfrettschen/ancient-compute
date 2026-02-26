# Bill of Materials CSV Schema

**Version**: 0.1
**Date**: 2026-02-26
**Charter ref**: S-6 (Software twin)

---

## Schema

| Column | Type | Required | Description |
|--------|------|----------|-------------|
| part_id | string | yes | Unique part identifier (e.g., "SHAFT-001") |
| subsystem | string | yes | Subsystem name (e.g., "Main shaft", "Store") |
| component | string | yes | Component within subsystem |
| material | string | yes | Material key from MaterialLibrary |
| quantity | int | yes | Number of this part |
| mass_kg | float | yes | Mass per unit (kg) |
| spec_ref | string | yes | SOURCE:<id> or "ASSUMPTION" |
| process | string | no | Manufacturing process (e.g., "turn", "mill", "cast") |
| inspection | string | no | Inspection method (e.g., "dial_indicator", "optical") |
| acceptance | string | no | Acceptance criterion (e.g., "runout < 0.05mm") |
| notes | string | no | Additional notes |

## Validation Rules

1. Every row must have a non-empty `part_id`, `subsystem`, `material`, and `spec_ref`.
2. `spec_ref` must be either `SOURCE:<valid_id>` or `ASSUMPTION`.
3. `quantity` must be a positive integer.
4. `mass_kg` must be non-negative.
5. `material` must match a key in MaterialLibrary or be documented.
6. No duplicate `part_id` values.

## Example

```csv
part_id,subsystem,component,material,quantity,mass_kg,spec_ref,process,inspection,acceptance,notes
SHAFT-001,Power train,Main shaft,steel,1,12.5,SOURCE:SWADE-2001,turn,dial_indicator,runout < 0.05mm,
BEAR-001,Power train,Journal bearing,bronze,4,0.8,ASSUMPTION,bore,micrometer,clearance 0.02-0.05mm,
```

## File Location

- Seed BOM: `hardware_twin/bom/bom_v0.csv`
- Validation: `tools/validate_bom.py`
- Make target: `make bom-validate`
