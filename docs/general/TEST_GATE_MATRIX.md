# Test Gate Matrix

**Date**: 2026-02-24  
**Status**: Active  
**Purpose**: Define deterministic quality gates while legacy suites are migrated to current contracts.

## Gate Tiers

## Tier A (Required on Every Change)

Command:

```bash
pytest -q \
  backend/tests/unit/test_executors_unit.py \
  backend/tests/unit/test_language_registry.py \
  backend/tests/integration/test_cross_language.py \
  backend/tests/unit/test_babbage_assembler_golden.py \
  backend/tests/unit/test_babbage_parameter_contract.py \
  backend/tests/unit/test_c_freestanding_subset.py \
  backend/tests/integration/test_babbage_abi_contract.py \
  backend/tests/integration/test_babbage_mechanical_profile.py
```

Scope:

1. Execute API and language capability contracts.
2. Simulation parameter integrity and mechanical-profile checks.
3. Validate canonical active docs link integrity:

```bash
make links-check
```

## Tier B (Scheduled Regression)

Command:

```bash
pytest -q backend/tests
```

Scope:

1. Broad backend suites and integration coverage.
2. Includes suites pending interface modernization.

Current known blockers are tracked in `docs/general/TEST_QUARANTINE.md`.

## Tier Q (Quarantined During Migration)

Tier Q suites are intentionally excluded from Tier A until they are migrated to current interfaces and imports.

Policy:

1. Each quarantined suite requires owner + migration notes.
2. Migration must reference target contract and acceptance tests.
3. Remove from quarantine only after passing in CI.

## CI Guidance

Recommended CI wiring:

1. `tier-a` job required for merge.
2. `tier-b` job required on schedule and release branches.
3. `tier-q` job optional/manual until migrations complete.
4. `archive-audit` scheduled job runs:

```bash
make archive-audit
make links-check-full
```
