# Test Quarantine Register

**Date**: 2026-02-24  
**Status**: Active  
**Purpose**: Explicitly track stale test suites that are not aligned with current backend interfaces.

## Quarantine Criteria

A suite belongs in quarantine when at least one condition is true:

1. Imports removed or renamed interfaces.
2. Assumes deprecated API contracts.
3. Fails collection due to known migration gaps unrelated to active delivery gates.

## Current Quarantined Suites

| Suite | Reason | Migration Target |
|---|---|---|
| `backend/tests/integration/test_phase2_languages.py` | Assumes parser/service interfaces that do not match current implementations (`IDRIS2Parser`, service capability shape). | Rewrite against `backend/src/services/languages` capability registry and current compiler modules. |
| `backend/tests/integration/test_user_workflows.py` | Depends on removed `CodeExecutionAPI` class and stale import path conventions (`src...`). | Rebuild workflows against FastAPI routes + execution orchestrator contracts. |
| `backend/tests/unit/test_antikythera_draconic_train.py` | Imports symbols not exported in current antikythera module. | Align tests to actual exported model interfaces or add missing exports with evidence. |
| `backend/tests/unit/test_antikythera_gears.py` | Imports `GearEdge`/`GearTrain` not available in current module API. | Update tests to current types or implement adapter layer. |
| `backend/tests/unit/test_debugger.py` | Expects `ExecutionTrace` export not present in current debugger contract. | Update to current debugger data model and public interfaces. |

## Non-Quarantined But Environment-Sensitive

| Suite | Risk | Action |
|---|---|---|
| `backend/tests/api/test_tools_router.py` | Requires environment with full backend runtime deps (`prometheus_client`). | Keep active after dependency parity validation in CI. |
| `backend/tests/integration/test_phase4_w1_api.py` | Same runtime dependency sensitivity as above. | Keep active after dependency parity validation in CI. |

## Exit Rule

A suite exits quarantine only when:

1. It passes under `pytest -q` without import/collection errors.
2. It is added back to Tier A or Tier B with owner approval.
3. This register is updated in the same change.
