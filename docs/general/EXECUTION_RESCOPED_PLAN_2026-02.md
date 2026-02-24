# Execution Rescoped Plan (February 2026)

**Date**: 2026-02-24  
**Status**: Active  
**Purpose**: Single execution ledger for simulation-first delivery, compiler bring-up, and full archive harmonization.

## Baseline

- Baseline commit: `ab580da`
- Canonical strategic roadmap: `docs/general/MASTER_ROADMAP.md`
- Canonical execution tracker: `docs/general/TODO_TRACKER.md`
- Hardware and language bring-up sequence: `docs/general/HARDWARE_LANGUAGE_BRINGUP_PLAN.md`

## Delivery Waves

## Wave 1: Babbage Full-Fidelity Simulation (Hard Gate)

Goal: achieve source-cited, mechanically parameterized Babbage simulation baseline before compiler/runtime expansion.

Tasks:

1. Replace unresolved simulation placeholders for Babbage-critical mechanics.
2. Add deterministic verifier for required Babbage parameters and citations.
3. Add tests for parameter integrity and profile ranges.
4. Publish evidence in `docs/simulation/CITATIONS.md` and `docs/simulation/GAP_REPORT.md`.

Completion criteria:

1. Required Babbage fields are populated (no `TBD`) and validated.
2. Parameter citations are present and non-placeholder.
3. New simulation tests pass.

## Wave 2: Compiler Bring-up (Assembly -> ABI -> C Subset)

Goal: execute gates from `HARDWARE_LANGUAGE_BRINGUP_PLAN.md` with verification-driven milestones.

Tasks:

1. Gate 0: assembler golden corpus and deterministic output checks.
2. Gate 1: ABI/runtime contract specification + conformance tests.
3. Gate 2: freestanding C subset profile + unsupported-feature diagnostics.
4. Gate 3 prep: keep stub languages as stubs until runtime evidence exists.

Progress (2026-02-24):

1. Gate 0 corpus tests are implemented in Tier A.
2. Gate 1 ABI contract doc and conformance tests are implemented in Tier A.
3. Gate 2 subset profile and explicit unsupported-feature diagnostics are implemented; compile/execute examples remain open.

Completion criteria:

1. Gate 0-2 have tests and acceptance evidence.
2. Language status metadata remains synchronized with implementation truth.

## Wave 3: Full Archive Repair and Documentation Harmonization

Goal: fully harmonize archive link health and canonical references with deterministic tooling.

Tasks:

1. Repair link-validator scope and path-resolution semantics.
2. Regenerate `docs/archive/audit_ledger.csv` and `docs/archive/AUDIT_LEDGER.md`.
3. Recompute quarantine set in `docs/archive/QUARANTINE.md`.
4. Update canonical general docs to reference repaired archive state.

Completion criteria:

1. Archive ledger and quarantine reflect current tree state.
2. No unresolved high-risk archive link clusters remain without explicit waiver.

## Active Gate Policy

The repository uses active-contract test gating:

1. Tier A: fast required tests (per `docs/general/TEST_GATE_MATRIX.md`).
2. Tier B: broader scheduled regression.
3. Tier Q: quarantined legacy suites pending migration (`docs/general/TEST_QUARANTINE.md`).

## Change Control

Rules for this rescope:

1. Do not promote a language to implemented without runtime evidence.
2. Do not close simulation gaps without source citation or explicit provisional marker.
3. Keep archive changes reversible and provenance-preserving.
4. Every completed task must leave a verifiable artifact path.
