# ADR 0003: Historical Machines Placement in backend/src/emulator/

## Status

Accepted (2026-03-19)

## Context

The Babbage Engine Buildout (Phase 5) added four historical machine
emulators: Scheutz Difference Engine, Ludgate Analytical Machine,
Torres Quevedo Arithmometer, and Zuse Z1.

Placement options:
- **backend/src/emulator/**: Co-located with the Analytical Engine emulator.
  Shares the MachineAdapter ABC, execution model, and test infrastructure.
- **hardware_twin/**: Already used for physical DE2 hardware modeling
  (de_tabulator.py, printer_formatter.py, golden traces).
- **New top-level package**: Would require separate test discovery, import
  paths, and adapter wiring.

## Decision

Historical machines live in `backend/src/emulator/` because they share
the software execution model (step/run/reset) and are wrapped by
MachineAdapter subclasses in `adapter.py`. The `hardware_twin/` directory
models the physical DE2 hardware (dimensions, BOM, cam timing), which is
a different concern.

## Consequences

- All machine adapters (ScheutzAdapter, LudgateAdapter, TorresQuevedoAdapter,
  ZuseZ1Adapter) use `.state.*` attributes and a common interface.
- Torres and Zuse adapters use `.to_float()` for decimal conversion.
- Tests for all machines run under `backend/tests/unit/` with shared fixtures.
- Future machines (e.g., Leibniz Stepped Drum) should follow this pattern.
