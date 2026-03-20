# ADR 0004: Simulation Engine Three-Module Split

## Status

Accepted (2026-03-19)

## Context

The physics simulation (Phase VIII) couples structural, tribological, and
thermodynamic models into a time-stepping engine. A single monolithic
module would mix state management, stepping logic, and physics coupling,
making it difficult to test or extend independently.

Design options:
- **Single module**: Simple but untestable in isolation; state leaks
  between concerns.
- **Two modules (state + engine)**: Cleaner but physics coupling logic
  is entangled with stepping.
- **Three modules**: state.py (snapshot/restore), engine.py (stepping),
  coupling.py (physics integration). Each module has a single responsibility
  and can be tested independently.

## Decision

The simulation lives in `backend/src/emulator/simulation/` with three
modules:

- **state.py**: `SimulationState` dataclass with snapshot() and restore()
  for deterministic replay and debugging.
- **engine.py**: `SimulationEngine.step()` orchestrates the update order:
  shaft angle -> friction -> heat -> temperature -> viscosity ->
  lubrication -> wear -> clearance -> load redistribution -> deflection ->
  failure check.
- **coupling.py**: Bidirectional coupling functions between physics domains
  (e.g., temperature affects viscosity, wear affects clearance, clearance
  affects load distribution).

## Consequences

- Each module can be unit-tested with mock state/coupling.
- The step order in engine.py is explicit and auditable.
- Adding a new physics domain requires: (1) state fields in state.py,
  (2) coupling functions in coupling.py, (3) step insertion in engine.py.
- Snapshot/restore enables deterministic debugging of divergent simulations.
