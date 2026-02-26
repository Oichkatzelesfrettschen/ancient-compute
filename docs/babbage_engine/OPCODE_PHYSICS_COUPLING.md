# Opcode-Coupled Physics Simulation

**Date**: 2026-02-26
**Phase**: 5 (Hypergranular Debt Resolution Roadmap)

## Overview

Each mechanical operation executed by the Analytical Engine emulator can now
advance a coupled physics simulation. Temperature, wear, clearances, and
lubrication state evolve realistically as instructions execute. If any
component exceeds its failure limit, execution halts with a
`MechanicalFailureError`.

## Architecture

```
AnalyticalEngine (backend/src/emulator/analytical_engine.py)
    |
    |-- physical_engine: SimulationBridge
    |       |
    |       |-- SimulationEngine (simulation/engine.py)
    |       |       |-- 13-phase physics step cycle
    |       |       |-- Failure limit checking
    |       |
    |       |-- BarrelTimingBridge (timing.py)
    |       |       |-- Maps opcodes to shaft degrees
    |       |       |-- Converts degrees to time via RPM
    |       |
    |       |-- PhysicsSnapshot / physics_report()
    |               |-- Temperature, wear, deflection, energy
    |
    |-- execute_instruction()
            |-- Computes dynamic lag from shaft deflection
            |-- Calls bridge.run(end_time_s) to advance physics
            |-- Checks bridge.failed after each instruction
```

## SimulationBridge

`backend/src/emulator/simulation/bridge.py`

Adapter between the logic-level AnalyticalEngine and the physics-level
SimulationEngine. Implements the interface expected by
`AnalyticalEngine.physical_engine`:

- `config` -- SimulationConfig (RPM, geometry, materials, limits)
- `state` -- SimulationState (temperature, clearances, wear, time)
- `failed` -- bool, checked before each instruction
- `failure_reason` -- string identifying the limiting component
- `run(end_time_s)` -- advance physics to absolute time

Additional methods:

- `opcode_advance(opcode)` -- compute and apply time cost for one opcode
- `snapshot()` -- capture PhysicsSnapshot at instruction boundary
- `physics_report()` -- full summary dict for CLI/API output
- `reset()` -- reset physics state and instruction counters

## Opcode-to-Time Mapping

Mechanical opcodes (ADD, SUB, MULT, DIV, SQRT, LOAD, STOR, SHL, SHR) are
mapped to barrel names via `_OPCODE_TO_BARREL`. The BarrelTimingBridge
computes total shaft degrees for each barrel operation. Time cost is:

```
time_s = degrees / (360 * rpm / 60)
```

Non-mechanical opcodes (JMP, JZ, CMP, etc.) use the TIMING_TABLE cycle
count converted to time:

```
time_s = cycles / (rpm / 60)
```

## Dynamic Lag

The AnalyticalEngine applies a dynamic lag factor based on shaft deflection:

```python
deflection = bridge.state.shaft_deflection_mm
lag_factor = 1.0 + (deflection / 0.1)  # 0.1mm reference
time_cost *= lag_factor
```

As wear accumulates and clearances grow, shaft deflection increases,
slowing effective execution speed -- a physically realistic feedback loop.

## Physics Step Cycle (per dt)

SimulationEngine.step() executes 13 phases per time step:

1. Advance shaft angle and time
2. Friction coefficient from lubrication regime
3. Bearing friction heat
4. Gear mesh heat + eddy current losses
5. Temperature step (Crank-Nicolson)
6. Oil viscosity update
7. Lubrication film thickness and regime
8. Bearing wear increments (Archard model)
9. Sliding distance accumulation
10. Bearing clearance update (thermal + wear)
11. Bearing load redistribution
12. Shaft deflection
13. Failure limit checks

## Failure Modes

- **Temperature** -- exceeds `temperature_limit_C` (default 60 C)
- **Bearing clearance** -- exceeds `clearance_limit_mm` (default 0.15 mm)
- **Bearing seizure** -- clearance drops to 0
- **Gear backlash** -- exceeds `backlash_limit_mm` (default 0.10 mm)

## CLI Usage

```bash
# Run Note G with physics report
python3 tools/babbage_emulator.py deck-runner -n 7 --physics

# Run arbitrary program with physics (future)
python3 tools/babbage_emulator.py program.ae --physics
```

## Test Coverage

`backend/tests/unit/test_opcode_physics_coupling.py` -- 13 tests:

- TestBridgeIntegration (4): engine accepts bridge, config, state, not failed
- TestTemperatureIncrease (2): 100 ADDs raise temp, plain engine reports {}
- TestWearAccumulation (1): MULT takes longer than ADD
- TestMechanicalFailure (1): negligible cooling triggers temperature failure
- TestPhysicsReport (3): report structure, empty without physics, opcode counts
- TestBridgeSnapshot (2): initial snapshot, snapshot updates after execution

## Operational Envelope (Note G, 5 Bernoulli numbers)

At 30 RPM nominal, computing B_1 through B_9:

| Metric | Value |
|--------|-------|
| Simulated time | ~15 s |
| Temperature | ~20 C (well within 60 C limit) |
| Shaft deflection | ~0.069 mm |
| Max bearing wear | ~9.8e-3 mm3 |
| Gear backlash | 0.020 mm (unchanged) |
| Lubrication regime | boundary |
| Energy consumed | ~556 J |
| Failed | No |

The engine operates well within its thermal envelope for short programs.
Extended runs (thousands of operations) would show progressive degradation.
