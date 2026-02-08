# Ancient Compute - Device Logic Audit & Circuit Analysis

**Date**: February 7, 2026
**Version**: 1.0
**Status**: Comprehensive

This document provides a granular logic tree and "circuit diagram" (state transition analysis) for every emulated device in the Ancient Compute ecosystem. It reconciles the physical mechanism with the software implementation.

---

## 1. Enigma Machine (Electro-Mechanical)

### Physical Logic Flow
1.  **Input**: Key Press (A-Z) closes a circuit.
2.  **Stepping**: 
    *   Right rotor advances (+1).
    *   If Right at notch -> Middle advances.
    *   If Middle at notch -> Middle advances (double step) AND Left advances.
3.  **Path**:
    *   `Input` -> `Plugboard` (swap) -> `Entry Wheel`
    *   -> `Rotor R` (Fwd) -> `Rotor M` (Fwd) -> `Rotor L` (Fwd)
    *   -> `Reflector` (In -> Out)
    *   -> `Rotor L` (Rev) -> `Rotor M` (Rev) -> `Rotor R` (Rev)
    *   -> `Plugboard` (swap) -> `Lamp`

### Software Reconciliation (`enigma.py`)
*   **State**: `rotors[]` (position, ring), `plugboard` (map).
*   **Logic**: `step_rotors()` handles the double-stepping anomaly correctly *before* signal processing.
*   **Signal**: `encipher_char()` pipelines the integer transformations (0-25) linearly, mimicking the series circuit.

---

## 2. Curta Calculator (Mechanical)

### Physical Logic Flow
1.  **Input**: Setting sliders position step drum segments.
2.  **Transmission**: 
    *   Main Crank Turn (360°).
    *   Drum teeth engage transmission gears based on slider value.
    *   Transmission gears rotate Result Dial.
3.  **Carry**:
    *   Tens-bell mechanism stores potential carry.
    *   Secondary wave (or simultaneous cascading) executes carry.
4.  **Mode**:
    *   Crank Down: Additive gearing.
    *   Crank Up: Reversing gear engages (complements).

### Software Reconciliation (`curta.py`)
*   **State**: `sliders[]`, `result_dial`, `counter_dial`, `carriage_pos`.
*   **Logic**: `turn_crank()` calculates the delta (`input * 10^pos`) and applies it atomically.
*   **Abstraction**: Mechanical simultaneous transmission is abstracted to integer arithmetic + modulo (for the bell/overflow). The "circuit" here is the gear train ratio.

---

## 3. Difference Engine No. 2 (Mechanical)

### Physical Logic Flow
1.  **Cycle**: 360° Main Shaft Rotation.
2.  **Phases**:
    *   0-90°: Input/Idle.
    *   90-135° (**Add**): Even columns add to Odd columns.
    *   135-180° (**Carry**): Anticipating carriage detects carry chains and updates high-order digits.
    *   180-225° (**Output**): Print/Store.
    *   225-360°: Advance/Reset.

### Software Reconciliation (`machine.py`, `columns.py`)
*   **State**: `ColumnBank` (8 cols x 31 digits).
*   **Logic**: `TimingController` dispatches phase events.
*   **Fidelity**: High. Separates "Addition" from "Carry" logic, respecting the physical constraint that carries cannot happen instantly during addition.

---

## 4. Analytical Engine (Abstract/Mechanical)

### Physical Logic Flow
1.  **Store**: Passive memory (columns of discs).
2.  **Mill**: Active processor (Barrels).
3.  **Control**: Jacquard Cards (Operation, Variable, Number).
4.  **Cycle**:
    *   Read Op Card -> Configure Mill (Add/Sub/Mul).
    *   Read Var Card -> Fetch from Store to Ingress Axis.
    *   Execute Mill -> Result on Egress Axis.
    *   Read Var Card -> Store Egress to Store.

### Software Reconciliation (`analytical_engine.py`)
*   **State**: `memory[]` (Store), `registers` (Mill Axes), `PC` (Card Reader).
*   **Logic**: Fetch-Decode-Execute loop.
*   **Abstraction**: `BabbageNumber` handles the 50-digit fixed-point arithmetic.
*   **Timing**: `TIMING_TABLE` assigns "abstract costs" (e.g., DIV=750) to mimic the mechanical latency of complex barrel maneuvers.

---

## 5. Napier's Bones (Analog)

### Physical Logic Flow
1.  **Setup**: Arrange rods matching multiplicand digits.
2.  **Read**: Select row corresponding to multiplier digit.
3.  **Process**: Diagonal Addition (Lattice).
    *   Unit = Sum(Diagonal 0).
    *   Ten = Sum(Diagonal 1) + Carry.

### Software Reconciliation (`napiers_bones.py`)
*   **State**: `active_bones` (List).
*   **Logic**: `get_lattice_row()` retrieves pre-computed tuples. `multiply_single_digit()` performs the diagonal summation algorithmically.

---

## Modularization Audit

*   **Adapters**: `adapter.py` unifies `DE2`, `AE`, and `Curta` under `MachineAdapter`. This allows the `Debugger` to be machine-agnostic.
*   **Compilers**: Each language (LISP, Java, etc.) has a dedicated package `backend/src/compilers/` with distinct `lexer`, `parser`, `compiler` modules.
*   **Shared Types**: `ir_types.py` provides the common language (Babbage IR) for all compilers.

**Conclusion**: The system is highly modular. New devices need only an Emulator and an Adapter to integrate with the full toolchain.
