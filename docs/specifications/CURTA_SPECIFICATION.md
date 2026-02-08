# Curta Calculator Implementation Plan

## 1. Technical Specification
**Device**: Curta Type I
**Mechanism**: Stepped Drum (Leibniz Wheel) derivative.
**Capacity**: 
- Setting Sliders: 8 digits
- Result Counter (Black): 11 digits
- Revolution Counter (White): 6 digits

## 2. Architecture (`backend/src/emulator/curta.py`)

### Class `Curta`
#### State
- `sliders`: List[int] (8 positions)
- `result_register`: int (11 digits)
- `turn_counter`: int (6 digits)
- `carriage_position`: int (1-6) - determines power of 10 multiplier
- `crank_state`: Enum (NORMAL/ADD, PULLED/SUBTRACT)

#### Operations
- `set_slider(index, value)`: Adjust input.
- `rotate_carriage(direction)`: Change 10^x multiplier.
- `turn_crank()`:
    - If NORMAL: `result += sliders * 10^carriage`, `counter += 1 * 10^carriage`
    - If PULLED: `result -= sliders * 10^carriage`, `counter -= 1 * 10^carriage`
    - Handle carry propagation (tens transmission).
- `clear_result()`: Reset result register.
- `clear_counter()`: Reset revolution counter.

## 3. Logic & Circuit Audit (Mechanical)
- **Transmission**: Unlike electronic ALUs, Curta uses simultaneous transmission. We will emulate this via integer arithmetic but validate digit-by-digit logic for "circuit" fidelity if needed.
- **Tens Carry**: Cascading carry mechanism (Ten-carry levers).

## 4. Integration
- Add `CurtaMachineAdapter` to `adapter.py` to allow the Debugger/Analyzer to profile Curta operations (e.g., counting crank turns as "cycles").
