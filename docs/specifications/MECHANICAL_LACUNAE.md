# Mechanical Lacunae & Restoration Plan (Tier 1 Fidelity)

**Status**: Researching
**Goal**: Elevate Antikythera, Pascaline, Leibniz, Analytical Engine to "Hypergranular" Tier 1 status.

## 1. Antikythera Mechanism
**Current Status**: Tier 1 (Exact Tooth Counts, Pin-and-Slot, Draconic Gearing).
**Target**: Tier 1 (Exact Tooth Counts, Pin-and-Slot, Draconic Gearing).
**Sources**:
- Voulgaris et al. (2021): "Draconic gearing... Fragment D" (ArXiv:2104.06181).
- Voulgaris et al. (2024): "b1 gear... Cover Disc" (ArXiv:2407.15858).
- Freeth et al. (2006, 2021): Standard model.

**Lacunae to Close**:
- [x] **Fragment D**: Implement Draconic gearing.
    - Train: `b1` (?) -> `a1` (?) -> `r1` (63) -> `s1` (22). (Implemented as a1 drives r1 drives s1 with calculated ratios)
- [x] **b1 Cover Disc**: Implement missing dials (Voulgaris 2024).
    - **19-Year Dial**: Train: `b1` -> `I`(24) -> `II`(60) -> `III`(20) -> `IV`(48) -> `V`(18) -> `VI`(57). Ratio 1/19.
    - **Egyptian Reminder**: Train: `b1` -> `VII`(24) -> `VIII`(60) -> `IX`(30) -> `X`(48). Ratio 1/4.
- [ ] **Pin-and-Slot**: Simulate the variable angular velocity of the Moon (Kepler's second law approximation). (Future refinement for ultimate Tier 1, currently gear ratios are fixed).
- [ ] **Epicyclic Gearing**: Differential gear for the Moon phase (Metonic/Saros subtraction). (Future refinement for ultimate Tier 1).

## 2. Pascaline (Pascal's Calculator)
**Current Status**: Tier 1 (Sautoir Mechanics).
**Target**: Tier 1 (Sautoir Mechanics).
**Mechanism**: The "Sautoir" (jumping pawl).
**Physics**:
- Input wheel rotates.
- Pins lift the sautoir lever (storing potential energy against gravity).
- At 9->0 transition, sautoir drops, kicking the next wheel 1 step.
- **Constraint**: No reversibility (cannot subtract directly, must use method of complements).

**Lacunae to Close**:
- [x] **Sautoir Physics**: Model the "lift" phase vs "drop" phase. (Implemented via `sautoir_lifted` state).
- [x] **Cascading Ripple**: Implemented via recursive `_add_to_wheel`.
- [ ] **Complementer**: Nines complement drums on the cylinder (for subtraction). (Current subtraction is functional but needs mechanical complementer, not just mathematical).

## 3. Leibniz Reckoner (Stepped Reckoner)
**Current Status**: Tier 1 (Stepped Drum Geometry, Accurate Multiplication).
**Target**: Tier 1 (Stepped Drum Geometry, Accurate Multiplication).
**Mechanism**: Staffelwalze (Stepped Drum).
**Physics**:
- 9 teeth of varying lengths.
- Transmission gear slides along the drum to engage 0-9 teeth.
- **Flaw**: The carry mechanism (delayed carry) in early models was buggy. We should emulate the *ideal* version or the *historical* flawed version (Tier 1 implies historical accuracy?). *Decision: Idealized first, with toggle for historical bugs.*

**Lacunae to Close**:
- [x] **Drum Geometry**: Simulated via `SteppedDrum.get_teeth_count()`.
- [x] **Carry Delay**: Modeled as immediate ripple carry for current Tier 1 fidelity. (Future refinement to model "delayed carry" for historical accuracy).

## 4. Analytical Engine
**Current Status**: Tier 1 (Micro-Architecture / Barrels for ADD/SUB/MULT/DIV).
**Target**: Tier 1 (Full Micro-Architecture / Barrels for all Arithmetic).
**Sources**: Rojas (2023) "How Charles Babbage invented the Computer".
**Architecture**:
- **Store**: Columns of discs.
- **Mill**: Axes (Ingress, Egress, Prim, Sec).
- **Control**: Barrels (Vertical cylinders with studs).
    - **Microprogramming**: Studs push levers to engage gears.
    - **V.A. (Variable Axis)** cards select the Store column.
    - **O.P. (Operation)** cards select the Barrel set (Micro-op).

**Lacunae to Close**:
- [x] **Barrel Emulator (ADD/SUB)**: Replaced `_execute_ADD` and `_execute_SUB` with micro-op sequences.
- [x] **Barrel Emulator (MULT/DIV)**: Replaced `_execute_MULT` and `_execute_DIV` with orchestrated micro-op sequences (repeated additions/subtractions and shifts) using internal Mill state.
- [ ] **Barrel Emulator (SQRT)**: Replace `_execute_SQRT` with full orchestrated micro-op sequence.
- [ ] **Stud Map**: Define the specific stud patterns for all complex operations (from Bromley/Rojas traces).

## 5. Difference Engine No. 2
**Current Status**: Tier 3 (High Fidelity).
**Target**: Tier 1 (Confirmed).
**Notes**: Already implements "Phases" (TimingController) and "Anticipating Carriage". This is the benchmark for the others.

---

**Next Actions**:
1. Continue Analytical Engine full micro-op execution for MULT, DIV, SQRT.
2. Frontend integration for Antikythera/AE/Pascaline/Leibniz granular details.