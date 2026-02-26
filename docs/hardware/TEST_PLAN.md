# Mechanical Test Plan

**Version**: 0.1
**Date**: 2026-02-26
**Charter ref**: S-6 (Software twin validates before metal is cut)

---

## 1. Test Tiers

### Tier 0: Unit Tests (Single Components)

Each component is tested individually before integration.

| Test | Component | Method | Acceptance Criterion | Source |
|------|-----------|--------|---------------------|--------|
| T0-01 | Figure wheel | Manual rotation | 10 stable detent positions (0-9) | SOURCE:SMG-DE2-TECH |
| T0-02 | Carry lever | Spring return | Engages at 9->0 transition, resets cleanly | SOURCE:BROMLEY-1990 |
| T0-03 | Card reader | Single card feed | Card registers in alignment holes within 0.1mm | ASSUMPTION |
| T0-04 | Print hammer | Drop test | Legible impression in soft metal at 0.5mm depth | SOURCE:SMG-DE2-MANUAL |
| T0-05 | Cam follower | Profile trace | Follower tracks conjugate cam pair without lift-off at 30 RPM | ASSUMPTION |

### Tier 1: Integration Tests (Subsystem)

Subsystems assembled from tested components.

| Test | Subsystem | Method | Acceptance Criterion | Source |
|------|-----------|--------|---------------------|--------|
| T1-01 | Single column (31 digits) | Driven rotation | Correct addition of 2 arbitrary 31-digit numbers | SOURCE:SMG-DE2-TECH |
| T1-02 | Anticipating carriage | Column pair | Carry propagates correctly for 999...9 + 1 | SOURCE:BROMLEY-1990 |
| T1-03 | Mill slice (1 column) | Card-driven | ADD, SUB produce correct results | SOURCE:BROMLEY-1982 |
| T1-04 | Card reader + barrel | Sequence test | 5-card deck executes in order | ASSUMPTION |
| T1-05 | Printer + type wheels | Print test | 8-digit number prints correctly | SOURCE:SMG-DE2-MANUAL |

### Tier 2: Endurance Tests

Sustained operation to verify wear and thermal stability.

| Test | Duration | Condition | Acceptance Criterion |
|------|----------|-----------|---------------------|
| T2-01 | 100 cycles | 30 RPM, ambient temp | No binding, clearance within spec |
| T2-02 | 1000 cycles | 30 RPM, ambient temp | Wear < 0.01mm on any bearing surface |
| T2-03 | 8 hours continuous | 30 RPM | Temperature rise < 20C above ambient |
| T2-04 | 100 cycles | With lubrication change | No performance degradation after oil change |

### Tier 3: Acceptance Tests

Full system validation against golden traces.

| Test | Description | Golden Trace | Acceptance |
|------|-------------|-------------|------------|
| T3-01 | Polynomial table (x^2, 100 rows) | hardware_twin/golden_traces/poly_table_100.csv | Bit-for-bit match |
| T3-02 | Note G deck (B1 through B7) | Bernoulli reference values | Match to 40 decimal digits |
| T3-03 | Full print run (100 rows) | Golden trace format | Legible, correctly formatted |
| T3-04 | Card round-trip | card_compiler round-trip | 100% fidelity |

## 2. Instrumentation

| Instrument | Purpose | Precision |
|------------|---------|-----------|
| Dial indicator | Shaft runout, clearance | 0.01mm |
| Thermocouple (K-type) | Bearing temperature | 1C |
| Tachometer | Shaft RPM | 1 RPM |
| Force gauge | Crank torque | 0.1 N-m |
| Optical comparator | Gear tooth profile | 0.005mm |

## 3. Test Sequence

1. Complete all Tier 0 tests before assembling subsystems
2. Complete all Tier 1 tests before endurance testing
3. Complete Tier 2 before golden-trace acceptance
4. Tier 3 acceptance tests are the final gate before declaring success

## 4. References

- [CHARTER.md](../program/CHARTER.md) -- Success criteria (Section 6)
- [SOURCE_MAP.md](../sources/SOURCE_MAP.md) -- Source traceability
- [POWER_TRAIN_SPEC.md](POWER_TRAIN_SPEC.md) -- Prime mover specification
