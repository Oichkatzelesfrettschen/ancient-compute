# Store Specification

**Version**: 0.1
**Date**: 2026-02-26
**Charter ref**: S-1 (DE2 subsystem), S-2 (AE mill)

---

## 1. Overview

The Store is the variable memory of the Analytical Engine. It consists of
vertical columns of figure wheels, each column holding one multi-digit
decimal number. The hybrid design uses DE2-compatible columns with AE
mill addressing.

## 2. Column Specification

| Parameter | Value | Source |
|-----------|-------|--------|
| Number of columns | 8 (initial), expandable to 50 | Decision D-3 in CHARTER.md |
| Digits per column | 31 | SOURCE:SMG-DE2-TECH |
| Digit range | 0-9 per wheel | SOURCE:SMG-DE2-TECH |
| Numeric range | 0 to 10^31 - 1 (unsigned) | Derived |
| Sign convention | Separate sign wheel (top position) | SOURCE:BROMLEY-1982 |

## 3. Figure Wheel

| Parameter | Value | Source |
|-----------|-------|--------|
| Outer diameter | 30 mm | ASSUMPTION: Scaled from DE2 drawings |
| Thickness | 5 mm | ASSUMPTION |
| Material | Brass (CuZn36) | SOURCE:SWADE-2001 |
| Detent mechanism | Spring-loaded V-groove, 10 positions | SOURCE:SMG-DE2-TECH |
| Detent force | ~0.5 N per position | ASSUMPTION |
| Rotation per digit | 36 degrees (360/10) | Derived |

## 4. Column Axis

| Parameter | Value | Source |
|-----------|-------|--------|
| Axis material | Steel (EN8/080M40) | ASSUMPTION |
| Axis diameter | 8 mm | ASSUMPTION |
| Axis length | 31 wheels * 5mm + spacers = ~200 mm | Derived |
| Bearing type | Plain journal (brass bushing) | SOURCE:SWADE-2001 |

## 5. Carry Mechanism

### 5.1 Simple Carry (DE2)

When a figure wheel passes from 9 to 0, a carry lever engages and
adds 1 to the next-higher digit position.

| Parameter | Value | Source |
|-----------|-------|--------|
| Carry lever material | Steel | SOURCE:SMG-DE2-TECH |
| Carry propagation | Sequential (ripple carry) | SOURCE:SMG-DE2-TECH |
| Worst-case propagation | 31 digits (all 9s + 1) | Derived |

### 5.2 Anticipating Carriage (AE Enhancement)

The anticipating carriage evaluates carry at positions n and n+1
simultaneously, reducing worst-case propagation time.
(SOURCE:BROMLEY-1990)

| Parameter | Value | Source |
|-----------|-------|--------|
| Look-ahead depth | 2 positions | SOURCE:BROMLEY-1990 |
| Carry evaluation angles | 60 deg (eval 1), 120 deg (eval 2) | ASSUMPTION: From TimingSpec |
| Carry execution angle | 150 deg | ASSUMPTION: From TimingSpec |

## 6. Addressing

Variable cards specify a column address (0-1023) and direction
(read: store -> mill, or write: mill -> store).

| Parameter | Value | Source |
|-----------|-------|--------|
| Address width | 10 bits (0-1023) | Per CARD_STANDARD.md |
| Initial column count | 8 (addresses 0-7) | Decision D-3 |
| Transfer mechanism | Rack-and-pinion between column and mill | SOURCE:BROMLEY-1982 |

## 7. References

- [CHARTER.md](../program/CHARTER.md) -- Decisions D-1, D-3
- [SOURCE_MAP.md](../sources/SOURCE_MAP.md) -- Source traceability
- [CAM_TIMING_SPEC.md](CAM_TIMING_SPEC.md) -- Phase timing for column operations
