# Cam Timing Specification

**Version**: 0.1
**Date**: 2026-02-26
**Charter ref**: S-1, S-2

---

## 1. Overview

The Analytical Engine's sequencing is controlled by conjugate cam pairs
mounted on the main shaft. Each cam pair drives one mechanical action
during a specific phase of the 360-degree cycle. The DE2 uses a similar
but simpler timing system.

Per Swade (SOURCE:SWADE-2001), Babbage planned 14 conjugate cam pairs
for the AE mill. The hybrid design uses a subset appropriate for the
8-column store with 4 arithmetic operations.

## 2. Conjugate Cam Pairs

A conjugate cam pair consists of two cam lobes on the same shaft,
ensuring positive drive in both directions (no spring return needed).
This eliminates the risk of follower lift-off at speed.

| Parameter | Value | Source |
|-----------|-------|--------|
| Number of cam pairs (AE, full) | 14 | SOURCE:SWADE-2001 |
| Number of cam pairs (hybrid, initial) | 8 | ASSUMPTION: Subset for 4-op mill |
| Cam material | Cast iron or steel | ASSUMPTION |
| Follower type | Roller follower (reduces wear) | ASSUMPTION |
| Lobe profile | Smooth polynomial (jerk-continuous) | ASSUMPTION |

## 3. Cam Assignments

### 3.1 Pipeline: 2-Half-Cycle Model

Per Swade (SOURCE:SWADE-2001), the AE operates on a 2-half-cycle
pipeline:
- **First half (0-180 deg)**: Fetch operands, execute arithmetic, carry
- **Second half (180-360 deg)**: Store results, print, advance

### 3.2 Cam Table

| Cam # | Function | Active Phase (deg) | Dwell (deg) | Source |
|-------|----------|-------------------|-------------|--------|
| C1 | Column latch open/close | 0-30 | 330-360 | ASSUMPTION |
| C2 | Addition engage | 30-60 | -- | ASSUMPTION |
| C3 | Carry evaluation (pass 1) | 60-90 | -- | SOURCE:BROMLEY-1990 |
| C4 | Column advance / carry execute | 90-150 | -- | ASSUMPTION |
| C5 | Carry evaluation (pass 2) | 120-150 | -- | SOURCE:BROMLEY-1990 |
| C6 | Print setup + inking | 180-240 | -- | ASSUMPTION |
| C7 | Print strike + platen advance | 240-300 | -- | ASSUMPTION |
| C8 | Stereotype + cycle reset | 300-360 | -- | ASSUMPTION |

## 4. Timing Tolerances

| Parameter | Value | Source |
|-----------|-------|--------|
| Angular accuracy (cam profile) | +/- 0.5 deg | ASSUMPTION |
| Phase overlap allowance | 0 deg (no overlap between adjacent cams) | ASSUMPTION |
| Follower backlash | < 0.05 mm | ASSUMPTION |
| Maximum cam speed | 60 RPM (main shaft) | ASSUMPTION |
| Minimum cam speed | 5 RPM (hand crank low speed) | ASSUMPTION |

## 5. Barrel Integration

The barrel (micro-program drum) sequences multi-cycle operations
(MUL, DIV) by repeating specific cam phases. The barrel is
mechanically linked to cam C4 (column advance) and advances
one step per main shaft revolution.

| Parameter | Value | Source |
|-----------|-------|--------|
| Barrel rows | 50 (micro-instruction slots) | SOURCE:BROMLEY-1982 |
| Barrel material | Brass drum with steel studs | SOURCE:BROMLEY-1982 |
| Step mechanism | Ratchet pawl, driven by C4 cam | ASSUMPTION |

## 6. Design Notes

1. All cam profiles must be jerk-continuous to prevent follower bounce.
   (ASSUMPTION: Modern cam design practice applied to period mechanism.)

2. Conjugate pairs eliminate the need for return springs, which reduces
   part count and eliminates a failure mode (spring fatigue).

3. The 2-half-cycle pipeline means carry propagation must complete
   within the first 180 degrees. With 31-digit columns, worst-case
   carry ripple time constrains maximum RPM.

## 7. References

- SOURCE:SWADE-2001 -- 14 cam pairs, 2-half-cycle pipeline
- SOURCE:BROMLEY-1990 -- Carry evaluation mechanism
- SOURCE:BROMLEY-1982 -- Barrel micro-programming
- [POWER_TRAIN_SPEC.md](POWER_TRAIN_SPEC.md) -- Main shaft speed
- [STORE_SPEC.md](STORE_SPEC.md) -- Column and carry mechanism
