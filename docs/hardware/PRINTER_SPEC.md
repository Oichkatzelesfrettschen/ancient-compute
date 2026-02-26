# Printer and Stereotyper Specification

**Version**: 0.1
**Date**: 2026-02-26
**Charter ref**: S-4 (Printer and stereotyper)

---

## 1. Overview

The output apparatus consists of two subsystems:
1. **Printer**: Type-wheel printer producing inked impressions on paper
2. **Stereotyper**: Soft-metal mold creating a reusable printing plate

Both subsystems operate in parallel during the print phase of each cycle.
(SOURCE:SMG-DE2-MANUAL)

## 2. Type-Wheel Printer

### 2.1 Type Wheels

| Parameter | Value | Source |
|-----------|-------|--------|
| Number of type wheels | 8 (one per column) | SOURCE:SMG-DE2-TECH |
| Digits per wheel | 10 (0-9) | SOURCE:SMG-DE2-TECH |
| Wheel diameter | 25 mm | ASSUMPTION |
| Setting mechanism | Column value transferred to wheel position | SOURCE:SMG-DE2-MANUAL |

### 2.2 Inking

| Parameter | Value | Source |
|-----------|-------|--------|
| Ink type | Printer's ink (carbon black, linseed oil base) | SOURCE:SWADE-2000 |
| Inking roller | Rubber-faced roller, spring-loaded | SOURCE:SMG-DE2-MANUAL |
| Inking angle | 210 degrees (per TimingSpec) | ASSUMPTION |

### 2.3 Print Mechanism

| Parameter | Value | Source |
|-----------|-------|--------|
| Hammer type | Spring-loaded flat hammer | SOURCE:SMG-DE2-MANUAL |
| Strike angle | 240 degrees (per TimingSpec) | ASSUMPTION |
| Paper advance | Platen advances one line per cycle | SOURCE:SMG-DE2-MANUAL |
| Lines per page | 50 | ASSUMPTION: Based on period printing standards |
| Characters per line | 8 digits + sign + spaces = ~12 | Derived |

## 3. Stereotyper

### 3.1 Mold Production

The stereotyper presses digit punches into a soft metal plate,
creating a reusable mold for subsequent printing runs.
(SOURCE:SMG-DE2-MANUAL)

| Parameter | Value | Source |
|-----------|-------|--------|
| Mold material | Type metal (lead-tin-antimony alloy) | SOURCE:SWADE-2000 |
| Mold dimensions | 120 x 80 mm per page | ASSUMPTION |
| Digit punch depth | 0.5 mm | ASSUMPTION |
| X positions | 8 (one per digit column) | SOURCE:SMG-DE2-TECH |
| Y positions | 50 (one per line) | ASSUMPTION |

### 3.2 Timing

| Parameter | Value | Source |
|-----------|-------|--------|
| Frame advance (X) angle | 300 degrees | ASSUMPTION: From TimingSpec |
| Mold extraction angle | 330 degrees | ASSUMPTION: From TimingSpec |
| Y advance | After each complete row | SOURCE:SMG-DE2-MANUAL |

## 4. Output Format

### 4.1 Printed Output

```
  Row    Col0  Col1  Col2  Col3  Col4  Col5  Col6  Col7
  001   +0000000000000000000000000000001
  002   +0000000000000000000000000000004
  003   +0000000000000000000000000000009
  ...
```

### 4.2 Golden Trace Format

The software twin produces golden traces in CSV format:

```csv
row,col0,col1,col2,col3,col4,col5,col6,col7
1,1,0,0,0,0,0,0,0
2,4,0,0,0,0,0,0,0
```

## 5. References

- [CHARTER.md](../program/CHARTER.md) -- Success criterion: printer output
- [SOURCE_MAP.md](../sources/SOURCE_MAP.md) -- Source traceability
- [TEST_PLAN.md](TEST_PLAN.md) -- Tests T0-04, T1-05, T3-03
