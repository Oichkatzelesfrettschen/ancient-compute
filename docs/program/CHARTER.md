# Program Charter: No-Electric Babbage Hybrid Engine

**Version**: 0.1
**Date**: 2026-02-26
**Status**: ACTIVE

---

## 1. Purpose

Build a physically operational, historically faithful hybrid of Babbage's
Analytical Engine and Difference Engine No. 2, powered exclusively by
non-electric prime movers (hand crank, spring barrel, weight drum, or steam).

A companion "software twin" validates every subsystem against golden traces
before metal is cut.

## 2. Scope

### In Scope

| ID | Item | Deliverable |
|----|------|-------------|
| S-1 | Difference Engine tabulator (method of finite differences) | Working 8-column, 31-digit DE2 subsystem |
| S-2 | Analytical Engine mill (4 arithmetic ops + conditional branch) | Working mill slice with anticipating carriage |
| S-3 | Card input system (operation, variable, number cards) | Mechanical card reader with Jacquard-derived format |
| S-4 | Printer and stereotyper | Type-wheel printer + soft-metal mold output |
| S-5 | Non-electric prime mover | Hand-crank baseline; spring/weight/steam optional |
| S-6 | Software twin (this repository) | Golden traces, BOM, test plans, simulation |
| S-7 | Source traceability | Every claim sourced or labeled ASSUMPTION |

### Out of Scope (Non-Goals)

| ID | Item | Rationale |
|----|------|-----------|
| N-1 | Full 1000-column Analytical Engine store | Scale target is 8-50 columns max |
| N-2 | Electric motors or solenoids | Violates "no-electric" constraint |
| N-3 | CNC-cut final parts | Goal is period-appropriate hand/machine tooling |
| N-4 | Commercial product | Educational and research artifact only |
| N-5 | Real-time digital control | All sequencing is mechanical (cams, barrels) |

## 3. Definitions

| Term | Definition | Source |
|------|-----------|--------|
| DE2 | Difference Engine No. 2, as built by Science Museum (1991) | SOURCE:SWADE-2001 |
| AE | Analytical Engine, Babbage's general-purpose design (1837-1871) | SOURCE:BROMLEY-1982 |
| Mill | Arithmetic unit of the AE; performs add/sub/mul/div | SOURCE:BROMLEY-1982 |
| Store | Variable memory of the AE; columns of figure wheels | SOURCE:BROMLEY-1982 |
| Anticipating carriage | Carry look-ahead mechanism (2-position depth) | SOURCE:BROMLEY-1990 |
| Figure wheel | Decimal digit wheel (0-9) on a column axis | SOURCE:SMG-DE2-TECH |
| Operation card | Specifies opcode (ADD, SUB, MUL, DIV, etc.) | SOURCE:MENABREA-1842 |
| Variable card | Specifies store column address and read/write direction | SOURCE:MENABREA-1842 |
| Number card | Loads a literal constant into the mill or store | SOURCE:MENABREA-1842 |
| Barrel | Micro-programmed cam drum sequencing mill operations | SOURCE:BROMLEY-1982 |
| Golden trace | Reference output from software twin used to validate hardware | -- |

## 4. Decision Log

| ID | Date | Decision | Rationale |
|----|------|----------|-----------|
| D-1 | 2026-02-26 | Hybrid DE2+AE, not pure AE | DE2 is built and documented; AE mill adds programmability |
| D-2 | 2026-02-26 | No electric components | Historical fidelity is a core project goal |
| D-3 | 2026-02-26 | 8-column initial store | Matches DE2 column count; sufficient for Note G |
| D-4 | 2026-02-26 | Card size: 129x56mm, 2 data rows | Per card grammar v0.1 spec |
| D-5 | 2026-02-26 | Software twin validates before metal is cut | Prevents costly rework on physical build |

## 5. Change Control

Changes to scope items (S-*) or non-goals (N-*) require:
1. Written rationale with source citations
2. Impact assessment on affected subsystems
3. Update to this charter and SOURCE_MAP.md

Minor corrections (typos, clarifications) do not require formal change control.

## 6. Success Criteria

| Criterion | Measure | Target |
|-----------|---------|--------|
| Tabulation | DE2 produces correct polynomial table (100 rows) | Matches golden trace bit-for-bit |
| Arithmetic | Mill computes ADD/SUB/MUL/DIV correctly | Passes all 4-op golden traces |
| Program | Note G deck executes, producing B1..B7 | Matches Bernoulli reference values |
| Card I/O | Cards encode/decode round-trip without loss | 100% fidelity on test deck |
| Printing | Printer produces readable numeric output | Matches golden trace format |
| Traceability | Every spec claim has SOURCE or ASSUMPTION label | 100% coverage in SOURCE_MAP |

## 7. References

- [SOURCE_MAP.md](../sources/SOURCE_MAP.md) -- source-to-requirement traceability
- [SOURCES.md](../sources/SOURCES.md) -- consolidated bibliography
- [CARD_STANDARD.md](../hardware/CARD_STANDARD.md) -- card grammar v0.1
- [TEST_PLAN.md](../hardware/TEST_PLAN.md) -- mechanical test tiers
