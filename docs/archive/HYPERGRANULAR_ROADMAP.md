# Hypergranular Roadmap: Ancient Compute Restoration

**Date**: February 7, 2026
**Status**: Active Execution
**Purpose**: Detailed technical breakdown of Phase 2 (Languages) and Phase 3 (Tools) completion.
**Last Verified Against Tests**: February 24, 2026 (targeted unit reruns for Haskell service and executor factory path).

## Planning Context

- Canonical strategy roadmap: `docs/general/MASTER_ROADMAP.md`
- Canonical execution tracker: `docs/general/TODO_TRACKER.md`
- Archive and supersedence policy: `docs/general/PLANNING_CANONICAL_MAP.md`

## Integrated From Archived Phase 4 Plans

- Preserved timeline-first decomposition from `docs/archive/PHASE_4_W4_DETAILED_EXECUTION_PLAN.md`.
- Preserved week-level visualization delivery structure from `docs/archive/PHASE_4_W2_IMPLEMENTATION_ROADMAP.md`.
- Preserved production-synthesis sequencing from `docs/archive/PHASE_4_W4_PLUS_SYNTHESIS_ROADMAP.md`.

---

## Phase 2: Modern Language Services

### 1. LISP Implementation
**Status**: 🟢 Functional (Tier 1)
**Highlights**: Supports `defun`, `let`, `if`, and n-ary arithmetic.
**Next**: `lambda` lifting and `cons` cells (requires heap in IR).

### 2. Java Implementation
**Status**: 🟢 Complete (Tier 1)
**Highlights**: 90/90 tests passing. Robust parser and IR emission for Classes, Methods, Variables, and Binary Ops.

### 3. System F
**Status**: 🟢 Functional (Tier 1)
**Highlights**: Successful type erasure and lambda lifting to IR. 3/3 tests passing.

### 4. IDRIS2
**Status**: 🟡 Functional (Proof of Concept)
**Highlights**: Basic function declarations and erasure working. 1/1 tests passing.

---

## Phase 3: Analytical Engine Tools

### 1. Unified Debugger
**Status**: 🟢 Complete
**Highlights**: Refactored to use `MachineAdapter` pattern. Supports both DE2 and AE engines.

### 2. Performance Analyzer
**Status**: 🟢 Complete
**Highlights**: Instruction frequency tracking, hot-spot detection, and bottleneck advice implemented.

---

## Phase 7+: Research & Lacunae (Future)

### 1. Missing Historical Compute
*   [x] **Napier's Bones**: Precursor to the slide rule, implemented with lattice multiplication logic. (Tier 1 Functional)
*   [x] **Enigma Machine**: Electro-mechanical substitution cipher with accurate double-stepping rotors and plugboard. (Tier 1 Functional)
*   [ ] **Curta Calculator**: The pinnacle of mechanical handheld compute.

### 2. Infrastructure Gaps
*   [~] **IR Dynamic Dispatch**: `IndirectCall` added to IR plus selector lowering to `CALL` via register/memory pointer. Compiler integration for closure-heavy paths is pending.
*   [~] **Heap Management**: Runtime `MemoryHeap` with first-fit `malloc`/`free` implemented and unit-tested; assembly/runtime wiring remains pending.
*   [~] **Garbage Collection**: Basic mark-and-sweep implemented in runtime heap model; integration with active execution pipelines remains pending.

Detailed notes: `docs/specifications/IR_RUNTIME_GAPS.md`
