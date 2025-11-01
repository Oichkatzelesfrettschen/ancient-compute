================================================================================
PHASE 3 OVERVIEW: BABBAGE ISA EMULATOR + I/O + DEBUGGER
Ancient Compute Project
================================================================================

PHASE 3 MISSION
================================================================================

Implement a complete emulator for the Difference Engine No. 2 (DE2) based on
Science Museum Group (SMG) technical documentation, with integrated I/O
(printer/stereotyper) and a debugger for symbolic inspection.

**NOT a hypothetical "Babbage ISA"—this is HISTORICAL ENGINEERING.**

The emulator faithfully models the mechanical behavior of DE2 as documented in:
- SMG Technical Description (232 pp., with appendices, timing diagrams, BOM)
- SMG working hardware (DE2 calculator 1991, printer/stereotyper 2002)
- Menabrea/Lovelace primary sources (1842–1843) for Analytical Engine
- Babbage 1826 Mechanical Notation for symbolic timing

================================================================================
KEY CONCEPTS & CONSTRAINTS
================================================================================

### Difference Engines (DE1, DE2)
- **Purpose:** Compute finite difference tables (polynomial evaluation)
- **Input:** Initial values for 8 difference columns (31 decimal digits each)
- **Computation:** Successive additions via difference columns + carry propagation
- **Output:** Printed page of results + stereotype molds (reusable printing plates)
- **Timeline:** 1820s design, 1991/2002 working reconstruction by SMG

### Analytical Engine (AE)
- **Purpose:** General-purpose programmable computer (store/mill architecture)
- **Input:** Punched cards (operation, variable, number cards)
- **Store:** 30 addressed columns (memory)
- **Mill:** Arithmetic logic unit (accumulator-like)
- **Output:** Printer (same as DE2)
- **Control:** Card-driven (no software; instructions on cards)
- **Timeline:** 1834–1871 design (never fully built in Babbage's lifetime)

### Mechanical Timing
- **Main shaft:** Single rotating axis driving all mechanical actions
- **One cycle:** 360° rotation ≈ one complete addition + print + stereotyping
- **Phases:** Column latch → addition → carry → print setup → strike → mold advance
- **Anticipating carriage:** Overlapped carry logic (Babbage's innovation)
  - Reduces addition from 8–16 rotations to just 2 rotations
  - Look-ahead cams predict carries 2 positions ahead
  - Drives carry lifters at optimal timing

### Key Timing (from SMG Technical Description)
```
0°–30°:    Column latch (read current state)
30°–60°:   Difference addition begins
60°–90°:   Anticipating carriage evaluates position 0
90°–120°:  Column advance (shift to next row)
120°–150°: Anticipating carriage evaluates position 1
150°–180°: Carry execution completes
180°–210°: Print setup (type setter positioning)
210°–240°: Inking roller engages
240°–270°: Print hammer strikes line
270°–300°: Platen advances (next line)
300°–330°: Stereotype frame advances (X-axis)
330°–360°: Stereotype mold extraction signal
```

================================================================================
PHASE 3 ARCHITECTURE (3 MODULES)
================================================================================

### Module 1: EMULATOR (Mechanical Simulation)
**Purpose:** Simulate DE2/AE mechanical behavior at shaft-angle granularity

**Components:**
- `DigitColumn`: One difference column (31 digits, carry state)
- `ColumnBank`: All 8 columns + shared state
- `AnticipatingCarriage`: Look-ahead carry logic
- `TimingController`: Main shaft angle → event dispatch
- `DEMachine`: Top-level emulator orchestrator

**Interface:**
```python
de = DEMachine(initial_differences=[1, 2, 2, 0, 0, 0, 0, 0])
de.run_cycle()  # Execute one 360° rotation
values = de.get_column_values()  # Read results
snapshot = de.get_snapshot()  # Get full mechanical state
```

**Validates against:** SMG timing diagrams, anticipating carriage specs

---

### Module 2: I/O SUBSYSTEM (Mechanical Output)
**Purpose:** Model printer apparatus and stereotype frame

**Components:**
- `PrinterApparatus`: Type setter, inking, hammer, platen
- `StereotypeFrame`: Mold creation and extraction
- `CardReader`: Punch card feed (for AE)
- `AnalyticalEngine`: Minimal AE implementation (store + mill + cards)

**Interface:**
```python
# DE2 output
de = DEMachine(...)
de.run_cycle()
page = de.printer.get_printed_page()  # list[str] of digit lines
molds = de.stereotyper.get_completed_molds()  # list[dict] of molds

# AE output
ae = AnalyticalEngine(store_size=20)
ae.load_program(cards)
page, molds = ae.execute()
```

**Validates against:** SMG printer/stereotyper drawings, Scheutz "Specimens"

---

### Module 3: DEBUGGER (Symbolic Inspection)
**Purpose:** Set breakpoints, step through execution, inspect variables

**Components:**
- `SymbolTable`: Map compiler IR variables → mechanical columns
- `BreakpointEngine`: Condition evaluation (mechanical events)
- `Stepper`: Single-step at angle/phase/cycle granularity
- `DebugSnapshot`: Captured mechanical state at breakpoint

**Interface:**
```python
debugger = Debugger(de_machine)
bp_id = debugger.set_breakpoint("carry_at_position == 1")
debugger.step_until_breakpoint(bp_id)
snapshot = debugger.get_snapshot()
print(debugger.inspect_variable("x", snapshot))
```

**Validates against:** Babbage 1826 Mechanical Notation (future enhancement)

================================================================================
DATA FLOW: PHASE 2 → PHASE 3
================================================================================

```
Phase 2 Output: Babbage IR
  │
  │  function polynomial() -> i64
  │    @block entry:
  │      %result = add i64 %col0, %col1
  │      print %result
  │
  ▼
Phase 3 Emulator (DE2):
  1. Map IR variables to columns (%col0 → column 0)
  2. Load initial values into columns
  3. Execute: run_cycle() → carry propagation → print phase
  4. Extract: printed_lines + molds
  │
  ▼
Output:
  - Printed page: list of 8-digit lines
  - Stereotype molds: reusable printing plates
  - Debug snapshots: variable values at breakpoints
```

================================================================================
TESTING PYRAMID (PHASE 3)
================================================================================

**Unit Tests (260+):**
- DigitColumn: arithmetic, carry generation
- ColumnBank: multi-column state, synchronized operations
- AnticipatingCarriage: carry prediction, timing
- TimingController: event dispatch, phase transitions
- PrinterApparatus: digit formatting, line accumulation
- StereotypeFrame: mold creation, extraction
- CardReader: card parsing, deck sequencing
- AnalyticalEngine: operation execution, mill behavior

**Integration Tests (200+):**
- Polynomial evaluation (x², x²+x, x²+x+1)
- Difference table generation (3-column table)
- Printer output formatting (8-digit lines)
- Stereotype mold extraction (50-line pages)
- Lovelace Bernoulli sequence (simplified AE)
- Debugger breakpoint + inspection
- Full pipeline: IR → emulator → printer → molds

**Validation Tests (50+):**
- SMG timing diagram compliance
- Anticipating carriage cycle reduction (2x vs 8–16)
- Carry propagation correctness
- Stereotype mold dimensions
- Symbol table integration

**Coverage Target:** >90% emulator, >85% I/O, >80% debugger

================================================================================
IMPLEMENTATION PHASES (9 WEEKS)
================================================================================

### Week 1–2: Emulator Core
**Deliverable:** DE2 mechanical simulator (columns, carry, timing)
**LOC:** ~1,200 code + 260 tests
**Validation:** Polynomial evaluation test passes

### Week 3–4: Printer & CardReader
**Deliverable:** I/O apparatus + AE support
**LOC:** ~700 code + 105 tests
**Validation:** Print output matches expected values

### Week 5–6: Debugger
**Deliverable:** Symbol table, breakpoints, stepper
**LOC:** ~600 code + 90 tests
**Validation:** Breakpoint + variable inspection works

### Week 7–8: Testing & Validation
**Deliverable:** 200+ integration tests, coverage >90%
**LOC:** ~1,500 tests
**Validation:** All tests passing, CI/CD green

### Week 9: Documentation
**Deliverable:** User guide, mechanism reference, examples
**LOC:** ~500 documentation
**Validation:** Ready for Phase 4

================================================================================
SUCCESS CRITERIA
================================================================================

**Must Achieve (Phase 3 Completion):**
1. ✅ DE2 emulator executes simple difference tables correctly
2. ✅ Printer produces formatted 8-digit lines (31 columns of output)
3. ✅ Stereotyper creates valid molds (extracted after 50 lines)
4. ✅ Debugger allows breakpoint setting on mechanical events
5. ✅ Debugger allows variable inspection (symbol table lookup)
6. ✅ >90% test coverage on emulator code
7. ✅ All 600+ tests passing; CI/CD green
8. ✅ Timing validated against SMG technical description

**Should Achieve (Phase 3 Extended):**
1. AE card deck execution (Lovelace Bernoulli sequence)
2. Multiple mold extraction per page (stereotype cadence correct)
3. Mechanical notation visualization (future: Babbage 1826 compiler)
4. Plotter/curve-drawing support (future: AE output diversity)

**Documentation Deliverables:**
1. Architecture specification (this document + detailed spec)
2. Implementation roadmap (9-week breakdown)
3. User guide (how to run emulator, use debugger)
4. Mechanism reference (component specs, SMG cross-links)
5. API documentation (all public interfaces)

================================================================================
RESOURCES & REFERENCES
================================================================================

**Primary Historical Sources:**
- SMG Technical Description: https://www.sciencemuseum.org.uk/sites/default/files/2023-09/DE2_Technical_Description.pdf
- SMG Technical Drawings: https://collection.sciencemuseumgroup.org.uk/
- Menabrea/Lovelace (1842–1843): https://www.fourmilab.ch/babbage/sketch.html
- Babbage 1826 Mechanical Notation: Royal Society Philosophical Transactions

**Implementation References:**
- Phase 3 Architecture Specification: `PHASE_3_ARCHITECTURE_SPECIFICATION.md`
- Phase 3 Implementation Roadmap: `PHASE_3_IMPLEMENTATION_ROADMAP.md`
- Phase 2 Compiler Output: Babbage IR (from Phase 2 services)

**Test Corpus:**
- Historical difference tables (polynomial evaluation)
- Scheutz "Specimens of Tables" (1857)—stereotype validation
- Lovelace Note G—Bernoulli sequence computation

================================================================================
NEXT IMMEDIATE STEPS
================================================================================

1. **Commit Phase 3 planning documents** ✅
   - PHASE_3_ARCHITECTURE_SPECIFICATION.md (created)
   - PHASE_3_IMPLEMENTATION_ROADMAP.md (created)
   - PHASE_3_OVERVIEW.md (this file)

2. **Create backend/src/emulator/ module structure** (Week 1)
   - Directory layout
   - Module skeleton (`__init__.py`)
   - Type definitions (`types.py`)

3. **Implement DigitColumn & ColumnBank** (Week 1)
   - Digit arithmetic
   - Carry generation
   - State snapshots
   - 80+ unit tests

4. **Implement AnticipatingCarriage** (Week 1)
   - Look-ahead logic
   - Position-based carry prediction
   - 50+ unit tests

5. **Implement TimingController** (Week 2)
   - Main shaft angle tracking
   - Phase dispatch (0°, 30°, 60°, …, 360°)
   - Event logging
   - 60+ unit tests

6. **Integration test: Polynomial evaluation** (Week 2)
   - Load initial differences
   - Run 6 cycles
   - Verify output values

---

**Phase 3 Status:** SPECIFICATION COMPLETE; READY FOR IMPLEMENTATION

Phase 2 completion: 62/62 tests passing (100%) ✅
Phase 3 architecture: Comprehensive, grounded in SMG documentation ✅
Phase 3 roadmap: Granular 9-week breakdown with deliverables ✅

**Proceeding to Week 1 implementation.**

================================================================================
END OVERVIEW
================================================================================
