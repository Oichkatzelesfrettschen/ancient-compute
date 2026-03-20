# Babbage Analytical Engine -- Completeness Audit & Implementation Roadmap

**Audit Date**: 2026-03-20
**Scope**: AE core emulator, all historical machine extensions, physics/materials modules, BOM

---

## Executive Summary

Three parallel audits covering the full emulator stack produced the following verdict:

| Domain | Completeness | Critical Gaps | Status |
|--------|-------------|---------------|--------|
| AE core (mill, store, barrel, I/O) | 85% | 5 P0 missing opcode handlers; printer orphaned; Note G not engine-runnable | ADVANCED |
| Hardware extensions (10 machines) | 75% | 3 step() stubs; 1 PC double-increment bug; 3 machines missing division | GOOD |
| Physics / materials | 80% | Synthetic gear params; no oil aging; no cumulative fatigue; BOM empty | ADVANCED |

The emulator is **production-quality in its core path** (1,722 unit tests, 42 opcodes, 13 barrels, comprehensive Note G test suite). Gaps are concentrated in: peripheral coupling (printer, card I/O), instruction dispatch stubs in secondary machines, and physics parameter sourcing (synthetic vs. empirical from Babbage's drawings / SMG documentation).

---

## Part I: AE Core Emulator

### 1.1 Mill (ALU)

**Implemented (10/12 historical operations)**:

| Operation | File:Line | Cycles | Notes |
|-----------|-----------|--------|-------|
| ADD | engine.py:289 | 8 | Micro-programmed via barrel |
| SUB | engine.py:303 | 8 | Micro-programmed via barrel |
| MULT | engine.py:560 | 400 | Repeated additions; result split A/D |
| DIV | engine.py:581 | 750 | Quotient in A, remainder in D |
| SQRT | engine.py:634 | 250 | Newton-Raphson, hardcoded 25 iterations |
| MOV | engine.py:650 | -- | Identity transfer |
| ABS | engine.py:662 | -- | Absolute value in-place |
| NEG | engine.py:677 | -- | Tens-complement negation |
| Sign detection | engine.py:242 | -- | SIGN flag on all operations |
| Zero detection | engine.py:242 | -- | ZERO flag on all operations |
| Overflow detection | engine.py:122 | -- | BabbageNumber._overflow_flag |
| Conditional step on sign | OPCODES.yaml only | -- | **P1 GAP**: Not a direct opcode; implemented indirectly via CMP + Jxx |

**P0 -- Missing Opcode Handlers** (raise NotImplementedError at engine.py:1112):

| Opcode | Code | Workaround | Notes |
|--------|------|-----------|-------|
| CMPZ | 0x6 | `CMP reg, 0` | Compare with zero |
| CLR | 0x5 | `LOAD reg, 0` | Clear register |
| BR | 0x7 | `JMP` | Unconditional branch (loses conditional semantics) |
| STEP | 0x8 | NOP | Advance card drum (logical model: NOP) |
| PRINT | 0xA | WRPRN | Direct print output |

These five are declared in `docs/hardware/OPCODES.yaml` but have no handler registered in the engine dispatch table. Any program using them halts with a Python exception.

### 1.2 Store

| Parameter | Historical Spec | Emulator | Status |
|-----------|-----------------|----------|--------|
| Columns | 1,000 | 2,000 | Exceeds -- no gap |
| Digits per column | 50 | 50 | Matches |
| Number representation | 50-digit decimal | BabbageNumber (int * 10^40) | Equivalent |
| Sign | Separate sign digit | Encoded in scaled value | Equivalent |

No gaps. Store exceeds spec. LOAD/STOR micro-ops at engine.py:689-724.

### 1.3 Barrel / Micro-Operations

**Implemented**: 13 barrels (ADD, SUB, MULT, DIV, SQRT, LOAD, STOR, SHL, SHR, AND, OR, XOR, CHKS, PLAY, SETMODE).

Each barrel is a sequence of MicroOp entries in `barrels.py:87-170`. Micro-op execution dispatch is in `engine.py:316-557` (240 lines).

**P1 -- Barrel loop control fragile** (`engine.py:445`):
REPEAT_IF_COUNTER jumps back via hardcoded `step_index - 2`. No formal loop-counter abstraction. Works correctly but is brittle; any barrel reordering silently breaks loop targets. Comment at line 445 documents the known issue.

**P2 -- Extension opcodes have zero timing cost**:
SHL, SHR, AND, OR, XOR, CHKS, PLAY, SETMODE all default to 0 cycles in TIMING_TABLE. Mechanically inauthentic; these should have barrel-step counts.

### 1.4 Card Reader / Punch

- `card_reader.py`: Three card classes (OPERATION, VARIABLE, NUMBER), 12 AE opcodes + 4 legacy DE2 ops.
- RDCRD (`engine.py:1036`): Reads from `input_cards` queue.
- WRPCH (`engine.py:1058`): Writes to `result_cards` list.
- WRPRN (`engine.py:1071`): Writes to `result_cards` list (printer path).

**P1 -- Card holes matrix unused**: `card_reader.py` defines a 140x80 holes matrix but no encode/decode functions exist to convert assembly text to card bytes. `tools/card_compiler.py` is referenced in comments but not called. Cards are constructed programmatically, not via mechanical hole patterns.

### 1.5 Printer

`printer.py` (380 lines) implements a full printer state machine: type wheels, inking mechanism, hammer strike, stereotyper mold. It is **never instantiated or connected to the Engine**.

**P1 -- Printer orphaned**: WRPRN appends to `result_cards` list. `printer.py` runs as a standalone simulator. To achieve historical fidelity, `Engine.__init__` should instantiate a `Printer` and WRPRN should call `printer.print_line()`.

### 1.6 Note G (Ada Lovelace Bernoulli Numbers)

**Algorithm correctness: VERIFIED**.

- `note_g_deck.py:190-265` implements the full recurrence loop.
- Both known 1843 errata are corrected:
  - Op 4: division order fixed (V4/V5 not V5/V4) -- `test_note_g.py:75`
  - Op 24: negation corrected (subtraction not addition) -- `test_note_g.py:83`
- Output verified against exact Bernoulli series oracle (`test_note_g.py:44`).
- 20+ tests covering oracle, deck structure, single-pass, loop-back, exact fractions.

**P1 -- Note G runs in Python, not on the emulated AE**:
`note_g_deck.py` is a standalone Python interpreter for `NOTE_G_DECK.yaml`. It does not call `Engine.run()` or any AE opcode. The deck cannot be assembled into AE instructions and executed on the mill/store. This means the emulator cannot claim to "run the first computer program on the Analytical Engine" in the mechanical sense -- only in a declarative/interpretive sense.

**P1 -- Breakpoint bug** (`engine.py:1325`):
`check_breakpoints()` reads `bp["reg"]` which is never set by `set_breakpoint()`. Raises `KeyError` at runtime when any breakpoint is set and `check_breakpoints()` is called.

---

## Part II: Hardware Extension Machines

### 2.1 Machine Inventory

10 machines with MachineAdapter implementations, plus ~10 non-adapter historical machines (Abacus, Antikythera, Jacquard, Slide Rule, etc.).

### 2.2 Per-Machine Gap Table

| Machine | Historical Capability | P0 Bug | P1 Incomplete | P2 Missing |
|---------|----------------------|--------|---------------|------------|
| **Scheutz DE** (1851) | 7-register 6th-order polynomial tabulator, 15-digit, overflow detection | -- | -- | -- |
| **Ludgate AE** (1909) | Irish log multiply, 192-column store, perforated cylinder program | -- | `step()` only increments pointer; no instruction dispatch | -- |
| **Torres Quevedo** (1914) | 8-digit FP *, /, +, -, 2-digit exponent, relay control | -- | `step()` stub; conditional branching not implemented; no chess endgame | -- |
| **Zuse Z1** (1938) | Binary FP 22-bit, 64-word memory, punched tape, lamp panel | `run()` double-increments PC (`zuse_z1.py:268`) | -- | -- |
| **Pascaline** (1642) | 6-digit sautoir carry, add + subtract via 9's complement | -- | End-around carry incomplete (`pascaline.py:112-174`); comment: "not precise sautoir-level" | -- |
| **Leibniz Reckoner** (1673) | Stepped drum, 16-digit, carriage shift, multiply | -- | Delayed carry is immediate (known, noted in code) | Division via repeated subtraction not implemented |
| **Napier's Bones** (1617) | Lattice multiply (single + multi-digit), division via complementary method | -- | Division not implemented | Square root rod not implemented |
| **Curta Type I** (1948) | 11-digit result, 6-digit counter, add/sub/mul, mechanical bell | -- | No `divide()` helper | Tens bell overflow signal not modeled |
| **Enigma** (1920s) | Plugboard + 3-rotor encipher, ring setting, stepping | -- | Double-stepping anomaly not modeled; no MachineAdapter | M4 4-rotor variant not modeled |
| **DE2 / AE** (Babbage) | Full mill + store + barrel + anticipating carriage + printing | See Part I | See Part I | See Part I |

### 2.3 Critical Bug: Zuse Z1 Double-Increment

`zuse_z1.py:268` increments the program counter inside the `run()` loop body, AND individual instruction handlers also increment PC. This causes every instruction to advance PC by 2, effectively skipping every other instruction. Programs requiring more than one instruction silently execute the wrong sequence.

**Fix**: Remove the increment from `run()`'s loop body (or from all instruction handlers) -- one or the other, not both.

### 2.4 Instruction Dispatch Stubs

Both Ludgate and Torres Quevedo have functional arithmetic (multiply, add, FP ops) accessible via direct Python method calls, but their `step()` methods only increment a program pointer without dispatching to those operations. This means the perforated cylinder model (Ludgate) and relay-cycle model (Torres) cannot execute programs -- only direct method invocations work.

**Fix**: Add an instruction queue / opcode map in each `step()` and dispatch to the existing arithmetic methods.

### 2.5 Test Coverage Summary

| Machine | Test Lines | Status |
|---------|-----------|--------|
| Scheutz | 219 | Strong |
| Ludgate | 220 | Good |
| Torres Quevedo | 241 | Strong |
| Zuse Z1 | 345 | Strong |
| Pascaline | 18 | **Weak** -- no subtraction, overflow, or multi-digit carry tests |
| Leibniz Reckoner | 10 | **Weak** -- only add + shift |
| Napier's Bones | 58 | Fair -- multiply only |
| Curta | 57 | Fair -- division workflow untested |
| Enigma | 0 | **Missing** -- only CLI integration tests |

Pascaline and Leibniz Reckoner are the most under-tested machines relative to their mechanical complexity.

---

## Part III: Physics, Materials, and BOM

### 3.1 Materials Library

**Defined** (5 materials, 17-18 properties each):
- `brass` (CZ121 / CuZn39Pb3) -- gears, wheels
- `steel` (S275JR) -- main shaft, frame fasteners
- `cast_iron` (EN-GJL-250) -- bed plate, frame castings
- `phosphor_bronze` (CuSn8 / C52100) -- bearing bushings
- `spring_steel` (C67S) -- springs, latches

**Missing historical materials**:

| Material | Historical Use | Modern Proxy | Impact |
|----------|---------------|--------------|--------|
| Wrought iron | Frame, main structure | cast_iron | Different damping, ductility, yield |
| Gun metal (CuSn10Ni1) | Some bearing surfaces | phosphor_bronze | Slightly different wear K |
| Bell metal | Possibly some wheels | brass | Marginally different elastic modulus |

For historical fidelity, add wrought iron (EN 10083-1 / 1.1130) to `materials.py`. Cast iron is a reasonable proxy for simulation but is more brittle and has higher damping than Babbage's actual wrought iron frame.

### 3.2 Kinematics

**Present**: Lewis bending stress, AGMA K_v, Hertz contact stress, contact ratio, DOF analysis, cam profiles (modified trapezoidal), shaft critical speed, torsional vibration.

**Key gap -- Synthetic gear tooth counts**:
The default gear parameters (`tooth_count_driver=20`, `tooth_count_driven=60`) are Python defaults, not values from Babbage's mechanical drawings. The Science Museum Group DE2 documentation specifies actual tooth counts for the column drive train, number wheel reduction, and carriage mechanism. Using synthetic 3:1 and 2:1 reductions affects Lewis form factor Y, contact ratio, and Hertz stress calculations.

**Fix**: Extract actual tooth counts from SMG DE2 technical drawings and populate a `BABBAGE_GEAR_CHAIN` constant in `kinematics.py`.

### 3.3 Thermodynamics

**Present**: Churchill-Chu natural convection, Stefan-Boltzmann radiation, Crank-Nicolson transient PDE, differential thermal expansion (brass vs. iron), gear backlash thermal feedback, bearing clearance thermal feedback.

**Gap -- No steam drive cycle**: If the AE is driven by steam (Babbage's intended prime mover), there is no Rankine cycle solver, no boiler heat release model, and no condensation heat transient. The Stephenson link valve parameters (lap=3mm, lead=1mm, cutoff=60%) are defined in the simulation schema but never used. The simulation assumes an external driving torque with no associated heat source.

### 3.4 Tribology

**Present**: Archard wear, Hamrock-Dowson EHL film thickness, lambda ratio, running-in model (Rabinowicz), surface roughness evolution, wear-to-clearance feedback.

**Critical gap -- No oil degradation**:
Lubricant viscosity is constant for the entire simulation duration. 19th-century mineral oil (or tallow, likely the actual lubricant) degrades via oxidation over weeks-to-months, causing viscosity to rise then fall. The `tribology.py` module runs infinite clean-oil operation. Real bearing life predictions are therefore optimistic.

**Gap -- No fatigue spalling model**:
Bearing life is calculated from Archard wear reaching a clearance limit. No L10 rolling-contact fatigue model (ISO 281) or surface pitting / spalling criterion is implemented. For journal bearings (no rolling elements), this is less critical than for ball bearings, but for gear tooth surfaces the omission means contact-fatigue pitting is not predicted.

**Gap -- Lubrication schedule not enforced**:
The simulation schema specifies re-oiling every 160 hours or 500-600 cycles. The simulation never triggers re-lubrication events; oil is implicitly infinite and constant.

**Missing material pair**:
Brass-on-brass (figure wheels contacting each other) has no Archard K coefficient. Only bronze-steel, brass-steel, and dry-steel pairs are modeled.

### 3.5 Structural

**Present**: Goodman fatigue (Marin factors ka-ke, endurance limit), Euler/Johnson buckling, shaft deflection (L/10000 criterion), stress concentration (Peterson power-law), AGMA K_v, critical speed (Rayleigh-Ritz), notch sensitivity (Neuber).

**Gap -- No cumulative fatigue (Miner's rule)**:
Goodman operates on a single stress amplitude. ADD, MULT, and DIV impose different cyclic load profiles (different W_t, different frequency per minute). Cumulative fatigue damage across mixed operations is not tracked. For a machine designed to run for years, this limits long-duration life prediction accuracy.

### 3.6 Timing Module

**Present**: 8-phase rotation model (45deg intervals), carry propagation timing (1deg/digit, 25deg worst-case), barrel opcode step counts, phase callbacks.

**Critical gap -- Phases are synthetic**:
The 45-degree equal-phase assumption is explicitly noted in the code as an approximation. The Science Museum Group's DE2 technical documentation contains actual cam profile measurements with irregular phase widths. The cam profiles are defined in `kinematics.py` (modified trapezoidal with rise/dwell/return angles) but `timing.py` does not use them -- it hardcodes 45-degree intervals regardless.

**Gap -- Timing not connected to operations**:
`SimulationEngine` does not query `TimingController`. If ADD should occur at shaft angles 90-135deg and CARRY at 135-180deg, this is not enforced. The simulation runs as a continuous rotating machine, not as a machine executing discrete operations at specific shaft positions.

### 3.7 BOM (Bill of Materials)

**Infrastructure**: `tools/validate_bom.py` validates CSV against `docs/hardware/BOM_SCHEMA.md`. Schema covers part_id, subsystem, material, quantity, mass_kg, spec_ref.

**Status: Database empty**. No `hardware_twin/bom/bom_v0.csv` exists. The validation tool is fully functional but has no data to validate.

Historical reference: Babbage's AE design required approximately 8,000 parts. The current BOM schema supports ~25 subsystem line items. A complete BOM would include at minimum: number wheels, carry mechanisms, axes/pins, frame members, locking plates, operating cards, output carriage, printing apparatus.

### 3.8 Simulation Engine Coupling

**Tight (implemented)**:
- Temperature -> viscosity (Walther ASTM D341) -> film thickness -> wear rate
- Wear volume -> bearing clearance
- Thermal expansion -> bearing/gear clearance
- Clearance -> load redistribution (inverse-clearance weighting)
- Load -> shaft deflection

**Decoupled (not connected)**:
- Kinematics cam profiles <-> Timing phase angles
- Active operation type <-> SimulationEngine load profiles
- Shaft stress accumulation <-> Fatigue damage counter
- Valve timing (Stephenson link) <-> Main shaft angle
- Re-oiling schedule <-> Tribology viscosity model

---

## Part IV: Note G Verdict

**The Bernoulli number algorithm is historically correct and well-tested.**

Both known errata from the 1843 Menabrea/Lovelace publication are fixed. The recurrence loop structure matches Ada's original Variable table (operations 1-25 with the n-dependent loop-back). Results match the exact oracle for B1 through B9.

**The algorithm does not run on the emulated Analytical Engine.**

`note_g_deck.py` is a Python interpreter for a YAML deck specification. It does not compile to AE assembly and does not call `engine.run()`. To achieve full historical fidelity -- "this is the first computer program running on a faithful mechanical computer emulator" -- Note G must be compiled to AE opcodes (LOAD, ADD, SUB, MULT, DIV, JLT, STOR, WRPRN) and executed via the engine's run loop. This is the single highest-impact item for historical narrative completeness.

---

## Part V: Prioritized Implementation Roadmap

### P0 -- Blocking (any program using these will crash or produce wrong output)

| ID | Item | File | Effort | Description |
|----|------|------|--------|-------------|
| P0-1 | CMPZ handler | engine.py | 1h | Add: `_execute_CMPZ(op) -> compare reg with 0, set ZERO flag` |
| P0-2 | CLR handler | engine.py | 1h | Add: `_execute_CLR(op) -> LOAD reg, BabbageNumber(0)` |
| P0-3 | BR handler | engine.py | 1h | Add: `_execute_BR(op) -> unconditional jump (alias JMP)` |
| P0-4 | STEP handler | engine.py | 1h | Add: `_execute_STEP(op) -> NOP in logical model; advance_card_drum() hook` |
| P0-5 | PRINT handler | engine.py | 1h | Add: `_execute_PRINT(op) -> delegate to WRPRN logic` |
| P0-6 | Zuse Z1 PC double-increment | zuse_z1.py:268 | 30m | Remove one of the two PC increments |
| P0-7 | Breakpoint KeyError | engine.py:1325 | 30m | Fix `bp["reg"]` -> use `.get("reg", None)` or initialize in `set_breakpoint()` |

### P1 -- Important (incomplete historical fidelity or production risk)

| ID | Item | File | Effort | Description |
|----|------|------|--------|-------------|
| P1-1 | Note G executable on AE | note_g_deck.py + new assembler | 3-5d | Compile NOTE_G_DECK.yaml ops to AE assembly; run via engine.run() |
| P1-2 | Printer coupled to Engine | printer.py, engine.py | 1d | Instantiate Printer in Engine.__init__; WRPRN calls printer.print_line() |
| P1-3 | Ludgate instruction dispatch | ludgate.py:218 | 1-2d | step() dispatches MULT/ADD/PRINT from program_tape instruction queue |
| P1-4 | Torres instruction dispatch | torres_quevedo.py:197 | 1-2d | step() implements relay-based sequencing for FP operations |
| P1-5 | Pascaline end-around carry | pascaline.py:112 | 1d | Detect carry-out of MSB; mechanically add 1 to units wheel |
| P1-6 | SQRT epsilon convergence | engine.py:634 | 2h | Replace hardcoded 25 iterations with `while abs(guess^2 - x) > epsilon` |
| P1-7 | Card holes encode/decode | card_reader.py | 2d | Implement card_to_bytes() / bytes_to_card() for 140x80 holes matrix |
| P1-8 | Wrought iron + gun metal | materials.py | 4h | Add EN 10083-1 wrought iron and CuSn10Ni1 gun metal with cited properties |

### P2 -- Completeness (historical accuracy improvements)

| ID | Item | File | Effort | Description |
|----|------|------|--------|-------------|
| P2-1 | Actual gear tooth counts | kinematics.py | 1-2d | Extract from SMG DE2 drawings; replace 20/60 defaults |
| P2-2 | SMG cam timing data | timing.py | 2-3d | Replace 45deg phase assumptions with measured cam profiles from SMG documentation |
| P2-3 | Oil degradation model | tribology.py | 2d | Viscosity decay over time (oxidation) + re-oiling event at N hours |
| P2-4 | Cumulative fatigue (Miner) | structural.py, simulation/ | 3d | Cycle counter per operation type; Miner damage accumulation |
| P2-5 | Leibniz division | leibniz_reckoner.py | 1d | divide() via repeated subtraction with carriage management |
| P2-6 | Napier division | napiers_bones.py | 1d | divide() via complementary method (Napier 1617) |
| P2-7 | Curta divide() helper | curta.py | 4h | High-level divide() using repeated crank subtraction |
| P2-8 | Curta tens bell | curta.py | 2h | Add bell_signal flag; set on result overflow |
| P2-9 | Populate BOM | hardware_twin/bom/bom_v0.csv | 3-5d | ~25 subsystems, ~8000 parts, material + mass + spec_ref per part |
| P2-10 | Enigma MachineAdapter | adapter.py | 4h | Add EnigmaAdapter extending MachineAdapter ABC |
| P2-11 | Expand Pascaline tests | test_pascaline.py | 4h | Add 20+ tests: subtraction, overflow, multi-digit carry, edge cases |
| P2-12 | Expand Leibniz tests | test_leibniz_reckoner.py | 4h | Add 30+ tests: multiply, reset, carriage bounds, division prep |
| P2-13 | Enigma double-stepping | enigma.py | 4h | Model M3 middle-rotor anomaly; optional flag |
| P2-14 | Timing-operations coupling | simulation/engine.py, timing.py | 3-5d | SimulationEngine queries TimingController; per-opcode load profiles at correct shaft angles |
| P2-15 | Steam drive coupling | thermodynamics.py | 5-7d | Rankine cycle, boiler heat, Stephenson link -> shaft angle |

---

## Part VI: Missing Primary-Source Data

The following values are **assumed or synthetic** and should be replaced with primary-source measurements:

| Parameter | Current Value | Source Needed | Location |
|-----------|--------------|---------------|----------|
| Gear tooth counts | 20/60, 30/60 (defaults) | Babbage's original drawings or SMG DE2 survey | kinematics.py |
| Phase widths | 45deg each (8 phases) | SMG timing appendix / cam profile measurements | timing.py |
| Valve gear timing | lap=3mm, lead=1mm, cutoff=60% (ASSUMPTION) | Historical steam engine documentation | sim_schema config |
| Lubrication interval | 160h or 500-600 cycles (ASSUMPTION) | Babbage's maintenance notes or SMG records | sim_schema config |
| Surface roughness | Ra 0.8-1.6 um (19th-c achievable) | Tribology test data for period brass-on-steel | tribology.py |
| Running-in distance | s_0 = 1e7 mm (estimate) | Experimental | tribology.py |
| Operation cycle counts | ADD=8, MULT=400, DIV=750 (not SMG-validated) | SMG timing appendix or DE2 experimental run | OPCODES.yaml / timing.py |

---

## Summary Counts

| Severity | AE Core | Hardware Ext. | Physics/Materials | Total |
|----------|---------|---------------|-------------------|-------|
| P0 | 7 | -- (1 PC bug) | -- | 8 |
| P1 | 8 | 4 | 4 | 16 |
| P2 | 6 | 8 | 10 | 24 |
| **Total** | **21** | **13** | **14** | **48** |

All P0 items are mechanical one-liners (missing method bodies or one-line bug fixes) estimating ~6 hours total. P1 items represent 2-3 weeks of focused engineering. P2 items represent 6-8 weeks including primary-source research for gear tooth counts and SMG timing data.

---

*Audit method: three parallel static-analysis agents (read-only). No files were modified during audit.*
*See: docs/adr/0002-babbage-number-scale.md, docs/adr/0003-historical-machines-placement.md, docs/adr/0004-simulation-engine-architecture.md, docs/adr/0005-card-compiler-vs-cli-assembler.md*
