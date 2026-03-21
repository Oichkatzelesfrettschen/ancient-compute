# Emulator Coverage Matrix

Purpose: map each historical mechanism to current emulator coverage and target fidelity.

Fidelity tiers
- Tier 0: Concept sketch only (no executable logic)
- Tier 1: Logic model (state + ops) with deterministic tests
- Tier 2: Mechanical simulation (timing/constraints modeled)
- Tier 3: Full ISA or historically faithful execution model

Legend
- Status: implemented | partial | spec-only | missing

-------------------------------------------------------------------------------

Mechanism | Status | Target Tier | Current Assets | Notes

Tally marks / tally sticks
- Status: implemented
- Target: Tier 1
- Assets: backend/src/emulator/tally_marks.py, backend/tests/unit/test_tally_marks.py
- Notes: logic graph in docs/simulation/LOGIC_GRAPHS.md

Clay tokens / bullae
- Status: implemented
- Target: Tier 1
- Assets: backend/src/emulator/clay_tokens.py, backend/tests/unit/test_clay_tokens.py
- Notes: seal/audit logic modeled

Counting rods / abacus
- Status: implemented
- Target: Tier 1 -> Tier 2
- Assets: backend/src/emulator/abacus.py, backend/tests/unit/test_abacus.py
- Notes: decimal carry/borrow model

Antikythera mechanism
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/antikythera.py, backend/tests/unit/test_antikythera_gears.py, docs/simulation/specs/antikythera_spec.md
- Notes: full 7-dial model (Metonic/Saros/Exeligmos/Olympiad/Lunar/Draconic/Solar); historically sourced Freeth 2006 gear ratios; MachineAdapter: AntikytheraAdapter

Quipu / khipu
- Status: implemented
- Target: Tier 1
- Assets: backend/src/emulator/quipu.py, backend/tests/unit/test_quipu.py, backend/src/emulator/quipu_kfg/*, backend/tests/unit/test_quipu_kfg_emulator.py
- Notes: includes Khipu Field Guide XLSX normalization pipeline and a dataset-driven emulator

Napier's Bones
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/napiers_bones.py, backend/tests/unit/test_napiers_bones.py
- Notes: lattice multiplication, divide(); MachineAdapter: NapierAdapter

Pascaline
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/pascaline.py, backend/tests/unit/test_pascaline.py
- Notes: 6-wheel carry chain with sautoir; end-around subtraction; MachineAdapter: PascalineAdapter

Leibniz stepped reckoner
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/leibniz_reckoner.py, backend/tests/unit/test_leibniz_reckoner.py
- Notes: Staffelwalze drum mechanism; divide(); MachineAdapter: LeibnizAdapter

Thomas Arithmometer
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/thomas_arithmometer.py, backend/tests/unit/test_thomas_arithmometer.py
- Notes: pinwheel add/subtract/multiply; MachineAdapter: ThomasArithometerAdapter

Scheutz Difference Engine
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/scheutz.py, backend/tests/unit/test_scheutz.py
- Notes: 4-register difference engine with tabulate(); MachineAdapter: ScheutzAdapter

Grant Difference Engine
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/grant_difference_engine.py, backend/tests/unit/test_grant_difference_engine.py
- Notes: n-register polynomial tabulation; MachineAdapter: GrantDEAdapter

Odhner Arithmometer / Brunsviga
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/odhner_arithmometer.py, backend/tests/unit/test_odhner_arithmometer.py
- Notes: variable-toothed pinwheels; MachineAdapter: OdhnerAdapter

Hollerith Tabulating Machine
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/hollerith_tabulator.py, backend/tests/unit/test_hollerith_tabulator.py
- Notes: punched-card counter grid; read/tabulate/sort; MachineAdapter: HollerithAdapter

Millionaire Calculator
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/millionaire_calculator.py, backend/tests/unit/test_millionaire_calculator.py
- Notes: lookup-table direct multiply; divide(); MachineAdapter: MillionaireAdapter

Ludgate Analytical Machine
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/ludgate.py, backend/tests/unit/test_ludgate.py
- Notes: Irish multiplication via index tables; step() dispatches program tape; MachineAdapter: LudgateAdapter

Torres Quevedo electromechanical calculator
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/torres_quevedo.py, backend/tests/unit/test_torres_quevedo.py
- Notes: relay-based floating-point arithmetic; step() dispatches program tape; MachineAdapter: TorresQuevedoAdapter

Enigma cipher machine
- Status: implemented
- Target: Tier 3
- Assets: backend/src/emulator/enigma.py, backend/tests/unit/test_enigma.py
- Notes: 3/4-rotor, double-stepping anomaly, reflector; MachineAdapter: EnigmaAdapter

Turing Bombe
- Status: implemented
- Target: Tier 3
- Assets: backend/src/emulator/bombe.py, backend/tests/unit/test_bombe.py
- Notes: BFS menu-graph attack; 26^3 position scan; self-reciprocal scrambler lookups; MachineAdapter: BombeAdapter

Zuse Z1
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/zuse_z1.py, backend/tests/unit/test_zuse_z1.py
- Notes: 22-bit mechanical floating point; MachineAdapter: ZuseZ1Adapter

Zuse Z3
- Status: implemented
- Target: Tier 3
- Assets: backend/src/emulator/zuse_z3.py, backend/tests/unit/test_zuse_z3.py
- Notes: 22-bit relay floating point; 64-word memory; LOAD/STORE/ADD/SUB/MULT/DIV/SQRT; MachineAdapter: ZuseZ3Adapter

Colossus
- Status: implemented
- Target: Tier 3
- Assets: backend/src/emulator/colossus.py, backend/tests/unit/test_colossus.py
- Notes: Lorenz SZ42 chi/psi/motor wheels; chi-breaking via XOR count peaks; MachineAdapter: ColossusAdapter

Harvard Mark I (IBM ASCC)
- Status: implemented
- Target: Tier 3
- Assets: backend/src/emulator/harvard_mark_i.py, backend/tests/unit/test_harvard_mark_i.py
- Notes: 72 counters + 60 read-only constants; 23-digit decimal; branch on last_result; MachineAdapter: HarvardMarkIAdapter

ENIAC
- Status: implemented
- Target: Tier 3
- Assets: backend/src/emulator/eniac.py, backend/tests/unit/test_eniac.py
- Notes: 20 decimal accumulators; ADD/SUB/MULT/DIV/SQRT/CLEAR/LOAD/PRINT/HALT; MachineAdapter: ENIACAdapter

Manchester Baby (SSEM)
- Status: implemented
- Target: Tier 3
- Assets: backend/src/emulator/manchester_baby.py, backend/tests/unit/test_manchester_baby.py
- Notes: 32-word 32-bit CRT store; 8 Baby instructions; first stored-program computer; MachineAdapter: ManchesterBabyAdapter

EDSAC
- Status: implemented
- Target: Tier 3
- Assets: backend/src/emulator/edsac.py, backend/tests/unit/test_edsac.py
- Notes: 512-word 17-bit mercury delay-line store; accumulator + multiplier register; full ISA; MachineAdapter: EDSACAdapter

Curta Type I/II
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/curta.py, backend/tests/unit/test_curta.py
- Notes: Leibniz-type stepped drums; divide() via repeated subtraction; tens-complement bell; MachineAdapter: CurtaAdapter

Jacquard loom (card control)
- Status: implemented
- Target: Tier 1
- Assets: backend/src/emulator/jacquard.py, backend/tests/unit/test_jacquard.py
- Notes: card->hook selection model

Babbage Analytical Engine (base)
- Status: implemented
- Target: Tier 3
- Assets: backend/src/emulator/analytical_engine.py
- Notes: op set includes historical + extensions; needs tier gating

Ada Lovelace Bernoulli deck
- Status: implemented
- Target: Tier 3
- Assets: docs/simulation/NOTE_G_DECK.yaml, backend/src/emulator/note_g_deck.py
- Notes: deck runner supports n-series, loop semantics partial

Ada Lovelace extensions beyond Note G
- Status: missing
- Target: Tier 1 -> Tier 2
- Assets: docs/simulation/ADA_EXTENSIONS.md
- Notes: extract algorithm steps from Notes A-F/H/I where applicable

Difference Engine No. 2 (DE2)
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/*, tests in backend/tests
- Notes: docs should reflect implementation state

Slide rule
- Status: implemented
- Target: Tier 1
- Assets: backend/src/emulator/slide_rule.py, backend/tests/unit/test_slide_rule.py
- Notes: log scale approximation model

Astrolabe
- Status: implemented
- Target: Tier 1 -> Tier 2
- Assets: backend/src/emulator/astrolabe.py, backend/tests/unit/test_astrolabe_table.py, docs/simulation/specs/astrolabe_spec.md, docs/sources/astrolabe/reference_table.json
- Notes: table-driven placeholder; replace with extracted primary tables

-------------------------------------------------------------------------------

Anachronistic / unconventional extensions (non-ancient, optional scope)

Cellular automata (Game of Life)
- Status: missing
- Target: Tier 1
- Assets: external PDFs in ~/Documents/AGL_Library
- Notes: baseline CA step engine + fixtures

DNA computing (strand displacement)
- Status: missing
- Target: Tier 1
- Assets: external PDFs in ~/Documents/AGL_Library
- Notes: model as reaction rules + concentration state

Reaction-diffusion computing
- Status: missing
- Target: Tier 1 -> Tier 2
- Assets: external PDFs in ~/Documents/AGL_Library
- Notes: grid-based reaction/diffusion solver

Slime mold computing (Physarum)
- Status: missing
- Target: Tier 1
- Assets: external PDFs in ~/Documents/AGL_Library
- Notes: graph growth + reinforcement model

Reservoir computing (neuromorphic/physical)
- Status: missing
- Target: Tier 1
- Assets: external PDFs in ~/Documents/AGL_Library
- Notes: state update + readout layer

Membrane computing (P systems)
- Status: missing
- Target: Tier 1
- Assets: external PDFs in ~/Documents/AGL_Library
- Notes: multiset rewrite + membrane tree

In-materio computing
- Status: missing
- Target: Tier 0 -> Tier 1
- Assets: external PDFs in ~/Documents/AGL_Library
- Notes: stimulus/response abstraction only

-------------------------------------------------------------------------------

Immediate gaps (as of 2026-03-20)
- EDSAC: P instruction (indexed addressing) not yet tested; output teleprinter encoding is approximate.
- Colossus: Motor-wheel limiter logic not modeled (psi stepping accuracy).
- AE Note G: DIV fractional precision lost in _execute_DIV_micro (integer-only); tracked in DEFERRED_WORK.md.
- Astrolabe: table-driven placeholder; primary-source derived tables not yet integrated.
- Ada Notes A-F: algorithm extraction incomplete; see ADA_EXTENSIONS.md.

Recommended next step
- AE DIV micro-op: replace int()/int() with BabbageNumber.__truediv__ to restore fractional precision.
- Astrolabe: integrate primary-source astrolabe tables (Chaucer/Al-Zarqali).
- Ada Notes A-F: extract arithmetic algorithms from historical notes into .ae assembly programs.
