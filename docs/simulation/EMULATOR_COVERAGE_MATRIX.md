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
- Status: spec-only
- Target: Tier 2
- Assets: docs/simulation/specs/antikythera_spec.md
- Notes: gear ratios + dials, needs source data

Quipu / khipu
- Status: implemented
- Target: Tier 1
- Assets: backend/src/emulator/quipu.py, backend/tests/unit/test_quipu.py
- Notes: simplified knot encoding/ledger

Pascaline
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/pascaline.py, backend/tests/unit/test_pascaline.py
- Notes: logic-only stub

Leibniz stepped reckoner
- Status: spec-only
- Target: Tier 2
- Assets: docs/simulation/specs/leibniz_spec.md
- Notes: stepped drum + carriage shift

Jacquard loom (card control)
- Status: spec-only
- Target: Tier 1
- Assets: docs/simulation/specs/jacquard_spec.md
- Notes: use card deck -> pattern row output

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
- Status: spec-only
- Target: Tier 1 -> Tier 2
- Assets: docs/simulation/specs/astrolabe_spec.md
- Notes: use simplified spherical model with reference tables

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

Immediate gaps
- No emulator modules for most non-Babbage mechanisms.
- Shared emulator interface defined; implementations pending.
- Note G deck fixture exists; loop semantics pending.

Recommended next step
- Define a shared emulator interface (step(), run(), state(), reset())
- Implement Tier 1 logic models for tally/tokens/abacus/quipu as fast wins
