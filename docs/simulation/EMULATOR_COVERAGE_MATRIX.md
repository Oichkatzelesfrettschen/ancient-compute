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
- Status: missing
- Target: Tier 1
- Assets: historical notes only
- Notes: needs ledger + seal model

Counting rods / abacus
- Status: missing
- Target: Tier 1 -> Tier 2
- Assets: content schema references, no emulator
- Notes: start with decimal carry/borrow model

Antikythera mechanism
- Status: missing
- Target: Tier 2
- Assets: content schema mentions AntikytheraSimulator
- Notes: gear ratios + dials, needs source data

Quipu / khipu
- Status: missing
- Target: Tier 1
- Assets: history sources list only
- Notes: knot encoding and ledger queries

Pascaline
- Status: missing
- Target: Tier 2
- Assets: sources list only
- Notes: wheel/carry simulation

Leibniz stepped reckoner
- Status: missing
- Target: Tier 2
- Assets: sources list only
- Notes: stepped drum + carriage shift

Jacquard loom (card control)
- Status: missing
- Target: Tier 1
- Assets: card format references in Babbage emulator
- Notes: use card deck -> pattern row output

Babbage Analytical Engine (base)
- Status: implemented
- Target: Tier 3
- Assets: backend/src/emulator/analytical_engine.py
- Notes: op set includes historical + extensions; needs tier gating

Ada Lovelace Bernoulli deck
- Status: spec-only
- Target: Tier 3
- Assets: Menabrea 1843 PDF + Fourmilab Note G images; no canonical deck fixture
- Notes: create test deck and expected outputs

Difference Engine No. 2 (DE2)
- Status: implemented
- Target: Tier 2
- Assets: backend/src/emulator/*, tests in backend/tests
- Notes: docs should reflect implementation state

Slide rule
- Status: missing
- Target: Tier 1
- Assets: none
- Notes: log scale approximation model

Astrolabe
- Status: missing
- Target: Tier 1 -> Tier 2
- Assets: none
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
- No emulator modules for non-Babbage mechanisms.
- No shared emulator interface for logic-only models.
- No canonical fixtures for Ada Lovelace Note G program.

Recommended next step
- Define a shared emulator interface (step(), run(), state(), reset())
- Implement Tier 1 logic models for tally/tokens/abacus/quipu as fast wins
