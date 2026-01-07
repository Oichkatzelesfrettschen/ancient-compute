# Emulator Spec: Quipu / Khipu

## 1. Mechanism Summary
- Name: Quipu (khipu)
- Era: Inca and pre-Inca
- Geography/Culture: Andes
- Primary purpose: Ledger and record keeping
- Physical substrate: Cord tree with knots

## 2. Source Basis
- Primary sources (citations + links): Khipu Field Guide datafiles (Internet Archive)
- Secondary sources (citations + links): Urton (book, paywalled)
- Local cache files:
  - docs/sources/cache/quipu/KFGdatafiles/KFGdatafiles_meta.xml
  - docs/sources/cache/quipu/KFGdatafiles/KFGdatafiles_files.xml
  - docs/sources/cache/quipu/KFGdatafiles/QU001.xlsx
  - docs/sources/cache/quipu/KFGdatafiles/UR001.xlsx
- Access limitations: full dataset not cached

## 3. Fidelity Tier
- Target tier: Tier 1
- Rationale: symbolic encoding/decoding first
- Known anachronisms: simplified knot taxonomy

## 4. Logic Graph
- [Record] -> (encode_value) -> {cord_tree}
- [Query] -> (decode_value) -> {cord_tree} -> [ledger_entry]

## 5. State Model
- State variables: cords, knot type/position, color metadata
- Invariants: position ordering within cord
- Initialization: empty cord tree

## 6. Operations
- Supported operations: encode_number, decode_number, sum_by_category
- Operation semantics: decimal positional knots
- Error handling: unknown knot type -> invalid

## 7. Inputs and Outputs
- Input formats: category + integer
- Output formats: decoded value list, category totals
- Determinism guarantees: strict

## 8. Timing / Constraints (Tier 2+)
- Not modeled

## 9. Test Vectors
- Encode 123 -> decode 123
- Sum two cords in same category

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/quipu.py
- API surface (step/run/state/reset): encode/decode, state(), reset()

## 11. Validation Checklist
- Sources cited: partial
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
