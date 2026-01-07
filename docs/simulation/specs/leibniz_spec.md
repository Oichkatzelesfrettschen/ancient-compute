# Emulator Spec: Leibniz Stepped Reckoner

## 1. Mechanism Summary
- Name: Leibniz stepped reckoner
- Era: 1670s
- Geography/Culture: Germany
- Primary purpose: Add/multiply/divide
- Physical substrate: Stepped drum + carriage

## 2. Source Basis
- Primary sources (citations + links): Leibniz manuscripts (missing)
- Secondary sources (citations + links): CNAM collection metadata
- Local cache files:
  - docs/sources/cache/CNAM_Leibniz_ccProxy_search.json
- Access limitations: manuscript scans needed

## 3. Fidelity Tier
- Target tier: Tier 2
- Rationale: stepped drum mechanics
- Known anachronisms: simplified carriage shift

## 4. Logic Graph
- [Rotate N] -> (add_cycle) -> {accumulator}
- [Shift] -> (shift_carriage) -> {position} -> [result]

## 5. State Model
- State variables: stepped drum, accumulator, carriage offset
- Invariants: offset bounds
- Initialization: zeroed accumulator

## 6. Operations
- Supported operations: add_cycle, sub_cycle, shift
- Operation semantics: repeated add with shift
- Error handling: division remainder tracking

## 7. Inputs and Outputs
- Input formats: multiplicand, multiplier
- Output formats: accumulator value, remainder
- Determinism guarantees: strict

## 8. Timing / Constraints (Tier 2+)
- Optional: rotation time per digit

## 9. Test Vectors
- 123 * 45 via repeated add and shift
- 100 / 4 via repeated sub

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/leibniz_reckoner.py
- API surface (step/run/state/reset): rotate/shift

## 11. Validation Checklist
- Sources cited: partial
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
