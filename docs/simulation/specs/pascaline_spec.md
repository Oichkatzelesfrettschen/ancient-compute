# Emulator Spec: Pascaline

## 1. Mechanism Summary
- Name: Pascaline
- Era: 1640s
- Geography/Culture: France
- Primary purpose: Addition/subtraction
- Physical substrate: Gear wheels with carry

## 2. Source Basis
- Primary sources (citations + links): Pascal 1645 description (missing)
- Secondary sources (citations + links): CNAM collection metadata
- Local cache files:
  - docs/sources/cache/CNAM_Pascaline_ccProxy_search.json
- Access limitations: museum object scans needed

## 3. Fidelity Tier
- Target tier: Tier 2
- Rationale: carry mechanics and wheel timing
- Known anachronisms: simplified carry pawls

## 4. Logic Graph
- [Digit input] -> (rotate_wheel) -> {wheels} -> (carry_forward) -> [display]

## 5. State Model
- State variables: wheel digits, carry levers
- Invariants: digits 0-9
- Initialization: zeroed wheels

## 6. Operations
- Supported operations: add, sub (via complement)
- Operation semantics: mechanical carry chain
- Error handling: overflow -> flag

## 7. Inputs and Outputs
- Input formats: integer addends
- Output formats: digit display
- Determinism guarantees: strict

## 8. Timing / Constraints (Tier 2+)
- Optional: time per wheel rotation

## 9. Test Vectors
- 59 + 1 = 60
- 99 + 1 = 100

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/pascaline.py
- API surface (step/run/state/reset): add, state(), reset()

## 11. Validation Checklist
- Sources cited: partial
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
