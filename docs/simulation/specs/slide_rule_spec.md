# Emulator Spec: Slide Rule

## 1. Mechanism Summary
- Name: Slide rule
- Era: 17th century onward
- Geography/Culture: Europe
- Primary purpose: Multiplication/division via logarithms
- Physical substrate: Logarithmic scales on sliding rulers

## 2. Source Basis
- Primary sources (citations + links): Oughtred 1632
- Secondary sources (citations + links): museum catalogs TBD
- Local cache files:
  - docs/sources/cache/Oughtred_Circles_of_Proportion_1632.pdf
- Access limitations: museum object scans needed

## 3. Fidelity Tier
- Target tier: Tier 1
- Rationale: logic model with error bounds
- Known anachronisms: perfect log scale

## 4. Logic Graph
- [Align x] -> (offset=log(x)) -> {scale_state}
- {scale_state} -> (read at y) -> [approx result]

## 5. State Model
- State variables: scale offset
- Invariants: offset in log space
- Initialization: offset = 0

## 6. Operations
- Supported operations: multiply, divide
- Operation semantics: add/sub logs
- Error handling: x<=0 invalid

## 7. Inputs and Outputs
- Input formats: positive floats
- Output formats: approximate float
- Determinism guarantees: deterministic rounding

## 8. Timing / Constraints (Tier 2+)
- Not modeled

## 9. Test Vectors
- 2 * 3 ~= 6
- 10 / 2 ~= 5

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/slide_rule.py
- API surface (step/run/state/reset): multiply/divide

## 11. Validation Checklist
- Sources cited: partial
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
