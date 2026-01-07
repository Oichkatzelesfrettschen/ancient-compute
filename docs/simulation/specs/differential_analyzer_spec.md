# Emulator Spec: Differential Analyzer / Planimeter (Optional)

## 1. Mechanism Summary
- Name: Differential analyzer / planimeter
- Era: late 19th to early 20th century
- Geography/Culture: Europe/US
- Primary purpose: analog integration/differential equation solving
- Physical substrate: rotating disks, integrators

## 2. Source Basis
- Primary sources (citations + links): TBD
- Secondary sources (citations + links): TBD
- Local cache files: none
- Access limitations: sources not acquired

## 3. Fidelity Tier
- Target tier: Tier 1 -> Tier 2
- Rationale: continuous integration model first
- Known anachronisms: idealized integrator

## 4. Logic Graph
- [Input curve] -> (integrate) -> {state} -> [area/result]

## 5. State Model
- State variables: integrator state, disk angle
- Invariants: bounded rotation
- Initialization: zero

## 6. Operations
- Supported operations: integrate, reset
- Operation semantics: continuous time step
- Error handling: out-of-range input

## 7. Inputs and Outputs
- Input formats: function samples
- Output formats: integrated value
- Determinism guarantees: deterministic step size

## 8. Timing / Constraints (Tier 2+)
- Optional: step size and drift model

## 9. Test Vectors
- Integrate constant 1 over [0,1] -> 1

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/differential_analyzer.py
- API surface (step/run/state/reset): integrate/step

## 11. Validation Checklist
- Sources cited: no
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
