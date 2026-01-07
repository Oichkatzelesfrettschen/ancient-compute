# Emulator Spec: Antikythera Mechanism

## 1. Mechanism Summary
- Name: Antikythera mechanism
- Era: Hellenistic (ca. 2nd century BCE)
- Geography/Culture: Greek
- Primary purpose: Astronomical cycle computation
- Physical substrate: Bronze gear trains with dials

## 2. Source Basis
- Primary sources (citations + links): AMRP reports (pending)
- Secondary sources (citations + links): Freeth et al. Nature (paywalled)
- Local cache files: none
- Access limitations: AMRP site is JS-only; museum object pages needed

## 3. Fidelity Tier
- Target tier: Tier 2
- Rationale: gear ratios and timing are essential
- Known anachronisms: simplified calendar alignment

## 4. Logic Graph
- [Days elapsed] -> (rotate) -> {gear_train}
- {gear_train} -> (propagate ratios) -> {dial_positions} -> [dial_readouts]

## 5. State Model
- State variables: gear angles, dial pointers, phase offsets
- Invariants: gear ratios preserved
- Initialization: dial zero positions

## 6. Operations
- Supported operations: advance_days, read_dial
- Operation semantics: ratio-propagated rotation
- Error handling: out-of-range dates -> clamp or error

## 7. Inputs and Outputs
- Input formats: integer days or date
- Output formats: dial positions, cycle indices
- Determinism guarantees: strict

## 8. Timing / Constraints (Tier 2+)
- Cycle model: one crank step per day
- Mechanical limits: gear backlash (optional)

## 9. Test Vectors
- 1 synodic month -> lunar phase cycle
- 235 months -> Metonic alignment

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/antikythera.py
- API surface (step/run/state/reset): advance_days, state(), reset()

## 11. Validation Checklist
- Sources cited: no
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
