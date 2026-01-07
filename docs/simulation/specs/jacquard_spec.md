# Emulator Spec: Jacquard Loom

## 1. Mechanism Summary
- Name: Jacquard loom (card control)
- Era: early 1800s
- Geography/Culture: France
- Primary purpose: programmable pattern weaving
- Physical substrate: punched cards + hook selection

## 2. Source Basis
- Primary sources (citations + links): Jacquard correspondence and loom additions
- Secondary sources (citations + links): museum catalogs TBD
- Local cache files:
  - docs/sources/cache/Jacquard_Addition_Loom_JSTOR_41326751.pdf
  - docs/sources/cache/Jacquard_Correspondence_Power_Loom.pdf
- Access limitations: catalog photography missing

## 3. Fidelity Tier
- Target tier: Tier 1
- Rationale: logic model for card -> pattern
- Known anachronisms: simplified hook count

## 4. Logic Graph
- [Card] -> (read_card) -> {hook_selection} -> (lift_warp) -> [pattern_row]

## 5. State Model
- State variables: hook positions, warp lift set
- Invariants: binary hook state
- Initialization: all hooks down

## 6. Operations
- Supported operations: read_card, select_hooks, insert_weft
- Operation semantics: holes -> lifted hooks
- Error handling: invalid card length -> error

## 7. Inputs and Outputs
- Input formats: card deck (bit rows)
- Output formats: pattern row (bit row)
- Determinism guarantees: strict

## 8. Timing / Constraints (Tier 2+)
- Not modeled

## 9. Test Vectors
- Alternating two-card deck yields striped pattern

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/jacquard.py
- API surface (step/run/state/reset): read/step

## 11. Validation Checklist
- Sources cited: partial
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
