# Emulator Spec: Tally Marks / Tally Sticks

## 1. Mechanism Summary
- Name: Tally marks / tally sticks
- Era: Prehistoric to medieval
- Geography/Culture: Widespread (Europe, Africa, Near East)
- Primary purpose: Counting and ledgering
- Physical substrate: Notches/marks on bone or wood

## 2. Source Basis
- Primary sources (citations + links): Ishango bone dataset (see below)
- Secondary sources (citations + links): TBD
- Local cache files:
  - docs/sources/cache/Ishango.zip
- Access limitations: museum object pages need updated URLs

## 3. Fidelity Tier
- Target tier: Tier 1
- Rationale: discrete mark logic, no timing constraints needed
- Known anachronisms: uniform group-of-five representation

## 4. Logic Graph
- ASCII graph (inputs -> ops -> state -> outputs):
  - [Event] -> (add_mark) -> {marks} -> (group_by_five) -> [tally_string]

## 5. State Model
- State variables: mark count, grouped marks
- Invariants: count >= 0
- Initialization: count = 0

## 6. Operations
- Supported operations: add_mark, remove_mark, count
- Operation semantics: idempotent removal at 0
- Error handling: underflow -> no-op

## 7. Inputs and Outputs
- Input formats: integer delta (+/-)
- Output formats: integer count, tally string
- Determinism guarantees: strict

## 8. Timing / Constraints (Tier 2+)
- Not modeled

## 9. Test Vectors
- 0 -> ""
- 4 -> "||||"
- 5 -> "|||||" (or "||||-")

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/tally_marks.py
- API surface (step/run/state/reset): step(delta), state(), reset()

## 11. Validation Checklist
- Sources cited: partial
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
