# Emulator Spec: Counting Rods / Abacus

## 1. Mechanism Summary
- Name: Counting rods / abacus
- Era: Ancient to early modern
- Geography/Culture: China and East Asia
- Primary purpose: Arithmetic computation
- Physical substrate: Rods or beads on columns

## 2. Source Basis
- Primary sources (citations + links): Nine Chapters, Sunzi Suan Jing
- Secondary sources (citations + links): TBD
- Local cache files:
  - docs/sources/cache/ctext_nine_chapters.html
  - docs/sources/cache/ctext_sunzi_suan_jing.html
- Access limitations: need scans of original manuals

## 3. Fidelity Tier
- Target tier: Tier 1 -> Tier 2
- Rationale: logic model first, add timing later
- Known anachronisms: base-10 only

## 4. Logic Graph
- [Number] -> (set_value) -> {columns}
- [Op + Operand] -> (apply op) -> {columns} -> (carry) -> [result]

## 5. State Model
- State variables: digit columns, carry flags
- Invariants: digits 0-9 per column
- Initialization: all zeros

## 6. Operations
- Supported operations: add, sub, mul (repeated add), div (repeated sub)
- Operation semantics: column-wise with carry/borrow
- Error handling: borrow across empty columns

## 7. Inputs and Outputs
- Input formats: integer operands
- Output formats: column array and integer
- Determinism guarantees: strict

## 8. Timing / Constraints (Tier 2+)
- Optional: steps per column move

## 9. Test Vectors
- 789 + 456 = 1245
- 1000 - 1 = 999

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/abacus.py
- API surface (step/run/state/reset): add/sub, state(), reset()

## 11. Validation Checklist
- Sources cited: partial
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
