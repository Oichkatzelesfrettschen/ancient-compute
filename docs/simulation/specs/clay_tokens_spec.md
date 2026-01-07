# Emulator Spec: Clay Tokens / Bullae

## 1. Mechanism Summary
- Name: Clay tokens / bullae
- Era: Neolithic to early writing systems
- Geography/Culture: Mesopotamia and Near East
- Primary purpose: Accounting and inventory
- Physical substrate: Clay tokens sealed in clay envelopes

## 2. Source Basis
- Primary sources (citations + links): Schmandt-Besserat (book scans TBD)
- Secondary sources (citations + links): museum catalogs TBD
- Local cache files: none
- Access limitations: primary books paywalled

## 3. Fidelity Tier
- Target tier: Tier 1
- Rationale: symbolic token counts and seals
- Known anachronisms: simplified token taxonomy

## 4. Logic Graph
- [Transaction] -> (add/remove token) -> {token_bag} -> (seal) -> {sealed_bulla}
- {sealed_bulla} -> (impress) -> [impression_ledger]

## 5. State Model
- State variables: token multiset, sealed flag, impression ledger
- Invariants: sealed bag cannot mutate
- Initialization: empty bag, sealed = false

## 6. Operations
- Supported operations: add_token, remove_token, seal, impress, audit
- Operation semantics: audit compares token multiset vs ledger
- Error handling: mutation blocked when sealed

## 7. Inputs and Outputs
- Input formats: token type + quantity
- Output formats: ledger summary, audit pass/fail
- Determinism guarantees: strict

## 8. Timing / Constraints (Tier 2+)
- Not modeled

## 9. Test Vectors
- Deposit 3 sheep, 2 grain -> ledger shows 3/2
- Seal then modify -> rejected

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/clay_tokens.py
- API surface (step/run/state/reset): step(txn), state(), reset()

## 11. Validation Checklist
- Sources cited: partial
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
