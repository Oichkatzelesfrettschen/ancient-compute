Errata
- Track corrections and disputes.
- Note: Plimpton 322 interpretation disputed (Robson vs right-triangle tables).
- Note: I Ching binary linkage is symbolic, not computational; mark accordingly.

## Note G (Lovelace 1843) -- Two Known Errata

The original Note G diagram in Menabrea/Lovelace "Sketch of the Analytical
Engine" (1843) contains two errors discovered when the algorithm was first
implemented on actual hardware in the modern era.

### 1. Operation 4: Division Operand Order

The diagram shows `2V5 / 2V4` but the "Statement of Results" column says
`(2n-1)/(2n+1)`. At this point V4 = 2n-1 and V5 = 2n+1, so the correct
operation is `V4 / V5`. The printed `V5 / V4` yields `(2n+1)/(2n-1)` --
the reciprocal of the intended value.

Likely a typesetting error rather than a conceptual mistake by Lovelace.

Fix: `NOTE_G_DECK.yaml` op 4 uses `lhs: V4, rhs: V5`.

### 2. Operation 24: Missing Negation

The recurrence relation (equation 9) is:
  0 = A0 + A1*B1 + A3*B3 + ... + B_{2n-1}

Therefore: B_{2n-1} = -(A0 + A1*B1 + A3*B3 + ...)

V13 accumulates the sum (A0 + A1*B1 + ...). Op 24 should negate V13
when storing to V24, but the diagram shows addition (+), which copies
V13 directly without negation.

Fix: `NOTE_G_DECK.yaml` op 24 uses `opcode: sub, lhs: V24, rhs: V13`.

### References

- Glaschick, R. (2016). "Ada Lovelace's Calculation of Bernoulli's Numbers"
- Two-Bit History (2018). "What Did Ada Lovelace's Program Actually Do?"
- Sinclair Target, GitHub Gist: Note G in C
- Wikipedia: Note G
