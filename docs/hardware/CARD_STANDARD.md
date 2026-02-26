# Card Standard v0.1

**Version**: 0.1
**Date**: 2026-02-26
**Status**: DRAFT
**Charter ref**: S-3 (Card input system)

---

## 1. Physical Format

| Parameter | Value | Source |
|-----------|-------|--------|
| Width | 129 mm | ASSUMPTION: Based on historical Jacquard proportions |
| Height | 56 mm | ASSUMPTION: Based on historical Jacquard proportions |
| Data rows | 2 (upper and lower) | ASSUMPTION: Simplified from Jacquard multi-row |
| Alignment holes | 2, centered on short edges | ASSUMPTION: Common card registration method |
| Edge cuts | Top-right corner clipped for orientation | ASSUMPTION: Standard practice |
| Material | Stiff pasteboard, 0.2mm thick | SOURCE:SWADE-2000 (Jacquard card stock) |

## 2. Card Classes

Per Menabrea/Lovelace (SOURCE:MENABREA-1842), the Analytical Engine uses
three distinct card classes:

### 2.1 Operation Cards

Specify the arithmetic or control operation to perform.

| Field | Bits | Position | Description |
|-------|------|----------|-------------|
| Opcode | 4 | Row 0, cols 0-3 | Operation code (see OPCODES.yaml) |
| Modifier | 4 | Row 0, cols 4-7 | Operation modifier / addressing mode |
| Reserved | 8 | Row 0, cols 8-15 | Future use (must be 0) |

### 2.2 Variable Cards

Specify which store column to read from or write to.

| Field | Bits | Position | Description |
|-------|------|----------|-------------|
| Direction | 1 | Row 0, col 0 | 0 = read (store -> mill), 1 = write (mill -> store) |
| Column address | 10 | Row 0, cols 1-10 | Store column (0-1023) |
| Reserved | 5 | Row 0, cols 11-15 | Future use (must be 0) |

### 2.3 Number Cards

Load a literal constant into the mill or directly into a store column.

| Field | Bits | Position | Description |
|-------|------|----------|-------------|
| Sign | 1 | Row 0, col 0 | 0 = positive, 1 = negative |
| Magnitude | 50 digits | Row 0-1, cols 1-200 | BCD-encoded 50-digit decimal |

## 3. Opcode Table

See [OPCODES.yaml](OPCODES.yaml) for the canonical machine-readable table.

| Code | Mnemonic | Description | Card class |
|------|----------|-------------|------------|
| 0x0 | ADD | Add mill ingress to egress | Operation |
| 0x1 | SUB | Subtract mill ingress from egress | Operation |
| 0x2 | MUL | Multiply (multi-cycle via barrel) | Operation |
| 0x3 | DIV | Divide (multi-cycle via barrel) | Operation |
| 0x4 | MOV | Copy value between registers | Operation |
| 0x5 | CLR | Clear register to zero | Operation |
| 0x6 | CMPZ | Compare with zero, set condition | Operation |
| 0x7 | BR | Branch (conditional/unconditional) | Operation |
| 0x8 | STEP | Advance card drum one position | Operation |
| 0x9 | HALT | Stop execution | Operation |
| 0xA | PRINT | Output current value to printer | Operation |
| 0xB | NOP | No operation (timing pad) | Operation |

## 4. Encoding Rules

1. All numeric values are BCD (Binary Coded Decimal), 4 bits per digit.
2. Multi-digit values are stored most-significant digit first.
3. Unused bit positions must be 0 (no spurious holes).
4. A card with all-zero content is a valid NOP card.
5. Card sequences terminate with a HALT card or end-of-deck.

## 5. Error Modes

| Error | Detection | Recovery |
|-------|-----------|----------|
| Misregistration | Alignment hole check fails | Reject card, halt |
| Double-feed | Thickness sensor (mechanical) | Reverse feed, retry once |
| Torn card | Checksum failure on decode | Reject card, halt |
| Invalid opcode | Opcode > 0xB | Reject card, halt |
| Invalid BCD | Digit nibble > 9 | Reject card, halt |

## 6. Round-Trip Verification

The card compiler (tools/card_compiler.py) must satisfy:

```
source_text -> encode() -> card_bytes -> decode() -> recovered_text
assert recovered_text == source_text
```

This property is tested in backend/tests/unit/test_card_compiler.py.

## 7. References

- SOURCE:MENABREA-1842 -- Three card classes (operation, variable, number)
- SOURCE:BROMLEY-1982 -- Card reader mechanism and barrel interaction
- SOURCE:SWADE-2000 -- Physical card construction
- [OPCODES.yaml](OPCODES.yaml) -- Machine-readable opcode table
- [CHARTER.md](../program/CHARTER.md) -- Decision D-4
