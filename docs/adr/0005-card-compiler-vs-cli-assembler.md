# ADR 0005: Card Compiler vs CLI Assembler

## Status

Accepted (2026-03-19)

## Context

Two tools produce executable card sequences from text input:

1. **tools/card_compiler.py**: Standalone encode/decode round-trip compiler
   built for CARD_STANDARD.md validation. Maps source lines 1:1 to card
   bytes and verifies `decode(encode(src)) == src`.

2. **CLI assembler** (`backend/src/emulator/cli/`): Two-pass assembler
   with label resolution, invoked via `ancient-compute assemble`. Supports
   symbolic labels, forward references, and macro expansion for writing
   real programs.

Consolidation was considered: one tool doing both jobs. However:
- The card compiler's value is its simplicity and round-trip guarantee.
  Adding label resolution would break the 1:1 line-to-card invariant.
- The CLI assembler needs multi-pass logic that has no place in a
  validation tool.

## Decision

Keep both tools with distinct responsibilities:
- **card_compiler.py**: Validates CARD_STANDARD encoding. No labels,
  no macros, no forward references. Canonical opcode source is
  `docs/hardware/OPCODES.yaml`; the compiler's Opcode enum must stay
  in sync (enforced by `tools/validate_opcodes.py`).
- **CLI assembler**: Full-featured assembler for program development.
  Uses the same opcode values but adds symbolic addressing.

## Consequences

- Changes to OPCODES.yaml must be reflected in both card_compiler.py
  and the CLI assembler; `make verify` catches drift via validate_opcodes.py.
- Users writing programs use `ancient-compute assemble`; users validating
  card encoding use `card_compiler.py`.
- If a third encoding tool is ever needed, consolidation should be
  reconsidered.
