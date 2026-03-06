"""Fourmilab card deck format parser and translator.

Fourmilab (fourmilab.ch/babbage/) is the de facto standard for AE card decks online.
This module parses Fourmilab syntax and translates it to internal Instructions.

Fourmilab card syntax:
  N000 +1234567890    # Number card: set column 0 to value
  L000                # Load from store column 0  (non-destructive)
  L000'               # Load from store column 0  (destructive -- clears after read)
  S002                # Store to column 2
  S002'               # Additive store (adds to existing value)
  +                   # Add
  -                   # Subtract
  *                   # Multiply
  /                   # Divide
  CF+005              # Conditional forward skip 5 cards
  CB-003              # Conditional backward skip 3 cards
  B+010               # Unconditional forward skip
  P                   # Print (punch output card)
  H                   # Halt

Translation strategy:
  - Number cards -> LOAD immediate + STOR to a scratch address
  - L/S cards   -> LOAD/STOR with memory address operands
  - Op cards    -> ADD/SUB/MULT/DIV
  - Branch cards -> JMP/JZ with computed absolute PC targets
  - P           -> WRPRN A
  - H           -> HALT
"""

import re
from dataclasses import dataclass

from ...analytical_engine import Instruction

# Fourmilab column addressing uses columns 0-999 as AE store addresses.
# We map them to memory addresses directly.

_NUM_CARD_RE = re.compile(r"^N(\d+)\s+([+-]?\d+)$")
_LOAD_RE = re.compile(r"^L(\d+)('?)$")
_STOR_RE = re.compile(r"^S(\d+)('?)$")
_CF_RE = re.compile(r"^CF([+-]\d+)$")
_CB_RE = re.compile(r"^CB([+-]\d+)$")
_B_RE = re.compile(r"^B([+-]\d+)$")


@dataclass
class FourmilabCard:
    """A parsed Fourmilab card."""
    line_num: int
    raw: str
    kind: str       # 'number', 'load', 'stor', 'op', 'branch', 'print', 'halt', 'comment'
    payload: object  # kind-specific data


def parse_fourmilab_source(source: str) -> list[FourmilabCard]:
    """Parse Fourmilab source into a list of FourmilabCards."""
    cards = []
    for i, raw_line in enumerate(source.splitlines()):
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue

        # Strip inline comments
        line = re.sub(r"\s+#.*$", "", line).strip()
        if not line:
            continue

        m = _NUM_CARD_RE.match(line)
        if m:
            cards.append(FourmilabCard(i, line, "number", (int(m.group(1)), int(m.group(2)))))
            continue

        m = _LOAD_RE.match(line)
        if m:
            cards.append(FourmilabCard(i, line, "load", (int(m.group(1)), m.group(2) == "'")))
            continue

        m = _STOR_RE.match(line)
        if m:
            cards.append(FourmilabCard(i, line, "stor", (int(m.group(1)), m.group(2) == "'")))
            continue

        if line in ("+", "-", "*", "/"):
            cards.append(FourmilabCard(i, line, "op", line))
            continue

        m = _CF_RE.match(line)
        if m:
            cards.append(FourmilabCard(i, line, "cf", int(m.group(1))))
            continue

        m = _CB_RE.match(line)
        if m:
            cards.append(FourmilabCard(i, line, "cb", int(m.group(1))))
            continue

        m = _B_RE.match(line)
        if m:
            cards.append(FourmilabCard(i, line, "b", int(m.group(1))))
            continue

        if line == "P":
            cards.append(FourmilabCard(i, line, "print", None))
            continue

        if line == "H":
            cards.append(FourmilabCard(i, line, "halt", None))
            continue

        # Unknown -- skip with warning
        cards.append(FourmilabCard(i, line, "unknown", line))

    return cards


def translate_fourmilab(cards: list[FourmilabCard]) -> list[Instruction]:
    """Translate a list of FourmilabCards into engine Instructions.

    The translation is one-to-one where possible. Branches are resolved
    to absolute PC values after a first pass that counts generated instructions.
    """
    # First pass: count instructions per card to build PC map
    # Each card type generates a fixed number of instructions.
    # We store (card_index -> first_instr_pc) for branch resolution.
    _OP_MAP = {"+": "ADD", "-": "SUB", "*": "MULT", "/": "DIV"}

    instrs: list[Instruction] = []
    card_pc: list[int] = []  # card_pc[i] = PC of first instruction from card i

    for card in cards:
        card_pc.append(len(instrs))
        if card.kind == "number":
            col, val = card.payload
            # Store value in column (memory address = col)
            instrs.append(Instruction("LOAD", ["A", str(val)]))
            instrs.append(Instruction("STOR", ["A", f"[{col}]"]))
        elif card.kind == "load":
            col, destructive = card.payload
            # Fourmilab L card: current result shifts into prior (ingress),
            # then memory loads into result.  This mirrors the physical two-register
            # "in-axis" / "out-axis" model where each L card ripples the value forward.
            # We represent this as: LOAD B A  (save result -> prior)
            #                       LOAD A [col]  (load new result from store)
            instrs.append(Instruction("LOAD", ["B", "A"]))
            instrs.append(Instruction("LOAD", ["A", f"[{col}]"]))
            if destructive:
                # Clear source after read
                instrs.append(Instruction("LOAD", ["C", "0"]))
                instrs.append(Instruction("STOR", ["C", f"[{col}]"]))
        elif card.kind == "stor":
            col, additive = card.payload
            if additive:
                # Additive store: memory[col] += A
                instrs.append(Instruction("LOAD", ["B", f"[{col}]"]))
                instrs.append(Instruction("ADD", ["B", "A"]))
                instrs.append(Instruction("STOR", ["B", f"[{col}]"]))
            else:
                instrs.append(Instruction("STOR", ["A", f"[{col}]"]))
        elif card.kind == "op":
            ae_op = _OP_MAP[card.payload]
            # A = A op B
            instrs.append(Instruction(ae_op, ["A", "B"]))
        elif card.kind in ("cf", "cb"):
            # Conditional branch: skip N cards forward/back from current card.
            # We use JNZ (skip if result != 0). Offset is relative to current card index.
            # Will be patched in a second pass.
            instrs.append(Instruction("JNZ", ["__PATCH__", str(len(card_pc) - 1), str(card.payload)]))
        elif card.kind == "b":
            # Unconditional branch
            instrs.append(Instruction("JMP", ["__PATCH__", str(len(card_pc) - 1), str(card.payload)]))
        elif card.kind == "print":
            instrs.append(Instruction("WRPRN", ["A"]))
        elif card.kind == "halt":
            instrs.append(Instruction("HALT", []))

    # Second pass: resolve branch targets
    # Branch instruction format: ["__PATCH__", card_idx_str, offset_str]
    for instr in instrs:
        if instr.operands and instr.operands[0] == "__PATCH__":
            card_idx = int(instr.operands[1])
            offset = int(instr.operands[2])
            target_card = card_idx + offset
            target_card = max(0, min(target_card, len(card_pc) - 1))
            target_pc = card_pc[target_card]
            instr.operands = [str(target_pc)]

    return instrs
