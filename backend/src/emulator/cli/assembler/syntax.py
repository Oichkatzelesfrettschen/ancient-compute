"""Assembly syntax definitions.

Supports two comment styles:
  - Legacy:  OPCODE operand1 operand2  # comment
  - Extended: OPCODE operand1, operand2  ; comment

Operand forms:
  - Register:   A, B, C, D
  - Memory:     [addr]  e.g. [0], [999]
  - Immediate:  integer literal or #value form  e.g. 42, #42
  - Label ref:  LABEL_NAME  (resolved in pass 2)
"""

import re

# Strip comments (# or ;) and clean whitespace
_COMMENT_RE = re.compile(r"[#;].*$")

# Label definition: LABEL: [rest-of-line]
_LABEL_DEF_RE = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*):\s*(.*)?$")

# Immediate with leading # (e.g. #42 -> 42)
_HASH_IMMED_RE = re.compile(r"^#(-?\d+)$")

OPCODES = {
    "NOP", "ADD", "SUB", "MULT", "DIV", "SQRT",
    "LOAD", "STOR",
    "JMP", "JZ", "JNZ", "JLT", "JGT", "JLE", "JGE",
    "CMP", "CALL", "RET",
    "PUSH", "POP",
    "RDCRD", "WRPCH", "WRPRN",
    "SHL", "SHR", "AND", "OR", "XOR",
    "CHKS", "HALT", "PLAY", "SETMODE", "OIL",
}

BRANCH_OPCODES = {"JMP", "JZ", "JNZ", "JLT", "JGT", "JLE", "JGE", "CALL"}


def strip_comment(line: str) -> str:
    """Remove inline comment from line."""
    return _COMMENT_RE.sub("", line).strip()


def normalize_operand(op: str) -> str:
    """Normalize operand: strip commas, handle #value form."""
    op = op.strip().rstrip(",")
    m = _HASH_IMMED_RE.match(op)
    if m:
        return m.group(1)
    return op


def tokenize_line(raw_line: str) -> tuple[str | None, str | None, list[str]]:
    """Parse one assembly source line.

    Returns:
        (label, opcode, operands)  -- any component may be None/empty.
    """
    line = strip_comment(raw_line)
    if not line:
        return None, None, []

    label = None
    m = _LABEL_DEF_RE.match(line)
    if m:
        label = m.group(1)
        line = m.group(2) or ""
        line = line.strip()
        if not line:
            return label, None, []

    parts = line.split()
    opcode = parts[0].upper() if parts else None
    raw_ops = parts[1:] if len(parts) > 1 else []

    # Join, then split on commas (handles "A, B" or "A B")
    joined = " ".join(raw_ops)
    if "," in joined:
        operands = [normalize_operand(o) for o in joined.split(",") if normalize_operand(o)]
    else:
        operands = [normalize_operand(o) for o in raw_ops if normalize_operand(o)]

    return label, opcode, operands
