"""Two-pass assembler for the Analytical Engine instruction set.

Pass 1: collect label positions.
Pass 2: resolve label references in branch/call operands.
"""

from ...analytical_engine import Instruction
from .syntax import tokenize_line


def parse_source(source: str) -> list[Instruction]:
    """Assemble source text into a list of Instructions."""
    lines = source.splitlines()
    instructions: list[Instruction] = []
    labels: dict[str, int] = {}
    pending: list[tuple[int, str, list[str]]] = []  # (instr_idx, opcode, operands)

    addr = 0
    for raw in lines:
        label, opcode, operands = tokenize_line(raw)
        if label:
            labels[label] = addr
        if opcode:
            pending.append((addr, opcode, operands))
            instructions.append(Instruction(opcode, operands[:]))
            addr += 1

    # Pass 2: resolve labels in branch operands
    for instr_idx, _opcode, operands in pending:
        resolved = []
        for op in operands:
            if op in labels:
                resolved.append(str(labels[op]))
            else:
                resolved.append(op)
        instructions[instr_idx].operands = resolved

    return instructions


def assemble(source: str) -> list[Instruction]:
    """Alias for parse_source (public API)."""
    return parse_source(source)


def assemble_file(path: str) -> list[Instruction]:
    """Read a .basm file and assemble it."""
    with open(path, encoding="utf-8") as fh:
        return parse_source(fh.read())
