"""Harvard Mark I (IBM ASCC) Emulator (United States, 1944).

Howard Aiken (1900-1973) of Harvard University designed the Automatic Sequence
Controlled Calculator (ASCC), built by IBM at their Endicott laboratory.
It began operation at Harvard on 7 August 1944, making it the first large-scale
automatically sequenced calculator in the United States.

Grace Murray Hopper (1906-1992) was among the first programmers; she and her
team debugged the machine and prepared its first handbook.

Architecture:
  - 72 storage counters (decimal accumulators), each holding a 23-digit signed
    decimal number (22 digits + sign).
  - 60 constant registers (read-only, set by switches).
  - 3 interpolation tables (not modelled here).
  - Arithmetic: addition/subtraction in one clock cycle (~0.3 sec),
    multiplication in 6 seconds, division in 15.3 seconds.
  - Instruction tape: punched paper tape, 24 channels wide.
    Each instruction specifies: source, destination, operation, sequence.
  - Sequence: 3 control sequence tapes for conditional branching (limited).
  - Output: 4 electric typewriters.

Instruction format (simplified):
  Mark I used a 24-channel punched tape. An instruction specified:
    - Source unit (counter or constant or function unit)
    - Destination unit (counter)
    - Operation (transfer, multiply, divide, etc.)
    - Next instruction address (for sequencing)

  This emulator uses a higher-level assembly-like instruction set:
    LOAD   acc  addr    -- acc = storage[addr]
    STORE  addr acc     -- storage[addr] = acc
    ADD    dst  src     -- storage[dst] = storage[dst] + storage[src]
    SUB    dst  src     -- storage[dst] = storage[dst] - storage[src]
    MULT   dst  src1 src2  -- storage[dst] = storage[src1] * storage[src2]
    DIV    dst  src1 src2  -- storage[dst] = storage[src1] / storage[src2]
    SET    addr val     -- storage[addr] = val (constant assignment)
    PRINT  addr         -- output storage[addr]
    BRANCH label        -- jump to label if last result != 0 (limited branch)
    HALT                -- stop execution

Historical notes:
  - The Mark I weighed 5 tons, was 51 feet long, and contained 765,299 parts.
  - It ran its first full computation (a table of Bessel functions) in 1944.
  - It remained in service at Harvard until 1959.
  - IBM called it the "Automatic Sequence Controlled Calculator" (ASCC).
  - It could add 23-digit numbers at 3 per second.

References:
  - Aiken, H. H., & Hopper, G. M. (1946). The Automatic Sequence Controlled
    Calculator. Electrical Engineering, 65(8/9), 384-391, 449-454, 522-528.
  - Cohen, I. B. (1999). Howard Aiken: Portrait of a Computer Pioneer.
    MIT Press.
  - Williams, M. R. (1985). A History of Computing Technology.
    Prentice Hall. pp. 223-232.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from decimal import Decimal, getcontext, InvalidOperation
from typing import NamedTuple

# 23-digit decimal precision (22 digits + sign)
getcontext().prec = 23

_NUM_COUNTERS = 72         # Main storage counters
_NUM_CONSTANTS = 60        # Read-only constant registers (0..59)
_TOTAL_STORAGE = _NUM_COUNTERS + _NUM_CONSTANTS  # Addresses 0..131


# ---------------------------------------------------------------------------
# Instruction types
# ---------------------------------------------------------------------------


class MarkIOp(str):
    """Mark I operation codes (string constants)."""
    LOAD = "LOAD"
    STORE = "STORE"
    ADD = "ADD"
    SUB = "SUB"
    MULT = "MULT"
    DIV = "DIV"
    SET = "SET"
    PRINT = "PRINT"
    BRANCH = "BRANCH"
    HALT = "HALT"


@dataclass
class MarkIInstruction:
    """One Mark I program-tape instruction.

    op:      Operation (see MarkIOp).
    args:    Positional arguments:
               LOAD   dst_reg, src_addr
               STORE  dst_addr, src_reg
               ADD    dst_addr, src_addr
               SUB    dst_addr, src_addr
               MULT   dst_addr, src1_addr, src2_addr
               DIV    dst_addr, src1_addr, src2_addr
               SET    addr, value
               PRINT  addr
               BRANCH label (str)
               HALT   (no args)
    label:   Optional instruction label (str) for BRANCH targets.
    """

    op: str
    args: tuple = field(default_factory=tuple)
    label: str = ""


# ---------------------------------------------------------------------------
# Machine state
# ---------------------------------------------------------------------------


@dataclass
class MarkIState:
    """Observable state of the Harvard Mark I."""

    # 72 accumulators + 60 constant registers
    storage: list[Decimal] = field(
        default_factory=lambda: [Decimal(0)] * _TOTAL_STORAGE
    )
    program_counter: int = 0
    halted: bool = False
    cycle_count: int = 0
    last_result: Decimal = field(default_factory=lambda: Decimal(0))
    output_tape: list[Decimal] = field(default_factory=list)
    overflow: bool = False


# ---------------------------------------------------------------------------
# Emulator
# ---------------------------------------------------------------------------


class HarvardMarkI:
    """Harvard Mark I (IBM ASCC) emulator.

    Usage::

        m1 = HarvardMarkI()
        m1.set_constant(0, Decimal("3.14159265358979"))   # pi in slot 0
        m1.set_counter(0, Decimal("2"))                   # counter 0 = 2

        prog = [
            MarkIInstruction(MarkIOp.MULT, (60, 0, 0)),   # counter 60 = 0 * 0 (from constants slot 0)
            MarkIInstruction(MarkIOp.HALT),
        ]
        m1.load_program(prog)
        m1.run()
    """

    def __init__(self) -> None:
        self.state = MarkIState()
        self._program: list[MarkIInstruction] = []
        self._labels: dict[str, int] = {}

    # ------------------------------------------------------------------
    # Storage helpers
    # ------------------------------------------------------------------

    def set_counter(self, index: int, value: Decimal) -> None:
        """Set accumulator counter (index 0..71)."""
        if not 0 <= index < _NUM_COUNTERS:
            raise IndexError(f"Counter index {index} out of range [0, {_NUM_COUNTERS-1}]")
        self.state.storage[index] = value

    def get_counter(self, index: int) -> Decimal:
        """Read accumulator counter (index 0..71)."""
        if not 0 <= index < _NUM_COUNTERS:
            raise IndexError(f"Counter index {index} out of range [0, {_NUM_COUNTERS-1}]")
        return self.state.storage[index]

    def set_constant(self, index: int, value: Decimal) -> None:
        """Set read-only constant register (index 0..59, maps to storage 72..131)."""
        if not 0 <= index < _NUM_CONSTANTS:
            raise IndexError(f"Constant index {index} out of range [0, {_NUM_CONSTANTS-1}]")
        self.state.storage[_NUM_COUNTERS + index] = value

    def get_constant(self, index: int) -> Decimal:
        """Read constant register (index 0..59)."""
        if not 0 <= index < _NUM_CONSTANTS:
            raise IndexError(f"Constant index {index} out of range [0, {_NUM_CONSTANTS-1}]")
        return self.state.storage[_NUM_COUNTERS + index]

    def _read(self, addr: int) -> Decimal:
        """Read any storage address (counter or constant)."""
        if not 0 <= addr < _TOTAL_STORAGE:
            raise IndexError(f"Storage address {addr} out of range [0, {_TOTAL_STORAGE-1}]")
        return self.state.storage[addr]

    def _write(self, addr: int, value: Decimal) -> None:
        """Write to storage address. Constant registers (72+) are read-only."""
        if addr >= _NUM_COUNTERS:
            raise PermissionError(
                f"Storage address {addr} is a read-only constant register"
            )
        if not 0 <= addr < _NUM_COUNTERS:
            raise IndexError(f"Counter address {addr} out of range [0, {_NUM_COUNTERS-1}]")
        self.state.storage[addr] = value
        self.state.last_result = value

    # ------------------------------------------------------------------
    # Program loading
    # ------------------------------------------------------------------

    def load_program(self, instructions: list[MarkIInstruction]) -> None:
        """Load program onto the sequence tape."""
        self._program = list(instructions)
        self._labels = {
            instr.label: i
            for i, instr in enumerate(instructions)
            if instr.label
        }
        self.state.program_counter = 0
        self.state.halted = False

    def reset(self) -> None:
        """Reset machine to initial state."""
        self.state = MarkIState()
        self._program = []
        self._labels = {}

    # ------------------------------------------------------------------
    # Execution
    # ------------------------------------------------------------------

    def step(self) -> bool:
        """Execute one instruction. Returns True if should continue."""
        if self.state.halted:
            return False
        if self.state.program_counter >= len(self._program):
            self.state.halted = True
            return False

        instr = self._program[self.state.program_counter]
        self.state.program_counter += 1
        self.state.cycle_count += 1
        self._execute(instr)
        return not self.state.halted

    def run(self) -> None:
        """Execute to completion."""
        while self.step():
            pass

    def _execute(self, instr: MarkIInstruction) -> None:
        op = instr.op
        a = instr.args

        if op == MarkIOp.LOAD:
            # LOAD dst_reg, src_addr: counter[dst] = storage[src]
            dst, src = int(a[0]), int(a[1])
            self._write(dst, self._read(src))

        elif op == MarkIOp.STORE:
            # STORE dst_addr, src_reg: storage[dst] = counter[src]
            dst, src = int(a[0]), int(a[1])
            self._write(dst, self._read(src))

        elif op == MarkIOp.ADD:
            # ADD dst, src: storage[dst] += storage[src]
            dst, src = int(a[0]), int(a[1])
            self._write(dst, self._read(dst) + self._read(src))

        elif op == MarkIOp.SUB:
            # SUB dst, src: storage[dst] -= storage[src]
            dst, src = int(a[0]), int(a[1])
            self._write(dst, self._read(dst) - self._read(src))

        elif op == MarkIOp.MULT:
            # MULT dst, src1, src2: storage[dst] = storage[src1] * storage[src2]
            dst, src1, src2 = int(a[0]), int(a[1]), int(a[2])
            self._write(dst, self._read(src1) * self._read(src2))

        elif op == MarkIOp.DIV:
            # DIV dst, src1, src2: storage[dst] = storage[src1] / storage[src2]
            dst, src1, src2 = int(a[0]), int(a[1]), int(a[2])
            divisor = self._read(src2)
            if divisor == 0:
                raise ZeroDivisionError("Mark I division by zero")
            self._write(dst, self._read(src1) / divisor)

        elif op == MarkIOp.SET:
            # SET addr, value: storage[addr] = value
            addr, val = int(a[0]), Decimal(str(a[1]))
            self._write(addr, val)

        elif op == MarkIOp.PRINT:
            # PRINT addr: output storage[addr]
            addr = int(a[0])
            val = self._read(addr)
            self.state.output_tape.append(val)

        elif op == MarkIOp.BRANCH:
            # BRANCH label: jump if last result != 0
            label = str(a[0])
            if self.state.last_result != 0:
                if label not in self._labels:
                    raise KeyError(f"Undefined label: {label!r}")
                self.state.program_counter = self._labels[label]

        elif op == MarkIOp.HALT:
            self.state.halted = True

        else:
            raise ValueError(f"Unknown Mark I operation: {op!r}")

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def state_snapshot(self) -> dict[str, object]:
        return {
            "program_counter": self.state.program_counter,
            "cycle_count": self.state.cycle_count,
            "halted": self.state.halted,
            "last_result": float(self.state.last_result),
            "output_tape": [float(v) for v in self.state.output_tape],
            "num_counters": _NUM_COUNTERS,
            "num_constants": _NUM_CONSTANTS,
        }
