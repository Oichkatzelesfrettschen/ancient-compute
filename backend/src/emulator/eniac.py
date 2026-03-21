"""ENIAC Emulator (Moore School of Electrical Engineering, 1945).

ENIAC (Electronic Numerical Integrator And Computer) was designed by
J. Presper Eckert (1919-1995) and John Mauchly (1907-1980) at the
University of Pennsylvania's Moore School of Electrical Engineering.
It became operational in November 1945 and was formally dedicated on
15 February 1946. ENIAC is often described as the first general-purpose
electronic digital computer.

Architecture:
  - 20 accumulators (10-digit signed decimal registers, each with two
    10-digit internal counters for addition/subtraction).
  - Multiplier unit: computes products of two 10-digit numbers.
  - Divider/square-root unit.
  - Function table units (3): hardwired ROM-like lookup tables, each with
    100 12-digit entries used for pre-computed functions.
  - Constant transmitter: reads numeric constants from punched cards.
  - Printer: outputs results to cards/printer.
  - Master programmer: controls loop repetition (digit capacity 6).
  - Cycling unit: 200 kHz clock (5 microseconds/cycle = 200,000 adds/sec).

Programming:
  ENIAC was programmed by physically rewiring patch cables and setting
  switches -- there were no stored program instructions. A problem was
  set up in hardware, then initiated with a single Go pulse.

  This emulator uses a high-level instruction set abstraction that captures
  the ENIAC's decimal accumulator arithmetic and functional units, without
  modelling the physical cable configuration.

Instruction set (emulator abstraction):
  ADD    dst  src          -- acc[dst] += acc[src]
  SUB    dst  src          -- acc[dst] -= acc[src]
  MULT   dst  src1  src2   -- acc[dst] = acc[src1] * acc[src2] (via multiplier)
  DIV    dst  src1  src2   -- acc[dst] = acc[src1] / acc[src2] (via div unit)
  SQRT   dst  src          -- acc[dst] = sqrt(acc[src])
  CLEAR  dst               -- acc[dst] = 0
  LOAD   dst  val          -- acc[dst] = val (constant transmitter)
  PRINT  src               -- output acc[src]
  HALT                     -- stop

Historical notes:
  - ENIAC contained 17,468 vacuum tubes, 70,000 resistors, 10,000 capacitors.
  - It weighed 30 tons, occupied 1,800 sq ft, consumed 150 kW.
  - Von Neumann's EDVAC report (1945) described the stored-program concept,
    which ENIAC itself lacked (though it was later modified in 1948).
  - The first computation on ENIAC was a classified calculation for the
    hydrogen bomb (Los Alamos, December 1945).
  - Six female mathematicians -- Kay McNulty, Betty Jennings, Betty Snyder,
    Marlyn Wescoff, Fran Bilas, and Ruth Lichterman -- were the original
    ENIAC programmers, though their contributions went unrecognized for decades.

References:
  - Eckert, J. P., & Mauchly, J. W. (1945). Automatic High Speed Computing.
    (Moore School internal report.)
  - Goldstine, H. H. (1972). The Computer from Pascal to von Neumann.
    Princeton University Press.
  - Haigh, T., Priestley, M., & Rope, C. (2016). ENIAC in Action: Making and
    Remaking the Modern Computer. MIT Press.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from decimal import Decimal, getcontext

# ENIAC used 10-digit decimal accumulators
getcontext().prec = 10

_NUM_ACCUMULATORS = 20
_ACC_DIGITS = 10  # 10-digit decimal accumulator


# ---------------------------------------------------------------------------
# Instruction types
# ---------------------------------------------------------------------------


class ENIACOp(str):
    ADD = "ADD"
    SUB = "SUB"
    MULT = "MULT"
    DIV = "DIV"
    SQRT = "SQRT"
    CLEAR = "CLEAR"
    LOAD = "LOAD"
    PRINT = "PRINT"
    HALT = "HALT"


@dataclass
class ENIACInstruction:
    """One ENIAC program instruction (emulator abstraction).

    op:    Operation (ENIACOp).
    args:  Tuple of operands (integers for accumulator indices, or numeric
           values for LOAD).
    label: Optional label for BRANCH targets (not used in base ENIAC; included
           for the master programmer loop emulation).
    """

    op: str
    args: tuple = field(default_factory=tuple)
    label: str = ""


# ---------------------------------------------------------------------------
# Machine state
# ---------------------------------------------------------------------------


@dataclass
class ENIACState:
    """Observable state of the ENIAC."""

    accumulators: list[Decimal] = field(default_factory=lambda: [Decimal(0)] * _NUM_ACCUMULATORS)
    program_counter: int = 0
    halted: bool = False
    cycle_count: int = 0
    output_tape: list[Decimal] = field(default_factory=list)
    overflow: bool = False


# ---------------------------------------------------------------------------
# Emulator
# ---------------------------------------------------------------------------


class ENIAC:
    """ENIAC emulator (1945).

    Usage::

        eniac = ENIAC()
        eniac.load_accumulator(0, Decimal("3"))
        eniac.load_accumulator(1, Decimal("4"))
        prog = [
            ENIACInstruction(ENIACOp.MULT, (2, 0, 0)),   # acc[2] = acc[0]^2
            ENIACInstruction(ENIACOp.MULT, (3, 1, 1)),   # acc[3] = acc[1]^2
            ENIACInstruction(ENIACOp.ADD, (2, 3)),        # acc[2] += acc[3]
            ENIACInstruction(ENIACOp.SQRT, (4, 2)),       # acc[4] = sqrt(9+16)
            ENIACInstruction(ENIACOp.PRINT, (4,)),
            ENIACInstruction(ENIACOp.HALT),
        ]
        eniac.load_program(prog)
        eniac.run()
        print(eniac.state.output_tape[0])   # 5
    """

    def __init__(self) -> None:
        self.state = ENIACState()
        self._program: list[ENIACInstruction] = []
        self._labels: dict[str, int] = {}

    def load_accumulator(self, index: int, value: Decimal) -> None:
        """Set accumulator value directly (mimics constant transmitter)."""
        self._check_acc(index)
        self.state.accumulators[index] = value

    def get_accumulator(self, index: int) -> Decimal:
        """Read accumulator value."""
        self._check_acc(index)
        return self.state.accumulators[index]

    def _check_acc(self, index: int) -> None:
        if not 0 <= index < _NUM_ACCUMULATORS:
            raise IndexError(f"Accumulator {index} out of range [0, {_NUM_ACCUMULATORS-1}]")

    def load_program(self, instructions: list[ENIACInstruction]) -> None:
        """Load program (replaces physical cable rewiring)."""
        self._program = list(instructions)
        self._labels = {instr.label: i for i, instr in enumerate(instructions) if instr.label}
        self.state.program_counter = 0
        self.state.halted = False

    def reset(self) -> None:
        self.state = ENIACState()
        self._program = []
        self._labels = {}

    def step(self) -> bool:
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
        while self.step():
            pass

    def _execute(self, instr: ENIACInstruction) -> None:
        op = instr.op
        a = instr.args
        acc = self.state.accumulators

        if op == ENIACOp.ADD:
            dst, src = int(a[0]), int(a[1])
            self._check_acc(dst)
            self._check_acc(src)
            acc[dst] += acc[src]

        elif op == ENIACOp.SUB:
            dst, src = int(a[0]), int(a[1])
            self._check_acc(dst)
            self._check_acc(src)
            acc[dst] -= acc[src]

        elif op == ENIACOp.MULT:
            dst, src1, src2 = int(a[0]), int(a[1]), int(a[2])
            self._check_acc(dst)
            self._check_acc(src1)
            self._check_acc(src2)
            acc[dst] = acc[src1] * acc[src2]

        elif op == ENIACOp.DIV:
            dst, src1, src2 = int(a[0]), int(a[1]), int(a[2])
            self._check_acc(dst)
            self._check_acc(src1)
            self._check_acc(src2)
            if acc[src2] == 0:
                raise ZeroDivisionError("ENIAC division by zero")
            acc[dst] = acc[src1] / acc[src2]

        elif op == ENIACOp.SQRT:
            dst, src = int(a[0]), int(a[1])
            self._check_acc(dst)
            self._check_acc(src)
            val = float(acc[src])
            if val < 0:
                raise ValueError(f"ENIAC SQRT of negative number: {val}")
            acc[dst] = Decimal(str(math.sqrt(val)))

        elif op == ENIACOp.CLEAR:
            dst = int(a[0])
            self._check_acc(dst)
            acc[dst] = Decimal(0)

        elif op == ENIACOp.LOAD:
            dst = int(a[0])
            self._check_acc(dst)
            acc[dst] = Decimal(str(a[1]))

        elif op == ENIACOp.PRINT:
            src = int(a[0])
            self._check_acc(src)
            self.state.output_tape.append(acc[src])

        elif op == ENIACOp.HALT:
            self.state.halted = True

        else:
            raise ValueError(f"Unknown ENIAC operation: {op!r}")

    def state_snapshot(self) -> dict[str, object]:
        return {
            "accumulators": [float(v) for v in self.state.accumulators],
            "program_counter": self.state.program_counter,
            "cycle_count": self.state.cycle_count,
            "halted": self.state.halted,
            "output_tape": [float(v) for v in self.state.output_tape],
            "num_accumulators": _NUM_ACCUMULATORS,
        }
