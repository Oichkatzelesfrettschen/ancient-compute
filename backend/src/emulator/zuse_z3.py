"""Zuse Z3 Emulator (Germany, 1941).

Konrad Zuse (1910-1995) completed the Z3 in May 1941 in Berlin, making it
the world's first operational, fully automatic, programmable computer. It
was built from approximately 2,600 telephone relays salvaged from telephone
exchanges.

Architecture:
  - 22-bit floating-point word (same format as the Z1, but implemented with
    electromechanical relays rather than mechanical logic).
  - 64 words of relay-based memory (Speicherwerk).
  - Two input registers (A and B) feeding the arithmetic unit (Rechenwerk).
  - Single accumulator (Ergebnis = result register).
  - Program tape: 35 mm celluloid film strip with holes, read-only and
    sequential -- NO stored-program memory. Programs could not branch.
  - I/O: Decimal keyboard input; lamp panel / film tape output.

22-bit float format (Zuse's own design, 1936-1941):
  [sign(1)] [exponent(7)] [mantissa(14)]
  - Sign: 0 = positive, 1 = negative (sign-magnitude).
  - Exponent: excess-64 code (stored = actual + 64).
  - Mantissa: normalized, 1.mantissa implicit leading 1 (hidden bit).
  - Special: word = 0 encodes the value 0.

Instruction set:
  The Z3 read instructions from a punched film tape. Each instruction is
  one step of an arithmetic or memory/IO operation. The critical constraint:
  NO CONDITIONAL BRANCH. Programs were straight-line sequences (Geradeaus-
  programm). Loops required physically cutting and splicing the tape.

  Z3Instruction operations:
    LOAD  m  -- Load memory[m] into accumulator.
    STORE m  -- Store accumulator to memory[m].
    ADD   m  -- accumulator = accumulator + memory[m].
    SUB   m  -- accumulator = accumulator - memory[m].
    MULT  m  -- accumulator = accumulator * memory[m].
    DIV   m  -- accumulator = accumulator / memory[m].
    SQRT     -- accumulator = sqrt(accumulator).
    READ     -- accumulator = next value from input tape.
    PRINT    -- Append accumulator to output tape.
    HALT     -- Stop execution.

Historical notes:
  - The Z3 performed one floating-point multiply in ~3 seconds at ~5-10 Hz.
  - It was destroyed by Allied bombing in Berlin on 21 December 1943.
  - Konrad Zuse built a replica for the Deutsches Museum Munich in 1960.
  - Zuse later proved (1998) that the Z3 was Turing-complete despite lacking
    conditional branches -- the tape loop provided an indirect looping mechanism.

References:
  - Zuse, K. (1993). The Computer -- My Life. Springer-Verlag.
  - Rojas, R. (1997). The Zuse computers. IEEE Annals of the History of
    Computing, 19(2), 5-47.
  - Rojas, R. (1998). How to make Zuse's Z3 a universal computer. IEEE
    Annals of the History of Computing, 20(3), 51-54.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from enum import StrEnum

# Reuse ZuseFloat from the Z1 (same 22-bit FP format)
from .zuse_z1 import ZuseFloat

_MEMORY_SIZE = 64


# ---------------------------------------------------------------------------
# Instruction set
# ---------------------------------------------------------------------------


class Z3Op(StrEnum):
    """Z3 instruction operation codes."""

    LOAD = "LOAD"  # Load memory[m] into accumulator
    STORE = "STORE"  # Store accumulator to memory[m]
    ADD = "ADD"  # accumulator += memory[m]
    SUB = "SUB"  # accumulator -= memory[m]
    MULT = "MULT"  # accumulator *= memory[m]
    DIV = "DIV"  # accumulator /= memory[m]
    SQRT = "SQRT"  # accumulator = sqrt(accumulator)
    READ = "READ"  # accumulator = next input
    PRINT = "PRINT"  # output <- accumulator
    HALT = "HALT"  # stop execution


@dataclass
class Z3Instruction:
    """One Z3 program-tape instruction.

    op:      Operation code (Z3Op).
    address: Memory address for LOAD/STORE/ADD/SUB/MULT/DIV (0..63).
             Ignored for SQRT/READ/PRINT/HALT.
    """

    op: Z3Op
    address: int = 0

    def __post_init__(self) -> None:
        if (
            self.op in (Z3Op.LOAD, Z3Op.STORE, Z3Op.ADD, Z3Op.SUB, Z3Op.MULT, Z3Op.DIV)
            and not 0 <= self.address < _MEMORY_SIZE
        ):
            raise ValueError(f"Memory address {self.address} out of range [0, {_MEMORY_SIZE - 1}]")


# ---------------------------------------------------------------------------
# Machine state
# ---------------------------------------------------------------------------


@dataclass
class Z3State:
    """Observable state of the Zuse Z3."""

    memory: list[ZuseFloat] = field(default_factory=lambda: [ZuseFloat(0)] * _MEMORY_SIZE)
    accumulator: ZuseFloat = field(default_factory=lambda: ZuseFloat(0))
    program_counter: int = 0  # Index into program tape
    halted: bool = False
    cycle_count: int = 0  # Total instructions executed
    output_tape: list[float] = field(default_factory=list)  # PRINT outputs
    input_tape: list[float] = field(default_factory=list)  # READ inputs
    input_pointer: int = 0  # Next unread position in input_tape
    overflow: bool = False  # Set when FP result is out of range


# ---------------------------------------------------------------------------
# Emulator
# ---------------------------------------------------------------------------


class ZuseZ3:
    """Zuse Z3 -- world's first operational programmable computer (1941).

    Usage::

        z3 = ZuseZ3()
        z3.load_memory(0, 3.14)          # Pre-load memory register 0 = pi
        z3.load_memory(1, 2.0)           # Pre-load memory register 1 = 2

        # Program: compute pi/2 = pi * 0.5
        z3.load_input_tape([0.5])
        prog = [
            Z3Instruction(Z3Op.READ),           # acc = 0.5 (from tape)
            Z3Instruction(Z3Op.STORE, 2),        # memory[2] = 0.5
            Z3Instruction(Z3Op.LOAD, 0),         # acc = 3.14
            Z3Instruction(Z3Op.MULT, 2),         # acc = 3.14 * 0.5
            Z3Instruction(Z3Op.PRINT),           # output 1.57
            Z3Instruction(Z3Op.HALT),
        ]
        z3.load_program(prog)
        z3.run()
        print(z3.state.output_tape[0])   # ~1.57
    """

    def __init__(self) -> None:
        self.state = Z3State()
        self._program: list[Z3Instruction] = []

    # ------------------------------------------------------------------
    # Loading
    # ------------------------------------------------------------------

    def load_memory(self, address: int, value: float) -> None:
        """Pre-load a float value into memory register ``address``."""
        if not 0 <= address < _MEMORY_SIZE:
            raise IndexError(f"Memory address {address} out of range [0, {_MEMORY_SIZE - 1}]")
        self.state.memory[address] = ZuseFloat.from_float(value)

    def get_memory(self, address: int) -> float:
        """Read memory register ``address`` as a Python float."""
        if not 0 <= address < _MEMORY_SIZE:
            raise IndexError(f"Memory address {address} out of range [0, {_MEMORY_SIZE - 1}]")
        return self.state.memory[address].to_float()

    def load_input_tape(self, values: list[float]) -> None:
        """Load values onto the input tape (consumed by READ instructions)."""
        self.state.input_tape = list(values)
        self.state.input_pointer = 0

    def load_program(self, instructions: list[Z3Instruction]) -> None:
        """Load a program (list of Z3Instructions) onto the program tape.

        The Z3 has NO stored-program memory -- the tape is separate from
        the data memory. Loading resets the program counter to 0.
        """
        self._program = list(instructions)
        self.state.program_counter = 0
        self.state.halted = False

    def reset(self) -> None:
        """Reset the machine to its initial state."""
        self.state = Z3State()
        self._program = []

    # ------------------------------------------------------------------
    # Execution
    # ------------------------------------------------------------------

    def step(self) -> bool:
        """Execute one instruction from the program tape.

        Returns True if execution should continue, False if halted.
        """
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
        """Execute the program tape to completion (until HALT or end of tape)."""
        while self.step():
            pass

    def _execute(self, instr: Z3Instruction) -> None:
        """Dispatch one instruction."""
        op = instr.op
        addr = instr.address

        if op == Z3Op.LOAD:
            self.state.accumulator = self.state.memory[addr]

        elif op == Z3Op.STORE:
            self.state.memory[addr] = self.state.accumulator

        elif op == Z3Op.ADD:
            self.state.accumulator = self._fp_add(self.state.accumulator, self.state.memory[addr])

        elif op == Z3Op.SUB:
            self.state.accumulator = self._fp_sub(self.state.accumulator, self.state.memory[addr])

        elif op == Z3Op.MULT:
            self.state.accumulator = self._fp_mult(self.state.accumulator, self.state.memory[addr])

        elif op == Z3Op.DIV:
            self.state.accumulator = self._fp_div(self.state.accumulator, self.state.memory[addr])

        elif op == Z3Op.SQRT:
            self.state.accumulator = self._fp_sqrt(self.state.accumulator)

        elif op == Z3Op.READ:
            if self.state.input_pointer >= len(self.state.input_tape):
                raise RuntimeError("Input tape exhausted: no value for READ")
            val = self.state.input_tape[self.state.input_pointer]
            self.state.input_pointer += 1
            self.state.accumulator = ZuseFloat.from_float(val)

        elif op == Z3Op.PRINT:
            self.state.output_tape.append(self.state.accumulator.to_float())

        elif op == Z3Op.HALT:
            self.state.halted = True

    # ------------------------------------------------------------------
    # Floating-point arithmetic (via ZuseFloat <-> Python float)
    # WHY: The Z3's relay arithmetic operated on the 22-bit format. For the
    # emulator we convert to/from Python float for each operation, which
    # introduces the same quantization error as the 14-bit mantissa.
    # ------------------------------------------------------------------

    def _fp_add(self, a: ZuseFloat, b: ZuseFloat) -> ZuseFloat:
        return ZuseFloat.from_float(a.to_float() + b.to_float())

    def _fp_sub(self, a: ZuseFloat, b: ZuseFloat) -> ZuseFloat:
        return ZuseFloat.from_float(a.to_float() - b.to_float())

    def _fp_mult(self, a: ZuseFloat, b: ZuseFloat) -> ZuseFloat:
        return ZuseFloat.from_float(a.to_float() * b.to_float())

    def _fp_div(self, a: ZuseFloat, b: ZuseFloat) -> ZuseFloat:
        bv = b.to_float()
        if bv == 0.0:
            raise ZeroDivisionError("Z3 division by zero")
        return ZuseFloat.from_float(a.to_float() / bv)

    def _fp_sqrt(self, a: ZuseFloat) -> ZuseFloat:
        av = a.to_float()
        if av < 0.0:
            raise ValueError(f"Z3 SQRT of negative number: {av}")
        return ZuseFloat.from_float(math.sqrt(av))

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def get_accumulator(self) -> float:
        """Return the current accumulator value as a Python float."""
        return self.state.accumulator.to_float()

    def state_snapshot(self) -> dict[str, object]:
        """Return a summary of the current machine state."""
        return {
            "accumulator": self.state.accumulator.to_float(),
            "program_counter": self.state.program_counter,
            "cycle_count": self.state.cycle_count,
            "halted": self.state.halted,
            "output_tape": list(self.state.output_tape),
            "input_pointer": self.state.input_pointer,
            "overflow": self.state.overflow,
        }
