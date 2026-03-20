"""Torres y Quevedo Electromechanical Calculator Emulator (Spain, 1914-1920).

Leonardo Torres y Quevedo built electromechanical calculating machines and
proposed floating-point arithmetic in 1914 -- the first such proposal.

Key innovations:
  - Floating-point number representation (mantissa + exponent).
  - Electromechanical relay registers (physical electrical contacts).
  - Remote typewriter output (telegraph-based I/O).
  - Proposed conditional branching in 1914 paper.

Architecture emulated here:
  - FloatingPointNumber: 8-digit mantissa, 2-digit exponent, sign bits.
  - TorresQuevedo: 8 relay registers (R0..R7), typewriter I/O buffer.
  - Arithmetic: add, subtract, multiply, divide on FloatingPointNumber.
  - Normalization: mantissa always in [0.1, 1.0) range.

References:
  - Torres Quevedo, L. (1914). Ensayos sobre automtica. Memoria.
    Revista de la Academia de Ciencias Exactas, Vol. 12, pp. 391-418.
  - Randell, B. (1982). From analytical engine to electronic digital computer.
    IEEE Annals, 4(4), 327-341.
  - Cortada, J. W. (1993). Before the Computer. Princeton University Press.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Any

_MANTISSA_DIGITS = 8
_EXPONENT_DIGITS = 2
_NUM_REGISTERS = 8

_MAX_MANTISSA = 10**_MANTISSA_DIGITS - 1  # 99999999
_MAX_EXPONENT = 10**_EXPONENT_DIGITS - 1  # 99
_MIN_EXPONENT = -(10**_EXPONENT_DIGITS - 1)  # -99


@dataclass
class FloatingPointNumber:
    """Torres y Quevedo floating-point number.

    Representation: mantissa * 10^exponent
    Mantissa: 8 decimal digits, normalized to [0.1, 1.0) when non-zero.
    Exponent: 2-digit signed integer [-99, 99].

    This is the first proposed floating-point format in computing history
    (Torres 1914), predating IEEE 754 by over 60 years.
    """

    mantissa: float = 0.0  # 8-digit decimal fraction in [0, 1)
    exponent: int = 0  # signed 2-digit exponent
    negative: bool = False  # sign bit

    def __post_init__(self) -> None:
        if abs(self.mantissa) >= 1.0 and self.mantissa != 0.0:
            raise ValueError(f"Mantissa must be in [0, 1): got {self.mantissa}")

    @classmethod
    def from_float(cls, value: float) -> FloatingPointNumber:
        """Convert Python float to Torres floating-point."""
        if value == 0.0:
            return cls(0.0, 0, False)
        neg = value < 0
        value = abs(value)
        # Normalize: find exponent such that 0.1 <= mantissa < 1.0
        exp = math.floor(math.log10(value)) + 1
        mantissa = value / (10.0**exp)
        # Round mantissa to 8 decimal digits
        mantissa = round(mantissa, _MANTISSA_DIGITS)
        # Clamp exponent
        exp = max(_MIN_EXPONENT, min(_MAX_EXPONENT, exp))
        return cls(mantissa, exp, neg)

    def to_float(self) -> float:
        """Convert to Python float."""
        if self.mantissa == 0.0:
            return 0.0
        val = self.mantissa * (10.0**self.exponent)
        return -val if self.negative else val

    def __repr__(self) -> str:
        sign = "-" if self.negative else "+"
        return f"TQ({sign}{self.mantissa:.{_MANTISSA_DIGITS}f}e{self.exponent:+d})"

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, FloatingPointNumber):
            return NotImplemented
        return abs(self.to_float() - other.to_float()) < 1e-12


def _fp_add(a: FloatingPointNumber, b: FloatingPointNumber) -> FloatingPointNumber:
    """Add two Torres floating-point numbers."""
    return FloatingPointNumber.from_float(a.to_float() + b.to_float())


def _fp_sub(a: FloatingPointNumber, b: FloatingPointNumber) -> FloatingPointNumber:
    """Subtract b from a."""
    return FloatingPointNumber.from_float(a.to_float() - b.to_float())


def _fp_mul(a: FloatingPointNumber, b: FloatingPointNumber) -> FloatingPointNumber:
    """Multiply two Torres floating-point numbers."""
    return FloatingPointNumber.from_float(a.to_float() * b.to_float())


def _fp_div(a: FloatingPointNumber, b: FloatingPointNumber) -> FloatingPointNumber:
    """Divide a by b."""
    if b.to_float() == 0.0:
        raise ZeroDivisionError("Torres FP division by zero")
    return FloatingPointNumber.from_float(a.to_float() / b.to_float())


@dataclass
class TorresState:
    """State of the Torres y Quevedo electromechanical calculator."""

    registers: list[FloatingPointNumber] = field(
        default_factory=lambda: [FloatingPointNumber() for _ in range(_NUM_REGISTERS)]
    )
    typewriter_output: list[str] = field(default_factory=list)
    cycle_count: int = 0
    program_pointer: int = 0
    program_tape: list[tuple[Any, ...]] = field(default_factory=list)


class TorresQuevedo:
    """Torres y Quevedo electromechanical calculator.

    Usage:
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(3.14))
        t.load_register(1, FloatingPointNumber.from_float(2.0))
        result = t.add(0, 1, dest=2)  # R2 = R0 + R1
        print(result.to_float())       # 5.14
    """

    def __init__(self) -> None:
        self.state = TorresState()

    def reset(self) -> None:
        """Reset all registers and output."""
        self.state = TorresState()

    def load_register(self, reg: int, value: FloatingPointNumber) -> None:
        """Load a floating-point value into register Rn."""
        self._check_reg(reg)
        self.state.registers[reg] = value
        self.state.cycle_count += 1

    def read_register(self, reg: int) -> FloatingPointNumber:
        """Read value from register Rn."""
        self._check_reg(reg)
        return self.state.registers[reg]

    def add(self, r1: int, r2: int, dest: int) -> FloatingPointNumber:
        """dest = R[r1] + R[r2]"""
        self._check_reg(r1, r2, dest)
        result = _fp_add(self.state.registers[r1], self.state.registers[r2])
        self.state.registers[dest] = result
        self.state.cycle_count += 1
        return result

    def subtract(self, r1: int, r2: int, dest: int) -> FloatingPointNumber:
        """dest = R[r1] - R[r2]"""
        self._check_reg(r1, r2, dest)
        result = _fp_sub(self.state.registers[r1], self.state.registers[r2])
        self.state.registers[dest] = result
        self.state.cycle_count += 1
        return result

    def multiply(self, r1: int, r2: int, dest: int) -> FloatingPointNumber:
        """dest = R[r1] * R[r2]"""
        self._check_reg(r1, r2, dest)
        result = _fp_mul(self.state.registers[r1], self.state.registers[r2])
        self.state.registers[dest] = result
        self.state.cycle_count += 1
        return result

    def divide(self, r1: int, r2: int, dest: int) -> FloatingPointNumber:
        """dest = R[r1] / R[r2]"""
        self._check_reg(r1, r2, dest)
        result = _fp_div(self.state.registers[r1], self.state.registers[r2])
        self.state.registers[dest] = result
        self.state.cycle_count += 1
        return result

    def typewriter_print(self, reg: int) -> str:
        """Print register value via typewriter (telegraph output)."""
        self._check_reg(reg)
        val = self.state.registers[reg]
        output = repr(val)
        self.state.typewriter_output.append(output)
        return output

    def load_program(self, tape: list[tuple[Any, ...]]) -> None:
        """Load a program tape and reset the program pointer.

        WHY: step() needs a program_tape to dispatch from. Loading a program
        here allows the MachineAdapter to drive execution via step() calls
        one relay-cycle at a time.
        """
        self.state.program_tape = list(tape)
        self.state.program_pointer = 0

    def step(self) -> None:
        """Execute one instruction from program_tape[program_pointer], then advance.

        WHY: Fixes P1 -- the original step() was a PC-increment stub. One step
        corresponds to one relay cycle: an instruction is selected and executed.
        """
        pp = self.state.program_pointer
        if pp < len(self.state.program_tape):
            instr = self.state.program_tape[pp]
            op = instr[0]
            if op == "add":
                self.add(instr[1], instr[2], instr[3])
            elif op == "sub":
                self.subtract(instr[1], instr[2], instr[3])
            elif op == "mul":
                self.multiply(instr[1], instr[2], instr[3])
            elif op == "div":
                self.divide(instr[1], instr[2], instr[3])
            elif op == "print":
                self.typewriter_print(instr[1])
        self.state.program_pointer += 1
        self.state.cycle_count += 1

    def state_dict(self) -> dict[str, object]:
        """Return current state as a plain dict."""
        return {
            "registers": [r.to_float() for r in self.state.registers],
            "cycle_count": self.state.cycle_count,
            "typewriter_output": list(self.state.typewriter_output),
        }

    def run(self, program: list[tuple[Any, ...]]) -> list[FloatingPointNumber]:
        """Run a simple program: list of (op, r1, r2, dest) tuples."""
        results = []
        for instr in program:
            op = instr[0]
            if op == "add":
                results.append(self.add(instr[1], instr[2], instr[3]))
            elif op == "sub":
                results.append(self.subtract(instr[1], instr[2], instr[3]))
            elif op == "mul":
                results.append(self.multiply(instr[1], instr[2], instr[3]))
            elif op == "div":
                results.append(self.divide(instr[1], instr[2], instr[3]))
            elif op == "print":
                self.typewriter_print(instr[1])
        return results

    def _check_reg(self, *regs: int) -> None:
        for r in regs:
            if not (0 <= r < _NUM_REGISTERS):
                raise IndexError(f"Register {r} out of range (0..{_NUM_REGISTERS - 1})")
