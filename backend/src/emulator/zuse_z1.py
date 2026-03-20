"""Konrad Zuse Z1 Emulator (Germany, 1938).

The Zuse Z1 was the first mechanical binary computer, built by Konrad Zuse
in his parents' living room in Berlin. It used binary floating-point arithmetic,
punched tape control, and mechanical relay memory.

Architecture:
  - 22-bit floating-point word: 1 sign + 7 exponent + 14 mantissa bits.
  - 64 words of mechanical relay memory.
  - Binary arithmetic unit (add, subtract, multiply, divide).
  - Punched (celluloid movie film) tape input.
  - Lamp panel output (lamp array showing binary state).

ZuseFloat encoding (Zuse Z1 format):
  Bit layout: [sign(1)] [exponent(7)] [mantissa(14)]
  - Sign: 0 = positive, 1 = negative (sign-magnitude).
  - Exponent: biased by 64 (stored = actual + 64).
  - Mantissa: normalized binary fraction in [0.5, 1.0), stored without leading 1
              (i.e. the hidden bit is assumed).
  - Zero: all bits zero.

References:
  - Zuse, K. (1993). The Computer -- My Life. Springer-Verlag.
  - Rojas, R. (1997). The Zuse computers. IEEE Annals of History of Computing, 19(2).
  - Bauer, F. L. (1999). Historical notes on recursive machines. Lecture Notes.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

# Z1 word format constants
_SIGN_BITS = 1
_EXPONENT_BITS = 7
_MANTISSA_BITS = 14
_WORD_BITS = _SIGN_BITS + _EXPONENT_BITS + _MANTISSA_BITS  # 22
_EXPONENT_BIAS = 64
_MANTISSA_SCALE = 2**_MANTISSA_BITS  # 16384
_MAX_EXPONENT = 2**_EXPONENT_BITS - 1  # 127 (stored), actual = 63
_MEMORY_SIZE = 64


@dataclass
class ZuseFloat:
    """Z1 22-bit binary floating-point number.

    Encoding: sign(1) | exponent(7) | mantissa(14)
    Value = (-1)^sign * 2^(exponent - BIAS) * (1 + mantissa/2^14)

    The Z1 used sign-magnitude representation (unusual -- most machines
    used two's complement, but Zuse chose sign-magnitude for clarity).
    """

    word: int = 0  # raw 22-bit integer

    @classmethod
    def from_float(cls, value: float) -> ZuseFloat:
        """Convert Python float to Z1 22-bit floating-point."""
        if value == 0.0:
            return cls(0)

        sign = 1 if value < 0 else 0
        value = abs(value)

        # Find exponent (base-2)
        import math

        exp = math.floor(math.log2(value))
        mantissa_val = value / (2.0**exp) - 1.0  # hidden bit subtracted

        # Clamp exponent to 7-bit biased range
        stored_exp = exp + _EXPONENT_BIAS
        stored_exp = max(0, min(_MAX_EXPONENT, stored_exp))

        # Quantize mantissa to 14 bits
        mantissa_int = int(round(mantissa_val * _MANTISSA_SCALE))
        mantissa_int = max(0, min(_MANTISSA_SCALE - 1, mantissa_int))

        word = (
            (sign << (_EXPONENT_BITS + _MANTISSA_BITS))
            | (stored_exp << _MANTISSA_BITS)
            | mantissa_int
        )
        return cls(word & ((1 << _WORD_BITS) - 1))

    def to_float(self) -> float:
        """Convert Z1 22-bit float to Python float."""
        if self.word == 0:
            return 0.0

        sign = (self.word >> (_EXPONENT_BITS + _MANTISSA_BITS)) & 1
        stored_exp = (self.word >> _MANTISSA_BITS) & ((1 << _EXPONENT_BITS) - 1)
        mantissa_int = self.word & ((1 << _MANTISSA_BITS) - 1)

        actual_exp = stored_exp - _EXPONENT_BIAS
        mantissa_val = 1.0 + mantissa_int / _MANTISSA_SCALE
        value = mantissa_val * (2.0**actual_exp)

        return float(-value if sign else value)

    def __repr__(self) -> str:
        return f"ZuseFloat({self.to_float():.8g}, word=0b{self.word:022b})"

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, ZuseFloat):
            return NotImplemented
        return abs(self.to_float() - other.to_float()) < 1e-6

    def __add__(self, other: ZuseFloat) -> ZuseFloat:
        return ZuseFloat.from_float(self.to_float() + other.to_float())

    def __sub__(self, other: ZuseFloat) -> ZuseFloat:
        return ZuseFloat.from_float(self.to_float() - other.to_float())

    def __mul__(self, other: ZuseFloat) -> ZuseFloat:
        return ZuseFloat.from_float(self.to_float() * other.to_float())

    def __truediv__(self, other: ZuseFloat) -> ZuseFloat:
        if other.to_float() == 0.0:
            raise ZeroDivisionError("Z1 division by zero")
        return ZuseFloat.from_float(self.to_float() / other.to_float())

    def is_zero(self) -> bool:
        return self.word == 0 or self.to_float() == 0.0


@dataclass
class ZuseZ1State:
    """State of the Zuse Z1 mechanical binary computer."""

    memory: list[ZuseFloat] = field(default_factory=lambda: [ZuseFloat(0)] * _MEMORY_SIZE)
    # Accumulator register (the Z1 had a single main accumulator)
    accumulator: ZuseFloat = field(default_factory=ZuseFloat)
    # Auxiliary register (for intermediate values)
    auxiliary: ZuseFloat = field(default_factory=ZuseFloat)
    program_counter: int = 0
    cycle_count: int = 0
    lamp_panel: int = 0  # 22-bit lamp display state
    output_tape: list[float] = field(default_factory=list)
    tape_input: list[float] = field(default_factory=list)


class ZuseZ1:
    """Zuse Z1 mechanical binary computer emulator.

    Usage:
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(3.5))
        z.store(1, ZuseFloat.from_float(2.0))
        z.load(0)                               # accumulator = 3.5
        z.add_memory(1)                         # accumulator += 2.0
        print(z.state.accumulator.to_float())   # 5.5
    """

    def __init__(self) -> None:
        self.state = ZuseZ1State()

    def reset(self) -> None:
        """Reset machine to initial state."""
        self.state = ZuseZ1State()

    def store(self, addr: int, value: ZuseFloat) -> None:
        """Store value in memory at address addr (0..63)."""
        self._check_addr(addr)
        self.state.memory[addr] = value
        self._update_lamp_panel()

    def load(self, addr: int) -> ZuseFloat:
        """Load value from memory into accumulator."""
        self._check_addr(addr)
        self.state.accumulator = self.state.memory[addr]
        self._update_lamp_panel()
        self.state.cycle_count += 1
        return self.state.accumulator

    def add_memory(self, addr: int) -> ZuseFloat:
        """Accumulator += memory[addr]."""
        self._check_addr(addr)
        self.state.accumulator = self.state.accumulator + self.state.memory[addr]
        self._update_lamp_panel()
        self.state.cycle_count += 1
        return self.state.accumulator

    def sub_memory(self, addr: int) -> ZuseFloat:
        """Accumulator -= memory[addr]."""
        self._check_addr(addr)
        self.state.accumulator = self.state.accumulator - self.state.memory[addr]
        self._update_lamp_panel()
        self.state.cycle_count += 1
        return self.state.accumulator

    def mul_memory(self, addr: int) -> ZuseFloat:
        """Accumulator *= memory[addr]."""
        self._check_addr(addr)
        self.state.accumulator = self.state.accumulator * self.state.memory[addr]
        self._update_lamp_panel()
        self.state.cycle_count += 1
        return self.state.accumulator

    def div_memory(self, addr: int) -> ZuseFloat:
        """Accumulator /= memory[addr]."""
        self._check_addr(addr)
        self.state.accumulator = self.state.accumulator / self.state.memory[addr]
        self._update_lamp_panel()
        self.state.cycle_count += 1
        return self.state.accumulator

    def store_accumulator(self, addr: int) -> None:
        """Store accumulator into memory[addr]."""
        self._check_addr(addr)
        self.state.memory[addr] = self.state.accumulator
        self.state.cycle_count += 1

    def read_tape(self) -> ZuseFloat:
        """Read next value from punched tape input."""
        if not self.state.tape_input:
            raise IndexError("Tape input exhausted")
        val = self.state.tape_input.pop(0)
        self.state.accumulator = ZuseFloat.from_float(val)
        self.state.cycle_count += 1
        return self.state.accumulator

    def write_output(self) -> float:
        """Write accumulator value to output tape and lamp panel."""
        val = self.state.accumulator.to_float()
        self.state.output_tape.append(val)
        self._update_lamp_panel()
        return val

    def step(self) -> None:
        """Advance program counter by 1."""
        self.state.program_counter += 1
        self.state.cycle_count += 1

    def run(self, program: list[tuple[Any, ...]]) -> list[float]:
        """Run a simple program: list of (op, ...) tuples.

        Supported ops:
          ('load', addr)        -- load memory[addr] into accumulator
          ('store', addr)       -- store accumulator to memory[addr]
          ('add', addr)         -- accumulator += memory[addr]
          ('sub', addr)         -- accumulator -= memory[addr]
          ('mul', addr)         -- accumulator *= memory[addr]
          ('div', addr)         -- accumulator /= memory[addr]
          ('imm', value)        -- load immediate float into accumulator
          ('output',)           -- write accumulator to output tape
        """
        results = []
        for instr in program:
            op = instr[0]
            if op == "load":
                self.load(instr[1])
            elif op == "store":
                self.store_accumulator(instr[1])
            elif op == "add":
                self.add_memory(instr[1])
            elif op == "sub":
                self.sub_memory(instr[1])
            elif op == "mul":
                self.mul_memory(instr[1])
            elif op == "div":
                self.div_memory(instr[1])
            elif op == "imm":
                self.state.accumulator = ZuseFloat.from_float(float(instr[1]))
            elif op == "output":
                results.append(self.write_output())
            self.state.program_counter += 1
        return results

    def state_dict(self) -> dict[str, object]:
        """Return current state as a plain dict."""
        return {
            "accumulator": self.state.accumulator.to_float(),
            "auxiliary": self.state.auxiliary.to_float(),
            "program_counter": self.state.program_counter,
            "cycle_count": self.state.cycle_count,
            "lamp_panel": f"0b{self.state.lamp_panel:022b}",
            "output_tape": list(self.state.output_tape),
        }

    def _check_addr(self, addr: int) -> None:
        if not (0 <= addr < _MEMORY_SIZE):
            raise IndexError(f"Memory address {addr} out of range (0..{_MEMORY_SIZE - 1})")

    def _update_lamp_panel(self) -> None:
        """Update lamp panel to reflect accumulator state."""
        self.state.lamp_panel = self.state.accumulator.word
