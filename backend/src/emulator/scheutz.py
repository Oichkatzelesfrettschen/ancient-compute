"""Scheutz Difference Engine Emulator (Sweden, 1851).

The Scheutz engine was the first commercially manufactured difference engine,
built by Georg and Edvard Scheutz and demonstrated in Stockholm in 1851.
It computes polynomial tables by finite differences.

Architecture:
  - 7 difference registers (D0 through D6) of 15 decimal digits each.
  - D0 holds the tabulated function value.
  - D1..D6 hold successive differences.
  - Each "crank" cycle adds D1 into D0, D2 into D1, ..., D6 into D5.
  - Supports polynomials up to 6th order (7 registers = 6th differences).
  - 15-digit precision (historical Scheutz specification).

Operation:
  - Load initial differences for the polynomial.
  - Call crank() to advance one step.
  - D0 holds the next tabulated value after each crank.

References:
  - Lindgren, M. (1990). Glory and Failure: The Difference Engines.
  - Scheutz, G. (1857). Specimens of Tables Calculated by the Engine.
  - Dubbey, J. M. (1978). The Mathematical Work of Charles Babbage.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from decimal import Decimal, getcontext

# 15-digit precision matching historical Scheutz specification
getcontext().prec = 15

_NUM_REGISTERS = 7
_DIGITS = 15


@dataclass
class ScheutzState:
    """State of the Scheutz Difference Engine."""

    registers: list[Decimal] = field(default_factory=lambda: [Decimal(0)] * _NUM_REGISTERS)
    cycle_count: int = 0
    output_table: list[Decimal] = field(default_factory=list)
    overflow_flags: list[bool] = field(default_factory=lambda: [False] * _NUM_REGISTERS)


class ScheutzDifferenceEngine:
    """Scheutz Difference Engine -- 7 registers, 15-digit, 6th-order polynomial.

    Usage:
        engine = ScheutzDifferenceEngine()
        engine.load([f(x0), delta1, delta2, ...])  # load initial differences
        engine.tabulate(10)                         # compute 10 table entries
        print(engine.state.output_table)
    """

    def __init__(self) -> None:
        self.state = ScheutzState()

    def reset(self) -> None:
        """Reset all registers and output to zero."""
        self.state = ScheutzState()

    def load(self, initial_values: list[float]) -> None:
        """Load initial difference values into registers D0..D(n-1).

        Args:
            initial_values: List of initial differences. Must have <= 7 values.
                            initial_values[0] = f(x0), the first table entry.
                            initial_values[1] = first difference, etc.
        """
        if len(initial_values) > _NUM_REGISTERS:
            raise ValueError(f"Too many initial values (max {_NUM_REGISTERS})")
        for i, v in enumerate(initial_values):
            self.state.registers[i] = Decimal(str(v))

    def crank(self) -> Decimal:
        """Execute one addition cycle.

        Adds differences from lowest to highest (D0+=D1, D1+=D2, ..., D5+=D6).
        Records D0 in the output table.

        Returns:
            The new value of D0 (the tabulated function value).
        """
        regs = self.state.registers

        # Propagate differences from lowest to highest order:
        # D0 += D1 (using old D1), then D1 += D2 (using old D2), etc.
        # This is the correct mechanical order for a difference engine.
        for i in range(_NUM_REGISTERS - 1):
            new_val = regs[i] + regs[i + 1]
            # Check 15-digit overflow
            if abs(new_val) >= Decimal(10**_DIGITS):
                self.state.overflow_flags[i] = True
            regs[i] = new_val

        self.state.cycle_count += 1
        self.state.output_table.append(regs[0])
        return regs[0]

    def tabulate(self, n: int) -> list[Decimal]:
        """Compute n successive table entries by cranking n times.

        Args:
            n: Number of table entries to compute.

        Returns:
            List of n Decimal values (D0 after each crank).
        """
        results = []
        for _ in range(n):
            results.append(self.crank())
        return results

    def state_dict(self) -> dict[str, object]:
        """Return current state as a plain dict."""
        return {
            "registers": [float(r) for r in self.state.registers],
            "cycle_count": self.state.cycle_count,
            "output_table": [float(v) for v in self.state.output_table],
            "overflow_flags": list(self.state.overflow_flags),
        }

    def step(self) -> Decimal:
        """Alias for crank() -- matches MachineAdapter interface."""
        return self.crank()

    def run(self, n: int) -> list[Decimal]:
        """Alias for tabulate(n)."""
        return self.tabulate(n)
