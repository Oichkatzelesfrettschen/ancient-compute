"""Grant Difference Engine Emulator (United States, 1876).

George Barnard Grant (1850-1903) built his Difference Engine in 1876 and
exhibited it at the Philadelphia Centennial Exposition. It was one of the
largest mechanical calculators ever constructed (1,500 lbs, 5 feet tall)
and the first American difference engine.

Architecture:
  - 15 difference registers (D0 through D14), each holding 30 decimal digits.
    Grant's machine actually supported up to 8 orders of difference, but the
    mechanical design allowed more; this emulator follows the architectural
    description in his 1876 paper.
  - D0 holds the tabulated function value.
  - D1..DN hold successive finite differences.
  - Each "revolution" adds D[k+1] into D[k] for k = N-1, N-2, ..., 0.
    (additions proceed from highest to lowest order, as in all difference
    engines, to avoid carry propagation across partially-updated registers).
  - 30-digit precision (Grant's stated specification).
  - Output: automatic printout of D0 after each revolution.

Historical context:
  Grant published the design in his 1871 paper "On a New Difference Engine"
  (American Journal of Science, 1871) and exhibited the working machine at
  Philadelphia in 1876. Unlike Babbage's unfinished engine, Grant's was
  fully operational. It was used by the US Coast and Geodetic Survey.

  The key mechanical innovation was Grant's use of a spur-gear contrate wheel
  ("sector wheel") to implement the carry mechanism more reliably than
  Babbage's anticipating carriage.

References:
  - Grant, G. B. (1871). On a New Difference Engine. American Journal of
    Science, 2nd series, Vol. 2, pp. 167-171.
  - Grant, G. B. (1876). A New Difference Engine. Exhibit catalog,
    US Centennial Exhibition, Philadelphia.
  - Williams, M. R. (1985). A History of Computing Technology.
    Prentice Hall, pp. 138-141.
  - Dubbey, J. M. (1978). The Mathematical Work of Charles Babbage.
    Cambridge University Press.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from decimal import Decimal, getcontext

# 30-digit precision matching Grant's stated specification
getcontext().prec = 30

_MAX_REGISTERS = 15  # D0..D14 (supports up to 14th-order differences)
_DIGITS = 30


@dataclass
class GrantState:
    """Observable state of Grant's Difference Engine."""

    registers: list[Decimal] = field(default_factory=lambda: [Decimal(0)] * _MAX_REGISTERS)
    num_orders: int = 0  # Number of active difference orders (1..14)
    revolution_count: int = 0  # Total number of crank revolutions
    output_table: list[Decimal] = field(default_factory=list)
    overflow_flags: list[bool] = field(default_factory=lambda: [False] * _MAX_REGISTERS)


class GrantDifferenceEngine:
    """Grant Difference Engine -- up to 15 registers, 30-digit, 14th-order polynomial.

    Usage::

        engine = GrantDifferenceEngine()
        engine.load([f(x0), d1, d2, d3])   # 3rd-order differences
        engine.tabulate(10)                  # compute 10 table entries
        for v in engine.state.output_table:
            print(v)

    The loaded list must have at least 2 entries: f(x0) and first difference.
    The highest-order difference (last entry in the load list) must be constant
    for exact tabulation of polynomials.
    """

    def __init__(self) -> None:
        self.state = GrantState()

    def reset(self) -> None:
        """Reset all registers, output table, and revolution count."""
        self.state = GrantState()

    def load(self, initial_values: list[float]) -> None:
        """Load initial difference values into registers D0..D(n-1).

        Args:
            initial_values: Initial differences, length 2..15.
                initial_values[0] = f(x0), the first table entry.
                initial_values[1] = first finite difference delta_1.
                initial_values[k] = k-th finite difference delta_k.

        Raises:
            ValueError: if too few or too many values supplied.
        """
        if len(initial_values) < 2:
            raise ValueError("Need at least 2 values: f(x0) and one difference")
        if len(initial_values) > _MAX_REGISTERS:
            raise ValueError(f"Too many initial values (max {_MAX_REGISTERS} registers)")
        for i, v in enumerate(initial_values):
            self.state.registers[i] = Decimal(str(v))
        self.state.num_orders = len(initial_values) - 1

    def crank(self) -> Decimal:
        """Execute one full revolution of the engine.

        Adds D[k+1] into D[k] for k = num_orders-1 down to 0.
        The additions proceed from the highest difference order to the lowest
        to prevent carry contamination between partially-updated registers
        (as in all difference engine designs, following Babbage's specification
        for the DE1 addend-order requirement).

        After all additions, D0 holds the next table value, which is appended
        to output_table.

        Returns:
            The new value of D0 (the latest table entry).
        """
        n = self.state.num_orders
        if n == 0:
            raise RuntimeError("Engine not loaded; call load() first")

        # Add D[k+1] into D[k] for k = 0, 1, ..., n-1  (lowest to highest).
        # This uses each D[k+1] BEFORE it is updated, which is the correct
        # mechanical order -- the same as the Scheutz engine and Babbage's DE1
        # specification (Dubbey 1978, p. 72; Grant 1871, p. 169).
        for k in range(n):
            new_val = self.state.registers[k] + self.state.registers[k + 1]
            # Check for 30-digit overflow
            limit = Decimal(10) ** _DIGITS
            if abs(new_val) >= limit:
                self.state.overflow_flags[k] = True
                new_val = new_val % limit
            else:
                self.state.overflow_flags[k] = False
            self.state.registers[k] = new_val

        self.state.revolution_count += 1
        result = self.state.registers[0]
        self.state.output_table.append(result)
        return result

    def tabulate(self, n: int) -> list[Decimal]:
        """Compute n consecutive table entries by cranking n times.

        Args:
            n: Number of table entries to generate.

        Returns:
            List of n Decimal values (also appended to state.output_table).
        """
        if n < 0:
            raise ValueError("n must be non-negative")
        return [self.crank() for _ in range(n)]

    def tabulate_from_x0(
        self, polynomial_fn: object, x0: float, step: float, n: int, order: int
    ) -> list[Decimal]:
        """Convenience: auto-compute initial differences from a callable, then tabulate.

        Computes the finite differences of polynomial_fn at x0 with increment step,
        loads them, and tabulates n entries.

        Args:
            polynomial_fn: Callable f(x) -> float.
            x0: Starting x value.
            step: Step size (h).
            n: Number of table entries to generate.
            order: Polynomial order (number of difference levels).

        Returns:
            List of n Decimal values starting from f(x0+step).
        """
        from collections.abc import Callable

        fn: Callable[[float], float] = polynomial_fn  # type: ignore[assignment]

        # Compute initial difference table
        # Row 0: f(x0), f(x0+h), ..., f(x0+order*h)
        vals = [Decimal(str(fn(x0 + i * step))) for i in range(order + 1)]
        diffs: list[Decimal] = [vals[0]]  # D0 = f(x0)

        # Build successive differences
        current = list(vals)
        for _level in range(order):
            current = [current[i + 1] - current[i] for i in range(len(current) - 1)]
            diffs.append(current[0])

        self.load([float(d) for d in diffs])
        return self.tabulate(n)

    def get_register(self, index: int) -> Decimal:
        """Return the current value of register D[index]."""
        if not 0 <= index < _MAX_REGISTERS:
            raise IndexError(f"Register index {index} out of range [0, {_MAX_REGISTERS-1}]")
        return self.state.registers[index]
