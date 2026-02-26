"""Difference Engine tabulator: finite-difference polynomial table generator.

Implements the method of finite differences as used by DE2 to produce
polynomial tables. Given initial difference values, the engine repeatedly
adds each difference to the next-lower order, producing exact polynomial
values without multiplication.

Example: x^2
  Initial values: D0=0, D1=1, D2=2
  Cycle 1: D0 += D1 -> 1;  D1 += D2 -> 3
  Cycle 2: D0 += D1 -> 4;  D1 += D2 -> 5
  Cycle 3: D0 += D1 -> 9;  D1 += D2 -> 7
  ...

This module operates on Python integers (arbitrary precision) to serve
as a reference oracle for the 31-digit hardware columns.

References:
  SOURCE:SMG-DE2-TECH -- Method of finite differences
  SOURCE:SWADE-2001 -- DE2 operational description
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import List, Optional


@dataclass
class TabulatorState:
    """State of the DE tabulator at one point in time."""
    row: int
    differences: List[int]  # D0, D1, D2, ..., Dn


def compute_initial_differences(
    coefficients: List[int],
    num_columns: int = 8,
) -> List[int]:
    """Compute initial difference values for a polynomial.

    Given polynomial coefficients [a0, a1, a2, ...] representing
    f(x) = a0 + a1*x + a2*x^2 + ..., compute the initial row of
    finite differences needed by the DE tabulator.

    The k-th order difference of a degree-n polynomial is:
      D^k f(0) = sum_{j=0}^{k} (-1)^{k-j} C(k,j) * f(j)

    Args:
        coefficients: Polynomial coefficients [a0, a1, a2, ...]
        num_columns: Number of difference columns (must be >= degree+1)

    Returns:
        List of initial differences [D0, D1, D2, ..., D_{num_columns-1}]
    """
    degree = len(coefficients) - 1

    def poly_eval(x: int) -> int:
        result = 0
        for i, c in enumerate(coefficients):
            result += c * (x ** i)
        return result

    # Compute f(0), f(1), f(2), ..., f(num_columns-1)
    values = [poly_eval(x) for x in range(num_columns)]

    # Forward differences: D^k[0] = sum of (-1)^{k-j} * C(k,j) * f(j)
    differences = list(values)
    for order in range(1, num_columns):
        new_diffs = []
        for i in range(len(differences) - 1):
            new_diffs.append(differences[i + 1] - differences[i])
        differences = new_diffs

        # After `order` rounds of differencing, differences[0] = D^order[0]
        # But we need to collect them differently...

    # Simpler approach: build the difference table directly
    table = [values]
    for order in range(1, num_columns):
        row = []
        prev = table[-1]
        for i in range(len(prev) - 1):
            row.append(prev[i + 1] - prev[i])
        table.append(row)

    # Initial differences are the first element of each row
    result = [table[i][0] if i < len(table) and table[i] else 0
              for i in range(num_columns)]

    return result


def tabulate(
    initial_differences: List[int],
    num_rows: int,
) -> List[TabulatorState]:
    """Run the DE tabulator for num_rows cycles.

    Starting from the given initial differences, repeatedly add each
    higher-order difference to the next-lower order (right to left).

    Args:
        initial_differences: [D0, D1, D2, ..., Dn] at row 0
        num_rows: Number of table rows to produce

    Returns:
        List of TabulatorState, one per row (including row 0)
    """
    diffs = list(initial_differences)
    n = len(diffs)
    states = [TabulatorState(row=0, differences=list(diffs))]

    for row_num in range(1, num_rows + 1):
        # DE2 adds each column using the PREVIOUS cycle's values.
        # Snapshot before updating to avoid cascading within one cycle.
        old = list(diffs)
        for i in range(n - 1):
            diffs[i] += old[i + 1]
        states.append(TabulatorState(row=row_num, differences=list(diffs)))

    return states


def tabulate_polynomial(
    coefficients: List[int],
    num_rows: int,
    num_columns: int = 8,
) -> List[TabulatorState]:
    """Compute a polynomial table using the method of finite differences.

    Convenience function combining compute_initial_differences and tabulate.

    Args:
        coefficients: Polynomial coefficients [a0, a1, a2, ...]
        num_rows: Number of rows to tabulate
        num_columns: Number of difference columns

    Returns:
        List of TabulatorState with D0 = f(x) at each row
    """
    init_diffs = compute_initial_differences(coefficients, num_columns)
    return tabulate(init_diffs, num_rows)


def tabulate_to_csv(states: List[TabulatorState]) -> str:
    """Convert tabulation results to CSV format.

    Format matches the golden trace specification in PRINTER_SPEC.md.
    """
    if not states:
        return ""

    num_cols = len(states[0].differences)
    header = "row," + ",".join(f"D{i}" for i in range(num_cols))
    lines = [header]

    for state in states:
        vals = ",".join(str(d) for d in state.differences)
        lines.append(f"{state.row},{vals}")

    return "\n".join(lines)
