"""Tests for the Difference Engine tabulator.

Verifies the finite-difference method produces correct polynomial tables.
These tests serve as the software twin's acceptance criteria for the
physical DE2 subsystem (see docs/hardware/TEST_PLAN.md, T3-01).
"""

import csv
from pathlib import Path

import pytest

from hardware_twin.de_tabulator import (
    compute_initial_differences,
    tabulate,
    tabulate_polynomial,
    tabulate_to_csv,
)

GOLDEN_TRACE_DIR = Path(__file__).resolve().parent.parent / "golden_traces"


class TestInitialDifferences:
    """compute_initial_differences produces correct D0..Dn."""

    def test_constant(self):
        # f(x) = 5 -> D0=5, D1=0, ...
        diffs = compute_initial_differences([5], 4)
        assert diffs[0] == 5
        assert all(d == 0 for d in diffs[1:])

    def test_linear(self):
        # f(x) = 3x + 2 -> D0=2, D1=3, D2=0, ...
        diffs = compute_initial_differences([2, 3], 4)
        assert diffs[0] == 2
        assert diffs[1] == 3
        assert all(d == 0 for d in diffs[2:])

    def test_quadratic(self):
        # f(x) = x^2 -> D0=0, D1=1, D2=2, D3=0, ...
        diffs = compute_initial_differences([0, 0, 1], 4)
        assert diffs == [0, 1, 2, 0]

    def test_cubic(self):
        # f(x) = x^3 -> D0=0, D1=1, D2=6, D3=6, D4=0, ...
        diffs = compute_initial_differences([0, 0, 0, 1], 5)
        assert diffs == [0, 1, 6, 6, 0]


class TestTabulate:
    """tabulate() produces correct polynomial values."""

    def test_x_squared(self):
        """f(x) = x^2 for x = 0..10."""
        states = tabulate_polynomial([0, 0, 1], num_rows=10, num_columns=4)
        for s in states:
            assert s.differences[0] == s.row ** 2

    def test_x_cubed(self):
        """f(x) = x^3 for x = 0..10."""
        states = tabulate_polynomial([0, 0, 0, 1], num_rows=10, num_columns=5)
        for s in states:
            assert s.differences[0] == s.row ** 3

    def test_2x_plus_1(self):
        """f(x) = 2x + 1 for x = 0..20."""
        states = tabulate_polynomial([1, 2], num_rows=20, num_columns=4)
        for s in states:
            assert s.differences[0] == 2 * s.row + 1

    def test_large_table(self):
        """100-row table for x^2 (matches acceptance test T3-01)."""
        states = tabulate_polynomial([0, 0, 1], num_rows=100, num_columns=8)
        assert len(states) == 101  # row 0 through row 100
        for s in states:
            assert s.differences[0] == s.row ** 2

    def test_constant_differences_preserved(self):
        """For degree-n polynomial, D_n is constant throughout."""
        states = tabulate_polynomial([0, 0, 1], num_rows=50, num_columns=4)
        # D2 should always be 2 for x^2
        for s in states:
            assert s.differences[2] == 2

    def test_row_zero_is_initial(self):
        """Row 0 contains the initial differences unchanged."""
        init = [5, 3, 2, 0]
        states = tabulate(init, num_rows=1)
        assert states[0].differences == init


class TestGoldenTrace:
    """Verify golden trace CSV matches computed values."""

    def test_poly_table_100_exists(self):
        trace_path = GOLDEN_TRACE_DIR / "poly_table_100.csv"
        assert trace_path.exists(), f"Golden trace missing: {trace_path}"

    def test_poly_table_100_matches(self):
        """Golden trace for x^2 matches tabulator output."""
        trace_path = GOLDEN_TRACE_DIR / "poly_table_100.csv"
        states = tabulate_polynomial([0, 0, 1], num_rows=100, num_columns=8)

        with open(trace_path, "r") as f:
            reader = csv.reader(f)
            header = next(reader)
            assert header[0] == "row"

            for csv_row, state in zip(reader, states):
                row_num = int(csv_row[0])
                assert row_num == state.row
                d0 = int(csv_row[1])
                assert d0 == state.differences[0], (
                    f"Row {row_num}: golden={d0}, computed={state.differences[0]}"
                )


class TestCSVOutput:
    """tabulate_to_csv produces valid CSV."""

    def test_csv_header(self):
        states = tabulate_polynomial([0, 0, 1], num_rows=2, num_columns=4)
        csv_text = tabulate_to_csv(states)
        lines = csv_text.splitlines()
        assert lines[0] == "row,D0,D1,D2,D3"

    def test_csv_row_count(self):
        states = tabulate_polynomial([0, 0, 1], num_rows=5, num_columns=4)
        csv_text = tabulate_to_csv(states)
        lines = csv_text.splitlines()
        assert len(lines) == 7  # header + 6 rows (0..5)

    def test_empty_states(self):
        assert tabulate_to_csv([]) == ""
