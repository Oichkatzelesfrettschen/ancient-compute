"""Unit tests for the Grant Difference Engine emulator (1876).

Tests cover: register loading, single-step crank, multi-step tabulation,
overflow detection, and tabulation of known polynomials.
"""

from decimal import Decimal

import pytest

from backend.src.emulator.grant_difference_engine import GrantDifferenceEngine

# ---------------------------------------------------------------------------
# Setup helpers
# ---------------------------------------------------------------------------


def _linear_diffs(a: float, b: float) -> list[float]:
    """Initial differences for f(x) = a + b*x (1st order).
    D0 = a, D1 = b (constant first difference).
    """
    return [a, b]


def _quadratic_diffs(a: float, b: float, c: float) -> list[float]:
    """Initial differences for f(x) = a + b*x + c*x^2 (2nd order).
    D0 = f(0), D1 = f(1)-f(0), D2 = 2c (constant second difference).
    """
    f0 = a
    f1 = a + b + c
    f2 = a + 2 * b + 4 * c
    return [f0, f1 - f0, f2 - 2 * f1 + f0]


# ---------------------------------------------------------------------------
# Load / reset
# ---------------------------------------------------------------------------


class TestLoad:
    def test_load_two_values(self):
        e = GrantDifferenceEngine()
        e.load([1.0, 2.0])
        assert e.get_register(0) == Decimal("1.0")
        assert e.get_register(1) == Decimal("2.0")

    def test_load_too_few_raises(self):
        e = GrantDifferenceEngine()
        with pytest.raises(ValueError):
            e.load([1.0])

    def test_load_too_many_raises(self):
        e = GrantDifferenceEngine()
        with pytest.raises(ValueError):
            e.load([0.0] * 16)

    def test_reset_clears_state(self):
        e = GrantDifferenceEngine()
        e.load([5.0, 1.0])
        e.crank()
        e.reset()
        assert e.state.revolution_count == 0
        assert len(e.state.output_table) == 0
        assert e.get_register(0) == Decimal("0")


# ---------------------------------------------------------------------------
# Linear sequence (1st-order polynomial)
# ---------------------------------------------------------------------------


class TestLinearTabulation:
    def test_arithmetic_sequence(self):
        """f(x) = 3 + 2*x starting at x=0: 3, 5, 7, 9, 11."""
        e = GrantDifferenceEngine()
        e.load(_linear_diffs(3.0, 2.0))
        results = [float(e.crank()) for _ in range(5)]
        assert results == [5.0, 7.0, 9.0, 11.0, 13.0]

    def test_constant_sequence(self):
        """D1 = 0: f always returns f(x0)."""
        e = GrantDifferenceEngine()
        e.load([7.0, 0.0])
        assert all(float(e.crank()) == 7.0 for _ in range(4))

    def test_negative_difference(self):
        """Descending sequence: 10, 7, 4, 1."""
        e = GrantDifferenceEngine()
        e.load([10.0, -3.0])
        results = [float(e.crank()) for _ in range(4)]
        assert results == [7.0, 4.0, 1.0, -2.0]


# ---------------------------------------------------------------------------
# Quadratic sequence (2nd-order polynomial)
# ---------------------------------------------------------------------------


class TestQuadraticTabulation:
    def test_squares(self):
        """f(x) = x^2: 0, 1, 4, 9, 16, 25, 36.
        D0=0, D1=1, D2=2 (constant second difference).
        After crank: D0 steps through 1, 4, 9, 16, 25, 36.
        """
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0, 2.0])
        results = [float(e.crank()) for _ in range(6)]
        assert results == [1.0, 4.0, 9.0, 16.0, 25.0, 36.0]

    def test_quadratic_general(self):
        """f(x) = 2 + 3*x + x^2 at x=0,1,..."""
        e = GrantDifferenceEngine()
        e.load(_quadratic_diffs(2.0, 3.0, 1.0))
        results = [float(e.crank()) for _ in range(5)]
        expected = [2 + 3 * x + x**2 for x in range(1, 6)]
        assert results == expected

    def test_revolution_count(self):
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0, 2.0])
        e.tabulate(7)
        assert e.state.revolution_count == 7


# ---------------------------------------------------------------------------
# Higher-order polynomial (3rd order)
# ---------------------------------------------------------------------------


class TestCubicTabulation:
    def test_cubes(self):
        """f(x) = x^3: 1, 8, 27, 64, 125.
        Finite differences: D0=0, D1=1, D2=6, D3=6.
        """
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0, 6.0, 6.0])
        results = [float(e.crank()) for _ in range(5)]
        assert results == [1.0, 8.0, 27.0, 64.0, 125.0]


# ---------------------------------------------------------------------------
# tabulate() convenience method
# ---------------------------------------------------------------------------


class TestTabulate:
    def test_tabulate_returns_list(self):
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0, 2.0])
        result = e.tabulate(3)
        assert len(result) == 3

    def test_tabulate_zero_entries(self):
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0, 2.0])
        assert e.tabulate(0) == []

    def test_tabulate_negative_raises(self):
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0])
        with pytest.raises(ValueError):
            e.tabulate(-1)

    def test_output_table_accumulates(self):
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0, 2.0])
        e.tabulate(3)
        e.tabulate(3)
        assert len(e.state.output_table) == 6


# ---------------------------------------------------------------------------
# Crank before load raises
# ---------------------------------------------------------------------------


class TestNotLoaded:
    def test_crank_without_load_raises(self):
        e = GrantDifferenceEngine()
        with pytest.raises(RuntimeError):
            e.crank()


# ---------------------------------------------------------------------------
# Register access
# ---------------------------------------------------------------------------


class TestRegisterAccess:
    def test_get_register_out_of_range(self):
        e = GrantDifferenceEngine()
        with pytest.raises(IndexError):
            e.get_register(15)

    def test_get_register_valid(self):
        e = GrantDifferenceEngine()
        e.load([3.0, 1.0])
        assert e.get_register(0) == Decimal("3.0")


# ---------------------------------------------------------------------------
# tabulate_from_x0 convenience method
# ---------------------------------------------------------------------------


class TestTabulateFromX0:
    def test_squares_from_callable(self):
        """Verify auto-difference-computation from f(x) = x^2."""
        e = GrantDifferenceEngine()
        results = e.tabulate_from_x0(lambda x: x**2, x0=0.0, step=1.0, n=5, order=2)
        # Starting at f(x0+h) = f(1) = 1
        assert [float(r) for r in results] == [1.0, 4.0, 9.0, 16.0, 25.0]

    def test_linear_from_callable(self):
        e = GrantDifferenceEngine()
        results = e.tabulate_from_x0(lambda x: 2 * x + 1, x0=0.0, step=1.0, n=4, order=1)
        assert [float(r) for r in results] == [3.0, 5.0, 7.0, 9.0]

    def test_cubic_from_callable(self) -> None:
        e = GrantDifferenceEngine()
        results = e.tabulate_from_x0(lambda x: x**3, x0=0.0, step=1.0, n=5, order=3)
        assert [float(r) for r in results] == [1.0, 8.0, 27.0, 64.0, 125.0]

    def test_non_unit_step(self) -> None:
        """step=0.5: f(x) = x^2 at x=0.5, 1.0, 1.5 -> 0.25, 1.0, 2.25."""
        e = GrantDifferenceEngine()
        results = e.tabulate_from_x0(lambda x: x**2, x0=0.0, step=0.5, n=3, order=2)
        expected = [0.25, 1.0, 2.25]
        for r, ex in zip(results, expected, strict=True):
            assert float(r) == pytest.approx(ex, abs=1e-10)

    def test_returns_decimal_values(self) -> None:
        e = GrantDifferenceEngine()
        results = e.tabulate_from_x0(lambda x: x, x0=0.0, step=1.0, n=3, order=1)
        assert all(isinstance(r, Decimal) for r in results)


class TestGrantStateFields:
    """State dataclass fields and mutation after operations."""

    def test_state_has_num_orders_after_load(self) -> None:
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0, 2.0])
        assert e.state.num_orders == 2

    def test_state_num_orders_one_for_linear(self) -> None:
        e = GrantDifferenceEngine()
        e.load([1.0, 2.0])
        assert e.state.num_orders == 1

    def test_overflow_flags_initialized_false(self) -> None:
        e = GrantDifferenceEngine()
        assert all(f is False for f in e.state.overflow_flags)

    def test_output_table_is_empty_initially(self) -> None:
        e = GrantDifferenceEngine()
        e.load([1.0, 1.0])
        assert e.state.output_table == []

    def test_output_table_appended_by_crank(self) -> None:
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0])
        v = e.crank()
        assert e.state.output_table[0] == v

    def test_crank_returns_decimal(self) -> None:
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0])
        v = e.crank()
        assert isinstance(v, Decimal)

    def test_registers_list_length_is_max(self) -> None:
        e = GrantDifferenceEngine()
        assert len(e.state.registers) == 15

    def test_overflow_flags_list_length_is_max(self) -> None:
        e = GrantDifferenceEngine()
        assert len(e.state.overflow_flags) == 15

    def test_revolution_count_increments_per_crank(self) -> None:
        e = GrantDifferenceEngine()
        e.load([0.0, 1.0])
        for i in range(1, 5):
            e.crank()
            assert e.state.revolution_count == i

    def test_state_reset_clears_num_orders(self) -> None:
        e = GrantDifferenceEngine()
        e.load([1.0, 2.0])
        e.reset()
        assert e.state.num_orders == 0


class TestGrantOverflowDetection:
    """Overflow flag is set when a register exceeds 30 digits."""

    def test_no_overflow_on_small_values(self) -> None:
        e = GrantDifferenceEngine()
        e.load([100.0, 1.0])
        e.crank()
        assert not any(e.state.overflow_flags)

    def test_overflow_flag_set_when_value_exceeds_30_digits(self) -> None:
        # D0 = D1 = 6*10^29; sum = 1.2*10^30 >= 10^30 -> overflow
        large = 6 * 10**29
        e = GrantDifferenceEngine()
        e.load([float(large), float(large)])
        e.crank()
        assert e.state.overflow_flags[0] is True

    def test_overflow_wraps_modulo(self) -> None:
        large = 6 * 10**29
        e = GrantDifferenceEngine()
        e.load([float(large), float(large)])
        v = e.crank()
        expected = (2 * Decimal(str(large))) % Decimal(10) ** 30
        assert v == expected
