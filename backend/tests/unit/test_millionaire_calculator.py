"""Unit tests for the Millionaire Calculator emulator (1893)."""

import pytest

from backend.src.emulator.millionaire_calculator import _PLATE, MillionaireCalculator

# ---------------------------------------------------------------------------
# Multiplication plate
# ---------------------------------------------------------------------------


class TestMultiplicationPlate:
    def test_plate_identity(self):
        for d in range(10):
            for m in range(10):
                assert _PLATE[d][m] == d * m

    def test_plate_max(self):
        assert _PLATE[9][9] == 81


# ---------------------------------------------------------------------------
# Input and lever
# ---------------------------------------------------------------------------


class TestInput:
    def test_set_input(self):
        m = MillionaireCalculator()
        m.set_input(12345678)
        assert m._input[0] == 8  # LSB first
        assert m._input[7] == 1  # MSB last

    def test_set_input_zero(self):
        m = MillionaireCalculator()
        m.set_input(0)
        assert all(d == 0 for d in m._input)

    def test_set_input_negative_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.set_input(-1)

    def test_set_input_overflow_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.set_input(10**MillionaireCalculator.INPUT_DIGITS)

    def test_set_lever_valid(self):
        m = MillionaireCalculator()
        m.set_multiplier_lever(7)
        assert m.multiplier_lever == 7

    def test_set_lever_invalid(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.set_multiplier_lever(10)
        with pytest.raises(ValueError):
            m.set_multiplier_lever(-1)


# ---------------------------------------------------------------------------
# Direct multiplication (single crank per multiplier digit)
# ---------------------------------------------------------------------------


class TestDirectMultiplication:
    def test_multiply_basic(self):
        m = MillionaireCalculator()
        assert m.multiply(6, 7) == 42

    def test_multiply_by_zero(self):
        m = MillionaireCalculator()
        assert m.multiply(999, 0) == 0

    def test_multiply_by_one(self):
        m = MillionaireCalculator()
        assert m.multiply(12345, 1) == 12345

    def test_multiply_two_digit(self):
        m = MillionaireCalculator()
        assert m.multiply(12, 34) == 408

    def test_multiply_large(self):
        m = MillionaireCalculator()
        assert m.multiply(99999999, 9) == 899999991

    def test_multiply_matches_python(self):
        m = MillionaireCalculator()
        cases = [(3, 17), (25, 40), (7, 111), (123, 456), (999, 999)]
        for a, b in cases:
            result = m.multiply(a, b)
            assert result == a * b, f"{a}*{b}: expected {a*b}, got {result}"

    def test_multiply_crank_count_equals_digit_count(self):
        """One crank per multiplier digit -- the Millionaire's key advantage."""
        m = MillionaireCalculator()
        m.multiply(999, 99)
        # multiplier 99 has 2 digits -> 2 crank turns
        assert m.get_counter() == 2

    def test_multiply_crank_count_for_three_digits(self):
        m = MillionaireCalculator()
        m.multiply(5, 123)
        assert m.get_counter() == 3

    def test_multiply_then_add(self):
        """After multiply, add should work with lever=1."""
        m = MillionaireCalculator()
        m.multiply(10, 5)  # result = 50
        assert m.add(3) == 53


# ---------------------------------------------------------------------------
# Carriage shift
# ---------------------------------------------------------------------------


class TestCarriageShift:
    def test_shift_by_one_multiplies_by_10(self):
        m = MillionaireCalculator()
        m.set_input(5)
        m.shift_carriage(1)
        m.set_multiplier_lever(1)
        m.turn_crank()
        assert m.get_result() == 50

    def test_shift_out_of_range_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.shift_carriage(MillionaireCalculator.RESULT_DIGITS)


# ---------------------------------------------------------------------------
# Division
# ---------------------------------------------------------------------------


class TestDivision:
    def test_divide_exact(self):
        m = MillionaireCalculator()
        q, r = m.divide(100, 4)
        assert q == 25
        assert r == 0

    def test_divide_with_remainder(self):
        m = MillionaireCalculator()
        q, r = m.divide(17, 3)
        assert q == 5
        assert r == 2

    def test_divide_by_zero_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.divide(10, 0)


# ---------------------------------------------------------------------------
# State and clear
# ---------------------------------------------------------------------------


class TestState:
    def test_clear_result(self):
        m = MillionaireCalculator()
        m.multiply(99, 99)
        m.clear_result()
        assert m.get_result() == 0

    def test_clear_counter(self):
        m = MillionaireCalculator()
        m.multiply(5, 5)
        m.clear_counter()
        assert m.get_counter() == 0

    def test_state_keys(self):
        m = MillionaireCalculator()
        s = m.state()
        for key in ("result", "counter", "carriage_position", "multiplier_lever", "input"):
            assert key in s
