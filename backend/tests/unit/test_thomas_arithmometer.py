"""Unit tests for the Thomas Arithmometer emulator (1820/1851).

Tests cover the core pinwheel/carry mechanism, carriage shift for
multi-digit work, and all four arithmetic operations.
"""

import pytest

from backend.src.emulator.thomas_arithmometer import (
    CrankMode,
    Pinwheel,
    ResultWheel,
    ThomasArithmometer,
)

# ---------------------------------------------------------------------------
# Component-level tests
# ---------------------------------------------------------------------------


class TestPinwheel:
    def test_set_zero(self):
        pw = Pinwheel()
        pw.set(0)
        assert pw.teeth_per_revolution() == 0

    def test_set_nine(self):
        pw = Pinwheel()
        pw.set(9)
        assert pw.teeth_per_revolution() == 9

    def test_invalid_negative(self):
        pw = Pinwheel()
        with pytest.raises(ValueError):
            pw.set(-1)

    def test_invalid_over_nine(self):
        pw = Pinwheel()
        with pytest.raises(ValueError):
            pw.set(10)


class TestResultWheel:
    def test_add_no_carry(self):
        w = ResultWheel()
        w.digit = 5
        carry = w.add(3)
        assert w.digit == 8
        assert carry == 0

    def test_add_carry(self):
        w = ResultWheel()
        w.digit = 7
        carry = w.add(5)
        assert w.digit == 2
        assert carry == 1

    def test_sub_no_borrow(self):
        w = ResultWheel()
        w.digit = 8
        borrow = w.sub(3)
        assert w.digit == 5
        assert borrow == 0

    def test_sub_borrow(self):
        w = ResultWheel()
        w.digit = 2
        borrow = w.sub(5)
        assert w.digit == 7
        assert borrow == 1


# ---------------------------------------------------------------------------
# Addition
# ---------------------------------------------------------------------------


class TestAddition:
    def test_add_single(self):
        m = ThomasArithmometer()
        result = m.add(42)
        assert result == 42

    def test_add_twice(self):
        m = ThomasArithmometer()
        m.add(100)
        assert m.add(50) == 150

    def test_add_zero(self):
        m = ThomasArithmometer()
        assert m.add(0) == 0

    def test_add_counter_increments(self):
        m = ThomasArithmometer()
        m.add(5)
        m.add(5)
        assert m.get_counter() == 2

    def test_add_large(self):
        m = ThomasArithmometer()
        m.add(99999999)
        assert m.get_result() == 99999999

    def test_add_carry_propagation(self):
        """999 + 1 should produce 1000 via carry chain."""
        m = ThomasArithmometer()
        m.add(999)
        assert m.add(1) == 1000

    def test_add_accumulation(self):
        m = ThomasArithmometer()
        for _ in range(5):
            m.set_input(3)
            m.set_mode(CrankMode.ADD)
            m.turn_crank()
        assert m.get_result() == 15


# ---------------------------------------------------------------------------
# Subtraction
# ---------------------------------------------------------------------------


class TestSubtraction:
    def test_subtract_basic(self):
        m = ThomasArithmometer()
        m.add(10)
        assert m.subtract(3) == 7

    def test_subtract_to_zero(self):
        m = ThomasArithmometer()
        m.add(5)
        assert m.subtract(5) == 0

    def test_subtract_borrow_propagation(self):
        """1000 - 1 = 999 requires borrow propagation."""
        m = ThomasArithmometer()
        m.add(1000)
        assert m.subtract(1) == 999

    def test_subtract_consecutive(self):
        m = ThomasArithmometer()
        m.add(100)
        m.subtract(40)
        assert m.subtract(20) == 40

    def test_subtract_zero(self):
        m = ThomasArithmometer()
        m.add(42)
        assert m.subtract(0) == 42


# ---------------------------------------------------------------------------
# Multiplication
# ---------------------------------------------------------------------------


class TestMultiplication:
    def test_multiply_basic(self):
        m = ThomasArithmometer()
        assert m.multiply(6, 7) == 42

    def test_multiply_by_zero(self):
        m = ThomasArithmometer()
        assert m.multiply(999, 0) == 0

    def test_multiply_by_one(self):
        m = ThomasArithmometer()
        assert m.multiply(123, 1) == 123

    def test_multiply_two_digit(self):
        m = ThomasArithmometer()
        assert m.multiply(12, 34) == 408

    def test_multiply_large(self):
        m = ThomasArithmometer()
        assert m.multiply(9999, 9999) == 9999 * 9999

    def test_multiply_matches_python(self):
        m = ThomasArithmometer()
        for a, b in [(3, 17), (25, 40), (7, 111)]:
            result = m.multiply(a, b)
            assert result == a * b, f"{a}*{b}: expected {a*b}, got {result}"

    def test_multiply_counter_records_turns(self):
        """Counter should equal the last digit used (units position turns)."""
        m = ThomasArithmometer()
        m.multiply(7, 3)
        # Counter at position 0 = number of crank turns at carriage 0 = 3
        assert m.counter[0].digit == 3


# ---------------------------------------------------------------------------
# Division
# ---------------------------------------------------------------------------


class TestDivision:
    def test_divide_exact(self):
        m = ThomasArithmometer()
        q, r = m.divide(100, 5)
        assert q == 20
        assert r == 0

    def test_divide_with_remainder(self):
        m = ThomasArithmometer()
        q, r = m.divide(17, 3)
        assert q == 5
        assert r == 2

    def test_divide_by_one(self):
        m = ThomasArithmometer()
        q, r = m.divide(42, 1)
        assert q == 42
        assert r == 0

    def test_divide_dividend_less_than_divisor(self):
        m = ThomasArithmometer()
        q, r = m.divide(3, 10)
        assert q == 0
        assert r == 3

    def test_divide_large(self):
        m = ThomasArithmometer()
        q, r = m.divide(999999, 999)
        assert q == 1001
        assert r == 0

    def test_divide_zero_dividend(self):
        m = ThomasArithmometer()
        q, r = m.divide(0, 5)
        assert q == 0
        assert r == 0

    def test_divide_by_zero_raises(self):
        m = ThomasArithmometer()
        with pytest.raises(ValueError):
            m.divide(10, 0)


# ---------------------------------------------------------------------------
# Carriage shift
# ---------------------------------------------------------------------------


class TestCarriageShift:
    def test_shift_and_add(self):
        """Shifting to position 1 means multiplying by 10 implicitly."""
        m = ThomasArithmometer()
        m.set_input(3)
        m.shift_carriage(1)
        m.set_mode(CrankMode.ADD)
        m.turn_crank()
        assert m.get_result() == 30

    def test_shift_out_of_range(self):
        m = ThomasArithmometer()
        with pytest.raises(ValueError):
            m.shift_carriage(ThomasArithmometer.RESULT_DIGITS)

    def test_shift_zero(self):
        m = ThomasArithmometer()
        m.shift_carriage(0)
        assert m.carriage_position == 0


# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------


class TestInputValidation:
    def test_input_negative_raises(self):
        m = ThomasArithmometer()
        with pytest.raises(ValueError):
            m.set_input(-1)

    def test_input_overflow_raises(self):
        m = ThomasArithmometer()
        with pytest.raises(ValueError):
            m.set_input(10**ThomasArithmometer.INPUT_DIGITS)


# ---------------------------------------------------------------------------
# State snapshot
# ---------------------------------------------------------------------------


class TestState:
    def test_state_keys(self):
        m = ThomasArithmometer()
        s = m.state()
        assert "result" in s
        assert "counter" in s
        assert "carriage_position" in s
        assert "crank_mode" in s
        assert "input" in s

    def test_clear_result(self):
        m = ThomasArithmometer()
        m.add(42)
        m.clear_result()
        assert m.get_result() == 0

    def test_clear_counter(self):
        m = ThomasArithmometer()
        m.add(1)
        m.add(1)
        m.clear_counter()
        assert m.get_counter() == 0
