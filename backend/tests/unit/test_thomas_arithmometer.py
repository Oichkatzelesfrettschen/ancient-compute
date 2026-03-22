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
            assert result == a * b, f"{a}*{b}: expected {a * b}, got {result}"

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


# ---------------------------------------------------------------------------
# CounterWheel component
# ---------------------------------------------------------------------------


class TestCounterWheel:
    def test_initial_zero(self) -> None:
        from backend.src.emulator.thomas_arithmometer import CounterWheel

        w = CounterWheel()
        assert w.digit == 0

    def test_increment_returns_no_carry_below_ten(self) -> None:
        from backend.src.emulator.thomas_arithmometer import CounterWheel

        w = CounterWheel()
        for _ in range(9):
            carry = w.increment()
            assert carry == 0

    def test_increment_wraps_and_carries(self) -> None:
        from backend.src.emulator.thomas_arithmometer import CounterWheel

        w = CounterWheel()
        w.digit = 9
        carry = w.increment()
        assert w.digit == 0
        assert carry == 1

    def test_decrement_no_borrow(self) -> None:
        from backend.src.emulator.thomas_arithmometer import CounterWheel

        w = CounterWheel()
        w.digit = 5
        borrow = w.decrement()
        assert w.digit == 4
        assert borrow == 0

    def test_decrement_from_zero_borrows(self) -> None:
        from backend.src.emulator.thomas_arithmometer import CounterWheel

        w = CounterWheel()
        borrow = w.decrement()
        assert w.digit == 9
        assert borrow == 1


# ---------------------------------------------------------------------------
# Extended Pinwheel tests
# ---------------------------------------------------------------------------


class TestPinwheelExtended:
    def test_default_value_zero(self) -> None:
        pw = Pinwheel()
        assert pw.teeth_per_revolution() == 0

    def test_slider_attribute(self) -> None:
        pw = Pinwheel()
        pw.set(5)
        assert pw.slider == 5

    def test_set_all_valid_values(self) -> None:
        pw = Pinwheel()
        for v in range(10):
            pw.set(v)
            assert pw.teeth_per_revolution() == v


# ---------------------------------------------------------------------------
# Extended ResultWheel tests
# ---------------------------------------------------------------------------


class TestResultWheelExtended:
    def test_add_zero_unchanged(self) -> None:
        w = ResultWheel()
        w.digit = 3
        carry = w.add(0)
        assert w.digit == 3
        assert carry == 0

    def test_add_to_nine_no_carry(self) -> None:
        w = ResultWheel()
        w.digit = 0
        carry = w.add(9)
        assert w.digit == 9
        assert carry == 0

    def test_add_ten_wraps_with_carry(self) -> None:
        w = ResultWheel()
        w.digit = 0
        carry = w.add(10)
        assert w.digit == 0
        assert carry == 1

    def test_sub_zero_no_borrow(self) -> None:
        w = ResultWheel()
        w.digit = 7
        borrow = w.sub(0)
        assert w.digit == 7
        assert borrow == 0

    def test_sub_nine_from_nine_zero(self) -> None:
        w = ResultWheel()
        w.digit = 9
        borrow = w.sub(9)
        assert w.digit == 0
        assert borrow == 0

    def test_add_multiple_times(self) -> None:
        w = ResultWheel()
        for _ in range(15):
            w.add(1)
        assert w.digit == 5  # 15 % 10 = 5


# ---------------------------------------------------------------------------
# Extended Addition tests
# ---------------------------------------------------------------------------


class TestAdditionExtended:
    def test_add_one_digit(self) -> None:
        m = ThomasArithmometer()
        assert m.add(9) == 9

    def test_add_max_input(self) -> None:
        m = ThomasArithmometer()
        assert m.add(99999999) == 99999999

    def test_add_chain_five_values(self) -> None:
        m = ThomasArithmometer()
        for v in [10, 20, 30, 40, 50]:
            m.add(v)
        assert m.get_result() == 150

    def test_counter_tracks_add_calls(self) -> None:
        m = ThomasArithmometer()
        for _ in range(7):
            m.add(1)
        assert m.get_counter() == 7

    def test_clear_result_after_add(self) -> None:
        m = ThomasArithmometer()
        m.add(1234)
        m.clear_result()
        assert m.get_result() == 0

    def test_clear_input_zeroes_pinwheels(self) -> None:
        m = ThomasArithmometer()
        m.set_input(99999999)
        m.clear_input()
        for pw in m.pinwheels:
            assert pw.slider == 0

    def test_add_then_clear_counter_then_add(self) -> None:
        m = ThomasArithmometer()
        m.add(5)
        m.add(5)
        m.clear_counter()
        m.add(3)
        assert m.get_counter() == 1
        assert m.get_result() == 13


# ---------------------------------------------------------------------------
# Extended Subtraction tests
# ---------------------------------------------------------------------------


class TestSubtractionExtended:
    def test_subtract_self_is_zero(self) -> None:
        m = ThomasArithmometer()
        m.add(99)
        assert m.subtract(99) == 0

    def test_subtract_ten_from_ten(self) -> None:
        m = ThomasArithmometer()
        m.add(10)
        assert m.subtract(10) == 0

    def test_subtract_1_from_100(self) -> None:
        m = ThomasArithmometer()
        m.add(100)
        assert m.subtract(1) == 99

    def test_subtract_all_borrows(self) -> None:
        # 10000 - 1 = 9999 requires 4 borrow propagations
        m = ThomasArithmometer()
        m.add(10000)
        assert m.subtract(1) == 9999

    def test_subtract_counter_increments_too(self) -> None:
        m = ThomasArithmometer()
        m.add(20)
        m.subtract(5)
        # Both add and subtract increment counter
        assert m.get_counter() == 2


# ---------------------------------------------------------------------------
# Extended Multiplication tests
# ---------------------------------------------------------------------------


class TestMultiplicationExtended:
    def test_multiply_commutative(self) -> None:
        m = ThomasArithmometer()
        assert m.multiply(7, 13) == m.multiply(13, 7)

    def test_multiply_three_digits(self) -> None:
        m = ThomasArithmometer()
        assert m.multiply(123, 456) == 123 * 456

    def test_multiply_by_ten(self) -> None:
        m = ThomasArithmometer()
        assert m.multiply(37, 10) == 370

    def test_multiply_by_100(self) -> None:
        m = ThomasArithmometer()
        assert m.multiply(42, 100) == 4200

    def test_counter_after_multiply_three(self) -> None:
        m = ThomasArithmometer()
        m.multiply(5, 3)
        # Counter at position 0 = 3 (three turns at carriage 0)
        assert m.counter[0].digit == 3

    def test_multiply_consistent_with_repeated_add(self) -> None:
        m = ThomasArithmometer()
        product = m.multiply(7, 8)
        m2 = ThomasArithmometer()
        for _ in range(8):
            m2.add(7)
        assert product == m2.get_result()


# ---------------------------------------------------------------------------
# Extended Division tests
# ---------------------------------------------------------------------------


class TestDivisionExtended:
    def test_divide_self(self) -> None:
        m = ThomasArithmometer()
        q, r = m.divide(100, 100)
        assert q == 1
        assert r == 0

    def test_divide_quotient_remainder_invariant(self) -> None:
        for dividend, divisor in [(17, 5), (100, 7), (999, 13), (1000, 3)]:
            m2 = ThomasArithmometer()
            q, r = m2.divide(dividend, divisor)
            assert q * divisor + r == dividend

    def test_divide_leaves_remainder_in_result(self) -> None:
        m = ThomasArithmometer()
        q, r = m.divide(17, 5)
        # After divide, result register holds the remainder
        assert m.get_result() == r

    def test_divide_negative_dividend_raises(self) -> None:
        m = ThomasArithmometer()
        with pytest.raises(ValueError):
            m.divide(-5, 2)

    def test_divide_large_exact(self) -> None:
        m = ThomasArithmometer()
        q, r = m.divide(1000000, 1000)
        assert q == 1000
        assert r == 0


# ---------------------------------------------------------------------------
# Extended Carriage tests
# ---------------------------------------------------------------------------


class TestCarriageExtended:
    def test_shift_to_position_2_adds_hundreds(self) -> None:
        m = ThomasArithmometer()
        m.set_input(1)
        m.shift_carriage(2)
        m.set_mode(CrankMode.ADD)
        m.turn_crank()
        assert m.get_result() == 100

    def test_shift_carriage_position_attribute(self) -> None:
        m = ThomasArithmometer()
        m.shift_carriage(3)
        assert m.carriage_position == 3

    def test_shift_carriage_max_valid(self) -> None:
        m = ThomasArithmometer()
        max_pos = ThomasArithmometer.RESULT_DIGITS - ThomasArithmometer.INPUT_DIGITS
        m.shift_carriage(max_pos)
        assert m.carriage_position == max_pos

    def test_shift_negative_raises(self) -> None:
        m = ThomasArithmometer()
        with pytest.raises(ValueError):
            m.shift_carriage(-1)


# ---------------------------------------------------------------------------
# Extended State tests
# ---------------------------------------------------------------------------


class TestStateExtended:
    def test_state_crank_mode_default_add(self) -> None:
        m = ThomasArithmometer()
        s = m.state()
        assert s["crank_mode"] == "ADD"

    def test_state_crank_mode_after_set_subtract(self) -> None:
        m = ThomasArithmometer()
        m.set_mode(CrankMode.SUBTRACT)
        s = m.state()
        assert s["crank_mode"] == "SUBTRACT"

    def test_state_result_reflects_add(self) -> None:
        m = ThomasArithmometer()
        m.add(77)
        s = m.state()
        assert s["result"] == 77

    def test_state_input_reflects_set_input(self) -> None:
        m = ThomasArithmometer()
        m.set_input(12345678)
        s = m.state()
        assert s["input"] == 12345678

    def test_state_carriage_position_reflects_shift(self) -> None:
        m = ThomasArithmometer()
        m.shift_carriage(4)
        s = m.state()
        assert s["carriage_position"] == 4

    def test_constants(self) -> None:
        assert ThomasArithmometer.INPUT_DIGITS == 8
        assert ThomasArithmometer.RESULT_DIGITS == 16
        assert ThomasArithmometer.COUNTER_DIGITS == 8
