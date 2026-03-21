"""Unit tests for the Odhner Arithmometer / Brunsviga emulator (1878)."""

import pytest

from backend.src.emulator.odhner_arithmometer import (
    CrankDirection,
    OdhnerArithmometer,
    OdhnerPinwheel,
    ResultRegister,
    RevolutionCounter,
)

# ---------------------------------------------------------------------------
# Component tests
# ---------------------------------------------------------------------------


class TestOdhnerPinwheel:
    def test_default_zero(self):
        pw = OdhnerPinwheel()
        assert pw.value == 0

    def test_set_valid(self):
        pw = OdhnerPinwheel()
        pw.set(7)
        assert pw.value == 7

    def test_set_invalid_low(self):
        pw = OdhnerPinwheel()
        with pytest.raises(ValueError):
            pw.set(-1)

    def test_set_invalid_high(self):
        pw = OdhnerPinwheel()
        with pytest.raises(ValueError):
            pw.set(10)


class TestResultRegister:
    def test_initial_zero(self):
        r = ResultRegister()
        assert r.get() == 0

    def test_add_simple(self):
        r = ResultRegister()
        r.add_at(0, 5)
        assert r.get() == 5

    def test_add_carry(self):
        r = ResultRegister()
        r.add_at(0, 7)
        r.add_at(0, 5)  # 12 -> digit=2, carry=1 to position 1
        assert r.get() == 12

    def test_sub_borrow(self):
        r = ResultRegister()
        r.add_at(0, 10)  # = 10
        r.sub_at(0, 3)  # 10 - 3 = 7
        assert r.get() == 7

    def test_clear(self):
        r = ResultRegister()
        r.add_at(0, 42)
        r.clear()
        assert r.get() == 0


class TestRevolutionCounter:
    def test_increment(self):
        c = RevolutionCounter()
        c.increment()
        c.increment()
        assert c.get() == 2

    def test_decrement(self):
        c = RevolutionCounter()
        c.increment()
        c.increment()
        c.decrement()
        assert c.get() == 1

    def test_clear(self):
        c = RevolutionCounter()
        c.increment()
        c.clear()
        assert c.get() == 0


# ---------------------------------------------------------------------------
# Addition
# ---------------------------------------------------------------------------


class TestAddition:
    def test_add_once(self):
        m = OdhnerArithmometer()
        assert m.add(123) == 123

    def test_add_twice(self):
        m = OdhnerArithmometer()
        m.add(100)
        assert m.add(50) == 150

    def test_add_zero(self):
        m = OdhnerArithmometer()
        assert m.add(0) == 0

    def test_add_counter(self):
        m = OdhnerArithmometer()
        m.add(5)
        m.add(5)
        assert m.counter.get() == 2

    def test_add_carry_chain(self):
        m = OdhnerArithmometer()
        m.add(999)
        assert m.add(1) == 1000


# ---------------------------------------------------------------------------
# Subtraction
# ---------------------------------------------------------------------------


class TestSubtraction:
    def test_sub_basic(self):
        m = OdhnerArithmometer()
        m.add(20)
        assert m.subtract(7) == 13

    def test_sub_to_zero(self):
        m = OdhnerArithmometer()
        m.add(42)
        assert m.subtract(42) == 0

    def test_sub_borrow(self):
        m = OdhnerArithmometer()
        m.add(1000)
        assert m.subtract(1) == 999


# ---------------------------------------------------------------------------
# Multiplication
# ---------------------------------------------------------------------------


class TestMultiplication:
    def test_multiply_basic(self):
        m = OdhnerArithmometer()
        assert m.multiply(6, 7) == 42

    def test_multiply_by_zero(self):
        m = OdhnerArithmometer()
        assert m.multiply(123, 0) == 0

    def test_multiply_by_one(self):
        m = OdhnerArithmometer()
        assert m.multiply(99, 1) == 99

    def test_multiply_two_digit(self):
        m = OdhnerArithmometer()
        assert m.multiply(12, 34) == 408

    def test_multiply_large(self):
        m = OdhnerArithmometer()
        assert m.multiply(9999, 9999) == 9999 * 9999

    def test_multiply_matches_python(self):
        m = OdhnerArithmometer()
        for a, b in [(7, 13), (25, 40), (3, 333)]:
            result = m.multiply(a, b)
            assert result == a * b


# ---------------------------------------------------------------------------
# Division
# ---------------------------------------------------------------------------


class TestDivision:
    def test_divide_exact(self):
        m = OdhnerArithmometer()
        q, r = m.divide(100, 4)
        assert q == 25
        assert r == 0

    def test_divide_with_remainder(self):
        m = OdhnerArithmometer()
        q, r = m.divide(17, 5)
        assert q == 3
        assert r == 2

    def test_divide_by_one(self):
        m = OdhnerArithmometer()
        q, r = m.divide(77, 1)
        assert q == 77
        assert r == 0

    def test_divide_zero_dividend(self):
        m = OdhnerArithmometer()
        q, r = m.divide(0, 5)
        assert q == 0
        assert r == 0

    def test_divide_by_zero_raises(self):
        m = OdhnerArithmometer()
        with pytest.raises(ValueError):
            m.divide(10, 0)

    def test_divide_large(self):
        m = OdhnerArithmometer()
        q, r = m.divide(123456, 456)
        expected_q, expected_r = divmod(123456, 456)
        assert q == expected_q
        assert r == expected_r


# ---------------------------------------------------------------------------
# Carriage shift
# ---------------------------------------------------------------------------


class TestCarriageShift:
    def test_shift_multiplies_by_10(self):
        m = OdhnerArithmometer()
        m.set_input(3)
        m.shift_carriage(1)
        m.set_direction(CrankDirection.ADD)
        m.turn_crank()
        assert m.result.get() == 30

    def test_shift_out_of_range(self):
        m = OdhnerArithmometer()
        with pytest.raises(ValueError):
            m.shift_carriage(ResultRegister.DIGITS)

    def test_shift_max_valid(self):
        m = OdhnerArithmometer()
        max_pos = ResultRegister.DIGITS - OdhnerArithmometer.INPUT_DIGITS
        m.shift_carriage(max_pos)
        assert m.carriage_position == max_pos


# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------


class TestInputValidation:
    def test_negative_input_raises(self):
        m = OdhnerArithmometer()
        with pytest.raises(ValueError):
            m.set_input(-1)

    def test_overflow_input_raises(self):
        m = OdhnerArithmometer()
        with pytest.raises(ValueError):
            m.set_input(10**OdhnerArithmometer.INPUT_DIGITS)


# ---------------------------------------------------------------------------
# State
# ---------------------------------------------------------------------------


class TestState:
    def test_state_has_expected_keys(self):
        m = OdhnerArithmometer()
        s = m.state()
        for key in ("result", "counter", "carriage_position", "direction", "input"):
            assert key in s

    def test_clear_all(self):
        m = OdhnerArithmometer()
        m.add(99)
        m.clear_result()
        m.clear_counter()
        assert m.result.get() == 0
        assert m.counter.get() == 0
