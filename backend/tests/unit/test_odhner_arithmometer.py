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


# ---------------------------------------------------------------------------
# Extended ResultRegister tests
# ---------------------------------------------------------------------------


class TestResultRegisterExtended:
    def test_add_at_position_1_is_tens(self) -> None:
        r = ResultRegister()
        r.add_at(1, 3)  # +30
        assert r.get() == 30

    def test_add_at_position_2_is_hundreds(self) -> None:
        r = ResultRegister()
        r.add_at(2, 5)  # +500
        assert r.get() == 500

    def test_carry_propagates_two_levels(self) -> None:
        r = ResultRegister()
        r.add_at(0, 99)  # 99 -> digit[0]=9, carry 9 -> digit[1]=9
        assert r.get() == 99
        r.add_at(0, 1)  # 100 -> carry propagates to digit[2]
        assert r.get() == 100

    def test_sub_at_with_borrow(self) -> None:
        r = ResultRegister()
        r.add_at(0, 100)
        r.sub_at(0, 1)  # 100 - 1 = 99
        assert r.get() == 99

    def test_sub_at_multi_digit_borrow(self) -> None:
        r = ResultRegister()
        r.add_at(0, 1000)
        r.sub_at(0, 1)
        assert r.get() == 999

    def test_multiple_adds_accumulate(self) -> None:
        r = ResultRegister()
        for _ in range(10):
            r.add_at(0, 7)
        assert r.get() == 70

    def test_clear_after_large_value(self) -> None:
        r = ResultRegister()
        r.add_at(0, 99999)
        r.clear()
        assert r.get() == 0

    def test_digits_constant(self) -> None:
        assert ResultRegister.DIGITS == 13


# ---------------------------------------------------------------------------
# Extended RevolutionCounter tests
# ---------------------------------------------------------------------------


class TestRevolutionCounterExtended:
    def test_get_after_zero_inits(self) -> None:
        c = RevolutionCounter()
        assert c.get() == 0

    def test_increment_ten_times(self) -> None:
        c = RevolutionCounter()
        for _ in range(10):
            c.increment()
        assert c.get() == 10

    def test_decrement_to_zero(self) -> None:
        c = RevolutionCounter()
        c.increment()
        c.decrement()
        assert c.get() == 0

    def test_digits_constant(self) -> None:
        assert RevolutionCounter.DIGITS == 8

    def test_clear_resets_to_zero(self) -> None:
        c = RevolutionCounter()
        for _ in range(5):
            c.increment()
        c.clear()
        assert c.get() == 0

    def test_increment_100_times(self) -> None:
        c = RevolutionCounter()
        for _ in range(100):
            c.increment()
        assert c.get() == 100

    def test_decrement_past_zero_wraps(self) -> None:
        # Decrement from 0 wraps (borrow goes out of MSB)
        c = RevolutionCounter()
        c.decrement()
        # Should wrap to max 8-digit value
        assert c.get() == 10**8 - 1


# ---------------------------------------------------------------------------
# Direction and turn_crank
# ---------------------------------------------------------------------------


class TestDirectionAndCrank:
    def test_direction_default_is_add(self) -> None:
        m = OdhnerArithmometer()
        assert m.direction == CrankDirection.ADD

    def test_set_direction_subtract(self) -> None:
        m = OdhnerArithmometer()
        m.set_direction(CrankDirection.SUBTRACT)
        assert m.direction == CrankDirection.SUBTRACT

    def test_set_direction_back_to_add(self) -> None:
        m = OdhnerArithmometer()
        m.set_direction(CrankDirection.SUBTRACT)
        m.set_direction(CrankDirection.ADD)
        assert m.direction == CrankDirection.ADD

    def test_turn_crank_add_increments_counter(self) -> None:
        m = OdhnerArithmometer()
        m.set_input(5)
        m.set_direction(CrankDirection.ADD)
        m.turn_crank()
        assert m.counter.get() == 1

    def test_turn_crank_subtract_decrements_counter(self) -> None:
        m = OdhnerArithmometer()
        m.set_input(5)
        m.set_direction(CrankDirection.ADD)
        m.turn_crank()  # counter = 1
        m.set_direction(CrankDirection.SUBTRACT)
        m.turn_crank()  # counter = 0
        assert m.counter.get() == 0

    def test_turn_crank_add_result(self) -> None:
        m = OdhnerArithmometer()
        m.set_input(7)
        m.set_direction(CrankDirection.ADD)
        m.turn_crank()
        assert m.result.get() == 7

    def test_turn_crank_subtract_result(self) -> None:
        m = OdhnerArithmometer()
        m.set_input(10)
        m.set_direction(CrankDirection.ADD)
        m.turn_crank()  # result = 10
        m.set_direction(CrankDirection.SUBTRACT)
        m.set_input(3)
        m.turn_crank()  # result = 7
        assert m.result.get() == 7

    def test_crank_direction_add_str_value(self) -> None:
        assert CrankDirection.ADD == "ADD"

    def test_crank_direction_subtract_str_value(self) -> None:
        assert CrankDirection.SUBTRACT == "SUBTRACT"


# ---------------------------------------------------------------------------
# Extended state and integration tests
# ---------------------------------------------------------------------------


class TestOdhnerStateExtended:
    def test_state_direction_value_is_string(self) -> None:
        m = OdhnerArithmometer()
        s = m.state()
        assert isinstance(s["direction"], str)

    def test_state_direction_reflects_set(self) -> None:
        m = OdhnerArithmometer()
        m.set_direction(CrankDirection.SUBTRACT)
        s = m.state()
        assert s["direction"] == "SUBTRACT"

    def test_state_result_reflects_add(self) -> None:
        m = OdhnerArithmometer()
        m.add(42)
        s = m.state()
        assert s["result"] == 42

    def test_state_counter_reflects_turns(self) -> None:
        m = OdhnerArithmometer()
        m.add(1)
        m.add(1)
        s = m.state()
        assert s["counter"] == 2

    def test_state_input_reflects_set_input(self) -> None:
        m = OdhnerArithmometer()
        m.set_input(123456)
        s = m.state()
        assert s["input"] == 123456

    def test_clear_input_resets_pinwheels(self) -> None:
        m = OdhnerArithmometer()
        m.set_input(9999)
        m.clear_input()
        for pw in m.pinwheels:
            assert pw.value == 0


class TestOdhnerIntegration:
    def test_multiply_is_commutative(self) -> None:
        m = OdhnerArithmometer()
        assert m.multiply(7, 13) == m.multiply(13, 7)

    def test_divide_multiply_inverse(self) -> None:
        m = OdhnerArithmometer()
        product = m.multiply(37, 24)
        q, r = m.divide(product, 37)
        assert q == 24
        assert r == 0

    def test_add_then_subtract_back_to_zero(self) -> None:
        m = OdhnerArithmometer()
        m.add(555)
        m.subtract(555)
        assert m.result.get() == 0

    def test_multiply_via_shift_and_crank_matches_multiply_method(self) -> None:
        # Manual shift-and-crank for 5 * 12
        m = OdhnerArithmometer()
        m.set_input(5)
        m.shift_carriage(0)
        m.set_direction(CrankDirection.ADD)
        for _ in range(2):  # 2 units
            m.turn_crank()
        m.shift_carriage(1)
        m.turn_crank()  # 1 ten
        manual = m.result.get()

        m2 = OdhnerArithmometer()
        method = m2.multiply(5, 12)
        assert manual == method

    def test_quotient_times_divisor_plus_remainder_equals_dividend(self) -> None:
        for dividend, divisor in [(17, 5), (100, 7), (999, 13)]:
            m = OdhnerArithmometer()
            q, r = m.divide(dividend, divisor)
            assert q * divisor + r == dividend
