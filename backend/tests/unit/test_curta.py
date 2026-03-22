"""
Curta Calculator Tests
"""

import pytest

from backend.src.emulator.curta import CrankMode, CurtaTypeI


def test_curta_addition():
    curta = CurtaTypeI()
    curta.set_input(123)
    curta.turn_crank()  # Add 123
    assert curta.result_dial == 123
    assert curta.counter_dial == 1

    curta.turn_crank()  # Add 123
    assert curta.result_dial == 246
    assert curta.counter_dial == 2


def test_curta_multiplication():
    # 12 * 12 = 144
    curta = CurtaTypeI()
    curta.set_input(12)

    # Unit position: turn 2 times
    curta.turn_crank()
    curta.turn_crank()

    # Tens position: shift and turn 1 time
    curta.shift_carriage(1)
    curta.turn_crank()

    assert curta.result_dial == 144
    assert curta.counter_dial == 12  # 1 ten + 2 units


def test_curta_subtraction():
    curta = CurtaTypeI()
    curta.set_input(100)
    curta.turn_crank()  # +100

    curta.set_input(10)
    curta.set_crank_mode(CrankMode.SUBTRACT)
    curta.turn_crank()  # -10

    assert curta.result_dial == 90

    # Verify counter subtraction
    assert curta.counter_dial == 0  # 1 - 1 = 0


def test_curta_divide_exact():
    curta = CurtaTypeI()
    q, r = curta.divide(100, 5)
    assert q == 20
    assert r == 0


def test_curta_divide_with_remainder():
    curta = CurtaTypeI()
    q, r = curta.divide(17, 3)
    assert q == 5
    assert r == 2


def test_curta_divide_by_one():
    curta = CurtaTypeI()
    q, r = curta.divide(999, 1)
    assert q == 999
    assert r == 0


def test_curta_divide_large():
    curta = CurtaTypeI()
    q, r = curta.divide(9999, 99)
    expected_q, expected_r = divmod(9999, 99)
    assert q == expected_q
    assert r == expected_r


def test_curta_divide_by_zero():
    curta = CurtaTypeI()
    with pytest.raises(ZeroDivisionError):
        curta.divide(10, 0)


def test_curta_bell_rings_on_overflow():
    curta = CurtaTypeI()
    curta.result_dial = curta.MAX_RESULT  # Set to maximum
    curta._add_result(1)  # This should wrap and ring the bell
    assert curta.bell_ringing is True


def test_curta_bell_clears_on_clear_result():
    curta = CurtaTypeI()
    curta.bell_ringing = True
    curta.clear_result()
    assert curta.bell_ringing is False


class TestCurtaTypeI:
    def test_initial_state_zeroed(self) -> None:
        c = CurtaTypeI()
        assert c.result_dial == 0
        assert c.counter_dial == 0
        assert c.carriage_position == 0
        assert c.crank_mode == CrankMode.ADD
        assert c.bell_ringing is False

    def test_set_slider_valid(self) -> None:
        c = CurtaTypeI()
        c.set_slider(0, 7)
        assert c.sliders[0] == 7

    def test_set_slider_out_of_range_raises(self) -> None:
        c = CurtaTypeI()
        with pytest.raises(ValueError):
            c.set_slider(8, 5)

    def test_set_slider_value_out_of_range_raises(self) -> None:
        c = CurtaTypeI()
        with pytest.raises(ValueError):
            c.set_slider(0, 10)

    def test_set_input_roundtrip(self) -> None:
        c = CurtaTypeI()
        c.set_input(12345678)
        assert c.get_input_value() == 12345678

    def test_set_input_overflow_raises(self) -> None:
        c = CurtaTypeI()
        with pytest.raises(OverflowError):
            c.set_input(999999999)  # 9 digits, exceeds 8

    def test_get_input_value_initial_zero(self) -> None:
        c = CurtaTypeI()
        assert c.get_input_value() == 0

    def test_shift_carriage_increments(self) -> None:
        c = CurtaTypeI()
        c.shift_carriage(2)
        assert c.carriage_position == 2

    def test_shift_carriage_clamps_at_max(self) -> None:
        c = CurtaTypeI()
        c.shift_carriage(10)  # Exceeds max of 5
        assert c.carriage_position <= 5

    def test_shift_carriage_cannot_go_negative(self) -> None:
        c = CurtaTypeI()
        c.shift_carriage(-1)  # Can't go below 0
        assert c.carriage_position == 0

    def test_add_increments_counter(self) -> None:
        c = CurtaTypeI()
        c.set_input(5)
        for _ in range(7):
            c.turn_crank()
        assert c.counter_dial == 7
        assert c.result_dial == 35

    def test_subtract_mode_reduces_result(self) -> None:
        c = CurtaTypeI()
        c.set_input(50)
        c.turn_crank()  # result = 50
        c.set_crank_mode(CrankMode.SUBTRACT)
        c.set_input(20)
        c.turn_crank()  # result = 30
        assert c.result_dial == 30

    def test_bell_rings_on_underflow(self) -> None:
        c = CurtaTypeI()
        c._sub_result(1)  # result_dial was 0, underflows
        assert c.bell_ringing is True

    def test_clear_result_zeroes_result_dial(self) -> None:
        c = CurtaTypeI()
        c.set_input(999)
        c.turn_crank()
        c.clear_result()
        assert c.result_dial == 0

    def test_clear_counter_zeroes_counter(self) -> None:
        c = CurtaTypeI()
        c.set_input(1)
        c.turn_crank()
        c.turn_crank()
        c.clear_counter()
        assert c.counter_dial == 0

    def test_overflow_wraps_result_modulo(self) -> None:
        c = CurtaTypeI()
        c.result_dial = c.MAX_RESULT
        c._add_result(1)
        assert c.result_dial == 0

    def test_set_crank_mode_subtract(self) -> None:
        c = CurtaTypeI()
        c.set_crank_mode(CrankMode.SUBTRACT)
        assert c.crank_mode == CrankMode.SUBTRACT

    def test_divide_self_equals_one(self) -> None:
        c = CurtaTypeI()
        q, r = c.divide(77, 77)
        assert q == 1
        assert r == 0

    def test_divide_negative_raises(self) -> None:
        c = CurtaTypeI()
        with pytest.raises(ValueError):
            c.divide(-10, 5)

    def test_divide_overflow_raises(self) -> None:
        c = CurtaTypeI()
        with pytest.raises(OverflowError):
            c.divide(c.MAX_RESULT + 1, 1)

    def test_carriage_multiplies_input(self) -> None:
        # Adding 5 at carriage position 1 should add 50
        c = CurtaTypeI()
        c.set_input(5)
        c.shift_carriage(1)
        c.turn_crank()
        assert c.result_dial == 50


# ---------------------------------------------------------------------------
# Extended slider tests
# ---------------------------------------------------------------------------


class TestCurtaSliders:
    def test_set_all_eight_sliders(self) -> None:
        c = CurtaTypeI()
        for i in range(8):
            c.set_slider(i, i + 1)  # sliders 0-7 set to 1-8
        for i in range(8):
            assert c.sliders[i] == i + 1

    def test_slider_zero_is_valid(self) -> None:
        c = CurtaTypeI()
        c.set_slider(0, 0)
        assert c.sliders[0] == 0

    def test_slider_nine_is_valid(self) -> None:
        c = CurtaTypeI()
        c.set_slider(0, 9)
        assert c.sliders[0] == 9

    def test_slider_index_zero_valid(self) -> None:
        c = CurtaTypeI()
        c.set_slider(0, 5)
        assert c.sliders[0] == 5

    def test_slider_index_seven_valid(self) -> None:
        c = CurtaTypeI()
        c.set_slider(7, 3)
        assert c.sliders[7] == 3

    def test_set_input_populates_sliders(self) -> None:
        c = CurtaTypeI()
        c.set_input(12345678)
        # Sliders: LSB at index 0, MSB at index 7
        assert c.sliders[0] == 8  # units
        assert c.sliders[1] == 7  # tens
        assert c.sliders[7] == 1  # ten-millions

    def test_get_input_after_set_slider(self) -> None:
        c = CurtaTypeI()
        c.set_slider(0, 5)  # units = 5
        assert c.get_input_value() == 5

    def test_set_input_zero_clears_sliders(self) -> None:
        c = CurtaTypeI()
        c.set_input(12345678)
        c.set_input(0)
        assert c.get_input_value() == 0


# ---------------------------------------------------------------------------
# Extended counter tests
# ---------------------------------------------------------------------------


class TestCurtaCounterExtended:
    def test_counter_increments_with_each_add_crank(self) -> None:
        c = CurtaTypeI()
        c.set_input(1)
        for i in range(1, 6):
            c.turn_crank()
            assert c.counter_dial == i

    def test_counter_decrements_in_subtract_mode(self) -> None:
        c = CurtaTypeI()
        c.set_input(1)
        c.turn_crank()  # counter = 1
        c.set_crank_mode(CrankMode.SUBTRACT)
        c.turn_crank()  # counter = 0
        assert c.counter_dial == 0

    def test_counter_at_carriage_position_1_increments_by_ten(self) -> None:
        c = CurtaTypeI()
        c.set_input(1)
        c.shift_carriage(1)
        c.turn_crank()
        assert c.counter_dial == 10

    def test_counter_overflow_wraps(self) -> None:
        c = CurtaTypeI()
        c.counter_dial = c.MAX_COUNTER
        c._add_counter(1)
        assert c.counter_dial == 0

    def test_clear_counter_after_operations(self) -> None:
        c = CurtaTypeI()
        c.set_input(7)
        for _ in range(5):
            c.turn_crank()
        c.clear_counter()
        assert c.counter_dial == 0

    def test_result_and_counter_independent(self) -> None:
        c = CurtaTypeI()
        c.set_input(5)
        c.turn_crank()  # result=5, counter=1
        c.clear_counter()
        assert c.result_dial == 5
        assert c.counter_dial == 0


# ---------------------------------------------------------------------------
# Extended division tests
# ---------------------------------------------------------------------------


class TestCurtaDivisionExtended:
    def test_divide_zero_by_positive(self) -> None:
        c = CurtaTypeI()
        q, r = c.divide(0, 7)
        assert q == 0
        assert r == 0

    def test_divide_one_by_one(self) -> None:
        c = CurtaTypeI()
        q, r = c.divide(1, 1)
        assert q == 1
        assert r == 0

    def test_divide_quotient_times_divisor_plus_remainder_equals_dividend(self) -> None:
        c = CurtaTypeI()
        for dividend, divisor in [(17, 3), (100, 7), (9999, 13), (1000, 9)]:
            q, r = c.divide(dividend, divisor)
            assert q * divisor + r == dividend

    def test_divide_large_exact(self) -> None:
        c = CurtaTypeI()
        q, r = c.divide(1000000, 1000)
        assert q == 1000
        assert r == 0

    def test_divide_negative_divisor_raises(self) -> None:
        c = CurtaTypeI()
        with pytest.raises(ValueError):
            c.divide(10, -3)

    def test_divide_both_negative_raises(self) -> None:
        c = CurtaTypeI()
        with pytest.raises(ValueError):
            c.divide(-10, -5)

    def test_divide_clears_bell(self) -> None:
        c = CurtaTypeI()
        c.bell_ringing = True
        c.divide(10, 2)
        assert c.bell_ringing is False

    def test_divide_remainder_is_less_than_divisor(self) -> None:
        c = CurtaTypeI()
        for divisor in [2, 3, 7, 11, 13]:
            q, r = c.divide(100, divisor)
            assert 0 <= r < divisor


# ---------------------------------------------------------------------------
# Carriage position tests
# ---------------------------------------------------------------------------


class TestCurtaCarriage:
    def test_shift_zero_delta_no_change(self) -> None:
        c = CurtaTypeI()
        c.shift_carriage(3)
        c.shift_carriage(0)
        assert c.carriage_position == 3  # shift by 0 steps = no movement

    def test_shift_accumulates(self) -> None:
        c = CurtaTypeI()
        c.shift_carriage(2)
        c.shift_carriage(3)  # 2 + 3 = 5
        assert c.carriage_position == 5

    def test_shift_beyond_max_clamps(self) -> None:
        c = CurtaTypeI()
        c.shift_carriage(100)
        assert c.carriage_position <= 5

    def test_shift_to_negative_clamps_at_zero(self) -> None:
        c = CurtaTypeI()
        c.shift_carriage(-5)
        assert c.carriage_position == 0

    def test_carriage_at_5_multiplies_by_100000(self) -> None:
        c = CurtaTypeI()
        c.set_input(1)
        c.shift_carriage(5)
        c.turn_crank()
        assert c.result_dial == 100000


# ---------------------------------------------------------------------------
# Integration: using Curta for known computations
# ---------------------------------------------------------------------------


class TestCurtaIntegration:
    def test_add_repeated_equals_multiply(self) -> None:
        # Add 7 exactly 9 times = 63 = 7 * 9
        c = CurtaTypeI()
        c.set_input(7)
        for _ in range(9):
            c.turn_crank()
        assert c.result_dial == 63
        assert c.counter_dial == 9

    def test_multiply_correctness_via_crank(self) -> None:
        # 12 * 12 = 144 (standard Curta multiplication demo)
        c = CurtaTypeI()
        c.set_input(12)
        c.turn_crank()
        c.turn_crank()  # 2 units -> 24
        c.shift_carriage(1)
        c.turn_crank()  # 1 ten -> 120
        assert c.result_dial == 144

    def test_clear_then_start_fresh(self) -> None:
        c = CurtaTypeI()
        c.set_input(99)
        for _ in range(5):
            c.turn_crank()
        c.clear_result()
        c.clear_counter()
        c.set_input(3)
        c.turn_crank()
        assert c.result_dial == 3
        assert c.counter_dial == 1

    def test_bell_resets_after_clear_result(self) -> None:
        c = CurtaTypeI()
        c.result_dial = c.MAX_RESULT
        c._add_result(1)  # triggers bell
        assert c.bell_ringing is True
        c.clear_result()
        assert c.bell_ringing is False

    def test_add_mode_then_subtract_net_zero(self) -> None:
        c = CurtaTypeI()
        c.set_input(50)
        c.turn_crank()  # +50
        c.set_crank_mode(CrankMode.SUBTRACT)
        c.turn_crank()  # -50
        assert c.result_dial == 0

    def test_max_result_constant(self) -> None:
        c = CurtaTypeI()
        assert c.MAX_RESULT == 10**11 - 1
