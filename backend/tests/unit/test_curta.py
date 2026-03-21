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
