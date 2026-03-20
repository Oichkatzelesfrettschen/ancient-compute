"""Tests for Leibniz reckoner emulator."""

from backend.src.emulator.leibniz_reckoner import LeibnizReckonerEmulator


def test_leibniz_add_and_shift():
    emu = LeibnizReckonerEmulator()
    assert emu.add_value(123) == 123
    emu.shift_carriage(1)
    assert emu.carriage_position == 1


def test_leibniz_multiply_two_digit():
    emu = LeibnizReckonerEmulator()
    result = emu.multiply(12, 34)
    assert result == 12 * 34


def test_leibniz_multiply_large():
    emu = LeibnizReckonerEmulator()
    result = emu.multiply(999, 999)
    assert result == 999 * 999


def test_leibniz_reset_clears_accumulator():
    emu = LeibnizReckonerEmulator()
    emu.add_value(500)
    emu.reset()
    assert emu.get_accumulator_value() == 0
    assert emu.turn_counter == 0


def test_leibniz_multiply_zero():
    emu = LeibnizReckonerEmulator()
    result = emu.multiply(0, 999)
    assert result == 0


def test_leibniz_divide_exact():
    emu = LeibnizReckonerEmulator()
    q, r = emu.divide(100, 5)
    assert q == 20
    assert r == 0


def test_leibniz_divide_with_remainder():
    emu = LeibnizReckonerEmulator()
    q, r = emu.divide(17, 3)
    assert q == 5
    assert r == 2


def test_leibniz_divide_by_one():
    emu = LeibnizReckonerEmulator()
    q, r = emu.divide(255, 1)
    assert q == 255
    assert r == 0


def test_leibniz_divide_large():
    emu = LeibnizReckonerEmulator()
    q, r = emu.divide(9999, 99)
    expected_q, expected_r = divmod(9999, 99)
    assert q == expected_q
    assert r == expected_r


def test_leibniz_divide_by_zero():
    import pytest

    emu = LeibnizReckonerEmulator()
    with pytest.raises(ZeroDivisionError):
        emu.divide(10, 0)


def test_leibniz_divide_accumulator_holds_remainder():
    emu = LeibnizReckonerEmulator()
    q, r = emu.divide(17, 3)
    assert emu.get_accumulator_value() == r


def test_leibniz_multiply_one():
    emu = LeibnizReckonerEmulator()
    assert emu.multiply(42, 1) == 42


def test_leibniz_multiply_by_two():
    emu = LeibnizReckonerEmulator()
    assert emu.multiply(7, 2) == 14


def test_leibniz_add_value_increments_accumulator():
    emu = LeibnizReckonerEmulator()
    emu.add_value(100)
    emu.add_value(200)
    assert emu.get_accumulator_value() == 300


def test_leibniz_set_input_loads_drums():
    emu = LeibnizReckonerEmulator()
    emu.set_input(123)
    # input_drums should reflect digits 1, 2, 3 (LSB first)
    assert emu.input_drums[0].value == 3
    assert emu.input_drums[1].value == 2
    assert emu.input_drums[2].value == 1


def test_leibniz_crank_turn_adds_input():
    emu = LeibnizReckonerEmulator()
    emu.set_input(5)
    emu.crank_turn(3)
    assert emu.get_accumulator_value() == 15


def test_leibniz_carriage_shift_zero():
    emu = LeibnizReckonerEmulator()
    emu.shift_carriage(0)
    assert emu.carriage_position == 0


def test_leibniz_divide_zero_dividend():
    emu = LeibnizReckonerEmulator()
    q, r = emu.divide(0, 7)
    assert q == 0
    assert r == 0


def test_leibniz_divide_dividend_equals_divisor():
    emu = LeibnizReckonerEmulator()
    q, r = emu.divide(9, 9)
    assert q == 1
    assert r == 0


def test_leibniz_divide_dividend_less_than_divisor():
    emu = LeibnizReckonerEmulator()
    q, r = emu.divide(3, 7)
    assert q == 0
    assert r == 3


def test_leibniz_state_includes_accumulator():
    emu = LeibnizReckonerEmulator()
    emu.add_value(77)
    s = emu.state()
    assert "accumulator" in s
    assert s["accumulator"] == 77


def test_leibniz_turn_counter_increments():
    emu = LeibnizReckonerEmulator()
    emu.set_input(1)
    emu.crank_turn(5)
    assert emu.turn_counter == 5


def test_leibniz_multiply_both_zero():
    emu = LeibnizReckonerEmulator()
    assert emu.multiply(0, 0) == 0


def test_leibniz_multiply_large_values():
    emu = LeibnizReckonerEmulator()
    assert emu.multiply(1000, 1000) == 1_000_000


def test_leibniz_reset_clears_carriage_position():
    emu = LeibnizReckonerEmulator()
    emu.shift_carriage(3)
    emu.reset()
    assert emu.carriage_position == 0


def test_leibniz_add_value_returns_accumulator():
    emu = LeibnizReckonerEmulator()
    result = emu.add_value(99)
    assert result == 99


def test_leibniz_divide_exact_large():
    emu = LeibnizReckonerEmulator()
    q, r = emu.divide(1000000, 1000)
    assert q == 1000
    assert r == 0
