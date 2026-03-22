"""Tests for Leibniz reckoner emulator."""

import pytest

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


def test_leibniz_state_all_keys():
    emu = LeibnizReckonerEmulator()
    s = emu.state()
    for key in ("accumulator", "carriage_position", "input_drums", "turn_counter"):
        assert key in s


def test_leibniz_state_input_drums_length():
    emu = LeibnizReckonerEmulator()
    s = emu.state()
    assert len(s["input_drums"]) == emu.num_input_digits


def test_leibniz_crank_turn_zero_raises():
    emu = LeibnizReckonerEmulator()
    emu.set_input(1)
    with pytest.raises(ValueError):
        emu.crank_turn(0)


def test_leibniz_carriage_shift_accumulates():
    emu = LeibnizReckonerEmulator()
    emu.shift_carriage(2)
    emu.shift_carriage(3)
    assert emu.carriage_position == 5


def test_leibniz_carriage_shift_out_of_bounds_raises():
    emu = LeibnizReckonerEmulator()
    with pytest.raises(ValueError):
        emu.shift_carriage(100)


def test_leibniz_set_input_overflow_raises():
    emu = LeibnizReckonerEmulator()
    with pytest.raises(OverflowError):
        emu.set_input(10**emu.num_input_digits)


def test_leibniz_multiply_correctness_edge_cases():
    emu = LeibnizReckonerEmulator()
    assert emu.multiply(1, 1) == 1
    assert emu.multiply(10, 10) == 100
    assert emu.multiply(100, 100) == 10000



# ---------------------------------------------------------------------------
# SteppedDrum class
# ---------------------------------------------------------------------------


class TestSteppedDrum:
    """SteppedDrum validation, teeth count, and repr."""

    def test_valid_value_zero(self) -> None:
        from backend.src.emulator.leibniz_reckoner import SteppedDrum
        drum = SteppedDrum(0)
        assert drum.get_teeth_count() == 0

    def test_valid_value_nine(self) -> None:
        from backend.src.emulator.leibniz_reckoner import SteppedDrum
        drum = SteppedDrum(9)
        assert drum.get_teeth_count() == 9

    def test_invalid_value_raises(self) -> None:
        from backend.src.emulator.leibniz_reckoner import SteppedDrum
        with pytest.raises(ValueError):
            SteppedDrum(10)

    def test_negative_value_raises(self) -> None:
        from backend.src.emulator.leibniz_reckoner import SteppedDrum
        with pytest.raises(ValueError):
            SteppedDrum(-1)

    def test_repr_contains_value(self) -> None:
        from backend.src.emulator.leibniz_reckoner import SteppedDrum
        drum = SteppedDrum(5)
        assert "5" in repr(drum)
        assert "SteppedDrum" in repr(drum)


# ---------------------------------------------------------------------------
# CountingWheel class
# ---------------------------------------------------------------------------


class TestCountingWheel:
    """CountingWheel advance, carry, position."""

    def test_initial_value_zero(self) -> None:
        from backend.src.emulator.leibniz_reckoner import CountingWheel
        w = CountingWheel(0)
        assert w.value == 0

    def test_advance_no_carry(self) -> None:
        from backend.src.emulator.leibniz_reckoner import CountingWheel
        w = CountingWheel(0)
        carry = w.advance(5)
        assert w.value == 5
        assert carry == 0

    def test_advance_with_carry(self) -> None:
        from backend.src.emulator.leibniz_reckoner import CountingWheel
        w = CountingWheel(0)
        w.value = 8
        carry = w.advance(3)
        assert w.value == 1
        assert carry == 1

    def test_advance_exact_ten_carry(self) -> None:
        from backend.src.emulator.leibniz_reckoner import CountingWheel
        w = CountingWheel(0)
        w.value = 5
        carry = w.advance(5)  # 5 + 5 = 10 -> 0 with carry
        assert w.value == 0
        assert carry == 1

    def test_position_attribute_stored(self) -> None:
        from backend.src.emulator.leibniz_reckoner import CountingWheel
        w = CountingWheel(7)
        assert w.position == 7

    def test_repr_contains_position_and_value(self) -> None:
        from backend.src.emulator.leibniz_reckoner import CountingWheel
        w = CountingWheel(3)
        w.value = 9
        r = repr(w)
        assert "3" in r
        assert "9" in r


# ---------------------------------------------------------------------------
# State dict extended
# ---------------------------------------------------------------------------


class TestLeibnizStateExtended:
    """state() completeness and accuracy."""

    def test_initial_accumulator_zero(self) -> None:
        emu = LeibnizReckonerEmulator()
        assert emu.state()["accumulator"] == 0

    def test_state_input_drums_all_zero_initially(self) -> None:
        emu = LeibnizReckonerEmulator()
        assert all(d == 0 for d in emu.state()["input_drums"])

    def test_state_turn_counter_zero_initially(self) -> None:
        emu = LeibnizReckonerEmulator()
        assert emu.state()["turn_counter"] == 0

    def test_state_carriage_position_zero_initially(self) -> None:
        emu = LeibnizReckonerEmulator()
        assert emu.state()["carriage_position"] == 0

    def test_state_reflects_after_set_input(self) -> None:
        emu = LeibnizReckonerEmulator()
        emu.set_input(321)
        s = emu.state()
        assert s["input_drums"][0] == 1  # units
        assert s["input_drums"][1] == 2  # tens
        assert s["input_drums"][2] == 3  # hundreds

    def test_state_input_drums_has_correct_length(self) -> None:
        emu = LeibnizReckonerEmulator()
        assert len(emu.state()["input_drums"]) == emu.num_input_digits


# ---------------------------------------------------------------------------
# Carriage extended
# ---------------------------------------------------------------------------


class TestLeibnizCarriageExtended:
    """shift_carriage() boundary and accumulation tests."""

    def test_shift_carriage_negative_raises(self) -> None:
        emu = LeibnizReckonerEmulator()
        with pytest.raises(ValueError):
            emu.shift_carriage(-1)

    def test_shift_to_valid_max(self) -> None:
        emu = LeibnizReckonerEmulator()
        # max valid position = num_accumulator_digits - num_input_digits
        max_pos = emu.num_accumulator_digits - emu.num_input_digits
        emu.shift_carriage(max_pos)
        assert emu.carriage_position == max_pos

    def test_carriage_position_accumulates(self) -> None:
        emu = LeibnizReckonerEmulator()
        emu.shift_carriage(2)
        emu.shift_carriage(3)
        assert emu.carriage_position == 5

    def test_shift_back_to_zero_after_reset(self) -> None:
        emu = LeibnizReckonerEmulator()
        emu.shift_carriage(3)
        emu.reset()
        assert emu.carriage_position == 0

    def test_multiply_uses_carriage_shift(self) -> None:
        # 5 * 23 = 115: units digit 3 cranked at pos 0, tens digit 2 at pos 1
        emu = LeibnizReckonerEmulator()
        assert emu.multiply(5, 23) == 115


# ---------------------------------------------------------------------------
# Arithmetic extended
# ---------------------------------------------------------------------------


class TestLeibnizArithmeticExtended:
    """Crank carry propagation, multiply resets, divide state."""

    def test_add_sequential_with_crank_turns(self) -> None:
        emu = LeibnizReckonerEmulator()
        emu.set_input(7)
        emu.crank_turn(4)
        assert emu.get_accumulator_value() == 28  # 7 * 4

    def test_crank_carry_propagation(self) -> None:
        # Add 999999999 ten times (1 digit past the max input digits)
        # Using 8-digit input, add 99999999 ten times = 999999990
        emu = LeibnizReckonerEmulator()
        emu.set_input(99999999)
        emu.crank_turn(10)
        assert emu.get_accumulator_value() == 999999990

    def test_multiply_resets_before_computing(self) -> None:
        emu = LeibnizReckonerEmulator()
        emu.add_value(100)  # add something first
        result = emu.multiply(5, 3)
        assert result == 15  # not 115

    def test_divide_leaves_turn_counter_as_quotient(self) -> None:
        emu = LeibnizReckonerEmulator()
        q, _ = emu.divide(20, 4)
        assert emu.turn_counter == q

    def test_divide_leaves_remainder_in_accumulator(self) -> None:
        emu = LeibnizReckonerEmulator()
        _, r = emu.divide(17, 5)
        assert emu.get_accumulator_value() == r

    def test_add_value_updates_turn_counter(self) -> None:
        emu = LeibnizReckonerEmulator()
        emu.add_value(1)
        emu.add_value(2)
        assert emu.turn_counter == 2  # two crank turns total
