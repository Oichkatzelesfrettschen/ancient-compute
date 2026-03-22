"""Tests for Pascaline emulator."""

import pytest

from backend.src.emulator.pascaline import PascalineEmulator


def test_pascaline_carry_chain():
    emu = PascalineEmulator()
    emu.add(59)
    assert emu.add(1) == 60
    # units at index 0, tens at index 1
    assert emu.state()["digits"][:2] == [0, 6]


def test_pascaline_multi_carry():
    emu = PascalineEmulator()
    emu.add(99)
    assert emu.add(1) == 100
    assert emu.state()["digits"][:3] == [0, 0, 1]


def test_pascaline_add_zero():
    emu = PascalineEmulator()
    emu.add(42)
    assert emu.add(0) == 42


def test_pascaline_add_large():
    emu = PascalineEmulator()
    assert emu.add(12345678) == 12345678


def test_pascaline_add_to_round():
    emu = PascalineEmulator()
    emu.add(1000)
    assert emu.add(0) == 1000
    assert emu.state()["digits"][:4] == [0, 0, 0, 1]


def test_pascaline_ripple_carry_all_nines():
    # 999 + 1 = 1000: carry must ripple through all three nines
    emu = PascalineEmulator()
    emu.add(999)
    assert emu.add(1) == 1000
    assert emu.state()["digits"][:4] == [0, 0, 0, 1]


def test_pascaline_add_overflow_wraps():
    # On an 8-digit machine, 99999999 + 1 wraps to 0 (carry past MSB is lost)
    emu = PascalineEmulator(digits=8)
    emu.add(99999999)
    emu.add(1)
    assert emu.get_value() == 0


def test_pascaline_sautoir_lifted_during_carry():
    # When carry propagates, sautoir_lifted is set during the add, then cleared
    emu = PascalineEmulator()
    emu.add(9)  # units wheel is at 9
    emu.add(1)  # triggers carry: sautoir lifts, then clears
    # After the operation completes, sautoir should not be permanently lifted
    assert not any(w.sautoir_lifted for w in emu.wheels)
    assert emu.get_value() == 10


def test_pascaline_subtract_small_from_large():
    emu = PascalineEmulator()
    emu.add(10)
    emu.set_nines_complement_mode(True)
    result = emu.subtract(3)
    assert result == 7


def test_pascaline_subtract_equal():
    # On the Pascaline, equal subtraction (5 - 5) produces no carry-out from MSB.
    # Without end-around carry, the result stays in 9's complement form:
    # 9-complement(5) = 99999994; 5 + 99999994 = 99999999 (no carry). Historical.
    emu = PascalineEmulator()
    emu.add(5)
    emu.set_nines_complement_mode(True)
    result = emu.subtract(5)
    assert result == 99999999  # 9's complement of 0 on 8-digit machine


def test_pascaline_subtract_with_carry():
    # 100 - 99 = 1
    emu = PascalineEmulator()
    emu.add(100)
    emu.set_nines_complement_mode(True)
    result = emu.subtract(99)
    assert result == 1


def test_pascaline_subtract_single_from_round():
    # 100 - 1 = 99
    emu = PascalineEmulator()
    emu.add(100)
    emu.set_nines_complement_mode(True)
    result = emu.subtract(1)
    assert result == 99


def test_pascaline_subtract_zero():
    emu = PascalineEmulator()
    emu.add(42)
    emu.set_nines_complement_mode(True)
    result = emu.subtract(0)
    assert result == 42


def test_pascaline_subtract_large():
    # 9999 - 1234 = 8765
    emu = PascalineEmulator()
    emu.add(9999)
    emu.set_nines_complement_mode(True)
    result = emu.subtract(1234)
    assert result == 8765


def test_pascaline_subtract_requires_nines_mode():
    emu = PascalineEmulator()
    emu.add(10)
    with pytest.raises(RuntimeError):
        emu.subtract(3)


def test_pascaline_set_value():
    emu = PascalineEmulator()
    emu.set_value(12345)
    assert emu.get_value() == 12345
    assert emu.state()["digits"][:5] == [5, 4, 3, 2, 1]


def test_pascaline_set_value_overflow():
    emu = PascalineEmulator(digits=4)
    with pytest.raises(OverflowError):
        emu.set_value(99999)


def test_pascaline_reset_clears_all():
    emu = PascalineEmulator()
    emu.add(12345)
    emu.set_nines_complement_mode(True)
    emu.reset()
    assert emu.get_value() == 0
    assert not emu.nines_complement_mode
    assert all(w.value == 0 for w in emu.wheels)


def test_pascaline_sequential_adds():
    emu = PascalineEmulator()
    for _ in range(10):
        emu.add(9)
    assert emu.get_value() == 90


def test_pascaline_nines_complement_mode_flag():
    emu = PascalineEmulator()
    assert not emu.nines_complement_mode
    emu.set_nines_complement_mode(True)
    assert emu.nines_complement_mode
    emu.set_nines_complement_mode(False)
    assert not emu.nines_complement_mode


def test_pascaline_eight_digit_max():
    emu = PascalineEmulator(digits=8)
    emu.add(99999998)
    emu.add(1)
    assert emu.get_value() == 99999999


def test_pascaline_get_value_fresh_machine():
    emu = PascalineEmulator()
    assert emu.get_value() == 0


def test_pascaline_state_keys_complete():
    emu = PascalineEmulator()
    s = emu.state()
    assert "value" in s
    assert "digits" in s
    assert "sautoirs_lifted" in s
    assert "nines_complement_mode" in s


def test_pascaline_state_digits_length():
    # state["digits"] has one entry per wheel
    emu = PascalineEmulator(digits=6)
    assert len(emu.state()["digits"]) == 6
    assert len(emu.state()["sautoirs_lifted"]) == 6


def test_pascaline_state_value_matches_get_value():
    emu = PascalineEmulator()
    emu.add(9876)
    s = emu.state()
    assert s["value"] == emu.get_value()


def test_pascaline_custom_digit_count_add():
    emu = PascalineEmulator(digits=4)
    emu.add(999)
    emu.add(1)
    assert emu.get_value() == 1000


def test_pascaline_rotate_input_wheel_adds_one_digit():
    emu = PascalineEmulator()
    emu.rotate_input_wheel(7)
    assert emu.get_value() == 7


def test_pascaline_rotate_input_wheel_carry():
    emu = PascalineEmulator()
    emu.add(9)
    emu.rotate_input_wheel(1)  # 9 + 1 = 10, carry to tens
    assert emu.get_value() == 10


def test_pascaline_rotate_input_wheel_invalid_raises():
    emu = PascalineEmulator()
    with pytest.raises(ValueError):
        emu.rotate_input_wheel(10)
    with pytest.raises(ValueError):
        emu.rotate_input_wheel(-1)


def test_pascaline_sequential_subtracts():
    # 1000 - 300 - 200 = 500
    emu = PascalineEmulator()
    emu.add(1000)
    emu.set_nines_complement_mode(True)
    emu.subtract(300)
    result = emu.subtract(200)
    assert result == 500


def test_pascaline_subtract_then_add():
    # add 50, subtract 20, add 10 => 40
    emu = PascalineEmulator()
    emu.add(50)
    emu.set_nines_complement_mode(True)
    emu.subtract(20)
    emu.set_nines_complement_mode(False)
    emu.add(10)
    assert emu.get_value() == 40


def test_pascaline_nines_complement_of_zero():
    # nines complement of 0 is 99999999 (8-digit machine)
    emu = PascalineEmulator()
    result = emu._nines_complement(0)
    assert result == 99999999


def test_pascaline_nines_complement_of_max():
    # nines complement of 99999999 is 0
    emu = PascalineEmulator()
    result = emu._nines_complement(99999999)
    assert result == 0


def test_pascaline_nines_complement_out_of_range():
    emu = PascalineEmulator(digits=4)
    with pytest.raises(ValueError):
        emu._nines_complement(10000)  # exceeds 4-digit range


def test_pascaline_set_value_zero():
    emu = PascalineEmulator()
    emu.add(999)
    emu.set_value(0)
    assert emu.get_value() == 0


# ---------------------------------------------------------------------------
# Wheel class properties
# ---------------------------------------------------------------------------


class TestWheel:
    """Wheel dataclass properties: initial values, flags, repr."""

    def test_initial_value_is_zero(self) -> None:
        from backend.src.emulator.pascaline import Wheel
        w = Wheel(0)
        assert w.value == 0

    def test_position_attribute_stored(self) -> None:
        from backend.src.emulator.pascaline import Wheel
        w = Wheel(3)
        assert w.position == 3

    def test_pins_active_initially_false(self) -> None:
        from backend.src.emulator.pascaline import Wheel
        w = Wheel(0)
        assert not w.pins_active

    def test_sautoir_lifted_initially_false(self) -> None:
        from backend.src.emulator.pascaline import Wheel
        w = Wheel(0)
        assert not w.sautoir_lifted

    def test_repr_contains_wheel_and_position(self) -> None:
        from backend.src.emulator.pascaline import Wheel
        w = Wheel(5)
        r = repr(w)
        assert "Wheel" in r
        assert "5" in r


# ---------------------------------------------------------------------------
# Nines complement extended properties
# ---------------------------------------------------------------------------


class TestPascalineNinesComplementExtended:
    """_nines_complement() across various digit configurations."""

    def test_complement_of_1(self) -> None:
        emu = PascalineEmulator()
        assert emu._nines_complement(1) == 99999998

    def test_complement_of_5(self) -> None:
        emu = PascalineEmulator()
        assert emu._nines_complement(5) == 99999994

    def test_complement_on_4digit_machine(self) -> None:
        emu = PascalineEmulator(digits=4)
        assert emu._nines_complement(1234) == 8765

    def test_complement_is_symmetric(self) -> None:
        # C(C(n)) == n for any valid n
        emu = PascalineEmulator()
        for n in [0, 1, 42, 99999, 50000000]:
            assert emu._nines_complement(emu._nines_complement(n)) == n

    def test_complement_sum_always_nines(self) -> None:
        # n + C(n) = 10^digits - 1 (all nines)
        emu = PascalineEmulator(digits=4)
        for n in [0, 1, 999, 4999]:
            c = emu._nines_complement(n)
            assert n + c == 9999

    def test_complement_boundary_1(self) -> None:
        # complement of 10^8 - 1 = 0 (maximum -> zero)
        emu = PascalineEmulator(digits=8)
        assert emu._nines_complement(99999999) == 0

    def test_complement_out_of_range_raises(self) -> None:
        emu = PascalineEmulator(digits=4)
        with pytest.raises(ValueError):
            emu._nines_complement(10000)


# ---------------------------------------------------------------------------
# State dict detailed tests
# ---------------------------------------------------------------------------


class TestPascalineStateDetailed:
    """state() dict completeness, initial values, updates after operations."""

    def test_initial_sautoirs_all_false(self) -> None:
        emu = PascalineEmulator()
        assert not any(emu.state()["sautoirs_lifted"])

    def test_sautoirs_cleared_after_carry(self) -> None:
        emu = PascalineEmulator()
        emu.add(9)
        emu.add(1)  # triggers carry, sautoir lifts then clears
        assert not any(emu.state()["sautoirs_lifted"])

    def test_state_nines_complement_mode_false_initially(self) -> None:
        emu = PascalineEmulator()
        assert emu.state()["nines_complement_mode"] is False

    def test_state_nines_complement_mode_updates(self) -> None:
        emu = PascalineEmulator()
        emu.set_nines_complement_mode(True)
        assert emu.state()["nines_complement_mode"] is True

    def test_digits_all_zero_initially(self) -> None:
        emu = PascalineEmulator()
        assert all(d == 0 for d in emu.state()["digits"])

    def test_state_value_matches_get_value_after_add(self) -> None:
        emu = PascalineEmulator()
        emu.add(54321)
        s = emu.state()
        assert s["value"] == emu.get_value()

    def test_state_after_reset_all_zeros(self) -> None:
        emu = PascalineEmulator()
        emu.add(9999)
        emu.reset()
        s = emu.state()
        assert s["value"] == 0
        assert all(d == 0 for d in s["digits"])
        assert s["nines_complement_mode"] is False


# ---------------------------------------------------------------------------
# Different digit configurations
# ---------------------------------------------------------------------------


class TestPascalineDigitConfig:
    """Behavior across 4/6/8/10-digit machines."""

    def test_4digit_max_value(self) -> None:
        emu = PascalineEmulator(digits=4)
        emu.add(9999)
        assert emu.get_value() == 9999

    def test_4digit_overflow_wraps(self) -> None:
        emu = PascalineEmulator(digits=4)
        emu.add(9999)
        emu.add(1)
        assert emu.get_value() == 0

    def test_6digit_state_length(self) -> None:
        emu = PascalineEmulator(digits=6)
        s = emu.state()
        assert len(s["digits"]) == 6
        assert len(s["sautoirs_lifted"]) == 6

    def test_10digit_machine_add(self) -> None:
        emu = PascalineEmulator(digits=10)
        emu.add(1234567890)
        assert emu.get_value() == 1234567890

    def test_set_value_on_6digit_machine(self) -> None:
        emu = PascalineEmulator(digits=6)
        emu.set_value(123456)
        assert emu.get_value() == 123456

    def test_4digit_subtract_valid(self) -> None:
        emu = PascalineEmulator(digits=4)
        emu.add(500)
        emu.set_nines_complement_mode(True)
        result = emu.subtract(200)
        assert result == 300
