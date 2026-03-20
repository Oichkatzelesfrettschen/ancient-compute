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
