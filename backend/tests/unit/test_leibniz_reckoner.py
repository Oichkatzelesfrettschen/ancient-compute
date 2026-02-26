"""Tests for Leibniz reckoner emulator."""

from backend.src.emulator.leibniz_reckoner import LeibnizReckonerEmulator


def test_leibniz_add_and_shift():
    emu = LeibnizReckonerEmulator()
    assert emu.add_value(123) == 123
    emu.shift_carriage(1)
    assert emu.carriage_position == 1
