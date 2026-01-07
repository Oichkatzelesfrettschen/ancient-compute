"""Tests for Leibniz reckoner emulator."""

from backend.src.emulator.leibniz_reckoner import LeibnizReckonerEmulator


def test_leibniz_add_and_shift():
    emu = LeibnizReckonerEmulator()
    assert emu.add_cycle(123) == 123
    assert emu.shift_carriage(1) == 1
