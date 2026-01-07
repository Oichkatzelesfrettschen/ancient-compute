"""Tests for abacus emulator."""

from backend.src.emulator.abacus import AbacusEmulator


def test_abacus_add_carry():
    emu = AbacusEmulator()
    emu.set_value(789)
    assert emu.add(456) == 1245
    assert emu.state()["digits"] == [1, 2, 4, 5]


def test_abacus_sub_borrow():
    emu = AbacusEmulator()
    emu.set_value(1000)
    assert emu.sub(1) == 999
    assert emu.state()["digits"] == [9, 9, 9]
