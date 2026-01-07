"""Tests for Pascaline emulator."""

from backend.src.emulator.pascaline import PascalineEmulator


def test_pascaline_carry_chain():
    emu = PascalineEmulator()
    emu.add(59)
    assert emu.add(1) == 60
    assert emu.state()["digits"] == [6, 0]


def test_pascaline_multi_carry():
    emu = PascalineEmulator()
    emu.add(99)
    assert emu.add(1) == 100
    assert emu.state()["digits"] == [1, 0, 0]
