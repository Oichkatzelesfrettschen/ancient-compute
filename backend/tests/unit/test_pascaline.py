"""Tests for Pascaline emulator."""

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
