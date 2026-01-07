"""Tests for Jacquard emulator."""

import pytest

from backend.src.emulator.jacquard import JacquardEmulator


def test_jacquard_reads_card():
    emu = JacquardEmulator(hooks=4)
    assert emu.read_card([1, 0, 1, 0]) == [1, 0, 1, 0]


def test_jacquard_invalid_card_length():
    emu = JacquardEmulator(hooks=3)
    with pytest.raises(ValueError):
        emu.read_card([1, 0, 1, 0])
