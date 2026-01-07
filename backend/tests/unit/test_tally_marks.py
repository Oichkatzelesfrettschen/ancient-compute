"""Tests for tally mark emulator."""

from backend.src.emulator.tally_marks import TallyMarksEmulator


def test_tally_marks_basic_counts():
    emu = TallyMarksEmulator()
    assert emu.step(4) == 4
    assert emu.render() == "||||"
    assert emu.step(1) == 5
    assert emu.render() == "|||||"


def test_tally_marks_underflow():
    emu = TallyMarksEmulator()
    emu.step(3)
    assert emu.step(-10) == 0
    assert emu.render() == ""
