"""Tests for quipu emulator."""

from backend.src.emulator.quipu import QuipuEmulator


def test_quipu_encode_decode():
    emu = QuipuEmulator()
    emu.encode_number("A", 123)
    emu.encode_number("A", 7)
    assert emu.decode_number("A") == [123, 7]
    assert emu.sum_by_category("A") == 130
