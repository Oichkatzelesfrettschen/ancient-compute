"""Tests for clay tokens emulator."""

import pytest

from backend.src.emulator.clay_tokens import ClayTokensEmulator


def test_clay_tokens_seal_and_audit():
    emu = ClayTokensEmulator()
    emu.add_token("sheep", 3)
    emu.add_token("grain", 2)
    emu.seal()
    assert emu.audit() is True


def test_clay_tokens_block_mutation_when_sealed():
    emu = ClayTokensEmulator()
    emu.add_token("sheep", 1)
    emu.seal()
    with pytest.raises(ValueError):
        emu.add_token("sheep", 1)
