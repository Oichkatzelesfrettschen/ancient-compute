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


class TestClayTokensEmulator:
    def test_initial_state_empty(self) -> None:
        emu = ClayTokensEmulator()
        s = emu.state()
        assert s["tokens"] == {}
        assert s["sealed"] is False
        assert s["impression"] == {}

    def test_add_token_single_type(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("oil", 3)
        assert emu.state()["tokens"]["oil"] == 3

    def test_add_token_accumulates(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 3)
        emu.add_token("sheep", 2)
        assert emu.state()["tokens"]["sheep"] == 5

    def test_add_multiple_types(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 3)
        emu.add_token("grain", 2)
        emu.add_token("oil", 1)
        tokens = emu.state()["tokens"]
        assert tokens == {"sheep": 3, "grain": 2, "oil": 1}

    def test_remove_token_partial(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 5)
        emu.remove_token("sheep", 2)
        assert emu.state()["tokens"]["sheep"] == 3

    def test_remove_token_all_removes_key(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("grain", 4)
        emu.remove_token("grain", 4)
        assert "grain" not in emu.state()["tokens"]

    def test_remove_more_than_present_clamps_zero(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("oil", 2)
        emu.remove_token("oil", 10)
        assert "oil" not in emu.state()["tokens"]

    def test_remove_nonexistent_type_noop(self) -> None:
        emu = ClayTokensEmulator()
        emu.remove_token("ghost", 1)
        assert emu.state()["tokens"] == {}

    def test_seal_sets_sealed_true(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 1)
        emu.seal()
        assert emu.state()["sealed"] is True

    def test_seal_captures_impression(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 3)
        emu.add_token("grain", 2)
        emu.seal()
        imp = emu.state()["impression"]
        assert imp["sheep"] == 3
        assert imp["grain"] == 2

    def test_audit_passes_when_sealed_unchanged(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 5)
        emu.seal()
        assert emu.audit() is True

    def test_remove_after_seal_raises(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 3)
        emu.seal()
        with pytest.raises(ValueError):
            emu.remove_token("sheep", 1)

    def test_reset_clears_everything(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 5)
        emu.seal()
        emu.reset()
        s = emu.state()
        assert s["tokens"] == {}
        assert s["sealed"] is False
        assert s["impression"] == {}

    def test_reset_allows_mutation_after_sealed(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 1)
        emu.seal()
        emu.reset()
        emu.add_token("grain", 4)
        assert emu.state()["tokens"]["grain"] == 4

    def test_add_default_qty_is_one(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("wheat")
        assert emu.state()["tokens"]["wheat"] == 1

    def test_state_structure_keys(self) -> None:
        emu = ClayTokensEmulator()
        s = emu.state()
        assert set(s.keys()) >= {"tokens", "sealed", "impression"}
