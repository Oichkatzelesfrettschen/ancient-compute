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


class TestClayTokensAudit:
    """Audit invariants and impression-snapshot semantics."""

    def test_audit_empty_before_seal_is_true(self) -> None:
        emu = ClayTokensEmulator()
        # Both _tokens and _impression are empty -- equal.
        assert emu.audit() is True

    def test_audit_with_tokens_before_seal_is_false(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("cattle", 5)
        # _impression still empty; tokens differs.
        assert emu.audit() is False

    def test_audit_after_seal_is_true(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("grain", 10)
        emu.seal()
        assert emu.audit() is True

    def test_audit_empty_seal_is_true(self) -> None:
        emu = ClayTokensEmulator()
        emu.seal()
        assert emu.audit() is True

    def test_impression_is_snapshot_at_seal_time(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("oil", 3)
        emu.seal()
        # Tokens dict is frozen; impression reflects the sealed snapshot.
        imp = emu.state()["impression"]
        assert imp["oil"] == 3

    def test_impression_captures_all_types_at_seal(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("A", 1)
        emu.add_token("B", 2)
        emu.add_token("C", 3)
        emu.seal()
        imp = emu.state()["impression"]
        assert imp == {"A": 1, "B": 2, "C": 3}

    def test_double_seal_updates_impression(self) -> None:
        # Second seal overwrites impression with current tokens.
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 5)
        emu.seal()
        # Unseal via reset, add more, re-seal.
        emu.reset()
        emu.add_token("sheep", 7)
        emu.seal()
        assert emu.state()["impression"]["sheep"] == 7

    def test_audit_true_after_reset_and_reseal(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("X", 10)
        emu.seal()
        emu.reset()
        emu.add_token("Y", 2)
        emu.seal()
        assert emu.audit() is True

    def test_audit_false_before_seal_multiple_types(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("grain", 4)
        emu.add_token("oil", 2)
        # impression is empty; should not match.
        assert emu.audit() is False


class TestClayTokensEdgeCases:
    """Edge cases: large quantities, many types, zero-qty adds."""

    def test_add_large_qty(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("grain", 100_000)
        assert emu.state()["tokens"]["grain"] == 100_000

    def test_add_default_qty_one_multiple_calls(self) -> None:
        emu = ClayTokensEmulator()
        for _ in range(7):
            emu.add_token("goats")
        assert emu.state()["tokens"]["goats"] == 7

    def test_remove_to_zero_removes_key(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("oil", 3)
        emu.remove_token("oil", 3)
        assert "oil" not in emu.state()["tokens"]

    def test_many_token_types(self) -> None:
        emu = ClayTokensEmulator()
        types = ["a", "b", "c", "d", "e", "f", "g", "h"]
        for i, t in enumerate(types):
            emu.add_token(t, i + 1)
        tokens = emu.state()["tokens"]
        assert set(tokens.keys()) == set(types)
        assert tokens["h"] == 8

    def test_tokens_values_always_non_negative(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("X", 2)
        emu.remove_token("X", 999)
        for v in emu.state()["tokens"].values():
            assert v >= 0

    def test_empty_state_has_empty_dicts(self) -> None:
        emu = ClayTokensEmulator()
        s = emu.state()
        assert s["tokens"] == {}
        assert s["impression"] == {}
        assert s["sealed"] is False

    def test_reset_after_multiple_additions(self) -> None:
        emu = ClayTokensEmulator()
        for i in range(10):
            emu.add_token(f"type_{i}", i)
        emu.reset()
        assert emu.state()["tokens"] == {}

    def test_add_zero_qty_does_not_add_key(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("phantom", 0)
        # Counter with 0 value should not appear in dict (Counter behaviour)
        # -- tolerate either behaviour but value must be 0 if present
        tokens = emu.state()["tokens"]
        assert tokens.get("phantom", 0) == 0

    def test_seal_then_reset_then_seal_again(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("X", 1)
        emu.seal()
        emu.reset()
        emu.add_token("X", 2)
        emu.seal()
        assert emu.state()["sealed"] is True
        assert emu.state()["tokens"]["X"] == 2


class TestClayTokensErrors:
    """ValueError semantics for sealed-bulla mutations."""

    def test_add_after_seal_raises_value_error(self) -> None:
        emu = ClayTokensEmulator()
        emu.seal()
        with pytest.raises(ValueError):
            emu.add_token("sheep")

    def test_remove_after_seal_raises_value_error(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("grain", 3)
        emu.seal()
        with pytest.raises(ValueError):
            emu.remove_token("grain")

    def test_error_message_mentions_sealed(self) -> None:
        emu = ClayTokensEmulator()
        emu.seal()
        try:
            emu.add_token("goat")
        except ValueError as exc:
            assert "seal" in str(exc).lower()
        else:
            raise AssertionError("Expected ValueError")

    def test_add_negative_qty_removes(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("sheep", 5)
        emu.add_token("sheep", -2)
        assert emu.state()["tokens"].get("sheep", 0) == 3

    def test_remove_default_qty_is_one(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("grain", 3)
        emu.remove_token("grain")
        assert emu.state()["tokens"].get("grain", 0) == 2


class TestClayTokensStateInvariant:
    """State dict invariants across operations."""

    def test_state_returns_new_dict_each_call(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("oil", 1)
        s1 = emu.state()
        s2 = emu.state()
        s1["tokens"]["oil"] = 999
        assert emu.state()["tokens"].get("oil", 0) != 999

    def test_impression_independent_of_tokens_after_seal(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("cattle", 5)
        emu.seal()
        # Post-seal tokens dict should not change
        # (and impression captures the snapshot)
        imp = emu.state()["impression"]
        tok = emu.state()["tokens"]
        assert imp == {"cattle": 5}
        assert tok == {"cattle": 5}

    def test_reset_returns_to_initial_state(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("X", 10)
        emu.add_token("Y", 20)
        emu.seal()
        emu.reset()
        s = emu.state()
        assert s["tokens"] == {}
        assert s["sealed"] is False
        assert s["impression"] == {}

    def test_multiple_resets_idempotent(self) -> None:
        emu = ClayTokensEmulator()
        emu.add_token("grain", 3)
        emu.reset()
        emu.reset()
        emu.reset()
        assert emu.state()["tokens"] == {}

    def test_state_tokens_key_is_dict(self) -> None:
        emu = ClayTokensEmulator()
        assert isinstance(emu.state()["tokens"], dict)

    def test_state_impression_key_is_dict(self) -> None:
        emu = ClayTokensEmulator()
        assert isinstance(emu.state()["impression"], dict)

    def test_state_sealed_key_is_bool(self) -> None:
        emu = ClayTokensEmulator()
        assert isinstance(emu.state()["sealed"], bool)

    def test_two_independent_emulators_no_shared_state(self) -> None:
        a = ClayTokensEmulator()
        b = ClayTokensEmulator()
        a.add_token("wheat", 7)
        assert b.state()["tokens"] == {}

    def test_remove_nonexistent_unsealed_is_noop(self) -> None:
        emu = ClayTokensEmulator()
        # Should not raise; graceful no-op.
        emu.remove_token("ghost", 5)
        assert emu.state()["tokens"] == {}
