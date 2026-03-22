"""Tests for JacquardLoom and JacquardEmulator (backward-compat alias)."""

import pytest

from backend.src.emulator.jacquard import JacquardEmulator, JacquardLoom

# ---------------------------------------------------------------------------
# JacquardLoom -- basic construction
# ---------------------------------------------------------------------------


def test_loom_default_num_hooks():
    loom = JacquardLoom()
    assert loom.num_hooks == 8


def test_loom_custom_num_hooks():
    loom = JacquardLoom(num_hooks=16)
    assert loom.num_hooks == 16


def test_loom_initial_state():
    loom = JacquardLoom(num_hooks=4)
    assert loom.weft_count == 0
    assert loom.card_index == 0
    assert loom.get_pattern() == []


# ---------------------------------------------------------------------------
# load_deck
# ---------------------------------------------------------------------------


def test_load_deck_sets_card_count():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1]])
    assert len(loom.state.card_deck) == 2


def test_load_deck_resets_counters():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0]])
    loom.step()
    loom.load_deck([[0, 1, 0, 1]])
    assert loom.card_index == 0
    assert loom.weft_count == 0
    assert loom.get_pattern() == []


def test_load_deck_wrong_width_raises():
    loom = JacquardLoom(num_hooks=4)
    with pytest.raises(ValueError, match="expected 4 hooks"):
        loom.load_deck([[1, 0, 1]])


def test_load_deck_first_card_wrong_width_raises():
    loom = JacquardLoom(num_hooks=4)
    with pytest.raises(ValueError, match="Card 0"):
        loom.load_deck([[1, 0]])


def test_load_deck_second_card_wrong_width_raises():
    loom = JacquardLoom(num_hooks=4)
    with pytest.raises(ValueError, match="Card 1"):
        loom.load_deck([[1, 0, 1, 0], [0, 1]])


# ---------------------------------------------------------------------------
# step
# ---------------------------------------------------------------------------


def test_step_returns_first_card():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1]])
    row = loom.step()
    assert row == [1, 0, 1, 0]


def test_step_advances_card_index():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1]])
    loom.step()
    assert loom.card_index == 1


def test_step_increments_weft_count():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1]])
    loom.step()
    loom.step()
    assert loom.weft_count == 2


def test_step_past_end_returns_none():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0]])
    loom.step()
    result = loom.step()
    assert result is None


def test_step_past_end_does_not_increment_weft():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0]])
    loom.step()
    loom.step()  # past end
    assert loom.weft_count == 1


def test_step_empty_deck_returns_none():
    loom = JacquardLoom(num_hooks=8)
    result = loom.step()
    assert result is None


# ---------------------------------------------------------------------------
# get_pattern
# ---------------------------------------------------------------------------


def test_get_pattern_accumulates_rows():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1], [1, 1, 0, 0]])
    loom.step()
    loom.step()
    loom.step()
    assert loom.get_pattern() == [[1, 0, 1, 0], [0, 1, 0, 1], [1, 1, 0, 0]]


def test_get_pattern_returns_copy():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0]])
    loom.step()
    p = loom.get_pattern()
    p.clear()
    assert len(loom.get_pattern()) == 1


# ---------------------------------------------------------------------------
# reset
# ---------------------------------------------------------------------------


def test_reset_restores_card_index():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1]])
    loom.step()
    loom.step()
    loom.reset()
    assert loom.card_index == 0


def test_reset_clears_weft_count():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0]])
    loom.step()
    loom.reset()
    assert loom.weft_count == 0


def test_reset_clears_pattern():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0]])
    loom.step()
    loom.reset()
    assert loom.get_pattern() == []


def test_reset_allows_rewinding_deck():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1]])
    loom.step()
    loom.step()
    loom.reset()
    assert loom.step() == [1, 0, 1, 0]


# ---------------------------------------------------------------------------
# read_card (legacy one-shot API)
# ---------------------------------------------------------------------------


def test_read_card_returns_pattern():
    loom = JacquardLoom(num_hooks=4)
    assert loom.read_card([1, 0, 1, 0]) == [1, 0, 1, 0]


def test_read_card_all_ones():
    loom = JacquardLoom(num_hooks=4)
    assert loom.read_card([1, 1, 1, 1]) == [1, 1, 1, 1]


def test_read_card_all_zeros():
    loom = JacquardLoom(num_hooks=4)
    assert loom.read_card([0, 0, 0, 0]) == [0, 0, 0, 0]


def test_read_card_wrong_length_raises():
    loom = JacquardLoom(num_hooks=4)
    with pytest.raises(ValueError, match="hook count"):
        loom.read_card([1, 0])


# ---------------------------------------------------------------------------
# state_dict
# ---------------------------------------------------------------------------


def test_state_dict_keys():
    loom = JacquardLoom(num_hooks=8)
    sd = loom.state_dict()
    assert set(sd.keys()) == {"num_hooks", "weft_count", "card_index", "deck_size", "pattern"}


def test_state_dict_reflects_step():
    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1]])
    loom.step()
    sd = loom.state_dict()
    assert sd["weft_count"] == 1
    assert sd["card_index"] == 1
    assert sd["deck_size"] == 2
    assert sd["pattern"] == [[1, 0, 1, 0]]


# ---------------------------------------------------------------------------
# JacquardEmulator backward-compat alias
# ---------------------------------------------------------------------------


def test_jacquard_emulator_hooks_kwarg():
    emu = JacquardEmulator(hooks=4)
    assert emu.num_hooks == 4


def test_jacquard_emulator_num_hooks_kwarg():
    emu = JacquardEmulator(num_hooks=6)
    assert emu.num_hooks == 6


def test_jacquard_emulator_default_hooks():
    emu = JacquardEmulator()
    assert emu.num_hooks == 8


def test_jacquard_reads_card():
    emu = JacquardEmulator(hooks=4)
    assert emu.read_card([1, 0, 1, 0]) == [1, 0, 1, 0]


def test_jacquard_invalid_card_length():
    emu = JacquardEmulator(hooks=3)
    with pytest.raises(ValueError):
        emu.read_card([1, 0, 1, 0])


def test_jacquard_emulator_is_loom_subclass():
    emu = JacquardEmulator(hooks=4)
    assert isinstance(emu, JacquardLoom)


# ---------------------------------------------------------------------------
# JacquardAdapter
# ---------------------------------------------------------------------------


def test_adapter_step_updates_last_card():
    from backend.src.emulator.adapter import JacquardAdapter

    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1]])
    adapter = JacquardAdapter(loom)
    adapter.step()
    assert adapter.get_column_values() == [1, 0, 1, 0]


def test_adapter_cycle_count_matches_weft():
    from backend.src.emulator.adapter import JacquardAdapter

    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1]])
    adapter = JacquardAdapter(loom)
    adapter.step()
    adapter.step()
    assert adapter.get_cycle_count() == 2


def test_adapter_get_register_values():
    from backend.src.emulator.adapter import JacquardAdapter

    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0]])
    adapter = JacquardAdapter(loom)
    regs = adapter.get_register_values()
    assert "weft_count" in regs
    assert "num_hooks" in regs
    assert regs["deck_size"] == 1


def test_adapter_load_deck():
    from backend.src.emulator.adapter import JacquardAdapter

    loom = JacquardLoom(num_hooks=4)
    adapter = JacquardAdapter(loom)
    adapter.load_deck([[1, 1, 0, 0], [0, 0, 1, 1]])
    adapter.step()
    assert adapter.get_column_values() == [1, 1, 0, 0]


def test_adapter_get_memory_value_returns_row():
    from backend.src.emulator.adapter import JacquardAdapter

    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0]])
    adapter = JacquardAdapter(loom)
    adapter.step()
    assert adapter.get_memory_value(0) == [1, 0, 1, 0]


# ---------------------------------------------------------------------------
# Extended: JacquardLoom structural properties
# ---------------------------------------------------------------------------


class TestJacquardLoomExtended:
    """Additional JacquardLoom state and step properties."""

    def test_num_hooks_attribute(self) -> None:
        loom = JacquardLoom(num_hooks=12)
        assert loom.num_hooks == 12

    def test_step_sequence_reads_all_cards(self) -> None:
        cards = [[1, 0, 1, 0], [0, 1, 0, 1], [1, 1, 0, 0]]
        loom = JacquardLoom(num_hooks=4)
        loom.load_deck(cards)
        seen = [loom.step() for _ in range(3)]
        assert seen == cards

    def test_step_past_end_card_index_stays(self) -> None:
        loom = JacquardLoom(num_hooks=4)
        loom.load_deck([[1, 0, 1, 0]])
        loom.step()
        loom.step()  # past end
        assert loom.card_index == 1

    def test_state_dict_deck_size_updates_on_load(self) -> None:
        loom = JacquardLoom(num_hooks=4)
        loom.load_deck([[1, 0, 1, 0], [0, 1, 0, 1]])
        assert loom.state_dict()["deck_size"] == 2

    def test_get_pattern_empty_before_steps(self) -> None:
        loom = JacquardLoom(num_hooks=4)
        loom.load_deck([[1, 0, 1, 0]])
        assert loom.get_pattern() == []

    def test_read_card_does_not_advance_index(self) -> None:
        loom = JacquardLoom(num_hooks=4)
        loom.load_deck([[1, 0, 1, 0]])
        loom.read_card([1, 0, 1, 0])
        assert loom.card_index == 0

    def test_load_deck_validates_all_cards(self) -> None:
        loom = JacquardLoom(num_hooks=4)
        with pytest.raises(ValueError):
            loom.load_deck([[1, 0, 1, 0], [1, 0]])  # second card too short

    def test_weft_count_matches_successful_steps(self) -> None:
        loom = JacquardLoom(num_hooks=4)
        cards = [[i % 2, (i + 1) % 2, i % 2, (i + 1) % 2] for i in range(5)]
        loom.load_deck(cards)
        for _ in range(4):
            loom.step()
        assert loom.weft_count == 4


class TestJacquardAdapterExtended:
    """JacquardAdapter extended property tests."""

    def _make_adapter(self, hooks: int = 4, cards: list | None = None):
        from backend.src.emulator.adapter import JacquardAdapter
        loom = JacquardLoom(num_hooks=hooks)
        if cards is not None:
            loom.load_deck(cards)
        return JacquardAdapter(loom)

    def test_get_operation_time_ms_returns_dict(self) -> None:
        adapter = self._make_adapter()
        t = adapter.get_operation_time_ms()
        assert isinstance(t, dict)

    def test_get_operation_time_ms_step_key(self) -> None:
        adapter = self._make_adapter()
        t = adapter.get_operation_time_ms()
        assert "step" in t

    def test_get_register_values_has_deck_size(self) -> None:
        adapter = self._make_adapter(cards=[[1, 0, 1, 0], [0, 1, 0, 1]])
        regs = adapter.get_register_values()
        assert regs["deck_size"] == 2

    def test_step_advances_cycle_count(self) -> None:
        adapter = self._make_adapter(cards=[[1, 0, 1, 0], [0, 1, 0, 1]])
        adapter.step()
        adapter.step()
        assert adapter.get_cycle_count() == 2


def test_adapter_get_memory_out_of_range_returns_zeros():
    from backend.src.emulator.adapter import JacquardAdapter

    loom = JacquardLoom(num_hooks=4)
    adapter = JacquardAdapter(loom)
    assert adapter.get_memory_value(99) == [0, 0, 0, 0]


def test_adapter_get_snapshot_keys():
    from backend.src.emulator.adapter import JacquardAdapter

    loom = JacquardLoom(num_hooks=4)
    adapter = JacquardAdapter(loom)
    snap = adapter.get_snapshot()
    assert set(snap.keys()) == {"weft_count", "card_index", "last_card", "pattern", "num_hooks"}


def test_adapter_step_at_end_is_noop():
    from backend.src.emulator.adapter import JacquardAdapter

    loom = JacquardLoom(num_hooks=4)
    loom.load_deck([[1, 0, 1, 0]])
    adapter = JacquardAdapter(loom)
    adapter.step()
    adapter.step()  # past end -- must not raise
    assert adapter.get_cycle_count() == 1  # weft_count did not increment past deck
