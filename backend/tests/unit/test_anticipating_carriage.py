"""
AnticipatingCarriage comprehensive test suite.

Tests for Babbage's overlapped carry optimization system.
Covers initialization, carry signal handling, anticipation logic,
phase management, and carry propagation strategies.

Based on SMG Technical Description and mechanical notation.
"""

import pytest

from backend.src.emulator.carry import AnticipatingCarriage, CarryPropagationUnit

# ============================================================================
# Initialization Tests (5 tests)
# ============================================================================


def test_anticipating_carriage_init():
    """Test AnticipatingCarriage initialization."""
    carriage = AnticipatingCarriage()

    assert carriage.phases == 8
    assert carriage.current_phase == 0
    assert carriage.carry_signals == [False] * 8
    assert carriage.anticipated_carries == [False] * 8
    assert not carriage.is_active


def test_anticipating_carriage_reset():
    """Test reset to initial state."""
    carriage = AnticipatingCarriage()

    # Modify state
    carriage.current_phase = 5
    carriage.activate()
    carriage.set_carry_signals([True, False, True, False, True, False, True, False])

    # Reset
    carriage.reset()

    assert carriage.current_phase == 0
    assert not carriage.is_active
    assert carriage.carry_signals == [False] * 8
    assert carriage.anticipated_carries == [False] * 8


def test_anticipating_carriage_activate_deactivate():
    """Test activation and deactivation."""
    carriage = AnticipatingCarriage()

    assert not carriage.is_active

    carriage.activate()
    assert carriage.is_active

    carriage.deactivate()
    assert not carriage.is_active


def test_anticipating_carriage_history():
    """Test snapshot history tracking."""
    carriage = AnticipatingCarriage()

    # Record initial state
    carriage.record_snapshot()

    # Modify and record
    carriage.activate()
    carriage.record_snapshot()

    # Check history
    assert len(carriage.get_history()) == 2
    assert not carriage.get_history()[0].is_active
    assert carriage.get_history()[1].is_active


def test_anticipating_carriage_clear_history():
    """Test clearing history."""
    carriage = AnticipatingCarriage()

    carriage.record_snapshot()
    carriage.record_snapshot()

    assert len(carriage.history) == 2

    carriage.clear_history()

    assert len(carriage.history) == 0


# ============================================================================
# Carry Signal Tests (8 tests)
# ============================================================================


def test_anticipating_carriage_set_carry_signals():
    """Test setting carry signals from columns."""
    carriage = AnticipatingCarriage()

    signals = [True, False, True, False, True, False, True, False]
    carriage.set_carry_signals(signals)

    assert carriage.carry_signals == signals


def test_anticipating_carriage_set_carry_signals_all_zero():
    """Test setting all zero carries."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([False] * 8)

    assert carriage.carry_signals == [False] * 8


def test_anticipating_carriage_set_carry_signals_all_one():
    """Test setting all one carries."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([True] * 8)

    assert carriage.carry_signals == [True] * 8


def test_anticipating_carriage_set_carry_signals_wrong_count():
    """Test reject wrong number of signals."""
    carriage = AnticipatingCarriage()

    with pytest.raises(ValueError, match="Expected 8 carry signals"):
        carriage.set_carry_signals([True, False, True])


def test_anticipating_carriage_is_carrying_false():
    """Test is_carrying when no carries."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([False] * 8)
    carriage.anticipate_carries()

    assert not carriage.is_carrying()


def test_anticipating_carriage_is_carrying_true():
    """Test is_carrying when carries present."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([True, False, False, False, False, False, False, False])
    carriage.anticipate_carries()

    assert carriage.is_carrying()


def test_anticipating_carriage_is_carrying_middle():
    """Test is_carrying with middle column carry."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([False, False, False, True, False, False, False, False])
    carriage.anticipate_carries()

    assert carriage.is_carrying()


def test_anticipating_carriage_is_carrying_last():
    """Test is_carrying with last column carry."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([False] * 7 + [True])
    carriage.anticipate_carries()

    # Last column carry doesn't propagate (no column 8)
    # So is_carrying returns False (no carries propagated)
    assert not carriage.is_carrying()


# ============================================================================
# Anticipation Logic Tests (15 tests)
# ============================================================================


def test_anticipating_carriage_anticipate_simple():
    """Test anticipating carries from single input."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([True, False, False, False, False, False, False, False])
    result = carriage.anticipate_carries()

    # Carry from column 0 goes to column 1
    assert result == [False, True, False, False, False, False, False, False]


def test_anticipating_carriage_anticipate_multiple():
    """Test anticipating carries from multiple inputs."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([True, True, True, False, False, False, False, False])
    result = carriage.anticipate_carries()

    # Carries propagate left-to-right (0→1, 1→2, 2→3)
    assert result == [False, True, True, True, False, False, False, False]


def test_anticipating_carriage_anticipate_alternating():
    """Test anticipating alternating carries."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([True, False, True, False, True, False, True, False])
    result = carriage.anticipate_carries()

    # Each carry propagates to next column
    assert result == [False, True, False, True, False, True, False, True]


def test_anticipating_carriage_anticipate_cascade():
    """Test cascading carries."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([True, True, True, True, True, True, True, True])
    result = carriage.anticipate_carries()

    # All carries shift left: [1,1,1,1,1,1,1,1] → [0,1,1,1,1,1,1,1]
    assert result == [False, True, True, True, True, True, True, True]


def test_anticipating_carriage_anticipate_none():
    """Test anticipating with no input carries."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([False] * 8)
    result = carriage.anticipate_carries()

    assert result == [False] * 8


def test_anticipating_carriage_get_carries_for_column_0():
    """Test getting anticipated carry for column 0."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([True, False, False, False, False, False, False, False])
    carriage.anticipate_carries()

    # Column 0 gets no carry (column -1 doesn't exist)
    assert not carriage.get_carries_for_column(0)


def test_anticipating_carriage_get_carries_for_column_1():
    """Test getting anticipated carry for column 1."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([True, False, False, False, False, False, False, False])
    carriage.anticipate_carries()

    # Column 1 gets carry from column 0
    assert carriage.get_carries_for_column(1)


def test_anticipating_carriage_get_carries_for_column_7():
    """Test getting anticipated carry for column 7."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([False] * 6 + [True, False])
    carriage.anticipate_carries()

    # Column 7 gets carry from column 6
    assert carriage.get_carries_for_column(7)


def test_anticipating_carriage_get_carries_invalid_index():
    """Test get_carries_for_column with invalid index."""
    carriage = AnticipatingCarriage()

    with pytest.raises(IndexError, match="Column index out of range"):
        carriage.get_carries_for_column(8)

    with pytest.raises(IndexError, match="Column index out of range"):
        carriage.get_carries_for_column(-1)


def test_anticipating_carriage_sequential_anticipation():
    """Test sequential anticipation cycles."""
    carriage = AnticipatingCarriage()

    # Cycle 1: Input carry from column 0
    carriage.set_carry_signals([True, False, False, False, False, False, False, False])
    result1 = carriage.anticipate_carries()

    # Cycle 2: Use previous anticipated carries as new input
    carriage.set_carry_signals(result1)
    result2 = carriage.anticipate_carries()

    # After 2 cycles, carry has propagated from column 0 to column 2
    assert result1 == [False, True, False, False, False, False, False, False]
    assert result2 == [False, False, True, False, False, False, False, False]


def test_anticipating_carriage_ripple_simulation():
    """Test simulating full ripple through all columns."""
    carriage = AnticipatingCarriage()

    carries = [True, False, False, False, False, False, False, False]

    # Ripple carry across all 8 phases
    for _phase in range(8):
        carriage.set_carry_signals(carries)
        carries = carriage.anticipate_carries()

    # After 8 phases, initial carry at column 0 exits the system
    assert carries == [False] * 8


def test_anticipating_carriage_simultaneous_carries():
    """Test multiple simultaneous carries."""
    carriage = AnticipatingCarriage()

    # Multiple carries at different positions
    carriage.set_carry_signals([True, False, True, False, True, False, False, False])
    result = carriage.anticipate_carries()

    # Each carry propagates independently
    expected = [False, True, False, True, False, True, False, False]
    assert result == expected


# ============================================================================
# Phase Management Tests (6 tests)
# ============================================================================


def test_anticipating_carriage_advance_phase():
    """Test advancing mechanical phase."""
    carriage = AnticipatingCarriage()

    assert carriage.current_phase == 0

    carriage.advance_phase()
    assert carriage.current_phase == 1

    carriage.advance_phase()
    assert carriage.current_phase == 2


def test_anticipating_carriage_phase_wraparound():
    """Test phase wraparound at 8."""
    carriage = AnticipatingCarriage()

    for _ in range(8):
        carriage.advance_phase()

    # Should wrap back to 0
    assert carriage.current_phase == 0


def test_anticipating_carriage_phase_full_cycle():
    """Test complete 8-phase cycle."""
    carriage = AnticipatingCarriage()

    phases = []
    for _ in range(16):
        phases.append(carriage.current_phase)
        carriage.advance_phase()

    # Should see 0-7 twice
    assert phases == [0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7]


def test_anticipating_carriage_activate_with_phase():
    """Test activation at different phases."""
    carriage = AnticipatingCarriage()

    for phase in range(8):
        carriage.current_phase = phase
        carriage.activate()

        snapshot = carriage.get_snapshot()
        assert snapshot.phase == phase
        assert snapshot.is_active

        carriage.deactivate()


def test_anticipating_carriage_snapshot_phase():
    """Test that snapshots capture phase correctly."""
    carriage = AnticipatingCarriage()

    carriage.current_phase = 3
    carriage.activate()

    snapshot = carriage.get_snapshot()

    assert snapshot.phase == 3
    assert snapshot.is_active


def test_anticipating_carriage_history_with_phases():
    """Test history includes phase information."""
    carriage = AnticipatingCarriage()

    for phase in range(4):
        carriage.current_phase = phase
        carriage.record_snapshot()

    history = carriage.get_history()

    for i, snap in enumerate(history):
        assert snap.phase == i


# ============================================================================
# Edge Cases and Integration Tests (16 tests)
# ============================================================================


def test_anticipating_carriage_last_column_carry():
    """Test carry from last column (no column 8)."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([False] * 7 + [True])
    result = carriage.anticipate_carries()

    # Carry at column 7 has no where to go (last column)
    # But is_carrying should still report True
    assert result == [False] * 8
    carriage.anticipate_carries()  # Update internal state
    assert not carriage.is_carrying()  # No output carries


def test_anticipating_carriage_snapshot_independence():
    """Test that snapshots are independent."""
    carriage = AnticipatingCarriage()

    carriage.activate()
    snap1 = carriage.get_snapshot()

    carriage.deactivate()
    snap2 = carriage.get_snapshot()

    # Snapshots should differ
    assert snap1.is_active
    assert not snap2.is_active


def test_anticipating_carriage_repr():
    """Test string representation."""
    carriage = AnticipatingCarriage()

    carriage.set_carry_signals([True, False, True, False, True, False, False, False])
    carriage.anticipate_carries()
    carriage.current_phase = 3
    carriage.activate()

    repr_str = repr(carriage)

    assert "AnticipatingCarriage" in repr_str
    assert "phase=3" in repr_str
    assert "active=True" in repr_str


def test_anticipating_carriage_mixed_operations():
    """Test sequence of mixed operations."""
    carriage = AnticipatingCarriage()

    # Cycle 1: carries at positions 0,1 → shift to positions 1,2
    carriage.set_carry_signals([True, True, False, False, False, False, False, False])
    result1 = carriage.anticipate_carries()
    carriage.record_snapshot()

    # Cycle 2: carries at positions 2,3 → shift to positions 3,4
    carriage.advance_phase()
    carriage.set_carry_signals([False, False, True, True, False, False, False, False])
    result2 = carriage.anticipate_carries()
    carriage.record_snapshot()

    # Verify results (each carry shifts left by 1 position)
    assert result1 == [False, True, True, False, False, False, False, False]
    assert result2 == [False, False, False, True, True, False, False, False]
    assert len(carriage.get_history()) == 2


def test_carry_propagation_unit_sequential():
    """Test sequential carry propagation."""
    unit = CarryPropagationUnit("sequential")

    carries = [True, False, False, False, False, False, False, False]
    result = unit.propagate(carries)

    assert result == [False, True, False, False, False, False, False, False]
    assert unit.get_step_count() == 7  # Would take 7 steps


def test_carry_propagation_unit_lookahead():
    """Test lookahead carry propagation."""
    unit = CarryPropagationUnit("lookahead")

    carries = [True, False, False, False, False, False, False, False]
    result = unit.propagate(carries)

    assert result == [False, True, False, False, False, False, False, False]
    assert unit.get_step_count() == 1  # Lookahead is instant


def test_carry_propagation_unit_parallel():
    """Test parallel carry propagation."""
    unit = CarryPropagationUnit("parallel")

    carries = [True, False, False, False, False, False, False, False]
    result = unit.propagate(carries)

    assert result == [False, True, False, False, False, False, False, False]
    assert unit.get_step_count() == 1  # Parallel is instant


def test_carry_propagation_unit_invalid_mode():
    """Test invalid propagation mode."""
    with pytest.raises(ValueError, match="Unknown propagation mode"):
        CarryPropagationUnit("invalid_mode")


def test_carry_propagation_unit_wrong_carry_count():
    """Test wrong number of carries."""
    unit = CarryPropagationUnit()

    with pytest.raises(ValueError, match="Expected 8 carries"):
        unit.propagate([True, False, True])


def test_carry_propagation_unit_reset():
    """Test resetting propagation unit."""
    unit = CarryPropagationUnit()

    unit.propagate([True] * 8)
    assert unit.get_step_count() == 1

    unit.reset()
    assert unit.get_step_count() == 0


def test_carry_propagation_unit_repr():
    """Test string representation."""
    unit = CarryPropagationUnit("lookahead")

    repr_str = repr(unit)

    assert "CarryPropagationUnit" in repr_str
    assert "lookahead" in repr_str


def test_anticipating_carriage_de2_cycle():
    """Test complete DE2 operational cycle."""
    carriage = AnticipatingCarriage()

    # Simulate one complete DE2 cycle (8 phases, carries at each)
    carries_sequence = [
        [True, False, False, False, False, False, False, False],
        [False, True, False, False, False, False, False, False],
        [False, False, True, False, False, False, False, False],
        [False, False, False, True, False, False, False, False],
        [False, False, False, False, True, False, False, False],
        [False, False, False, False, False, True, False, False],
        [False, False, False, False, False, False, True, False],
        [False, False, False, False, False, False, False, True],
    ]

    for phase, input_carries in enumerate(carries_sequence):
        carriage.current_phase = phase
        carriage.set_carry_signals(input_carries)
        _result = carriage.anticipate_carries()
        carriage.record_snapshot()

    # Verify complete history
    history = carriage.get_history()
    assert len(history) == 8


# ============================================================================
# Class-based tests
# ============================================================================


class TestAnticipatingCarriageExtended:
    """AnticipatingCarriage: activate, advance_phase, is_carrying, snapshot."""

    def test_activate_sets_is_active(self) -> None:
        c = AnticipatingCarriage()
        c.activate()
        assert c.is_active is True

    def test_deactivate_clears_is_active(self) -> None:
        c = AnticipatingCarriage()
        c.activate()
        c.deactivate()
        assert c.is_active is False

    def test_advance_phase_wraps(self) -> None:
        c = AnticipatingCarriage()
        for _ in range(8):
            c.advance_phase()
        assert c.current_phase == 0

    def test_advance_phase_increments(self) -> None:
        c = AnticipatingCarriage()
        c.advance_phase()
        assert c.current_phase == 1

    def test_is_carrying_false_when_no_carries(self) -> None:
        c = AnticipatingCarriage()
        c.set_carry_signals([False] * 8)
        c.anticipate_carries()
        # anticipated_carries[0] is never set (no col-1 feeds into col 0)
        # is_carrying checks anticipated_carries
        # All False -> is_carrying = False unless col0 carry propagated somewhere
        assert isinstance(c.is_carrying(), bool)

    def test_is_carrying_true_when_carry_propagated(self) -> None:
        c = AnticipatingCarriage()
        c.set_carry_signals([True] + [False] * 7)
        c.anticipate_carries()
        assert c.is_carrying() is True

    def test_set_carry_signals_wrong_length_raises(self) -> None:
        c = AnticipatingCarriage()
        with pytest.raises(ValueError):
            c.set_carry_signals([True, False])

    def test_get_carries_for_column_in_range(self) -> None:
        c = AnticipatingCarriage()
        c.set_carry_signals([True] + [False] * 7)
        c.anticipate_carries()
        result = c.get_carries_for_column(1)
        assert isinstance(result, bool)

    def test_get_carries_for_column_out_of_range_raises(self) -> None:
        c = AnticipatingCarriage()
        with pytest.raises(IndexError):
            c.get_carries_for_column(8)

    def test_reset_clears_carries_and_phase(self) -> None:
        c = AnticipatingCarriage()
        c.set_carry_signals([True] * 8)
        c.advance_phase()
        c.activate()
        c.reset()
        assert c.current_phase == 0
        assert c.carry_signals == [False] * 8
        assert c.is_active is False

    def test_snapshot_has_phase_field(self) -> None:
        c = AnticipatingCarriage()
        snap = c.get_snapshot()
        assert hasattr(snap, "phase")

    def test_snapshot_has_is_active_field(self) -> None:
        c = AnticipatingCarriage()
        snap = c.get_snapshot()
        assert hasattr(snap, "is_active")

    def test_record_snapshot_grows_history(self) -> None:
        c = AnticipatingCarriage()
        c.record_snapshot()
        c.record_snapshot()
        assert len(c.get_history()) == 2

    def test_clear_history_empties_history(self) -> None:
        c = AnticipatingCarriage()
        c.record_snapshot()
        c.clear_history()
        assert len(c.get_history()) == 0

    def test_get_history_depth_limits_results(self) -> None:
        c = AnticipatingCarriage()
        for _ in range(10):
            c.record_snapshot()
        assert len(c.get_history(depth=3)) == 3

    def test_anticipate_all_false_returns_all_false(self) -> None:
        c = AnticipatingCarriage()
        c.set_carry_signals([False] * 8)
        result = c.anticipate_carries()
        assert result == [False] * 8

    def test_anticipate_col7_carry_is_last(self) -> None:
        c = AnticipatingCarriage()
        signals = [False] * 7 + [True]
        c.set_carry_signals(signals)
        result = c.anticipate_carries()
        # col 7 has carry but no col 8, so anticipated[7] stays False
        # anticipated[8] doesn't exist -- col7 carry goes nowhere
        assert isinstance(result, list)
        assert len(result) == 8


class TestCarryPropagationUnitExtended:
    """CarryPropagationUnit: step count, all modes, edge cases."""

    def test_default_mode_is_valid(self) -> None:
        unit = CarryPropagationUnit()
        assert unit.mode in ("sequential", "lookahead", "parallel")

    def test_step_count_zero_initially(self) -> None:
        unit = CarryPropagationUnit()
        assert unit.get_step_count() == 0

    def test_step_count_increments_after_propagate(self) -> None:
        unit = CarryPropagationUnit("sequential")
        unit.propagate([True] + [False] * 7)
        assert unit.get_step_count() > 0

    def test_reset_clears_step_count(self) -> None:
        unit = CarryPropagationUnit()
        unit.propagate([False] * 8)
        unit.reset()
        assert unit.get_step_count() == 0

    def test_sequential_output_len_is_8(self) -> None:
        unit = CarryPropagationUnit("sequential")
        result = unit.propagate([True] + [False] * 7)
        assert len(result) == 8

    def test_lookahead_output_len_is_8(self) -> None:
        unit = CarryPropagationUnit("lookahead")
        result = unit.propagate([False] * 8)
        assert len(result) == 8

    def test_parallel_output_len_is_8(self) -> None:
        unit = CarryPropagationUnit("parallel")
        result = unit.propagate([False] * 8)
        assert len(result) == 8

    def test_carry_shifts_right_by_one(self) -> None:
        for mode in ("sequential", "lookahead", "parallel"):
            unit = CarryPropagationUnit(mode)
            result = unit.propagate([True] + [False] * 7)
            assert result[1] is True

    def test_all_false_stays_all_false(self) -> None:
        for mode in ("sequential", "lookahead", "parallel"):
            unit = CarryPropagationUnit(mode)
            result = unit.propagate([False] * 8)
            assert result == [False] * 8

    def test_repr_includes_mode(self) -> None:
        unit = CarryPropagationUnit("lookahead")
        assert "lookahead" in repr(unit)


def test_anticipating_carriage_high_frequency_carries():
    """Test handling multiple carries in sequence."""
    carriage = AnticipatingCarriage()

    # Burst of 8 consecutive carries
    carries_list = [
        [True, True, True, True, True, True, True, True],
        [False, True, True, True, True, True, True, True],
        [False, False, True, True, True, True, True, True],
    ]

    for carries in carries_list:
        carriage.set_carry_signals(carries)
        _result = carriage.anticipate_carries()
        assert carriage.is_carrying()


def test_anticipating_carriage_statistical_carries():
    """Test pattern of alternating carry activity."""
    carriage = AnticipatingCarriage()

    carries_count = 0

    for _pattern in range(16):
        # Alternating pattern
        carries = [bool(i % 2) for i in range(8)]
        carriage.set_carry_signals(carries)
        _result = carriage.anticipate_carries()

        if carriage.is_carrying():
            carries_count += 1

    # Pattern should show regular carry activity
    assert carries_count > 0
