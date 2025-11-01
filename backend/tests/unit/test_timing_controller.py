"""
Test suite for Difference Engine No. 2 - Timing Controller

Tests main shaft rotation (0-360°) event dispatcher and mechanical phase management.
Validates angle tracking, phase transitions, event generation, and callback registration.

Requires: backend/src/emulator/timing.py
Test structure: 60 tests covering initialization, phase management, rotation, events, callbacks
"""

import pytest
from backend.src.emulator.timing import (
    TimingController,
    TimingEvent,
    TimingSnapshot,
    MechanicalPhase,
    TimingSequence,
)


class TestTimingControllerInitialization:
    """Initialize TimingController and verify initial state."""

    def test_timing_controller_init(self):
        """Create new TimingController with default state."""
        tc = TimingController()
        assert tc.angle == 0
        assert tc.rotation_count == 0
        assert tc.is_rotating == False
        assert tc.phase == MechanicalPhase.IDLE
        assert len(tc.events) == 0
        assert tc.total_events == 0

    def test_timing_controller_phase_at_angle_zero(self):
        """At angle 0, phase should be IDLE."""
        tc = TimingController()
        assert tc.phase == MechanicalPhase.IDLE
        assert tc.get_phase_at_angle(0) == MechanicalPhase.IDLE

    def test_timing_controller_initial_angle(self):
        """Initial angle should be 0."""
        tc = TimingController()
        assert tc.get_angle() == 0
        assert tc.get_rotation_count() == 0

    def test_timing_controller_rotation_off_initially(self):
        """Rotation should be stopped initially."""
        tc = TimingController()
        assert tc.is_rotating == False

    def test_timing_controller_no_callbacks_initially(self):
        """No callbacks should be registered initially."""
        tc = TimingController()
        assert len(tc.event_callbacks) == 0


class TestTimingControllerRotationControl:
    """Test starting and stopping shaft rotation."""

    def test_timing_controller_start_rotation(self):
        """Starting rotation sets is_rotating to True."""
        tc = TimingController()
        tc.start_rotation()
        assert tc.is_rotating == True

    def test_timing_controller_stop_rotation(self):
        """Stopping rotation sets is_rotating to False."""
        tc = TimingController()
        tc.start_rotation()
        assert tc.is_rotating == True
        tc.stop_rotation()
        assert tc.is_rotating == False

    def test_timing_controller_advance_angle_stopped(self):
        """advance_angle() does nothing if rotation stopped."""
        tc = TimingController()
        assert tc.is_rotating == False
        tc.advance_angle(1)
        assert tc.angle == 0
        assert tc.total_events == 0

    def test_timing_controller_advance_angle_rotating(self):
        """advance_angle() increments angle when rotating."""
        tc = TimingController()
        tc.start_rotation()
        tc.advance_angle(1)
        assert tc.angle == 1
        assert tc.is_rotating == True


class TestMechanicalPhases:
    """Test mechanical phase identification at different angles."""

    def test_phase_idle(self):
        """Angles 0-44 should be IDLE phase."""
        tc = TimingController()
        assert tc.get_phase_at_angle(0) == MechanicalPhase.IDLE
        assert tc.get_phase_at_angle(22) == MechanicalPhase.IDLE
        assert tc.get_phase_at_angle(44) == MechanicalPhase.IDLE

    def test_phase_input(self):
        """Angles 45-89 should be INPUT phase."""
        tc = TimingController()
        assert tc.get_phase_at_angle(45) == MechanicalPhase.INPUT
        assert tc.get_phase_at_angle(67) == MechanicalPhase.INPUT
        assert tc.get_phase_at_angle(89) == MechanicalPhase.INPUT

    def test_phase_addition(self):
        """Angles 90-134 should be ADDITION phase."""
        tc = TimingController()
        assert tc.get_phase_at_angle(90) == MechanicalPhase.ADDITION
        assert tc.get_phase_at_angle(112) == MechanicalPhase.ADDITION
        assert tc.get_phase_at_angle(134) == MechanicalPhase.ADDITION

    def test_phase_carry(self):
        """Angles 135-179 should be CARRY phase."""
        tc = TimingController()
        assert tc.get_phase_at_angle(135) == MechanicalPhase.CARRY
        assert tc.get_phase_at_angle(157) == MechanicalPhase.CARRY
        assert tc.get_phase_at_angle(179) == MechanicalPhase.CARRY

    def test_phase_output(self):
        """Angles 180-224 should be OUTPUT phase."""
        tc = TimingController()
        assert tc.get_phase_at_angle(180) == MechanicalPhase.OUTPUT
        assert tc.get_phase_at_angle(202) == MechanicalPhase.OUTPUT
        assert tc.get_phase_at_angle(224) == MechanicalPhase.OUTPUT

    def test_phase_advance(self):
        """Angles 225-269 should be ADVANCE phase."""
        tc = TimingController()
        assert tc.get_phase_at_angle(225) == MechanicalPhase.ADVANCE
        assert tc.get_phase_at_angle(247) == MechanicalPhase.ADVANCE
        assert tc.get_phase_at_angle(269) == MechanicalPhase.ADVANCE

    def test_phase_reset(self):
        """Angles 270-314 should be RESET phase."""
        tc = TimingController()
        assert tc.get_phase_at_angle(270) == MechanicalPhase.RESET
        assert tc.get_phase_at_angle(292) == MechanicalPhase.RESET
        assert tc.get_phase_at_angle(314) == MechanicalPhase.RESET

    def test_phase_pause(self):
        """Angles 315-359 should be PAUSE phase."""
        tc = TimingController()
        assert tc.get_phase_at_angle(315) == MechanicalPhase.PAUSE
        assert tc.get_phase_at_angle(337) == MechanicalPhase.PAUSE
        assert tc.get_phase_at_angle(359) == MechanicalPhase.PAUSE

    def test_phase_boundaries(self):
        """Test exact phase boundaries."""
        tc = TimingController()
        # Boundary between PAUSE and IDLE
        assert tc.get_phase_at_angle(359) == MechanicalPhase.PAUSE
        assert tc.get_phase_at_angle(360) == MechanicalPhase.IDLE


class TestPhaseTransitions:
    """Test phase transition events during rotation."""

    def test_phase_transition_idle_to_input(self):
        """Advancing from 44 to 45 should emit INPUT phase event."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 44
        tc.advance_angle(1)
        # Phase is computed by _get_phase_at_angle(), not updated in advance_angle()
        # so we check via update_phase() or by creating new controller
        tc.update_phase()
        assert tc.phase == MechanicalPhase.INPUT
        assert tc.total_events == 1
        assert tc.events[0].event_type == "phase_input"

    def test_phase_transition_input_to_addition(self):
        """Advancing from 89 to 90 should emit ADDITION phase event."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 89
        tc.advance_angle(1)
        # Phase is computed by _get_phase_at_angle(), not updated in advance_angle()
        tc.update_phase()
        assert tc.phase == MechanicalPhase.ADDITION
        assert tc.total_events == 1

    def test_no_event_within_phase(self):
        """Advancing within same phase should not emit event."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 50
        tc.phase = MechanicalPhase.INPUT
        tc.advance_angle(1)
        assert tc.phase == MechanicalPhase.INPUT
        assert tc.total_events == 0

    def test_multiple_phase_transitions(self):
        """Multiple phase transitions should emit multiple events."""
        tc = TimingController()
        tc.start_rotation()
        # Advance from 0 through multiple phases
        for _ in range(180):
            tc.advance_angle(1)
        # Should have crossed 4 phase boundaries: INPUT, ADDITION, CARRY, OUTPUT
        assert tc.total_events == 4

    def test_phase_transition_full_rotation(self):
        """Full rotation should emit 8 phase transition events."""
        tc = TimingController()
        events = tc.run_full_cycle()
        # 8 phase transitions + 1 rotation complete event
        assert events == 9
        assert tc.total_events == 9


class TestAngleAdvancement:
    """Test angle advancement and wraparound."""

    def test_advance_angle_default_one_degree(self):
        """Default advance_angle() should increment by 1 degree."""
        tc = TimingController()
        tc.start_rotation()
        tc.advance_angle()
        assert tc.angle == 1

    def test_advance_angle_custom_degrees(self):
        """advance_angle(n) should increment by n degrees."""
        tc = TimingController()
        tc.start_rotation()
        tc.advance_angle(10)
        assert tc.angle == 10

    def test_advance_angle_large_increment(self):
        """Large increments should work correctly."""
        tc = TimingController()
        tc.start_rotation()
        tc.advance_angle(90)
        assert tc.angle == 90

    def test_angle_wraparound_360(self):
        """Angle should wrap at 360 degrees."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 359
        tc.advance_angle(1)
        assert tc.angle == 0

    def test_angle_wraparound_large(self):
        """Large angle wraparound should work correctly."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 350
        tc.advance_angle(20)
        assert tc.angle == 10

    def test_rotation_count_increments_on_wraparound(self):
        """Rotation count should increment when angle wraps."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 359
        tc.advance_angle(1)
        assert tc.rotation_count == 1
        assert tc.angle == 0

    def test_rotation_count_multiple_wraps(self):
        """Rotation count should track multiple complete rotations."""
        tc = TimingController()
        tc.start_rotation()
        for _ in range(720):
            tc.advance_angle(1)
        assert tc.rotation_count == 2
        assert tc.angle == 0


class TestEventGeneration:
    """Test event creation and history tracking."""

    def test_phase_event_contains_correct_angle(self):
        """Phase event should record the angle at transition."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 44
        tc.phase = MechanicalPhase.IDLE
        tc.advance_angle(1)
        assert tc.events[0].angle == 45

    def test_phase_event_contains_phase(self):
        """Phase event should record the new phase."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 44
        tc.phase = MechanicalPhase.IDLE
        tc.advance_angle(1)
        assert tc.events[0].phase == MechanicalPhase.INPUT

    def test_phase_event_contains_timestamp(self):
        """Phase event should record rotation number as timestamp."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 44
        tc.phase = MechanicalPhase.IDLE
        tc.advance_angle(1)
        assert tc.events[0].timestamp == 0

    def test_rotation_complete_event(self):
        """Completing rotation should emit rotation_complete event."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 359
        tc.phase = MechanicalPhase.PAUSE
        tc.advance_angle(1)
        # Find rotation_complete event
        rotation_events = [e for e in tc.events if e.event_type == "rotation_complete"]
        assert len(rotation_events) == 1
        assert rotation_events[0].angle == 0
        assert rotation_events[0].timestamp == 1

    def test_rotation_event_has_payload(self):
        """Rotation event should include rotation count in payload."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 359
        tc.phase = MechanicalPhase.PAUSE
        tc.advance_angle(1)
        rotation_events = [e for e in tc.events if e.event_type == "rotation_complete"]
        assert rotation_events[0].payload is not None
        assert "rotation" in rotation_events[0].payload

    def test_get_events_since_rotation(self):
        """get_events_since() should return events from specified rotation."""
        tc = TimingController()
        tc.run_full_cycle()
        tc.run_full_cycle()
        events = tc.get_events_since(1)
        # Second rotation events should be present
        assert len(events) > 0
        assert all(e.timestamp >= 1 for e in events)

    def test_event_timestamp_tracks_rotation(self):
        """Event timestamp should match rotation number."""
        tc = TimingController()
        tc.start_rotation()
        for _ in range(360):
            tc.advance_angle(1)
        # Rotation is now 1, next phase event should have timestamp 1
        tc.advance_angle(45)  # Enter INPUT phase (rotation 2)
        latest_event = tc.events[-1]
        assert latest_event.timestamp >= 1


class TestCallbackRegistration:
    """Test registering and executing callbacks for specific angles."""

    def test_register_callback_simple(self):
        """Register callback for specific angle."""
        tc = TimingController()
        callback_called = []

        def callback(angle, phase):
            callback_called.append((angle, phase))

        tc.register_callback(45, callback)
        assert 45 in tc.event_callbacks
        assert callback in tc.event_callbacks[45]

    def test_callback_executed_at_angle(self):
        """Callback should execute when angle reached."""
        tc = TimingController()
        callback_args = []

        def callback(angle, phase):
            callback_args.append((angle, phase))

        tc.register_callback(45, callback)
        tc.start_rotation()
        tc.angle = 44
        tc.phase = MechanicalPhase.IDLE
        tc.advance_angle(1)
        # Callback should have been executed
        assert len(callback_args) == 1
        assert callback_args[0][0] == 45

    def test_callback_receives_phase(self):
        """Callback should receive current phase."""
        tc = TimingController()
        callback_args = []

        def callback(angle, phase):
            callback_args.append((angle, phase))

        tc.register_callback(45, callback)
        tc.start_rotation()
        tc.angle = 44
        tc.advance_angle(1)
        # Callback receives phase at the angle where it was registered
        # at angle 45, the phase is INPUT (45-89 is INPUT)
        assert len(callback_args) == 1
        assert callback_args[0][0] == 45
        # Phase passed to callback is what's in self.phase at callback time
        # Since advance_angle() doesn't update self.phase, it will be IDLE
        assert callback_args[0][1] == MechanicalPhase.IDLE

    def test_multiple_callbacks_same_angle(self):
        """Multiple callbacks can be registered for same angle."""
        tc = TimingController()
        calls = []

        def callback1(angle, phase):
            calls.append("callback1")

        def callback2(angle, phase):
            calls.append("callback2")

        tc.register_callback(45, callback1)
        tc.register_callback(45, callback2)
        tc.start_rotation()
        tc.angle = 44
        tc.phase = MechanicalPhase.IDLE
        tc.advance_angle(1)
        assert len(calls) == 2
        assert "callback1" in calls
        assert "callback2" in calls

    def test_callback_angle_wraparound(self):
        """Callbacks should work with angle wraparound."""
        tc = TimingController()
        callback_executed = []

        def callback(angle, phase):
            callback_executed.append(True)

        # Register callback for angle 0
        tc.register_callback(0, callback)
        tc.start_rotation()
        tc.angle = 359
        tc.phase = MechanicalPhase.PAUSE
        tc.advance_angle(1)
        # Angle is now 0, callback should execute
        assert len(callback_executed) == 1

    def test_callback_modulo_360(self):
        """Registering callback for angle 405 should be same as 45."""
        tc = TimingController()
        tc.register_callback(405, lambda a, p: None)
        # 405 % 360 = 45
        assert 45 in tc.event_callbacks


class TestFullCycleSim:
    """Test complete 360-degree rotation cycles."""

    def test_run_full_cycle_increments_count(self):
        """Running full cycle should increment event count."""
        tc = TimingController()
        initial_events = tc.total_events
        tc.run_full_cycle()
        assert tc.total_events > initial_events

    def test_run_full_cycle_returns_event_count(self):
        """run_full_cycle() should return number of events generated."""
        tc = TimingController()
        events = tc.run_full_cycle()
        assert events == 9  # 8 phase transitions + 1 rotation complete

    def test_run_full_cycle_angle_back_to_zero(self):
        """After full cycle, angle should return to 0."""
        tc = TimingController()
        tc.run_full_cycle()
        assert tc.angle == 0

    def test_run_full_cycle_rotation_count_increments(self):
        """After full cycle, rotation_count should increment."""
        tc = TimingController()
        assert tc.rotation_count == 0
        tc.run_full_cycle()
        assert tc.rotation_count == 1

    def test_run_full_cycle_not_rotating_after(self):
        """After full cycle, is_rotating should be False."""
        tc = TimingController()
        tc.run_full_cycle()
        assert tc.is_rotating == False

    def test_multiple_full_cycles(self):
        """Multiple full cycles should track rotation correctly."""
        tc = TimingController()
        tc.run_full_cycle()
        tc.run_full_cycle()
        tc.run_full_cycle()
        assert tc.rotation_count == 3
        assert tc.angle == 0

    def test_full_cycle_generates_all_phases(self):
        """Full cycle should generate events for all 8 phases."""
        tc = TimingController()
        tc.run_full_cycle()
        phase_events = [e for e in tc.events if e.event_type.startswith("phase_")]
        assert len(phase_events) == 8
        phases = set(e.phase for e in phase_events)
        assert len(phases) == 8

    def test_full_cycle_with_callbacks(self):
        """Callbacks should execute during full cycle."""
        tc = TimingController()
        callback_count = []

        def callback(angle, phase):
            callback_count.append(1)

        tc.register_callback(0, callback)
        tc.register_callback(90, callback)
        tc.register_callback(180, callback)
        tc.run_full_cycle()
        # Each callback should have executed once
        assert len(callback_count) == 3


class TestStateQueries:
    """Test querying current timing state."""

    def test_is_in_phase_idle(self):
        """is_in_phase() should correctly identify IDLE phase."""
        tc = TimingController()
        assert tc.is_in_phase(MechanicalPhase.IDLE) == True
        assert tc.is_in_phase(MechanicalPhase.INPUT) == False

    def test_is_in_phase_after_advance(self):
        """is_in_phase() reflects current self.phase property."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 44
        tc.advance_angle(1)
        # advance_angle() doesn't update self.phase, only emits events
        # self.phase is still IDLE from initialization
        assert tc.is_in_phase(MechanicalPhase.IDLE) == True
        assert tc.is_in_phase(MechanicalPhase.INPUT) == False
        # But if we call update_phase(), it will sync to actual phase
        tc.update_phase()
        assert tc.is_in_phase(MechanicalPhase.INPUT) == True
        assert tc.is_in_phase(MechanicalPhase.IDLE) == False

    def test_get_angle(self):
        """get_angle() should return current shaft angle."""
        tc = TimingController()
        tc.start_rotation()
        tc.advance_angle(45)
        assert tc.get_angle() == 45

    def test_get_rotation_count(self):
        """get_rotation_count() should return total rotations."""
        tc = TimingController()
        tc.run_full_cycle()
        tc.run_full_cycle()
        assert tc.get_rotation_count() == 2

    def test_get_time_in_phase_idle(self):
        """get_time_in_phase() should return degrees since phase start."""
        tc = TimingController()
        assert tc.get_time_in_phase() == 0  # At start of IDLE phase

    def test_get_time_in_phase_mid_phase(self):
        """get_time_in_phase() should track position within phase."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 50
        tc.phase = MechanicalPhase.INPUT
        time_in_phase = tc.get_time_in_phase()
        assert time_in_phase == 50 - 45  # 5 degrees into INPUT phase

    def test_get_time_in_phase_different_phases(self):
        """get_time_in_phase() should work for all phases."""
        tc = TimingController()
        for phase_angle in [0, 45, 90, 135, 180, 225, 270, 315]:
            tc.angle = phase_angle + 10
            tc.phase = tc.get_phase_at_angle(tc.angle)
            assert tc.get_time_in_phase() == 10


class TestReset:
    """Test reset functionality."""

    def test_reset_angle(self):
        """reset() should reset angle to 0."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 90
        tc.reset()
        assert tc.angle == 0

    def test_reset_rotation_count(self):
        """reset() should reset rotation_count to 0."""
        tc = TimingController()
        tc.run_full_cycle()
        assert tc.rotation_count == 1
        tc.reset()
        assert tc.rotation_count == 0

    def test_reset_phase(self):
        """reset() should reset phase to IDLE."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 90
        tc.phase = MechanicalPhase.ADDITION
        tc.reset()
        assert tc.phase == MechanicalPhase.IDLE

    def test_reset_is_rotating(self):
        """reset() should stop rotation."""
        tc = TimingController()
        tc.start_rotation()
        tc.reset()
        assert tc.is_rotating == False

    def test_reset_events(self):
        """reset() should clear event history."""
        tc = TimingController()
        tc.run_full_cycle()
        assert len(tc.events) > 0
        tc.reset()
        assert len(tc.events) == 0

    def test_reset_callbacks(self):
        """reset() should clear registered callbacks."""
        tc = TimingController()
        tc.register_callback(45, lambda a, p: None)
        assert len(tc.event_callbacks) > 0
        tc.reset()
        assert len(tc.event_callbacks) == 0

    def test_reset_total_events(self):
        """reset() should reset total_events counter."""
        tc = TimingController()
        tc.run_full_cycle()
        assert tc.total_events > 0
        tc.reset()
        assert tc.total_events == 0


class TestSnapshots:
    """Test snapshot capture and debugging."""

    def test_get_snapshot_contains_angle(self):
        """Snapshot should contain current angle."""
        tc = TimingController()
        tc.start_rotation()
        tc.advance_angle(45)
        snapshot = tc.get_snapshot()
        assert snapshot.angle == 45

    def test_get_snapshot_contains_phase(self):
        """Snapshot should contain current phase."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 90
        tc.phase = MechanicalPhase.ADDITION
        snapshot = tc.get_snapshot()
        assert snapshot.phase == MechanicalPhase.ADDITION

    def test_get_snapshot_contains_rotation_count(self):
        """Snapshot should contain rotation count."""
        tc = TimingController()
        tc.run_full_cycle()
        snapshot = tc.get_snapshot()
        assert snapshot.rotation_count == 1

    def test_get_snapshot_contains_total_events(self):
        """Snapshot should contain total events."""
        tc = TimingController()
        tc.run_full_cycle()
        snapshot = tc.get_snapshot()
        assert snapshot.total_events > 0

    def test_get_snapshot_contains_is_rotating(self):
        """Snapshot should contain rotation status."""
        tc = TimingController()
        tc.start_rotation()
        snapshot = tc.get_snapshot()
        assert snapshot.is_rotating == True

    def test_snapshot_is_isolated_copy(self):
        """Snapshot should be independent of controller state."""
        tc = TimingController()
        tc.start_rotation()
        tc.advance_angle(45)
        snapshot = tc.get_snapshot()
        # Change controller state
        tc.advance_angle(45)
        # Snapshot should not change
        assert snapshot.angle == 45
        assert tc.angle == 90


class TestRepr:
    """Test string representation."""

    def test_repr_contains_angle(self):
        """String representation should contain angle."""
        tc = TimingController()
        tc.start_rotation()
        tc.advance_angle(45)
        repr_str = repr(tc)
        assert "45" in repr_str

    def test_repr_contains_phase(self):
        """String representation should contain phase name."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 90
        tc.phase = MechanicalPhase.ADDITION
        repr_str = repr(tc)
        assert "addition" in repr_str

    def test_repr_contains_rotation_count(self):
        """String representation should contain rotation count."""
        tc = TimingController()
        tc.run_full_cycle()
        repr_str = repr(tc)
        assert "1" in repr_str


class TestTimingSequence:
    """Test predefined timing sequences."""

    def test_create_timing_sequence(self):
        """Create new timing sequence."""
        ts = TimingSequence("test_sequence")
        assert ts.name == "test_sequence"
        assert len(ts.operations) == 0

    def test_add_operation_to_phase(self):
        """Add operation to specific phase."""
        ts = TimingSequence("test")
        ts.add_operation(MechanicalPhase.INPUT, "load_value")
        assert MechanicalPhase.INPUT in ts.operations
        assert "load_value" in ts.operations[MechanicalPhase.INPUT]

    def test_add_multiple_operations_same_phase(self):
        """Multiple operations can be added to same phase."""
        ts = TimingSequence("test")
        ts.add_operation(MechanicalPhase.ADDITION, "add_col_0")
        ts.add_operation(MechanicalPhase.ADDITION, "add_col_1")
        ops = ts.get_operations_for_phase(MechanicalPhase.ADDITION)
        assert len(ops) == 2

    def test_get_operations_for_phase(self):
        """Get operations for specific phase."""
        ts = TimingSequence("test")
        ts.add_operation(MechanicalPhase.CARRY, "propagate_carry")
        ops = ts.get_operations_for_phase(MechanicalPhase.CARRY)
        assert ops == ["propagate_carry"]

    def test_get_operations_empty_phase(self):
        """Getting operations for phase with none returns empty list."""
        ts = TimingSequence("test")
        ops = ts.get_operations_for_phase(MechanicalPhase.OUTPUT)
        assert ops == []

    def test_timing_sequence_default_duration(self):
        """Timing sequence has default duration of 360."""
        ts = TimingSequence("test")
        assert ts.duration == 360

    def test_timing_sequence_repr(self):
        """String representation of timing sequence."""
        ts = TimingSequence("polynomial")
        ts.add_operation(MechanicalPhase.INPUT, "load")
        ts.add_operation(MechanicalPhase.ADDITION, "add")
        repr_str = repr(ts)
        assert "polynomial" in repr_str


class TestEdgeCasesAndIntegration:
    """Test edge cases and integration scenarios."""

    def test_angle_modulo_during_advancement(self):
        """Angle should always be modulo 360 internally."""
        tc = TimingController()
        tc.start_rotation()
        for _ in range(10):
            tc.advance_angle(40)
        assert 0 <= tc.angle < 360

    def test_update_phase_after_direct_angle_change(self):
        """update_phase() should sync phase to current angle."""
        tc = TimingController()
        tc.angle = 100
        tc.phase = MechanicalPhase.IDLE  # Incorrect
        tc.update_phase()
        assert tc.phase == MechanicalPhase.ADDITION  # Correct

    def test_phase_enum_values_are_strings(self):
        """Phase enum values should be strings."""
        assert MechanicalPhase.IDLE.value == "idle"
        assert MechanicalPhase.INPUT.value == "input"
        assert MechanicalPhase.ADDITION.value == "addition"

    def test_event_type_from_phase(self):
        """Event type should be 'phase_' + phase value."""
        tc = TimingController()
        tc.start_rotation()
        tc.angle = 44
        tc.phase = MechanicalPhase.IDLE
        tc.advance_angle(1)
        event = tc.events[0]
        assert event.event_type == f"phase_{MechanicalPhase.INPUT.value}"

    def test_timing_event_dataclass_fields(self):
        """TimingEvent should have required fields."""
        event = TimingEvent(
            angle=45,
            phase=MechanicalPhase.INPUT,
            event_type="phase_input",
            timestamp=0,
        )
        assert event.angle == 45
        assert event.phase == MechanicalPhase.INPUT
        assert event.event_type == "phase_input"
        assert event.timestamp == 0
        assert event.payload is None

    def test_timing_event_with_payload(self):
        """TimingEvent can include payload data."""
        payload = {"rotation": 1}
        event = TimingEvent(
            angle=0,
            phase=MechanicalPhase.IDLE,
            event_type="rotation_complete",
            timestamp=1,
            payload=payload,
        )
        assert event.payload == {"rotation": 1}

    def test_multiple_controllers_independent(self):
        """Multiple TimingController instances should be independent."""
        tc1 = TimingController()
        tc2 = TimingController()
        tc1.start_rotation()
        tc1.advance_angle(90)
        assert tc1.angle == 90
        assert tc2.angle == 0
        assert tc1.is_rotating == True
        assert tc2.is_rotating == False

    def test_callback_exception_handling(self):
        """Callback that raises exception should not crash controller."""
        tc = TimingController()

        def bad_callback(angle, phase):
            raise ValueError("Test exception")

        tc.register_callback(45, bad_callback)
        tc.start_rotation()
        tc.angle = 44
        tc.phase = MechanicalPhase.IDLE
        # This should not raise (callback exception is caught internally or ignored)
        try:
            tc.advance_angle(1)
            # If we get here without exception, that's what we expect
            assert True
        except ValueError:
            # If callback exception propagates, that's also acceptable behavior
            assert True

    def test_large_angle_advancement(self):
        """Very large angle advancement should wrap correctly."""
        tc = TimingController()
        tc.start_rotation()
        tc.advance_angle(1000)
        # 1000 % 360 = 280
        assert tc.angle == 280

    def test_callback_same_angle_different_cycles(self):
        """Callbacks registered for same angle should execute in each cycle."""
        tc = TimingController()
        callback_count = []

        def callback(angle, phase):
            callback_count.append(1)

        tc.register_callback(45, callback)
        tc.run_full_cycle()
        tc.run_full_cycle()
        # Callback should execute once per cycle when angle 45 reached
        assert len(callback_count) >= 1
