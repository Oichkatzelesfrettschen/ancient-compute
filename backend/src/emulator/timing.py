"""
Difference Engine No. 2 - Timing Controller

Main shaft rotation (0-360 degrees) event dispatcher for DE2 mechanical phases.

The DE2's main shaft rotates once per operational cycle (360°). Different
mechanical operations occur at specific shaft angles, creating a precise
sequence of events: input, addition, carry, output, advance.

Architecture:
  - Main shaft angle tracking (0-360°, 1° resolution)
  - Phase markers at 45° intervals (8 phases × 45° = 360°)
  - Event generation based on angle thresholds
  - Integration with AnticipatingCarriage and column operations

References:
  - SMG Technical Description: Main Shaft and Timing
  - Babbage's mechanical notation for phase diagrams
  - Working DE2 hardware mechanical documentation
"""

from typing import List, Dict, Optional, Callable
from enum import Enum
from dataclasses import dataclass


class MechanicalPhase(Enum):
    """Mechanical phases during DE2 rotation (0-360°)."""

    IDLE = "idle"                          # 0°: No operation
    INPUT = "input"                        # 45°: Load difference values
    ADDITION = "addition"                  # 90°: Add columns
    CARRY = "carry"                        # 135°: Propagate carries
    OUTPUT = "output"                      # 180°: Prepare output
    ADVANCE = "advance"                    # 225°: Advance to next row
    RESET = "reset"                        # 270°: Reset mechanical state
    PAUSE = "pause"                        # 315°: Pause before cycle repeat


@dataclass
class TimingEvent:
    """Event generated at specific shaft angle."""

    angle: int                              # 0-360 degrees
    phase: MechanicalPhase                  # Current phase
    event_type: str                         # Event name
    timestamp: int                          # Cycle counter
    payload: Optional[Dict] = None          # Additional data


@dataclass
class TimingSnapshot:
    """Snapshot of TimingController state for debugging."""

    angle: int                              # Current angle
    phase: MechanicalPhase                  # Current phase
    rotation_count: int                     # Total rotations
    total_events: int                       # Events generated
    is_rotating: bool                       # Currently rotating


class TimingController:
    """
    Timing Controller for DE2 main shaft rotation.

    Manages 360° rotation cycle with 8 mechanical phases. Each phase
    corresponds to a specific operation in the difference table evaluation.

    Rotation Model:
      0-45°:    IDLE (no operation)
      45-90°:   INPUT (load difference values)
      90-135°:  ADDITION (add columns with carries)
      135-180°: CARRY (propagate anticipated carries)
      180-225°: OUTPUT (prepare output, lock result)
      225-270°: ADVANCE (shift to next row)
      270-315°: RESET (reset mechanical state)
      315-360°: PAUSE (brief pause before next cycle)

    Event Generation:
      - Events emitted when entering each phase (45° increments)
      - Custom event callbacks registered for specific angles
      - Integration point for column operations and carry handling
    """

    def __init__(self):
        """Initialize TimingController."""
        self.angle = 0                      # Current shaft angle (0-360)
        self.rotation_count = 0             # Total rotations completed
        self.is_rotating = False            # Currently rotating
        self.phase = self._get_phase_at_angle(0)
        self.events: List[TimingEvent] = []
        self.event_callbacks: Dict[int, List[Callable]] = {}  # angle → handlers
        self.total_events = 0

    def _get_phase_at_angle(self, angle: int) -> MechanicalPhase:
        """
        Get mechanical phase at given angle.

        Parameters:
            angle: 0-360 degrees

        Returns:
            MechanicalPhase corresponding to angle
        """
        angle = angle % 360
        if angle < 45:
            return MechanicalPhase.IDLE
        elif angle < 90:
            return MechanicalPhase.INPUT
        elif angle < 135:
            return MechanicalPhase.ADDITION
        elif angle < 180:
            return MechanicalPhase.CARRY
        elif angle < 225:
            return MechanicalPhase.OUTPUT
        elif angle < 270:
            return MechanicalPhase.ADVANCE
        elif angle < 315:
            return MechanicalPhase.RESET
        else:
            return MechanicalPhase.PAUSE

    def start_rotation(self) -> None:
        """Begin main shaft rotation."""
        self.is_rotating = True

    def stop_rotation(self) -> None:
        """Stop main shaft rotation."""
        self.is_rotating = False

    def advance_angle(self, degrees: int = 1) -> None:
        """
        Advance shaft angle by given degrees.

        Parameters:
            degrees: Degrees to advance (default 1°)
        """
        if not self.is_rotating:
            return

        old_angle = self.angle
        self.angle = (self.angle + degrees) % 360

        # Check for phase transitions
        old_phase = self._get_phase_at_angle(old_angle)
        new_phase = self._get_phase_at_angle(self.angle)

        if new_phase != old_phase:
            self._emit_phase_event(new_phase)

        # Check for complete rotation
        if self.angle < old_angle:
            self.rotation_count += 1
            self._emit_rotation_event()

        # Execute registered callbacks
        self._execute_callbacks(self.angle)

    def run_full_cycle(self) -> int:
        """
        Run one complete 360° rotation.

        Returns:
            Number of events generated
        """
        initial_event_count = self.total_events

        self.start_rotation()
        for _ in range(360):
            self.advance_angle(1)
        self.stop_rotation()

        return self.total_events - initial_event_count

    def _emit_phase_event(self, phase: MechanicalPhase) -> None:
        """Emit event for phase transition."""
        event = TimingEvent(
            angle=self.angle,
            phase=phase,
            event_type=f"phase_{phase.value}",
            timestamp=self.rotation_count,
        )
        self.events.append(event)
        self.total_events += 1

    def _emit_rotation_event(self) -> None:
        """Emit event for complete rotation."""
        event = TimingEvent(
            angle=0,
            phase=MechanicalPhase.IDLE,
            event_type="rotation_complete",
            timestamp=self.rotation_count,
            payload={"rotation": self.rotation_count},
        )
        self.events.append(event)
        self.total_events += 1

    def _execute_callbacks(self, angle: int) -> None:
        """Execute registered callbacks for current angle."""
        if angle in self.event_callbacks:
            for callback in self.event_callbacks[angle]:
                callback(angle, self.phase)

    def register_callback(
        self, angle: int, callback: Callable
    ) -> None:
        """
        Register callback for specific angle.

        Parameters:
            angle: 0-359 degrees
            callback: Function(angle, phase) to execute
        """
        angle = angle % 360
        if angle not in self.event_callbacks:
            self.event_callbacks[angle] = []
        self.event_callbacks[angle].append(callback)

    def get_events_since(self, rotation: int) -> List[TimingEvent]:
        """
        Get all events since given rotation number.

        Parameters:
            rotation: Starting rotation number

        Returns:
            List of events from that rotation onward
        """
        return [e for e in self.events if e.timestamp >= rotation]

    def get_phase_at_angle(self, angle: int) -> MechanicalPhase:
        """Get phase at specific angle (for queries)."""
        return self._get_phase_at_angle(angle)

    def get_angle(self) -> int:
        """Get current shaft angle."""
        return self.angle

    def get_rotation_count(self) -> int:
        """Get total rotations completed."""
        return self.rotation_count

    def is_in_phase(self, phase: MechanicalPhase) -> bool:
        """Check if currently in given phase."""
        return self.phase == phase

    def get_time_in_phase(self) -> int:
        """Get time (degrees) since phase started."""
        phase_start = {
            MechanicalPhase.IDLE: 0,
            MechanicalPhase.INPUT: 45,
            MechanicalPhase.ADDITION: 90,
            MechanicalPhase.CARRY: 135,
            MechanicalPhase.OUTPUT: 180,
            MechanicalPhase.ADVANCE: 225,
            MechanicalPhase.RESET: 270,
            MechanicalPhase.PAUSE: 315,
        }
        return self.angle - phase_start.get(self.phase, 0)

    def reset(self) -> None:
        """Reset to initial state."""
        self.angle = 0
        self.rotation_count = 0
        self.is_rotating = False
        self.phase = MechanicalPhase.IDLE
        self.events.clear()
        self.event_callbacks.clear()
        self.total_events = 0

    def get_snapshot(self) -> TimingSnapshot:
        """Capture complete state for debugging."""
        return TimingSnapshot(
            angle=self.angle,
            phase=self.phase,
            rotation_count=self.rotation_count,
            total_events=self.total_events,
            is_rotating=self.is_rotating,
        )

    def update_phase(self) -> None:
        """Update current phase based on angle."""
        self.phase = self._get_phase_at_angle(self.angle)

    def __repr__(self) -> str:
        """String representation for debugging."""
        return (
            f"TimingController(angle={self.angle}°, "
            f"phase={self.phase.value}, "
            f"rotation={self.rotation_count}, "
            f"events={self.total_events})"
        )


class TimingSequence:
    """
    Predefined timing sequence for specific operations.

    Allows specification of what should happen at each phase
    without modifying TimingController directly.
    """

    def __init__(self, name: str):
        """Initialize timing sequence."""
        self.name = name
        self.operations: Dict[MechanicalPhase, List[str]] = {}
        self.duration = 360  # Default one rotation

    def add_operation(
        self, phase: MechanicalPhase, operation: str
    ) -> None:
        """Add operation to phase."""
        if phase not in self.operations:
            self.operations[phase] = []
        self.operations[phase].append(operation)

    def get_operations_for_phase(
        self, phase: MechanicalPhase
    ) -> List[str]:
        """Get operations for given phase."""
        return self.operations.get(phase, [])

    def __repr__(self) -> str:
        """String representation."""
        return f"TimingSequence({self.name}, phases={len(self.operations)})"
