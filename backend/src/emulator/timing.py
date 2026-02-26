"""
Difference Engine No. 2 - Timing Controller

Main shaft rotation (0-360 degrees) event dispatcher for DE2 mechanical phases.

The DE2's main shaft rotates once per operational cycle (360°). Different
mechanical operations occur at specific shaft angles, creating a precise
sequence of events: input, addition, carry, output, advance.

Architecture:
  - Main shaft angle tracking (0-360°, 1° resolution)
  - Phase markers at 45deg intervals (8 phases x 45deg = 360deg)
    ASSUMPTION: 45-degree equal intervals approximate the SMG timing appendix
  - Event generation based on angle thresholds
  - Integration with AnticipatingCarriage and column operations

References:
  - SMG Technical Description: Main Shaft and Timing
  - Babbage's mechanical notation for phase diagrams
  - Working DE2 hardware mechanical documentation
"""

from collections.abc import Callable
from dataclasses import dataclass

# MechanicalPhase: canonical definition in types.py, re-imported here.
from .types import MechanicalPhase


@dataclass
class TimingEvent:
    """Event generated at specific shaft angle."""

    angle: int                              # 0-360 degrees
    phase: MechanicalPhase                  # Current phase
    event_type: str                         # Event name
    timestamp: int                          # Cycle counter
    payload: dict | None = None          # Additional data


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

    def __init__(self) -> None:
        """Initialize TimingController."""
        self.angle = 0                      # Current shaft angle (0-360)
        self.rotation_count = 0             # Total rotations completed
        self.is_rotating = False            # Currently rotating
        self.phase = self._get_phase_at_angle(0)
        self.events: list[TimingEvent] = []
        self.event_callbacks: dict[int, list[Callable]] = {}  # angle → handlers
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

        return int(self.total_events - initial_event_count)

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

    def get_events_since(self, rotation: int) -> list[TimingEvent]:
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
        return int(self.angle)

    def get_rotation_count(self) -> int:
        """Get total rotations completed."""
        return int(self.rotation_count)

    def is_in_phase(self, phase: MechanicalPhase) -> bool:
        """Check if currently in given phase."""
        return bool(self.phase == phase)

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
        return int(self.angle - phase_start.get(self.phase, 0))

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
        self.operations: dict[MechanicalPhase, list[str]] = {}
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
    ) -> list[str]:
        """Get operations for given phase."""
        return self.operations.get(phase, [])

    def __repr__(self) -> str:
        """String representation."""
        return f"TimingSequence({self.name}, phases={len(self.operations)})"


# ---------------------------------------------------------------------------
# Carry Propagation Timing Model
# ---------------------------------------------------------------------------

class CarryPropagationModel:
    """Models carry propagation timing in the Anticipating Carriage.

    Each digit position requires ~1 degree of shaft rotation for carry
    propagation. The 2-position look-ahead reduces worst-case propagation.
    All carry events must fit within the CARRY phase (135-180 degrees).
    """

    DEGREES_PER_DIGIT = 1.0       # Carry propagation rate
    LOOKAHEAD_POSITIONS = 2       # Anticipating carriage look-ahead
    CARRY_PHASE_START = 135       # Degrees
    CARRY_PHASE_END = 180         # Degrees
    CARRY_PHASE_WIDTH = 45        # Degrees available

    @staticmethod
    def carry_propagation_degrees(
        digit_count: int,
        lookahead: int = 2,
    ) -> float:
        """Degrees of shaft rotation needed for carry propagation.

        With look-ahead, effective propagation distance is reduced:
        degrees = ceil(digit_count / (1 + lookahead)) * DEGREES_PER_DIGIT
        """
        if digit_count <= 0:
            return 0.0
        effective = digit_count / (1 + lookahead)
        return CarryPropagationModel.DEGREES_PER_DIGIT * effective

    @staticmethod
    def worst_case_degrees(digit_count: int = 50) -> float:
        """Worst-case carry chain: all digits ripple."""
        return CarryPropagationModel.carry_propagation_degrees(
            digit_count, CarryPropagationModel.LOOKAHEAD_POSITIONS,
        )

    @staticmethod
    def fits_within_phase(carry_degrees: float) -> bool:
        """Check if carry propagation fits within the CARRY phase."""
        return carry_degrees <= CarryPropagationModel.CARRY_PHASE_WIDTH


# ---------------------------------------------------------------------------
# Barrel-Timing Bridge
# ---------------------------------------------------------------------------

# Mapping of barrel step counts to shaft degree allocations
BARREL_TIMING_MAP: dict[str, dict[str, int | list[str]]] = {
    "ADD": {"steps": 6, "degrees": 45, "phases": ["ADDITION", "CARRY"]},
    "SUB": {"steps": 6, "degrees": 45, "phases": ["ADDITION", "CARRY"]},
    "MULT": {"steps": 4, "degrees": 360, "phases": ["ADDITION", "CARRY"]},
    "DIV": {"steps": 13, "degrees": 360, "phases": ["ADDITION", "CARRY"]},
    "SQRT": {"steps": 6, "degrees": 360, "phases": ["ADDITION", "CARRY"]},
    "LOAD": {"steps": 3, "degrees": 45, "phases": ["INPUT"]},
    "STOR": {"steps": 3, "degrees": 45, "phases": ["OUTPUT"]},
}


class BarrelTimingBridge:
    """Maps barrel micro-op steps to shaft angle allocations.

    Each barrel operation is allocated a number of shaft degrees.
    For simple ops (ADD, SUB, LOAD, STOR) this fits within one rotation.
    For complex ops (MULT, DIV, SQRT) this may span multiple rotations.
    """

    @staticmethod
    def degrees_per_step(barrel_name: str) -> float:
        """Shaft degrees allocated per barrel step."""
        entry = BARREL_TIMING_MAP.get(barrel_name)
        if entry is None:
            return 0.0
        steps = int(entry["steps"])  # type: ignore[arg-type]
        if steps <= 0:
            return 0.0
        return float(int(entry["degrees"])) / steps  # type: ignore[arg-type]

    @staticmethod
    def total_degrees(barrel_name: str) -> float:
        """Total shaft degrees for a complete barrel execution."""
        entry = BARREL_TIMING_MAP.get(barrel_name)
        if entry is None:
            return 0.0
        return float(int(entry["degrees"]))  # type: ignore[arg-type]

    @staticmethod
    def rotations_required(barrel_name: str) -> float:
        """Number of full shaft rotations required."""
        return BarrelTimingBridge.total_degrees(barrel_name) / 360.0

    @staticmethod
    def uses_phases(barrel_name: str) -> list[str]:
        """Return list of mechanical phases used by this barrel."""
        entry = BARREL_TIMING_MAP.get(barrel_name)
        if entry is None:
            return []
        phases = entry["phases"]
        if isinstance(phases, list):
            return [str(p) for p in phases]
        return []


# ---------------------------------------------------------------------------
# Opcode Timing Sequences
# ---------------------------------------------------------------------------

def build_opcode_timing_sequences() -> dict[str, TimingSequence]:
    """Build concrete TimingSequence for each supported opcode.

    These define which mechanical phases each opcode uses during execution.
    """
    sequences = {}

    # ADD: uses ADDITION + CARRY phases within one rotation
    add_seq = TimingSequence("ADD")
    add_seq.add_operation(MechanicalPhase.INPUT, "fetch_operand")
    add_seq.add_operation(MechanicalPhase.ADDITION, "add_to_accumulator")
    add_seq.add_operation(MechanicalPhase.CARRY, "propagate_carry")
    add_seq.add_operation(MechanicalPhase.OUTPUT, "store_result")
    add_seq.duration = 360
    sequences["ADD"] = add_seq

    # SUB: same timing as ADD
    sub_seq = TimingSequence("SUB")
    sub_seq.add_operation(MechanicalPhase.INPUT, "fetch_operand")
    sub_seq.add_operation(MechanicalPhase.ADDITION, "subtract_from_accumulator")
    sub_seq.add_operation(MechanicalPhase.CARRY, "propagate_borrow")
    sub_seq.add_operation(MechanicalPhase.OUTPUT, "store_result")
    sub_seq.duration = 360
    sequences["SUB"] = sub_seq

    # MULT: spans multiple rotations (repeated addition per digit)
    mult_seq = TimingSequence("MULT")
    mult_seq.add_operation(MechanicalPhase.INPUT, "fetch_multiplier_digit")
    mult_seq.add_operation(MechanicalPhase.ADDITION, "repeated_addition")
    mult_seq.add_operation(MechanicalPhase.CARRY, "propagate_carry")
    mult_seq.add_operation(MechanicalPhase.ADVANCE, "shift_accumulator")
    mult_seq.duration = 360 * 50  # Up to 50 digit positions
    sequences["MULT"] = mult_seq

    # DIV: spans multiple rotations (repeated subtraction per digit)
    div_seq = TimingSequence("DIV")
    div_seq.add_operation(MechanicalPhase.INPUT, "fetch_dividend_divisor")
    div_seq.add_operation(MechanicalPhase.ADDITION, "trial_subtraction")
    div_seq.add_operation(MechanicalPhase.CARRY, "propagate_borrow")
    div_seq.add_operation(MechanicalPhase.ADVANCE, "shift_divisor")
    div_seq.duration = 360 * 50
    sequences["DIV"] = div_seq

    # SQRT: Newton-Raphson iterations (~25 for 50 digits)
    sqrt_seq = TimingSequence("SQRT")
    sqrt_seq.add_operation(MechanicalPhase.INPUT, "load_operand")
    sqrt_seq.add_operation(MechanicalPhase.ADDITION, "newton_raphson_step")
    sqrt_seq.add_operation(MechanicalPhase.CARRY, "propagate_carry")
    sqrt_seq.duration = 360 * 25  # ~25 iterations
    sequences["SQRT"] = sqrt_seq

    # LOAD: single rotation, INPUT phase only
    load_seq = TimingSequence("LOAD")
    load_seq.add_operation(MechanicalPhase.INPUT, "transfer_store_to_mill")
    load_seq.duration = 360
    sequences["LOAD"] = load_seq

    # STOR: single rotation, OUTPUT phase only
    stor_seq = TimingSequence("STOR")
    stor_seq.add_operation(MechanicalPhase.OUTPUT, "transfer_mill_to_store")
    stor_seq.duration = 360
    sequences["STOR"] = stor_seq

    return sequences
