"""
Type Definitions for Difference Engine No. 2 Emulator

Dataclasses and types used throughout the DE2 emulator, including:
- Mechanical state snapshots
- Timing events
- Carry states
- Debug information
"""

from dataclasses import dataclass, field
from typing import Dict, List, Tuple, Optional, Any
from enum import Enum

# --- Common Types ---

class MechanicalPhase(Enum):
    """Mechanical phases during DE2 rotation (0-360°)."""
    IDLE = "idle"
    INPUT = "input"
    ADDITION = "addition"
    CARRY = "carry"
    OUTPUT = "output"
    ADVANCE = "advance"
    RESET = "reset"
    PAUSE = "pause"

class CardType(Enum):
    NUMBER = "Number"
    OPERATION = "Operation"
    VARIABLE = "Variable"
    COMBINATORIAL = "Combinatorial"

# BabbageNumber: canonical definition in analytical_engine.py
# Re-exported here for backward compatibility.
from backend.src.emulator.analytical_engine import BabbageNumber

# --- Analytical Engine Types ---

@dataclass
class MillState:
    registers: Dict[str, BabbageNumber]
    flags: Dict[str, bool]

@dataclass
class MachineState:
    memory: List[BabbageNumber]
    mill: MillState
    pc: int

@dataclass
class OperationCard:
    opcode: str
    operands: List[str]

@dataclass
class VariableCard:
    address: int
    operation: str # READ/WRITE

@dataclass
class JacquardCard:
    card_type: CardType
    content: Any

# --- Antikythera Types ---

@dataclass
class AntikytheraGear:
    name: str
    teeth: int
    angle: float

@dataclass
class AntikytheraMechanismState:
    gears: Dict[str, AntikytheraGear]
    pointers: Dict[str, float]

# --- DE2 Types ---

@dataclass
class DebugSnapshot:
    """Complete mechanical state at a moment in time"""
    main_shaft_angle: int  # 0–360°
    column_states: Dict[int, List[int]]  # column_index → digit array (31 digits)
    carry_states: Dict[int, Tuple[bool, bool]]  # position → (carry_in, carry_out)
    printer_position: int  # line number on printed page
    stereo_position: Tuple[int, int]  # (x, y) in stereotype frame
    event_log: List[str]  # mechanical events at this angle
    phase_name: str  # "column_latch", "addition", "carry", "print", etc.
    cycle_count: int  # which 360° cycle we're in
    timestamp: Optional[float] = None  # for performance analysis


@dataclass
class TimeEvent:
    """One mechanical event at a specific main shaft angle"""
    angle: int  # 0–360°
    phase: str  # "column_latch", "addition", "carry", "print", "stereo"
    component: str  # "column_0", "carry", "printer", "stereotyper"
    action: str  # "latch_open", "add_begin", "carry_execute", "strike", etc.
    data: Dict = field(default_factory=dict)  # component-specific data


@dataclass
class CarryState:
    """State of carry mechanism at one position"""
    position: int  # which column position (0–30)
    carry_in: bool  # incoming carry from previous position
    carry_out: bool  # outgoing carry to next position
    anticipating_carry_in: bool  # look-ahead signal for position+1
    is_active: bool  # whether carry lifter is engaged
    timestamp: int  # main shaft angle when this state was recorded


@dataclass
class ColumnSnapshot:
    """State of one digit column"""
    column_index: int  # 0–7
    digits: List[int]  # 31 decimal digits
    carry_state: bool  # current carry flag
    is_latched: bool  # whether column latch is closed
    is_advancing: bool  # whether column is shifting to next row
    phase: str  # current mechanical phase for this column

# ColumnState is a deprecated alias for ColumnSnapshot.
# Kept for backward compatibility; new code should use ColumnSnapshot.
ColumnState = ColumnSnapshot


@dataclass
class PrinterSnapshot:
    """State of printer apparatus"""
    type_wheels: List[int]  # 8 digit positions (0–9 each)
    inking_engaged: bool  # inking roller active
    hammer_ready: bool  # hammer positioned
    platen_position: int  # line number
    printed_lines: List[str] = field(default_factory=list)  # lines on page so far


@dataclass
class StereotyperSnapshot:
    """State of stereotype frame"""
    x_position: int  # 0–7 (digit positions)
    y_position: int  # 0–49 (line positions)
    mold_image: Dict[Tuple[int, int], int] = field(default_factory=dict)  # (x,y) → raised (1) or flat (0)
    completed_molds: List[Dict] = field(default_factory=list)


@dataclass
class OperationResult:
    """Result of one machine operation (e.g., one cycle)"""
    success: bool
    cycle_count: int
    events: List[TimeEvent] = field(default_factory=list)
    column_values: List[int] = field(default_factory=list)  # 8 column values after cycle
    printed_lines: List[str] = field(default_factory=list)  # lines printed this cycle
    extracted_mold: Optional[Dict] = None  # if mold was extracted
    error: Optional[str] = None
    execution_time_ms: float = 0.0


@dataclass
class TimingSpec:
    """Timing specification (from SMG Technical Description)"""
    # Phase timing (0–360° per cycle)
    phase_map: Dict[Tuple[int, int], str] = field(default_factory=lambda: {
        (0, 30): "column_latch",
        (30, 60): "addition_begin",
        (60, 90): "carry_evaluation_1",
        (90, 120): "carry_execution",
        (120, 150): "carry_evaluation_2",
        (150, 180): "settle_phase",
        (180, 210): "print_setup",
        (210, 240): "inking",
        (240, 270): "print_strike",
        (270, 300): "platen_advance",
        (300, 330): "stereo_advance",
        (330, 360): "cycle_reset"
    })

    # Events that fire at specific angles
    event_angles: Dict[int, str] = field(default_factory=lambda: {
        0: "cycle_start / column_latch_open",
        30: "difference_addition_begins",
        60: "anticipating_carriage_evaluates_position_0",
        90: "column_advance / figure_wheel_shift",
        120: "anticipating_carriage_evaluates_position_1",
        150: "carry_execution_completes",
        180: "print_setup_begins / type_setter_positioning",
        210: "inking_roller_engages",
        240: "print_hammer_strikes",
        270: "platen_advances",
        300: "stereotype_frame_advances_x_axis",
        330: "stereotype_mold_extraction",
        360: "cycle_reset / ready_for_next_rotation"
    })

    # Carry propagation timing (from SMG appendix)
    carry_look_ahead_depth: int = 2  # Check 2 positions ahead
    carry_execution_angle: int = 150  # Carry propagates at this angle
    carry_evaluation_1_angle: int = 60  # First evaluation
    carry_evaluation_2_angle: int = 120  # Second evaluation


DEFAULT_TIMING_SPEC = TimingSpec()


@dataclass
class MachineConfig:
    """Configuration for DE2 machine"""
    num_columns: int = 8  # Always 8 for DE2
    digits_per_column: int = 31  # Always 31 for DE2
    timing_spec: TimingSpec = field(default_factory=TimingSpec)
    enable_logging: bool = False
    debug_mode: bool = False