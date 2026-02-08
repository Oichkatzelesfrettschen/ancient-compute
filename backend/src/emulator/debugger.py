"""
Ancient Compute - Interactive Debugger

Provides interactive debugging capabilities for both DE2 and AE emulators:
  - Symbol table for named variables
  - Breakpoint engine with conditions
  - Runtime state inspection
  - Mechanical cycle tracing
  - Step-through execution
"""

from typing import List, Optional, Dict, Callable, Tuple, Any
from dataclasses import dataclass, field
from enum import Enum
from .adapter import MachineAdapter, MechanicalPhase

class BreakpointType(Enum):
    """Types of breakpoints."""
    CYCLE = "CYCLE"                    # Break at specific cycle number (or clock time)
    PHASE = "PHASE"                    # Break at specific mechanical phase
    VALUE_CHANGE = "VALUE_CHANGE"      # Break when variable changes
    CONDITION = "CONDITION"            # Break when condition is true
    INSTRUCTION = "INSTRUCTION"        # Break at specific instruction (PC)


@dataclass
class SymbolEntry:
    """Entry in the debugger's symbol table."""
    name: str                           # Variable name
    initial_value: float                # Initial value
    current_value: float = 0.0          # Current value
    read_count: int = 0                 # Times read
    write_count: int = 0                # Times written
    first_access_cycle: Optional[int] = None  # First access cycle
    last_access_cycle: Optional[int] = None   # Last access cycle
    access_history: List[Tuple[int, str, float]] = field(default_factory=list)  # (cycle, op, value)


@dataclass
class Breakpoint:
    """Represents a single breakpoint."""
    breakpoint_id: int                  # Unique ID
    breakpoint_type: BreakpointType     # Type of breakpoint
    enabled: bool = True                # Breakpoint enabled
    hit_count: int = 0                  # Times this breakpoint was hit
    cycle_target: Optional[int] = None  # For CYCLE breakpoints
    phase_target: Optional[MechanicalPhase] = None  # For PHASE breakpoints
    variable_name: Optional[str] = None # For VALUE_CHANGE breakpoints
    instruction_target: Optional[int] = None # For INSTRUCTION breakpoints
    condition_func: Optional[Callable[[Any], bool]] = None  # For CONDITION
    trigger_on_change: Optional[float] = None  # Previous value for VALUE_CHANGE


class SymbolTable:
    """Manages variable symbols and their values during execution."""

    def __init__(self):
        """Initialize empty symbol table."""
        self.symbols: Dict[str, SymbolEntry] = {}
        self.total_accesses = 0

    def define_symbol(self, name: str, initial_value: float) -> None:
        """Define a new symbol."""
        if name in self.symbols:
            raise ValueError(f"Symbol '{name}' already defined")
        self.symbols[name] = SymbolEntry(name=name, initial_value=initial_value, current_value=initial_value)

    def read_symbol(self, name: str, cycle: int) -> float:
        """Read symbol value and record access."""
        if name not in self.symbols:
            raise KeyError(f"Symbol '{name}' not defined")

        entry = self.symbols[name]
        entry.read_count += 1
        entry.last_access_cycle = cycle
        if entry.first_access_cycle is None:
            entry.first_access_cycle = cycle
        entry.access_history.append((cycle, "READ", entry.current_value))
        self.total_accesses += 1
        return entry.current_value

    def write_symbol(self, name: str, value: float, cycle: int) -> None:
        """Write symbol value and record access."""
        if name not in self.symbols:
            raise KeyError(f"Symbol '{name}' not defined")

        entry = self.symbols[name]
        entry.write_count += 1
        entry.current_value = value
        entry.last_access_cycle = cycle
        if entry.first_access_cycle is None:
            entry.first_access_cycle = cycle
        entry.access_history.append((cycle, "WRITE", value))
        self.total_accesses += 1

    def get_symbol(self, name: str) -> float:
        """Get symbol value without recording access."""
        if name not in self.symbols:
            raise KeyError(f"Symbol '{name}' not defined")
        return self.symbols[name].current_value

    def get_all_symbols(self) -> Dict[str, float]:
        """Get all symbol names and values."""
        return {name: entry.current_value for name, entry in self.symbols.items()}

    def reset(self) -> None:
        """Reset all symbols to initial values."""
        for entry in self.symbols.values():
            entry.current_value = entry.initial_value
            entry.read_count = 0
            entry.write_count = 0
            entry.first_access_cycle = None
            entry.last_access_cycle = None
            entry.access_history = []
        self.total_accesses = 0


class BreakpointManager:
    """Manages breakpoints and condition evaluation."""

    def __init__(self):
        """Initialize breakpoint manager."""
        self.breakpoints: Dict[int, Breakpoint] = {}
        self.next_id = 1

    def set_breakpoint(self, breakpoint_type: BreakpointType, **kwargs) -> int:
        """Set a new breakpoint."""
        breakpoint_id = self.next_id
        self.next_id += 1

        breakpoint = Breakpoint(breakpoint_id=breakpoint_id, breakpoint_type=breakpoint_type)

        if breakpoint_type == BreakpointType.CYCLE:
            breakpoint.cycle_target = kwargs.get("cycle_target")
        elif breakpoint_type == BreakpointType.PHASE:
            breakpoint.phase_target = kwargs.get("phase_target")
        elif breakpoint_type == BreakpointType.VALUE_CHANGE:
            breakpoint.variable_name = kwargs.get("variable_name")
        elif breakpoint_type == BreakpointType.CONDITION:
            breakpoint.condition_func = kwargs.get("condition_func")
        elif breakpoint_type == BreakpointType.INSTRUCTION:
            breakpoint.instruction_target = kwargs.get("instruction_target")

        self.breakpoints[breakpoint_id] = breakpoint
        return breakpoint_id

    def check_breakpoints(self, adapter: MachineAdapter, symbol_table: SymbolTable) -> List[int]:
        """Check if any breakpoints are triggered."""
        triggered = []
        snapshot = adapter.get_snapshot()
        cycle = adapter.get_cycle_count()
        phase = adapter.get_current_phase()
        pc = snapshot.get("pc") if isinstance(snapshot, dict) else getattr(snapshot, "pc", None)

        for bp_id, bp in self.breakpoints.items():
            if not bp.enabled:
                continue

            hit = False
            if bp.breakpoint_type == BreakpointType.CYCLE:
                if cycle >= (bp.cycle_target or 0):
                    hit = True
            elif bp.breakpoint_type == BreakpointType.PHASE:
                if phase == bp.phase_target:
                    hit = True
            elif bp.breakpoint_type == BreakpointType.INSTRUCTION:
                if pc == bp.instruction_target:
                    hit = True
            elif bp.breakpoint_type == BreakpointType.VALUE_CHANGE:
                if bp.variable_name in symbol_table.symbols:
                    current_value = symbol_table.get_symbol(bp.variable_name)
                    if bp.trigger_on_change is not None and current_value != bp.trigger_on_change:
                        hit = True
                    bp.trigger_on_change = current_value
            elif bp.breakpoint_type == BreakpointType.CONDITION:
                if bp.condition_func and bp.condition_func(snapshot):
                    hit = True

            if hit:
                bp.hit_count += 1
                triggered.append(bp_id)

        return triggered


class Debugger:
    """
    Interactive Debugger for Babbage Engines.
    """

    def __init__(self, adapter: MachineAdapter):
        """Initialize debugger with a machine adapter."""
        self.adapter = adapter
        self.symbol_table = SymbolTable()
        self.breakpoint_manager = BreakpointManager()
        self.is_paused = False

    def define_variable(self, name: str, value: float) -> None:
        self.symbol_table.define_symbol(name, value)

    def set_instruction_breakpoint(self, pc: int) -> int:
        return self.breakpoint_manager.set_breakpoint(BreakpointType.INSTRUCTION, instruction_target=pc)

    def step(self) -> Optional[List[int]]:
        """Execute one step and check breakpoints."""
        self.adapter.step()
        triggered = self.breakpoint_manager.check_breakpoints(self.adapter, self.symbol_table)
        if triggered:
            self.is_paused = True
        return triggered if triggered else None

    def get_state(self) -> Dict[str, Any]:
        """Get current state via adapter."""
        return {
            "cycle": self.adapter.get_cycle_count(),
            "phase": self.adapter.get_current_phase(),
            "registers": self.adapter.get_register_values(),
            "snapshot": self.adapter.get_snapshot(),
            "variables": self.symbol_table.get_all_symbols(),
        }