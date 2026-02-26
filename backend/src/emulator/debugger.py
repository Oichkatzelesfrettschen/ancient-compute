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

    def get_symbol_stats(self, name: str) -> Dict[str, Any]:
        """Get detailed statistics for a symbol."""
        if name not in self.symbols:
            raise KeyError(f"Symbol '{name}' not defined")
        entry = self.symbols[name]
        return {
            "name": entry.name,
            "initial_value": entry.initial_value,
            "current_value": entry.current_value,
            "read_count": entry.read_count,
            "write_count": entry.write_count,
            "first_access_cycle": entry.first_access_cycle,
            "last_access_cycle": entry.last_access_cycle,
            "access_history": list(entry.access_history),
        }

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

    def disable_breakpoint(self, bp_id: int) -> None:
        """Disable a breakpoint."""
        if bp_id in self.breakpoints:
            self.breakpoints[bp_id].enabled = False

    def enable_breakpoint(self, bp_id: int) -> None:
        """Enable a breakpoint."""
        if bp_id in self.breakpoints:
            self.breakpoints[bp_id].enabled = True

    def remove_breakpoint(self, bp_id: int) -> None:
        """Remove a breakpoint."""
        self.breakpoints.pop(bp_id, None)

    def get_breakpoint_info(self, bp_id: int) -> Dict[str, Any]:
        """Get information about a breakpoint."""
        if bp_id not in self.breakpoints:
            raise KeyError(f"Breakpoint {bp_id} not found")
        bp = self.breakpoints[bp_id]
        return {
            "id": bp.breakpoint_id,
            "type": bp.breakpoint_type.value,
            "enabled": bp.enabled,
            "hit_count": bp.hit_count,
            "cycle_target": bp.cycle_target,
            "phase_target": bp.phase_target,
            "variable_name": bp.variable_name,
            "instruction_target": bp.instruction_target,
        }


class Debugger:
    """
    Interactive Debugger for Babbage Engines.

    Accepts either a MachineAdapter or a raw machine object
    (auto-wrapping with DEMachineAdapter for convenience).
    """

    def __init__(self, adapter):
        """Initialize debugger with a machine adapter or raw machine."""
        if isinstance(adapter, MachineAdapter):
            self.adapter = adapter
        else:
            # Auto-wrap raw machine objects for convenience
            from .adapter import DEMachineAdapter
            self.adapter = DEMachineAdapter(adapter)
            self.machine = adapter  # keep reference for backward compat
        self.symbol_table = SymbolTable()
        self.breakpoint_manager = BreakpointManager()
        self.is_paused = False
        self.current_cycle = 0

    # -- Variable management --

    def define_variable(self, name: str, value: float) -> None:
        self.symbol_table.define_symbol(name, value)

    def get_variable(self, name: str) -> float:
        return self.symbol_table.read_symbol(name, cycle=self.current_cycle)

    def set_variable(self, name: str, value: float) -> None:
        self.symbol_table.write_symbol(name, value, cycle=self.current_cycle)

    def list_variables(self) -> Dict[str, float]:
        return self.symbol_table.get_all_symbols()

    def get_variable_stats(self, name: str) -> Dict[str, Any]:
        return self.symbol_table.get_symbol_stats(name)

    # -- Breakpoint management --

    def set_instruction_breakpoint(self, pc: int) -> int:
        return self.breakpoint_manager.set_breakpoint(BreakpointType.INSTRUCTION, instruction_target=pc)

    def set_cycle_breakpoint(self, cycle: int) -> int:
        return self.breakpoint_manager.set_breakpoint(BreakpointType.CYCLE, cycle_target=cycle)

    def set_phase_breakpoint(self, phase: MechanicalPhase) -> int:
        return self.breakpoint_manager.set_breakpoint(BreakpointType.PHASE, phase_target=phase)

    def set_value_breakpoint(self, variable_name: str) -> int:
        return self.breakpoint_manager.set_breakpoint(BreakpointType.VALUE_CHANGE, variable_name=variable_name)

    def set_condition_breakpoint(self, condition_func: Callable) -> int:
        return self.breakpoint_manager.set_breakpoint(BreakpointType.CONDITION, condition_func=condition_func)

    def disable_breakpoint(self, bp_id: int) -> None:
        self.breakpoint_manager.disable_breakpoint(bp_id)

    def enable_breakpoint(self, bp_id: int) -> None:
        self.breakpoint_manager.enable_breakpoint(bp_id)

    def remove_breakpoint(self, bp_id: int) -> None:
        self.breakpoint_manager.remove_breakpoint(bp_id)

    def list_breakpoints(self) -> List[Dict[str, Any]]:
        return [
            self.breakpoint_manager.get_breakpoint_info(bp_id)
            for bp_id in self.breakpoint_manager.breakpoints
        ]

    # -- Execution control --

    def step(self) -> Optional[List[int]]:
        """Execute one step and check breakpoints."""
        self.adapter.step()
        self.current_cycle = self.adapter.get_cycle_count()
        triggered = self.breakpoint_manager.check_breakpoints(self.adapter, self.symbol_table)
        if triggered:
            self.is_paused = True
        return triggered if triggered else None

    def step_cycle(self) -> Optional[List[int]]:
        """Execute one cycle and check breakpoints. Alias for step()."""
        return self.step()

    def continue_execution(self, max_cycles: int = 1000) -> Dict[str, Any]:
        """Run until breakpoint hit or max_cycles reached."""
        initial_cycle = self.current_cycle
        breakpoint_hit = None
        for _ in range(max_cycles):
            result = self.step()
            if result is not None:
                breakpoint_hit = result
                break
        return {
            "cycles_run": self.current_cycle - initial_cycle,
            "current_cycle": self.current_cycle,
            "breakpoint_hit": breakpoint_hit,
        }

    def reset(self) -> None:
        """Reset debugger state."""
        self.current_cycle = 0
        self.symbol_table.reset()
        self.breakpoint_manager = BreakpointManager()
        self.is_paused = False

    # -- State inspection --

    def get_state(self) -> Dict[str, Any]:
        """Get current state via adapter."""
        return {
            "cycle": self.adapter.get_cycle_count(),
            "phase": self.adapter.get_current_phase(),
            "registers": self.adapter.get_register_values(),
            "snapshot": self.adapter.get_snapshot(),
            "variables": self.symbol_table.get_all_symbols(),
        }

    def get_current_state(self) -> Dict[str, Any]:
        """Get current execution state (user-facing)."""
        return {
            "cycle": self.current_cycle,
            "phase": str(self.adapter.get_current_phase()),
            "columns": self.adapter.get_column_values(),
            "accumulator": 0,  # Provided by snapshot
            "variables": self.symbol_table.get_all_symbols(),
        }