"""
Difference Engine No. 2 - Interactive Debugger

Provides interactive debugging capabilities for DE2 emulation:
  - Symbol table for named variables
  - Breakpoint engine with conditions
  - Runtime state inspection
  - Mechanical cycle tracing
  - Step-through execution

References:
  - Ada Lovelace's Notes: First description of program debugging concepts
  - Babbage's Engine: Documentation of mechanical inspection points
"""

from typing import List, Optional, Dict, Callable, Tuple, Any
from dataclasses import dataclass, field
from enum import Enum
from backend.src.emulator.machine import DEMachine, DEMachineSnapshot
from backend.src.emulator.timing import MechanicalPhase


class BreakpointType(Enum):
    """Types of breakpoints."""

    CYCLE = "CYCLE"  # Break at specific cycle number
    PHASE = "PHASE"  # Break at specific mechanical phase
    VALUE_CHANGE = "VALUE_CHANGE"  # Break when variable changes
    CONDITION = "CONDITION"  # Break when condition is true
    INSTRUCTION = "INSTRUCTION"  # Break at specific instruction


@dataclass
class SymbolEntry:
    """Entry in the debugger's symbol table."""

    name: str  # Variable name
    initial_value: int  # Initial value
    current_value: int = 0  # Current value
    read_count: int = 0  # Times read
    write_count: int = 0  # Times written
    first_access_cycle: Optional[int] = None  # First access cycle
    last_access_cycle: Optional[int] = None  # Last access cycle
    access_history: List[Tuple[int, str, int]] = field(default_factory=list)  # (cycle, op, value)


@dataclass
class Breakpoint:
    """Represents a single breakpoint."""

    breakpoint_id: int  # Unique ID
    breakpoint_type: BreakpointType  # Type of breakpoint
    enabled: bool = True  # Breakpoint enabled
    hit_count: int = 0  # Times this breakpoint was hit
    cycle_target: Optional[int] = None  # For CYCLE breakpoints
    phase_target: Optional[MechanicalPhase] = None  # For PHASE breakpoints
    variable_name: Optional[str] = None  # For VALUE_CHANGE breakpoints
    condition_func: Optional[Callable[[DEMachineSnapshot], bool]] = None  # For CONDITION
    trigger_on_change: Optional[int] = None  # Previous value for VALUE_CHANGE


@dataclass
class ExecutionTrace:
    """Trace of a single execution step."""

    cycle: int  # Cycle number
    phase: MechanicalPhase  # Mechanical phase
    angle: int  # Shaft angle
    column_values: List[int]  # Column values before operation
    operation: str  # Operation performed
    result_values: List[int]  # Column values after operation
    carry_signals: List[bool]  # Carry signals
    breakpoint_hit: Optional[int] = None  # Breakpoint ID if hit


class SymbolTable:
    """Manages variable symbols and their values during execution."""

    def __init__(self):
        """Initialize empty symbol table."""
        self.symbols: Dict[str, SymbolEntry] = {}
        self.total_accesses = 0

    def define_symbol(self, name: str, initial_value: int) -> None:
        """
        Define a new symbol.

        Args:
            name: Variable name
            initial_value: Initial value
        """
        if name in self.symbols:
            raise ValueError(f"Symbol '{name}' already defined")

        self.symbols[name] = SymbolEntry(
            name=name, initial_value=initial_value, current_value=initial_value
        )

    def read_symbol(self, name: str, cycle: int) -> int:
        """
        Read symbol value and record access.

        Args:
            name: Variable name
            cycle: Current cycle number

        Returns:
            Symbol value

        Raises:
            KeyError: If symbol not found
        """
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

    def write_symbol(self, name: str, value: int, cycle: int) -> None:
        """
        Write symbol value and record access.

        Args:
            name: Variable name
            value: New value
            cycle: Current cycle number

        Raises:
            KeyError: If symbol not defined
        """
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

    def get_symbol(self, name: str) -> int:
        """Get symbol value without recording access."""
        if name not in self.symbols:
            raise KeyError(f"Symbol '{name}' not defined")
        return self.symbols[name].current_value

    def get_all_symbols(self) -> Dict[str, int]:
        """Get all symbol names and values."""
        return {name: entry.current_value for name, entry in self.symbols.items()}

    def get_symbol_stats(self, name: str) -> Dict[str, Any]:
        """Get detailed statistics for a symbol."""
        if name not in self.symbols:
            raise KeyError(f"Symbol '{name}' not defined")

        entry = self.symbols[name]
        return {
            "name": name,
            "initial_value": entry.initial_value,
            "current_value": entry.current_value,
            "read_count": entry.read_count,
            "write_count": entry.write_count,
            "first_access_cycle": entry.first_access_cycle,
            "last_access_cycle": entry.last_access_cycle,
            "access_history": entry.access_history,
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
        self.triggered_breakpoints: List[int] = []

    def set_breakpoint(self, breakpoint_type: BreakpointType, **kwargs) -> int:
        """
        Set a new breakpoint.

        Args:
            breakpoint_type: Type of breakpoint
            **kwargs: Type-specific arguments
                - CYCLE: cycle_target
                - PHASE: phase_target
                - VALUE_CHANGE: variable_name
                - CONDITION: condition_func

        Returns:
            Breakpoint ID
        """
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

        self.breakpoints[breakpoint_id] = breakpoint
        return breakpoint_id

    def enable_breakpoint(self, breakpoint_id: int) -> None:
        """Enable a breakpoint."""
        if breakpoint_id not in self.breakpoints:
            raise ValueError(f"Breakpoint {breakpoint_id} not found")
        self.breakpoints[breakpoint_id].enabled = True

    def disable_breakpoint(self, breakpoint_id: int) -> None:
        """Disable a breakpoint."""
        if breakpoint_id not in self.breakpoints:
            raise ValueError(f"Breakpoint {breakpoint_id} not found")
        self.breakpoints[breakpoint_id].enabled = False

    def remove_breakpoint(self, breakpoint_id: int) -> None:
        """Remove a breakpoint."""
        if breakpoint_id not in self.breakpoints:
            raise ValueError(f"Breakpoint {breakpoint_id} not found")
        del self.breakpoints[breakpoint_id]

    def check_breakpoints(
        self,
        cycle: int,
        phase: MechanicalPhase,
        snapshot: DEMachineSnapshot,
        symbol_table: SymbolTable,
    ) -> List[int]:
        """
        Check if any breakpoints are triggered.

        Args:
            cycle: Current cycle number
            phase: Current mechanical phase
            snapshot: Current machine state
            symbol_table: Symbol table for variable inspection

        Returns:
            List of triggered breakpoint IDs
        """
        triggered = []

        for bp_id, bp in self.breakpoints.items():
            if not bp.enabled:
                continue

            hit = False

            if bp.breakpoint_type == BreakpointType.CYCLE:
                if cycle == bp.cycle_target:
                    hit = True
            elif bp.breakpoint_type == BreakpointType.PHASE:
                if phase == bp.phase_target:
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

        self.triggered_breakpoints = triggered
        return triggered

    def get_breakpoint_info(self, breakpoint_id: int) -> Dict[str, Any]:
        """Get information about a breakpoint."""
        if breakpoint_id not in self.breakpoints:
            raise ValueError(f"Breakpoint {breakpoint_id} not found")

        bp = self.breakpoints[breakpoint_id]
        info = {
            "id": bp.breakpoint_id,
            "type": bp.breakpoint_type.value,
            "enabled": bp.enabled,
            "hit_count": bp.hit_count,
        }

        if bp.breakpoint_type == BreakpointType.CYCLE:
            info["cycle_target"] = bp.cycle_target
        elif bp.breakpoint_type == BreakpointType.PHASE:
            info["phase_target"] = bp.phase_target.value if bp.phase_target else None
        elif bp.breakpoint_type == BreakpointType.VALUE_CHANGE:
            info["variable_name"] = bp.variable_name

        return info


class Debugger:
    """
    Interactive Debugger for Difference Engine No. 2.

    Provides:
      - Symbol table management
      - Breakpoint engine
      - Execution stepping
      - State inspection
      - Cycle tracing
    """

    def __init__(self, machine: DEMachine):
        """
        Initialize debugger.

        Args:
            machine: DEMachine instance to debug
        """
        self.machine = machine
        self.symbol_table = SymbolTable()
        self.breakpoint_manager = BreakpointManager()
        self.execution_trace: List[ExecutionTrace] = []
        self.is_paused = False
        self.current_cycle = 0
        self.current_phase: Optional[MechanicalPhase] = None

    def define_variable(self, name: str, value: int) -> None:
        """Define a named variable."""
        self.symbol_table.define_symbol(name, value)

    def get_variable(self, name: str) -> int:
        """Get current value of a variable."""
        return self.symbol_table.get_symbol(name)

    def set_variable(self, name: str, value: int) -> None:
        """Set variable value."""
        self.symbol_table.write_symbol(name, value, self.current_cycle)

    def list_variables(self) -> Dict[str, int]:
        """List all variables and their current values."""
        return self.symbol_table.get_all_symbols()

    def get_variable_stats(self, name: str) -> Dict[str, Any]:
        """Get detailed statistics for a variable."""
        return self.symbol_table.get_symbol_stats(name)

    def set_cycle_breakpoint(self, cycle: int) -> int:
        """Set breakpoint at specific cycle."""
        return self.breakpoint_manager.set_breakpoint(BreakpointType.CYCLE, cycle_target=cycle)

    def set_phase_breakpoint(self, phase: MechanicalPhase) -> int:
        """Set breakpoint at specific phase."""
        return self.breakpoint_manager.set_breakpoint(BreakpointType.PHASE, phase_target=phase)

    def set_value_breakpoint(self, variable_name: str) -> int:
        """Set breakpoint when variable changes."""
        return self.breakpoint_manager.set_breakpoint(
            BreakpointType.VALUE_CHANGE, variable_name=variable_name
        )

    def set_condition_breakpoint(self, condition: Callable[[DEMachineSnapshot], bool]) -> int:
        """Set breakpoint when condition is true."""
        return self.breakpoint_manager.set_breakpoint(
            BreakpointType.CONDITION, condition_func=condition
        )

    def disable_breakpoint(self, breakpoint_id: int) -> None:
        """Disable a breakpoint."""
        self.breakpoint_manager.disable_breakpoint(breakpoint_id)

    def enable_breakpoint(self, breakpoint_id: int) -> None:
        """Enable a breakpoint."""
        self.breakpoint_manager.enable_breakpoint(breakpoint_id)

    def remove_breakpoint(self, breakpoint_id: int) -> None:
        """Remove a breakpoint."""
        self.breakpoint_manager.remove_breakpoint(breakpoint_id)

    def list_breakpoints(self) -> List[Dict[str, Any]]:
        """List all breakpoints."""
        return [
            self.breakpoint_manager.get_breakpoint_info(bp_id)
            for bp_id in self.breakpoint_manager.breakpoints
        ]

    def step_cycle(self) -> Optional[List[int]]:
        """
        Execute one complete mechanical cycle.

        Returns:
            List of triggered breakpoint IDs, or None if no breakpoints
        """
        snapshot_before = DEMachineSnapshot(
            cycle_count=self.machine.cycle_count,
            current_phase=self.machine.timing.phase,
            timing_angle=self.machine.timing.angle,
            column_values=self.machine.column_bank.get_all_values(),
            carry_signals=self.machine.carriage.carry_signals.copy(),
            ae_accumulator=int(self.machine.analytical_engine.registers["A"]),
            total_operations=self.machine.total_operations,
        )

        # Execute cycle
        self.machine.run_full_cycle()
        self.current_cycle = self.machine.cycle_count

        snapshot_after = DEMachineSnapshot(
            cycle_count=self.machine.cycle_count,
            current_phase=self.machine.timing.phase,
            timing_angle=self.machine.timing.angle,
            column_values=self.machine.column_bank.get_all_values(),
            carry_signals=self.machine.carriage.carry_signals.copy(),
            ae_accumulator=int(self.machine.analytical_engine.registers["A"]),
            total_operations=self.machine.total_operations,
        )

        # Check breakpoints
        triggered = self.breakpoint_manager.check_breakpoints(
            self.current_cycle, snapshot_after.current_phase, snapshot_after, self.symbol_table
        )

        if triggered:
            self.is_paused = True

        return triggered if triggered else None

    def continue_execution(self, max_cycles: Optional[int] = None) -> Dict[str, Any]:
        """
        Continue execution until breakpoint or max cycles.

        Args:
            max_cycles: Maximum cycles to run (None = unlimited)

        Returns:
            Execution summary
        """
        cycles_run = 0
        breakpoint_hit = None

        while True:
            if max_cycles and cycles_run >= max_cycles:
                break

            result = self.step_cycle()
            cycles_run += 1

            if result:
                breakpoint_hit = result
                break

        self.is_paused = False if not breakpoint_hit else True

        return {
            "cycles_run": cycles_run,
            "current_cycle": self.current_cycle,
            "breakpoint_hit": breakpoint_hit,
        }

    def get_current_state(self) -> Dict[str, Any]:
        """Get current execution state."""
        snapshot = DEMachineSnapshot(
            cycle_count=self.machine.cycle_count,
            current_phase=self.machine.timing.phase,
            timing_angle=self.machine.timing.angle,
            column_values=self.machine.column_bank.get_all_values(),
            carry_signals=self.machine.carriage.carry_signals.copy(),
            ae_accumulator=int(self.machine.analytical_engine.registers["A"]),
            total_operations=self.machine.total_operations,
        )

        return {
            "cycle": snapshot.cycle_count,
            "phase": snapshot.current_phase.value if snapshot.current_phase else None,
            "angle": snapshot.timing_angle,
            "columns": snapshot.column_values,
            "carry_signals": snapshot.carry_signals,
            "accumulator": snapshot.ae_accumulator,
            "total_operations": snapshot.total_operations,
            "variables": self.symbol_table.get_all_symbols(),
        }

    def reset(self) -> None:
        """Reset debugger and machine state."""
        self.machine.__init__()  # Re-initialize machine
        self.symbol_table.reset()
        self.execution_trace = []
        self.is_paused = False
        self.current_cycle = 0
        self.current_phase = None
