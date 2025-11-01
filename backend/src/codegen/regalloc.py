"""
Linear Scan Register Allocation

Implements the linear scan algorithm for allocating IR values to physical registers.

Algorithm:
1. Sort live intervals by start position
2. For each interval:
   - Expire old intervals (end < current start)
   - Allocate free register if available
   - Otherwise spill value with furthest next use

Babbage constraints:
- 4 physical registers: A (0), B (1), C (2), D (3)
- Spilled values stored on stack (addresses 256+)
- Memory layout: globals 0-255, stack 256-511, heap 512-2047
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Dict, Set, List, Optional, Tuple
from backend.src.ir_types import Register, Function
from backend.src.codegen.liveness import LivenessAnalyzer, LiveInterval


@dataclass
class AllocationMap:
    """Result of register allocation"""
    allocations: Dict[str, str] = field(default_factory=dict)  # var_name → register name or "mem_NNN"
    spilled: Dict[str, int] = field(default_factory=dict)      # var_name → memory address
    spill_count: int = 0
    register_pressure: float = 0.0  # percentage of time registers at capacity
    
    def get_allocation(self, var_name: str) -> str:
        """Get register or memory address for variable"""
        return self.allocations.get(var_name, "?")
    
    def is_spilled(self, var_name: str) -> bool:
        """Check if variable was spilled to memory"""
        return var_name in self.spilled
    
    def print_allocation(self) -> str:
        """Print human-readable allocation"""
        lines = ["=== Register Allocation ===\n"]
        
        lines.append("Register Assignments:")
        for var_name in sorted(self.allocations.keys()):
            alloc = self.allocations[var_name]
            if var_name in self.spilled:
                lines.append(f"  {var_name:20} → {alloc:5} (memory[{self.spilled[var_name]}])")
            else:
                lines.append(f"  {var_name:20} → {alloc:5}")
        
        lines.append(f"\nSpill count: {self.spill_count}")
        lines.append(f"Register pressure: {self.register_pressure:.1%}")
        
        return "\n".join(lines)


class LinearScanAllocator:
    """
    Linear scan register allocator for Babbage.
    
    Allocates IR values (from liveness analysis) to 4 physical registers,
    spilling excess values to stack memory.
    """
    
    PHYSICAL_REGISTERS = [Register.A, Register.B, Register.C, Register.D]
    STACK_BASE = 256  # Stack memory starts at address 256
    
    def __init__(self, function: Function, intervals: Dict[str, LiveInterval]):
        self.function = function
        self.intervals = intervals
        self.allocations: Dict[str, str] = {}  # var → register name
        self.spilled: Dict[str, int] = {}      # var → stack address
        self.register_to_interval: Dict[Register, Optional[LiveInterval]] = {
            reg: None for reg in self.PHYSICAL_REGISTERS
        }
        self.active_intervals: List[Tuple[LiveInterval, Register]] = []
        self.spill_counter = 0
        self.stack_pointer = self.STACK_BASE
        self.peak_simultaneous = 0
    
    def allocate(self) -> AllocationMap:
        """Run linear scan allocation algorithm"""
        # Get intervals sorted by start position
        sorted_intervals = sorted(self.intervals.values(), key=lambda iv: (iv.start, iv.end))
        
        # Linear scan
        for interval in sorted_intervals:
            self._expire_old_intervals(interval.start)
            
            # Try to allocate a free register
            free_reg = self._find_free_register()
            
            if free_reg is not None:
                # Register available
                self.allocations[interval.name] = free_reg.value.upper()
                self.register_to_interval[free_reg] = interval
                self.active_intervals.append((interval, free_reg))
            else:
                # No free register: spill value with furthest next use
                self._spill_interval(interval)
        
        # Compute register pressure
        register_pressure = self._compute_register_pressure()
        
        # Build result
        result = AllocationMap(
            allocations=self.allocations,
            spilled=self.spilled,
            spill_count=len(self.spilled),
            register_pressure=register_pressure,
        )
        
        return result
    
    def _expire_old_intervals(self, current_start: int) -> None:
        """Remove intervals that ended before current start"""
        expired = [
            (interval, reg) for interval, reg in self.active_intervals
            if interval.end < current_start
        ]
        
        for interval, reg in expired:
            # Free register
            self.register_to_interval[reg] = None
            self.active_intervals.remove((interval, reg))
    
    def _find_free_register(self) -> Optional[Register]:
        """Find a free physical register"""
        for reg in self.PHYSICAL_REGISTERS:
            if self.register_to_interval[reg] is None:
                return reg
        return None
    
    def _spill_interval(self, interval: LiveInterval) -> None:
        """Spill interval to memory (choose victim with furthest next use)"""
        if not self.active_intervals:
            # Shouldn't happen
            raise RuntimeError(f"Cannot spill {interval.name}: no active intervals")
        
        # Choose victim: active interval with furthest next use
        victim_interval, victim_reg = max(
            self.active_intervals,
            key=lambda x: self._furthest_next_use(x[0])
        )
        
        # Spill victim to memory
        spill_addr = self._allocate_stack_slot()
        self.spilled[victim_interval.name] = spill_addr
        
        # Allocate freed register to current interval
        self.allocations[interval.name] = victim_reg.value.upper()
        self.register_to_interval[victim_reg] = interval
        
        # Update active intervals (replace victim with current)
        self.active_intervals.remove((victim_interval, victim_reg))
        self.active_intervals.append((interval, victim_reg))
        
        self.spill_counter += 1
    
    def _furthest_next_use(self, interval: LiveInterval) -> int:
        """Get position of furthest next use (for spill selection)"""
        # Returns the end position (furthest use in interval)
        return interval.end
    
    def _allocate_stack_slot(self) -> int:
        """Allocate stack space for spilled value"""
        addr = self.stack_pointer
        self.stack_pointer += 1
        
        if self.stack_pointer >= 512:
            # Stack overflow (max stack: 256-511)
            raise RuntimeError(f"Stack overflow: allocated {self.stack_pointer - self.STACK_BASE} stack slots")
        
        return addr
    
    def _compute_register_pressure(self) -> float:
        """Compute register pressure: % of time at full capacity"""
        if not self.intervals:
            return 0.0
        
        total_instructions = max(iv.end for iv in self.intervals.values()) + 1
        at_capacity_count = 0
        
        for i in range(total_instructions):
            # Count live intervals at instruction i
            live_count = sum(
                1 for iv in self.intervals.values()
                if iv.start <= i <= iv.end
            )
            if live_count >= len(self.PHYSICAL_REGISTERS):
                at_capacity_count += 1
        
        return at_capacity_count / total_instructions
    
    def print_allocation(self, allocation: AllocationMap) -> str:
        """Print allocation results"""
        return allocation.print_allocation()


def example_register_allocation():
    """Example: Register allocation for simple function"""
    from ir_types import IRBuilder, Constant, VariableValue
    
    # Build IR: function with 5 simultaneous values (needs spill)
    builder = IRBuilder("example", [])
    block = builder.new_block("entry")
    
    # a = 1, b = 2, c = 3, d = 4, e = 5
    builder.emit_assignment("a", Constant(1.0))
    builder.emit_assignment("b", Constant(2.0))
    builder.emit_assignment("c", Constant(3.0))
    builder.emit_assignment("d", Constant(4.0))
    builder.emit_assignment("e", Constant(5.0))
    
    # result = a + b + c + d + e (all live simultaneously)
    builder.emit_binary_op("add", "tmp1", VariableValue("a"), VariableValue("b"))
    builder.emit_binary_op("add", "tmp2", VariableValue("c"), VariableValue("d"))
    builder.emit_binary_op("add", "result", VariableValue("tmp1"), VariableValue("tmp2"))
    builder.emit_binary_op("add", "result", VariableValue("result"), VariableValue("e"))
    
    builder.emit_return(VariableValue("result"))
    
    func = builder.finalize()
    
    # Run liveness analysis
    analyzer = LivenessAnalyzer(func)
    intervals = analyzer.analyze()
    
    print(analyzer.print_analysis())
    print()
    
    # Run register allocation
    allocator = LinearScanAllocator(func, intervals)
    allocation = allocator.allocate()
    
    print(allocation.print_allocation())
    print()
    
    # Check results
    assert allocation.spill_count > 0, "Should have spilled some values"
    assert allocation.register_pressure > 0.5, "Should have high register pressure"
    
    print("Register allocation test PASSED")


if __name__ == "__main__":
    example_register_allocation()
