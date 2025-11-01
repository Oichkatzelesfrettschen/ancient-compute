"""
Liveness Analysis for IR

Computes which IR values are simultaneously live (needed for register allocation).

Key concepts:
- Live range: interval from value definition to last use
- Liveness: which values are simultaneously live at each instruction
- Conflict graph: which values cannot share registers
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Dict, Set, List, Tuple, Optional
from ir_types import (
    Function, BasicBlock, Instruction, Operand,
    Assignment, BinaryOp, Load, Store, Call, Return,
    VariableValue, RegisterValue, Constant, MemoryValue,
    BranchTerminator, JumpTerminator, ReturnTerminator,
)


@dataclass
class LiveInterval:
    """Live interval for a single IR value"""
    name: str                    # Variable/value name
    start: int                   # First instruction where live
    end: int                     # Last instruction where live
    definitions: Set[int] = field(default_factory=set)  # Instruction indices where defined
    uses: Set[int] = field(default_factory=set)         # Instruction indices where used
    
    def overlaps_with(self, other: LiveInterval) -> bool:
        """Check if two intervals overlap"""
        return not (self.end < other.start or other.end < self.start)
    
    def __repr__(self) -> str:
        return f"LiveInterval({self.name}, {self.start}-{self.end})"


class LivenessAnalyzer:
    """
    Analyzes liveness for IR function.
    
    Algorithm:
    1. Linear scan through instructions
    2. For each instruction, identify definitions and uses
    3. Compute interval for each variable (first def to last use)
    4. Build conflict map (which variables live simultaneously)
    """
    
    def __init__(self, function: Function):
        self.function = function
        self.intervals: Dict[str, LiveInterval] = {}
        self.definitions: Dict[str, int] = {}  # Variable name → instruction index where defined
        self.uses: Dict[str, List[int]] = {}   # Variable name → list of instruction indices where used
        self.instr_index = 0                   # Current instruction index
        self.parameters_live = True            # Parameters live from function entry
    
    def analyze(self) -> Dict[str, LiveInterval]:
        """Run liveness analysis, return live intervals"""
        # Initialize: all parameters are live from entry
        for param in self.function.parameters:
            self.uses[param] = []
        
        # Scan through all basic blocks and instructions
        for block in self.function.basic_blocks:
            for instr in block.instructions:
                self._analyze_instruction(instr)
            
            # Analyze terminator
            if block.terminator:
                self._analyze_terminator(block.terminator)
        
        # Compute intervals: from definition (or parameter) to last use
        self._compute_intervals()
        
        return self.intervals
    
    def _analyze_instruction(self, instr: Instruction) -> None:
        """Analyze single instruction for definitions and uses"""
        if isinstance(instr, Assignment):
            # target = source
            self.definitions[instr.target] = self.instr_index
            self._use_operand(instr.source)
        
        elif isinstance(instr, BinaryOp):
            # target = op operand1, operand2
            self.definitions[instr.target] = self.instr_index
            self._use_operand(instr.operand1)
            if instr.operand2:
                self._use_operand(instr.operand2)
        
        elif isinstance(instr, Load):
            # target = load address
            self.definitions[instr.target] = self.instr_index
            self._use_operand(instr.address)
        
        elif isinstance(instr, Store):
            # store value, address
            self._use_operand(instr.value)
            self._use_operand(instr.address)
        
        elif isinstance(instr, Call):
            # target = call func, args
            if instr.target:
                self.definitions[instr.target] = self.instr_index
            for arg in instr.arguments:
                self._use_operand(arg)
        
        elif isinstance(instr, Return):
            # return value
            if instr.value:
                self._use_operand(instr.value)
        
        self.instr_index += 1
    
    def _analyze_terminator(self, term) -> None:
        """Analyze terminator for uses"""
        if isinstance(term, BranchTerminator):
            self._use_operand(term.operand1)
            if term.operand2:
                self._use_operand(term.operand2)
        elif isinstance(term, ReturnTerminator):
            if term.value:
                self._use_operand(term.value)
    
    def _use_operand(self, operand: Operand) -> None:
        """Record use of operand"""
        if isinstance(operand, VariableValue):
            if operand.name not in self.uses:
                self.uses[operand.name] = []
            self.uses[operand.name].append(self.instr_index)
        # Ignore constants, registers, memory (not IR values that need allocation)
    
    def _compute_intervals(self) -> None:
        """Compute live intervals from definitions and uses"""
        all_vars = set(self.definitions.keys()) | set(self.uses.keys())
        
        for var in all_vars:
            if var in self.function.parameters:
                # Parameter: live from start (instruction 0)
                start = 0
            elif var in self.definitions:
                # Normal variable: live from definition
                start = self.definitions[var]
            else:
                # Used but not defined: error (shouldn't happen in valid IR)
                continue
            
            # End: last use (or definition if no uses)
            if var in self.uses and self.uses[var]:
                end = max(self.uses[var])
            else:
                end = start
            
            self.intervals[var] = LiveInterval(
                name=var,
                start=start,
                end=end,
                definitions=set([self.definitions.get(var, start)]) if var in self.definitions else set(),
                uses=set(self.uses.get(var, [])),
            )
    
    def get_interval(self, var_name: str) -> Optional[LiveInterval]:
        """Get live interval for variable"""
        return self.intervals.get(var_name)
    
    def get_conflict_set(self, var_name: str) -> Set[str]:
        """Get all variables that live simultaneously with given variable"""
        if var_name not in self.intervals:
            return set()
        
        interval = self.intervals[var_name]
        conflicts = set()
        
        for other_name, other_interval in self.intervals.items():
            if other_name != var_name and interval.overlaps_with(other_interval):
                conflicts.add(other_name)
        
        return conflicts
    
    def build_conflict_graph(self) -> Dict[str, Set[str]]:
        """Build conflict graph: var → set of conflicting vars"""
        conflict_graph: Dict[str, Set[str]] = {}
        
        for var_name in self.intervals:
            conflict_graph[var_name] = self.get_conflict_set(var_name)
        
        return conflict_graph
    
    def get_simultaneous_liveness(self, instr_index: int) -> Set[str]:
        """Get all variables live at specific instruction"""
        live_at_instr: Set[str] = set()
        
        for var_name, interval in self.intervals.items():
            if interval.start <= instr_index <= interval.end:
                live_at_instr.add(var_name)
        
        return live_at_instr
    
    def max_simultaneous_liveness(self) -> int:
        """Get maximum number of simultaneously live variables"""
        max_live = 0
        
        for instr_idx in range(self.instr_index):
            live_at_instr = self.get_simultaneous_liveness(instr_idx)
            max_live = max(max_live, len(live_at_instr))
        
        return max_live
    
    def get_sorted_intervals(self) -> List[LiveInterval]:
        """Get all intervals sorted by start position (for linear scan)"""
        intervals = list(self.intervals.values())
        intervals.sort(key=lambda iv: (iv.start, iv.end))
        return intervals
    
    def print_analysis(self) -> str:
        """Print human-readable liveness analysis"""
        lines = ["=== Liveness Analysis ===\n"]
        
        lines.append("Live Intervals:")
        for interval in self.get_sorted_intervals():
            lines.append(
                f"  {interval.name:20} [{interval.start:3}-{interval.end:3}] "
                f"(defs: {interval.definitions}, uses: {interval.uses})"
            )
        
        lines.append(f"\nMax simultaneous: {self.max_simultaneous_liveness()} variables")
        
        lines.append("\nConflict Graph:")
        conflict_graph = self.build_conflict_graph()
        for var_name in sorted(conflict_graph.keys()):
            conflicts = conflict_graph[var_name]
            lines.append(f"  {var_name:20} conflicts with: {', '.join(sorted(conflicts))}")
        
        return "\n".join(lines)


def example_liveness_analysis():
    """Example: Liveness analysis for simple function"""
    from ir_types import IRBuilder, Constant, VariableValue
    
    # Build IR: simple function with 3 values
    builder = IRBuilder("example", ["n"])
    block = builder.new_block("entry")
    
    # a = 10
    builder.emit_assignment("a", Constant(10.0))
    # b = n + a
    builder.emit_binary_op("add", "b", VariableValue("n"), VariableValue("a"))
    # c = a * 2
    builder.emit_binary_op("mul", "c", VariableValue("a"), Constant(2.0))
    # return b + c
    builder.emit_return(VariableValue("b"))
    
    func = builder.finalize()
    
    # Run liveness analysis
    analyzer = LivenessAnalyzer(func)
    intervals = analyzer.analyze()
    
    print(analyzer.print_analysis())
    
    # Check results
    assert "n" in intervals
    assert "a" in intervals
    assert "b" in intervals
    assert "c" in intervals
    
    print("\nLiveness analysis test PASSED")


if __name__ == "__main__":
    example_liveness_analysis()
