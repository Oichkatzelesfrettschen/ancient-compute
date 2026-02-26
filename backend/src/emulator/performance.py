"""
Ancient Compute - Performance Analyzer

Analyzes execution metrics for Babbage Engines:
- Instruction frequency
- Cycle counts
- Bottleneck detection
- Optimization suggestions
"""

from dataclasses import dataclass, field
from typing import Any


@dataclass
class PerformanceMetrics:
    total_cycles: int = 0
    instruction_counts: dict[str, int] = field(default_factory=dict)
    memory_access_counts: dict[int, int] = field(default_factory=dict)
    branch_stats: dict[str, dict[str, int]] = field(default_factory=lambda: {"taken": 0, "not_taken": 0})

class PerformanceAnalyzer:
    def __init__(self, adapter):
        self.adapter = adapter
        self.metrics = PerformanceMetrics()
        self.history: list[dict[str, Any]] = []

    def record_step(self, instruction: Any | None = None):
        """Record metrics for a single execution step."""
        self.metrics.total_cycles = self.adapter.get_cycle_count()

        if instruction:
            op = getattr(instruction, "opcode", "UNKNOWN")
            self.metrics.instruction_counts[op] = self.metrics.instruction_counts.get(op, 0) + 1

            # Simplified: track memory if operands look like addresses
            for operand in getattr(instruction, "operands", []):
                if isinstance(operand, str) and operand.startswith('[') and operand.endswith(']'):
                    try:
                        addr = int(operand[1:-1])
                        self.metrics.memory_access_counts[addr] = self.metrics.memory_access_counts.get(addr, 0) + 1
                    except ValueError:
                        pass

    def get_report(self) -> dict[str, Any]:
        """Generate a performance report."""
        most_used_instr = None
        if self.metrics.instruction_counts:
            most_used_instr = max(self.metrics.instruction_counts, key=self.metrics.instruction_counts.get)

        hot_memory = []
        if self.metrics.memory_access_counts:
            sorted_mem = sorted(self.metrics.memory_access_counts.items(), key=lambda x: x[1], reverse=True)
            hot_memory = sorted_mem[:5]

        return {
            "total_cycles": self.metrics.total_cycles,
            "instruction_frequency": self.metrics.instruction_counts,
            "hot_spots": {
                "opcode": most_used_instr,
                "memory": hot_memory
            },
            "suggestions": self._generate_suggestions()
        }

    def _generate_suggestions(self) -> list[str]:
        suggestions = []
        # Basic bottleneck detection
        if self.metrics.instruction_counts.get('MULT', 0) > 10:
            suggestions.append("High MULT usage detected. Consider shift-and-add or pre-calculated tables.")
        if self.metrics.instruction_counts.get('DIV', 0) > 5:
            suggestions.append("High DIV usage detected. Division is expensive (750 cycles).")

        # Memory bottleneck
        for addr, count in self.metrics.memory_access_counts.items():
            if count > self.metrics.total_cycles * 0.5:
                suggestions.append(f"Memory address [{addr}] is a bottleneck (accessed in >50% of cycles).")

        return suggestions
