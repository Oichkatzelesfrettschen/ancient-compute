# Ancient Compute - Analytical Engine Tools Integration

from typing import Dict, Any, Optional
from ..emulator.analytical_engine import Engine
from ..emulator.adapter import AEMachineAdapter
from ..emulator.debugger import Debugger
from ..emulator.performance import PerformanceAnalyzer

class EngineTools:
    """
    Facade for interacting with Analytical Engine tools.
    """
    def __init__(self):
        self.engine = Engine()
        self.adapter = AEMachineAdapter(self.engine)
        self.debugger = Debugger(self.adapter)
        self.profiler = PerformanceAnalyzer(self.adapter)

    def load_program(self, ir_program: str):
        """Parse IR text and load into engine memory/instructions.

        Accepts Babbage assembly format (one instruction per line):
            OPCODE OPERAND1, OPERAND2, ...
        Lines starting with # are comments. Blank lines are skipped.
        Labels are lines ending with ':' and resolve to instruction indices.
        """
        from ..emulator.analytical_engine import Instruction

        self.engine.__init__()
        lines = ir_program.strip().splitlines()

        # First pass: resolve labels
        labels = {}
        instruction_lines = []
        for line in lines:
            stripped = line.strip()
            if not stripped or stripped.startswith("#"):
                continue
            if stripped.endswith(":"):
                labels[stripped[:-1]] = len(instruction_lines)
            else:
                instruction_lines.append(stripped)

        # Second pass: parse instructions
        for line in instruction_lines:
            parts = line.split(None, 1)
            opcode = parts[0].upper()
            operands = []
            if len(parts) > 1:
                raw_operands = parts[1].split(",")
                for op in raw_operands:
                    op = op.strip()
                    if op in labels:
                        operands.append(labels[op])
                    else:
                        try:
                            operands.append(int(op))
                        except ValueError:
                            operands.append(op)
            self.engine.instruction_cards.append(Instruction(opcode, operands))

    def step(self):
        """Execute one step with debugging and profiling."""
        # 1. Debugger step (checks breakpoints)
        triggered = self.debugger.step()
        
        # 2. Profiler record
        # Get the instruction that was just executed (or about to be)
        if self.engine.PC < len(self.engine.instruction_cards):
            instr = self.engine.instruction_cards[self.engine.PC]
            self.profiler.record_step(instr)
            
        return {
            "triggered_breakpoints": triggered,
            "state": self.debugger.get_state()
        }

    def get_performance_report(self):
        return self.profiler.get_report()
