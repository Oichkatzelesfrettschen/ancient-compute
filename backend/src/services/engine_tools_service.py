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
        # STUB: Parse IR and load into engine memory/instructions
        # For now, just reset
        self.engine.__init__() 
        # In a real implementation, we'd use an Assembler to convert IR to AE instructions
        pass

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
