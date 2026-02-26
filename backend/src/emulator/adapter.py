"""
Ancient Compute - Machine Adapters for Debugger

Provides a unified interface for the Debugger to interact with different
mechanical and analytical engines (DE2, AE, Curta).
"""

from abc import ABC, abstractmethod
from typing import Any

from .timing import MechanicalPhase


class MachineAdapter(ABC):
    @abstractmethod
    def get_cycle_count(self) -> int:
        pass

    @abstractmethod
    def get_current_phase(self) -> MechanicalPhase | None:
        pass

    @abstractmethod
    def get_column_values(self) -> list[int]:
        pass

    @abstractmethod
    def get_register_values(self) -> dict[str, Any]:
        pass

    @abstractmethod
    def get_memory_value(self, address: int) -> Any:
        pass

    @abstractmethod
    def step(self) -> None:
        """Execute one atomic step (cycle or instruction)."""
        pass

    @abstractmethod
    def get_snapshot(self) -> Any:
        pass

class DEMachineAdapter(MachineAdapter):
    def __init__(self, machine):
        self.machine = machine

    def get_cycle_count(self) -> int:
        return self.machine.cycle_count

    def get_current_phase(self) -> MechanicalPhase | None:
        return self.machine.timing.phase

    def get_column_values(self) -> list[int]:
        return self.machine.get_column_values()

    def get_register_values(self) -> dict[str, Any]:
        return {k: v.to_decimal() for k, v in self.machine.analytical_engine.registers.items()}

    def get_memory_value(self, address: int) -> Any:
        return self.machine.analytical_engine.memory[address].to_decimal()

    def step(self) -> None:
        self.machine.run_full_cycle()

    def get_snapshot(self) -> Any:
        return self.machine.get_snapshot()

class AEMachineAdapter(MachineAdapter):
    def __init__(self, engine):
        self.engine = engine

    def get_cycle_count(self) -> int:
        return self.engine.clock_time

    def get_current_phase(self) -> MechanicalPhase | None:
        if self.engine.barrels.active_barrel:
            return MechanicalPhase.ADDITION
        return None

    def get_column_values(self) -> list[int]:
        return []

    def get_register_values(self) -> dict[str, Any]:
        return {k: v.to_decimal() for k, v in self.engine.registers.items()}

    def get_memory_value(self, address: int) -> Any:
        return self.engine.memory[address].to_decimal()

    def step(self) -> None:
        self.engine.step_one_instruction()

    def get_snapshot(self) -> Any:
        return {
            "pc": self.engine.PC,
            "registers": self.get_register_values(),
            "flags": self.engine.flags,
            "clock_time": self.engine.clock_time,
            "barrel": {
                "active": self.engine.barrels.active_barrel,
                "step": self.engine.barrels.step_index
            },
            "mill_operand_buffer": self.engine.mill_operand_buffer.to_decimal(),
            "mill_result_buffer": self.engine.mill_result_buffer.to_decimal(),
            "active_store_address": self.engine.active_store_address
        }

class CurtaAdapter(MachineAdapter):
    def __init__(self, curta):
        self.curta = curta
        self.turns = 0

    def get_cycle_count(self) -> int:
        return self.turns

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        return self.curta.sliders

    def get_register_values(self) -> dict[str, Any]:
        return {
            "Result": self.curta.result_dial,
            "Counter": self.curta.counter_dial,
            "Carriage": self.curta.carriage_position
        }

    def get_memory_value(self, address: int) -> Any:
        return 0

    def step(self) -> None:
        self.curta.turn_crank()
        self.turns += 1

    def get_snapshot(self) -> Any:
        return {
            "result": self.curta.result_dial,
            "counter": self.curta.counter_dial,
            "sliders": self.curta.sliders,
            "mode": self.curta.crank_mode.name
        }
