"""
Ancient Compute - Machine Adapters for Debugger

Provides a unified interface for the Debugger to interact with different
mechanical and analytical engines (DE2, AE, Curta).
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any

from .types import MechanicalPhase

if TYPE_CHECKING:
    from .analytical_engine import Engine
    from .enigma import EnigmaMachine
    from .leibniz_reckoner import LeibnizReckonerEmulator
    from .napiers_bones import NapiersBones
    from .pascaline import PascalineEmulator


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
    def __init__(self, machine: Any) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return int(self.machine.cycle_count)

    def get_current_phase(self) -> MechanicalPhase | None:
        return self.machine.timing.phase  # type: ignore[no-any-return]

    def get_column_values(self) -> list[int]:
        return [int(v) for v in self.machine.get_column_values()]

    def get_register_values(self) -> dict[str, Any]:
        return {k: v.to_decimal() for k, v in self.machine.analytical_engine.registers.items()}

    def get_memory_value(self, address: int) -> Any:
        return self.machine.analytical_engine.memory[address].to_decimal()

    def step(self) -> None:
        self.machine.run_full_cycle()

    def get_snapshot(self) -> Any:
        return self.machine.get_snapshot()


class AEMachineAdapter(MachineAdapter):
    def __init__(self, engine: Engine) -> None:
        self.engine = engine

    def get_cycle_count(self) -> int:
        return self.engine.clock_time

    def get_current_phase(self) -> MechanicalPhase | None:
        if self.engine.barrels.active_barrel:
            return MechanicalPhase.ADDITION
        return None

    def get_column_values(self) -> list[int]:
        return [int(col.to_decimal()) for col in self.engine.memory[:50]]

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
                "step": self.engine.barrels.step_index,
            },
            "mill_operand_buffer": self.engine.mill_operand_buffer.to_decimal(),
            "mill_result_buffer": self.engine.mill_result_buffer.to_decimal(),
            "active_store_address": self.engine.active_store_address,
        }


class ScheutzAdapter(MachineAdapter):
    """Adapter for ScheutzDifferenceEngine."""

    def __init__(self, machine: Any) -> None:
        self.machine = machine
        self._cranks: int = 0

    def get_cycle_count(self) -> int:
        return self._cranks

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        return [int(r) for r in self.machine.state.registers]

    def get_register_values(self) -> dict[str, Any]:
        return {
            f"D{i}": float(self.machine.state.registers[i])
            for i in range(len(self.machine.state.registers))
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < len(self.machine.state.registers):
            return float(self.machine.state.registers[address])
        return 0

    def step(self) -> None:
        self.machine.crank()
        self._cranks += 1

    def get_snapshot(self) -> Any:
        return {
            "registers": self.get_register_values(),
            "cranks": self._cranks,
            "cycle_count": self.machine.state.cycle_count,
        }


class LudgateAdapter(MachineAdapter):
    """Adapter for LudgateMachine.

    One step = one perforated-cylinder advance (program_pointer += 1, cycle_count += 1).
    Arithmetic operations are triggered explicitly via machine.add/multiply/etc.
    """

    def __init__(self, machine: Any) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return int(self.machine.state.cycle_count)

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Return first 32 store columns (192 total; expose a visible window).
        return [int(v) for v in self.machine.state.store[:32]]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "accumulator": self.machine.state.accumulator,
            "program_pointer": self.machine.state.program_pointer,
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < len(self.machine.state.store):
            return self.machine.state.store[address]
        return 0

    def step(self) -> None:
        # Advances the perforated cylinder one position (Ludgate's control mechanism).
        self.machine.step()

    def get_snapshot(self) -> Any:
        return {
            "accumulator": self.machine.state.accumulator,
            "program_pointer": self.machine.state.program_pointer,
            "cycle_count": self.machine.state.cycle_count,
            "output_tape": list(self.machine.state.output_tape),
        }


class TorresQuevedoAdapter(MachineAdapter):
    """Adapter for TorresQuevedo electromechanical calculator (Spain, 1914-1920).

    One step = one relay-cycle advance (program_pointer += 1, cycle_count += 1).
    Arithmetic is triggered explicitly via machine.add/subtract/multiply/divide.
    """

    def __init__(self, machine: Any) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return int(self.machine.state.cycle_count)

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Torres uses relay registers, not visible columns; expose register mantissas.
        return [int(abs(r.to_float() * 1e6)) % (10**8) for r in self.machine.state.registers]

    def get_register_values(self) -> dict[str, Any]:
        return {
            f"R{i}": self.machine.state.registers[i].to_float()
            for i in range(len(self.machine.state.registers))
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < len(self.machine.state.registers):
            return self.machine.state.registers[address].to_float()
        return 0.0

    def step(self) -> None:
        # Advances the relay-cycle program pointer (Torres's sequencing mechanism).
        self.machine.step()

    def get_snapshot(self) -> Any:
        return {
            "registers": self.get_register_values(),
            "program_pointer": self.machine.state.program_pointer,
            "cycle_count": self.machine.state.cycle_count,
            "typewriter_output": list(self.machine.state.typewriter_output),
        }


class ZuseZ1Adapter(MachineAdapter):
    """Adapter for ZuseZ1."""

    def __init__(self, machine: Any) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return int(self.machine.state.program_counter)

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        return []

    def get_register_values(self) -> dict[str, Any]:
        return {"accumulator": self.machine.state.accumulator.to_float()}

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < len(self.machine.state.memory):
            return self.machine.state.memory[address].to_float()
        return 0.0

    def step(self) -> None:
        self.machine.step()

    def get_snapshot(self) -> Any:
        return {
            "pc": self.machine.state.program_counter,
            "accumulator": self.machine.state.accumulator.to_float(),
            "cycle_count": self.machine.state.cycle_count,
        }


class CurtaAdapter(MachineAdapter):
    def __init__(self, curta: Any) -> None:
        self.curta = curta
        self.turns: int = 0

    def get_cycle_count(self) -> int:
        return self.turns

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        return [int(v) for v in self.curta.sliders]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "Result": self.curta.result_dial,
            "Counter": self.curta.counter_dial,
            "Carriage": self.curta.carriage_position,
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
            "mode": self.curta.crank_mode.name,
        }


class PascalineAdapter(MachineAdapter):
    """Adapter for PascalineEmulator (France, 1642).

    One step = one digit-wheel turn on the units wheel (rotate_input_wheel(1)).
    The Pascaline has no program counter; step() advances the units carry chain.
    """

    def __init__(self, machine: PascalineEmulator) -> None:
        self.machine = machine
        self._turns = 0

    def get_cycle_count(self) -> int:
        return self._turns

    def get_current_phase(self) -> MechanicalPhase | None:
        return (
            MechanicalPhase.ADDITION if any(w.sautoir_lifted for w in self.machine.wheels) else None
        )

    def get_column_values(self) -> list[int]:
        return [w.value for w in self.machine.wheels]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "value": self.machine.get_value(),
            "nines_complement_mode": self.machine.nines_complement_mode,
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < len(self.machine.wheels):
            return self.machine.wheels[address].value
        return 0

    def step(self) -> None:
        # One turn of the Pascaline's input handle: adds 1 to the units wheel.
        # The sautoir mechanism propagates carry automatically.
        self.machine.rotate_input_wheel(1)
        self._turns += 1

    def get_snapshot(self) -> Any:
        return {
            "value": self.machine.get_value(),
            "digits": [w.value for w in self.machine.wheels],
            "sautoirs_lifted": [w.sautoir_lifted for w in self.machine.wheels],
            "nines_complement_mode": self.machine.nines_complement_mode,
            "turns": self._turns,
        }


class LeibnizAdapter(MachineAdapter):
    """Adapter for LeibnizReckonerEmulator (Germany, 1673).

    One step = one crank turn (crank_turn(1)).
    The stepped drums add the input value to the accumulator each turn.
    """

    def __init__(self, machine: LeibnizReckonerEmulator) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return self.machine.turn_counter

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose input drums (Staffelwalze tooth counts) as visible columns.
        return [d.value for d in self.machine.input_drums]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "accumulator": self.machine.get_accumulator_value(),
            "carriage_position": self.machine.carriage_position,
            "turn_counter": self.machine.turn_counter,
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < len(self.machine.accumulator_wheels):
            return self.machine.accumulator_wheels[address].value
        return 0

    def step(self) -> None:
        # One revolution of the main crank: stepped drums engage accumulator.
        self.machine.crank_turn(1)

    def get_snapshot(self) -> Any:
        return {
            "accumulator": self.machine.get_accumulator_value(),
            "accumulator_digits": [w.value for w in self.machine.accumulator_wheels],
            "input_drums": [d.value for d in self.machine.input_drums],
            "carriage_position": self.machine.carriage_position,
            "turn_counter": self.machine.turn_counter,
        }


class EnigmaAdapter(MachineAdapter):
    """Adapter for EnigmaMachine (Germany, 1918-1945).

    step() enciphers the next character from the loaded input tape.
    load_input(text) queues characters for step-by-step processing.
    output_tape accumulates enciphered characters.
    """

    def __init__(self, machine: EnigmaMachine) -> None:
        self.machine = machine
        self._input_tape: list[str] = []
        self._output_tape: list[str] = []
        self._steps = 0

    def load_input(self, text: str) -> None:
        """Queue plaintext (or ciphertext) characters for encipherment."""
        self._input_tape = [c for c in text.upper() if c.isalpha()]

    def get_cycle_count(self) -> int:
        return self._steps

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose current rotor positions as column values (0-25).
        return [r.position for r in self.machine.rotors]

    def get_register_values(self) -> dict[str, Any]:
        return {f"R{i}": self.machine.rotors[i].position for i in range(len(self.machine.rotors))}

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < len(self.machine.rotors):
            return self.machine.rotors[address].position
        return 0

    def step(self) -> None:
        """Encipher one character from the input tape."""
        if not self._input_tape:
            return
        char = self._input_tape.pop(0)
        out = self.machine.encipher_char(char)
        self._output_tape.append(out)
        self._steps += 1

    def get_output(self) -> str:
        """Return all enciphered output so far as a string."""
        return "".join(self._output_tape)

    def get_snapshot(self) -> Any:
        return {
            "rotor_positions": [r.position for r in self.machine.rotors],
            "input_remaining": len(self._input_tape),
            "output_tape": "".join(self._output_tape),
            "steps": self._steps,
        }


class NapierAdapter(MachineAdapter):
    """Adapter for NapiersBones (Scotland, 1617).

    Napier's Bones are a manual lookup aid, not a state machine.
    step() performs one diagonal-addition pass on the currently loaded number.
    cycle_count reflects how many multiply-by-single-digit operations were done.
    """

    def __init__(self, machine: NapiersBones) -> None:
        self.machine = machine
        self._operations = 0
        self._last_result = 0

    def get_cycle_count(self) -> int:
        return self._operations

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose the digit of each active bone as its column value.
        return [b.digit for b in self.machine.active_bones]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "last_result": self._last_result,
            "active_bones": len(self.machine.active_bones),
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < len(self.machine.active_bones):
            return self.machine.active_bones[address].digit
        return 0

    def step(self) -> None:
        # One multiplication pass: multiply loaded number by (operations+1) mod 9 + 1.
        # This lets the debugger cycle through multiplier values 1-9 repeatedly.
        multiplier = (self._operations % 9) + 1
        self._last_result = self.machine.multiply_single_digit(multiplier)
        self._operations += 1

    def get_snapshot(self) -> Any:
        return {
            "active_bones": [b.digit for b in self.machine.active_bones],
            "last_result": self._last_result,
            "operations": self._operations,
        }
