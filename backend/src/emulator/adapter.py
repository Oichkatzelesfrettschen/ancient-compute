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
    from .abacus import AbacusEmulator
    from .analytical_engine import Engine
    from .antikythera import AntikytheraMechanism
    from .astrolabe import AstrolabeEmulator
    from .bombe import Bombe, BombeMenu
    from .clay_tokens import ClayTokensEmulator
    from .colossus import Colossus
    from .edsac import EDSAC
    from .eniac import ENIAC
    from .enigma import EnigmaMachine
    from .grant_difference_engine import GrantDifferenceEngine
    from .harvard_mark_i import HarvardMarkI
    from .hollerith_tabulator import HollerithTabulator
    from .jacquard import JacquardLoom
    from .leibniz_reckoner import LeibnizReckonerEmulator
    from .manchester_baby import ManchesterBaby
    from .millionaire_calculator import MillionaireCalculator
    from .napiers_bones import NapiersBones
    from .odhner_arithmometer import OdhnerArithmometer
    from .pascaline import PascalineEmulator
    from .quipu import QuipuEmulator
    from .slide_rule import SlideRuleEmulator
    from .tally_marks import TallyMarksEmulator
    from .thomas_arithmometer import ThomasArithmometer
    from .zuse_z3 import ZuseZ3


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

    def get_operation_time_ms(self) -> dict[str, float]:
        """Return historical real-world milliseconds per operation type.

        Returns empty dict for machines with no known historical timing.
        Subclasses override to return machine-specific timing constants.
        """
        return {}


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
    _TIMING_MS: dict[str, float] = {"add": 250.0, "multiply": 2000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

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

    _TIMING_MS: dict[str, float] = {"tabulate": 2000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

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

    _TIMING_MS: dict[str, float] = {"multiply": 10000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

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

    _TIMING_MS: dict[str, float] = {"add": 1000.0, "multiply": 5000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

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

    _TIMING_MS: dict[str, float] = {"add": 1000.0, "multiply": 3000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

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
    _TIMING_MS: dict[str, float] = {"add": 300.0, "multiply": 2000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

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

    _TIMING_MS: dict[str, float] = {"add": 500.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

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

    _TIMING_MS: dict[str, float] = {"add": 800.0, "multiply": 5000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

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

    _TIMING_MS: dict[str, float] = {"encipher_char": 300.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

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

    _TIMING_MS: dict[str, float] = {"multiply": 15000.0}
    _A68_SOURCE: str = str(
        __import__("pathlib").Path(__file__).resolve().parents[3]
        / "tools"
        / "algol68"
        / "napiers_bones.a68"
    )

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

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

    def execute_algol68_ops(
        self, operations: list[dict[str, Any]]
    ) -> dict[str, Any]:
        """Run Napier's Bones operations through the ALGOL68 napiers_bones.a68 backend.

        Supported operation shapes:
          {"op": "set_multiplicand", "value": <int>}  -- set bones for a number
          {"op": "multiply", "digit": <1-9>}          -- multiply by single digit
          {"op": "reset"}                              -- clear bones and result
          {"op": "print"}                              -- emit current state (opcode 3)

        Returns {"result": int, "multiplicand": int, "last_multiplier": int}.
        Raises RuntimeError if a68g is unavailable or returns a non-zero exit code.
        """
        import subprocess

        _OP_MAP = {
            "set_multiplicand": lambda d: f"1 {int(d.get('value', 0))}",
            "multiply": lambda d: f"2 {int(d.get('digit', 1))}",
            "reset": lambda _: "4",
            "print": lambda _: "3",
        }
        lines: list[str] = []
        for op_dict in operations:
            op = str(op_dict.get("op", "")).lower()
            encoder = _OP_MAP.get(op)
            if encoder is not None:
                lines.append(encoder(op_dict))
        lines.append("0")  # print final state + quit
        stdin_data = "\n".join(lines) + "\n"

        try:
            proc = subprocess.run(
                ["/usr/bin/a68g", self._A68_SOURCE],
                input=stdin_data,
                capture_output=True,
                text=True,
                timeout=5,
            )
        except FileNotFoundError as exc:
            raise RuntimeError("a68g not found at /usr/bin/a68g") from exc
        except subprocess.TimeoutExpired as exc:
            raise RuntimeError("ALGOL68 napiers_bones timed out") from exc

        if proc.returncode != 0:
            raise RuntimeError(f"ALGOL68 napiers_bones error: {proc.stderr.strip()}")

        parsed = _parse_napier_a68_output(proc.stdout)
        # Sync the Python emulator so step() calls remain consistent
        if parsed["multiplicand"] > 0:
            self.machine.load_number(parsed["multiplicand"])
        self._last_result = parsed["result"]
        self._operations += len(operations)
        return parsed


def _parse_napier_a68_output(output: str) -> dict[str, Any]:
    """Parse napiers_bones.a68 print_state output.

    Expected format (last block wins if PRINT opcode was used multiple times):
      result=<int>
      multiplicand=<int>
      last_multiplier=<int>

    Returns {"result": int, "multiplicand": int, "last_multiplier": int}.
    """
    import contextlib

    result = 0
    multiplicand = 0
    last_multiplier = 0
    for line in output.splitlines():
        if line.startswith("result="):
            with contextlib.suppress(ValueError):
                result = int(line.split("=", 1)[1].strip())
        elif line.startswith("multiplicand="):
            with contextlib.suppress(ValueError):
                multiplicand = int(line.split("=", 1)[1].strip())
        elif line.startswith("last_multiplier="):
            with contextlib.suppress(ValueError):
                last_multiplier = int(line.split("=", 1)[1].strip())
    return {"result": result, "multiplicand": multiplicand, "last_multiplier": last_multiplier}


class ThomasArithometerAdapter(MachineAdapter):
    """Adapter for ThomasArithmometer (France, 1820).

    One step = one crank turn. step() calls turn_crank() in the current mode
    (ADD or SUBTRACT). The result dial and counter are exposed as registers.
    """

    _TIMING_MS: dict[str, float] = {"add": 500.0, "multiply": 3000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: ThomasArithmometer) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return self.machine.get_counter()

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose result digits as column values (units -> MSD).
        result = self.machine.get_result()
        return [(result // (10**i)) % 10 for i in range(16)]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "result": self.machine.get_result(),
            "counter": self.machine.get_counter(),
        }

    def get_memory_value(self, address: int) -> Any:
        return 0

    def step(self) -> None:
        self.machine.turn_crank()

    def get_snapshot(self) -> Any:
        return {
            "result": self.machine.get_result(),
            "counter": self.machine.get_counter(),
        }


class OdhnerAdapter(MachineAdapter):
    """Adapter for OdhnerArithmometer (Sweden/Russia, 1878).

    One step = one crank turn. The pinwheel mechanism rotates once per step,
    adding or subtracting the input value from the result register.
    """

    _TIMING_MS: dict[str, float] = {"add": 400.0, "multiply": 2500.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: OdhnerArithmometer) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return self.machine.counter.get()

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose result register digits as column values.
        return list(self.machine.result._digits)

    def get_register_values(self) -> dict[str, Any]:
        return {
            "result": self.machine.result.get(),
            "counter": self.machine.counter.get(),
            "direction": self.machine.direction.value,
        }

    def get_memory_value(self, address: int) -> Any:
        digits = self.machine.result._digits
        if 0 <= address < len(digits):
            return digits[address]
        return 0

    def step(self) -> None:
        self.machine.turn_crank()

    def get_snapshot(self) -> Any:
        return {
            "result": self.machine.result.get(),
            "counter": self.machine.counter.get(),
            "input": [pw.value for pw in self.machine.pinwheels],
            "carriage_position": self.machine.carriage_position,
        }


class GrantDEAdapter(MachineAdapter):
    """Adapter for GrantDifferenceEngine (USA, 1876).

    One step = one crank turn (crank()). Registers D0..D_n hold tabulated
    polynomial differences; D0 is the current function value.
    """

    _TIMING_MS: dict[str, float] = {"tabulate": 2500.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: GrantDifferenceEngine) -> None:
        self.machine = machine
        self._cranks = 0

    def get_cycle_count(self) -> int:
        return self._cranks

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        return [int(self.machine.get_register(i)) for i in range(len(self.machine.state.registers))]

    def get_register_values(self) -> dict[str, Any]:
        return {
            f"D{i}": float(self.machine.get_register(i))
            for i in range(len(self.machine.state.registers))
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < len(self.machine.state.registers):
            return float(self.machine.get_register(address))
        return 0.0

    def step(self) -> None:
        self.machine.crank()
        self._cranks += 1

    def get_snapshot(self) -> Any:
        return {
            "registers": self.get_register_values(),
            "cranks": self._cranks,
        }


class HollerithAdapter(MachineAdapter):
    """Adapter for HollerithTabulator (USA, 1890).

    One step = read one punched card from the loaded deck. The counters
    increment as holes are detected; the counter grid is exposed as memory.
    """

    _TIMING_MS: dict[str, float] = {"card_read": 750.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: HollerithTabulator) -> None:
        self.machine = machine
        self._cards_read = 0
        self._deck: list[Any] = []

    def load_deck(self, cards: list[Any]) -> None:
        """Load a deck of PunchedCard objects for step-by-step processing."""
        self._deck = list(cards)
        self._cards_read = 0

    def get_cycle_count(self) -> int:
        return self._cards_read

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose counter (0,c) values for columns 0..11 as visible columns.
        return [self.machine.counter(0, c).value for c in range(12)]

    def get_register_values(self) -> dict[str, Any]:
        report = self.machine.report()
        return {
            "cards_read": self._cards_read,
            "total_holes": report.get("total_holes_counted", 0),
        }

    def get_memory_value(self, address: int) -> Any:
        row, col = divmod(address, 12)
        return self.machine.counter(row % 12, col % 12).value

    def step(self) -> None:
        if self._cards_read < len(self._deck):
            self.machine.read_card(self._deck[self._cards_read])
            self._cards_read += 1

    def get_snapshot(self) -> Any:
        return {
            "cards_read": self._cards_read,
            "deck_size": len(self._deck),
            "report": self.machine.report(),
        }


class MillionaireAdapter(MachineAdapter):
    """Adapter for MillionaireCalculator (Switzerland, 1893).

    One step = one crank turn. The Millionaire uses a pre-computed
    multiplication table; each crank adds multiplier_lever * input to result.
    """

    _TIMING_MS: dict[str, float] = {"multiply": 1000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: MillionaireCalculator) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return self.machine.get_counter()

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        result = self.machine.get_result()
        return [(result // (10**i)) % 10 for i in range(16)]

    def get_register_values(self) -> dict[str, Any]:
        state = self.machine.state()
        return {
            "result": state["result"],
            "counter": state["counter"],
            "multiplier_lever": state["multiplier_lever"],
        }

    def get_memory_value(self, address: int) -> Any:
        return 0

    def step(self) -> None:
        self.machine.turn_crank()

    def get_snapshot(self) -> Any:
        return dict(self.machine.state())


class AntikytheraAdapter(MachineAdapter):
    """Adapter for AntikytheraMechanism (Rhodes, ~100 BCE).

    One step = advance the input shaft by 1/365 of a year (one solar day).
    All dial pointer positions are exposed as registers.
    """

    _STEP_SIZE = 1.0 / 365.25  # one solar day
    _TIMING_MS: dict[str, float] = {"step": 86400000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: AntikytheraMechanism) -> None:
        self.machine = machine
        self._date: float = 0.0
        self._steps = 0

    def get_cycle_count(self) -> int:
        return self._steps

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose gear angles as integer degrees (0-359) for 8 key gears.
        key_gears = ["b1", "b2", "e3", "e4", "k1", "k2", "SA4", "LN2"]
        return [
            int(self.machine.gears[g].angle * 180 / 3.14159265) % 360
            for g in key_gears
            if g in self.machine.gears
        ]

    def get_register_values(self) -> dict[str, Any]:
        return dict(self.machine.pointers)

    def get_memory_value(self, address: int) -> Any:
        keys = sorted(self.machine.pointers.keys())
        if 0 <= address < len(keys):
            return self.machine.pointers[keys[address]]
        return 0.0

    def step(self) -> None:
        self._date += self._STEP_SIZE
        self.machine.set_input_date(self._date)
        self._steps += 1

    def get_snapshot(self) -> Any:
        return {
            "date_years": self._date,
            "steps": self._steps,
            "pointers": dict(self.machine.pointers),
        }


class BombeAdapter(MachineAdapter):
    """Adapter for Bombe (Bletchley Park, 1940).

    One step = test one rotor position (L, M, R) with the current menu.
    Positions advance through 26^3 combinations sequentially (R fastest).
    Stops (candidate solutions) are accumulated and accessible via get_stops().
    """

    _TIMING_MS: dict[str, float] = {"test_position": 1.5}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: Bombe, menu: BombeMenu) -> None:
        self.machine = machine
        self.menu = menu
        self._pos = 0  # linear index 0..26^3-1
        self._stops: list[Any] = []

    def get_cycle_count(self) -> int:
        return self._pos

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Current (L, M, R) rotor positions as 0-25.
        L = (self._pos // 676) % 26
        M = (self._pos // 26) % 26
        R = self._pos % 26
        return [L, M, R]

    def get_register_values(self) -> dict[str, Any]:
        L, M, R = self.get_column_values()
        return {
            "L": L,
            "M": M,
            "R": R,
            "stops_found": len(self._stops),
            "positions_tested": self._pos,
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < len(self._stops):
            return self._stops[address]
        return None

    def step(self) -> None:
        if self._pos >= 26**3:
            return
        L = (self._pos // 676) % 26
        M = (self._pos // 26) % 26
        R = self._pos % 26
        stop = self.machine.run_single(self.menu, L, M, R)
        if stop is not None:
            self._stops.append(stop)
        self._pos += 1

    def get_stops(self) -> list[Any]:
        return list(self._stops)

    def get_snapshot(self) -> Any:
        L, M, R = self.get_column_values()
        return {
            "position": {"L": L, "M": M, "R": R},
            "positions_tested": self._pos,
            "stops": [s.positions_str() for s in self._stops],
        }


class ZuseZ3Adapter(MachineAdapter):
    """Adapter for ZuseZ3 (Germany, 1941).

    One step = execute one Z3 instruction. Memory[0..63] holds 22-bit
    floating-point words; the accumulator is exposed as a register.
    """

    _TIMING_MS: dict[str, float] = {"add": 280.0, "multiply": 3200.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: ZuseZ3) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return self.machine.state.cycle_count

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose first 32 memory slots as integer column values.
        return [int(self.machine.get_memory(i)) for i in range(32)]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "accumulator": self.machine.get_accumulator(),
            "pc": self.machine.state.program_counter,
            "halted": self.machine.state.halted,
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < 64:
            return self.machine.get_memory(address)
        return 0.0

    def step(self) -> None:
        self.machine.step()

    def get_snapshot(self) -> Any:
        return self.machine.state_snapshot()


class ColossusAdapter(MachineAdapter):
    """Adapter for Colossus (Bletchley Park, 1943).

    One step = advance the paper tape by one position (one XOR-and-count
    cycle). The chi-break count accumulators are exposed as registers.
    The adapter wraps the internal Lorenz wheel state for column display.
    """

    _TIMING_MS: dict[str, float] = {"char_read": 0.2}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: Colossus) -> None:
        self.machine = machine
        self._tape_pos = 0
        self._chi_counts: dict[int, int] = {}
        self._tape: list[list[int]] = []

    def load_tape(self, tape: list[list[int]]) -> None:
        """Load a Lorenz-encoded tape (list of 5-bit vectors)."""
        self._tape = tape
        self._tape_pos = 0
        self._chi_counts = {}

    def get_cycle_count(self) -> int:
        return self._tape_pos

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Current chi-wheel positions (5 chi wheels, each 0 or 1).
        return list(self.machine.lorenz.current_chi())

    def get_register_values(self) -> dict[str, Any]:
        return {
            "tape_position": self._tape_pos,
            "tape_length": len(self._tape),
            "chi_counts_max": max(self._chi_counts.values(), default=0),
        }

    def get_memory_value(self, address: int) -> Any:
        return self._chi_counts.get(address, 0)

    def step(self) -> None:
        if self._tape_pos < len(self._tape):
            self.machine.lorenz._step()
            self._tape_pos += 1

    def get_snapshot(self) -> Any:
        return {
            "tape_position": self._tape_pos,
            "tape_length": len(self._tape),
            "chi_wheel_positions": list(self.machine.lorenz.current_chi()),
            "psi_wheel_positions": list(self.machine.lorenz.current_psi()),
            "chi_counts": dict(self._chi_counts),
        }


class HarvardMarkIAdapter(MachineAdapter):
    """Adapter for HarvardMarkI (IBM ASCC, USA, 1944).

    One step = execute one Mark I instruction. The 72 counters and 60
    constants (read-only) are accessible as memory[0..131].
    """

    _TIMING_MS: dict[str, float] = {"add": 300.0, "multiply": 3000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: HarvardMarkI) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return self.machine.state.cycle_count

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose first 20 counters as column values (integer truncated).
        return [int(self.machine.get_counter(i)) for i in range(20)]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "pc": self.machine.state.program_counter,
            "last_result": float(self.machine.state.last_result),
            "halted": self.machine.state.halted,
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < 72:
            return float(self.machine.get_counter(address))
        elif 72 <= address < 132:
            return float(self.machine.get_constant(address - 72))
        return 0.0

    def step(self) -> None:
        self.machine.step()

    def get_snapshot(self) -> Any:
        return self.machine.state_snapshot()


class ENIACAdapter(MachineAdapter):
    """Adapter for ENIAC (USA, 1945).

    One step = execute one ENIAC instruction. The 20 decimal accumulators
    are exposed as registers and as memory[0..19].
    """

    _TIMING_MS: dict[str, float] = {"add": 0.2, "multiply": 2.8}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: ENIAC) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return self.machine.state.cycle_count

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose all 20 accumulators as integer column values.
        return [int(self.machine.get_accumulator(i)) for i in range(20)]

    def get_register_values(self) -> dict[str, Any]:
        return {f"A{i}": float(self.machine.get_accumulator(i)) for i in range(20)}

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < 20:
            return float(self.machine.get_accumulator(address))
        return 0.0

    def step(self) -> None:
        self.machine.step()

    def get_snapshot(self) -> Any:
        return self.machine.state_snapshot()


class ManchesterBabyAdapter(MachineAdapter):
    """Adapter for ManchesterBaby / SSEM (UK, 1948).

    One step = one Baby fetch-execute cycle (CI increments, fetch, execute).
    The 32-word CRT store is exposed as memory[0..31].
    """

    _TIMING_MS: dict[str, float] = {"instruction": 1.2}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: ManchesterBaby) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return self.machine.state.cycle_count

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose all 32 store words as signed column values (truncated to int).
        return [self.machine.get_store(i) for i in range(32)]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "accumulator": self.machine.state.accumulator,
            "ci": self.machine.state.ci,
            "pi": self.machine.state.pi,
            "halted": self.machine.state.halted,
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < 32:
            return self.machine.get_store(address)
        return 0

    def step(self) -> None:
        self.machine.step()

    def get_snapshot(self) -> Any:
        return self.machine.state_snapshot()


class EDSACAdapter(MachineAdapter):
    """Adapter for EDSAC (Cambridge, 1949).

    One step = one EDSAC fetch-execute cycle (CI increments, fetch, execute).
    The 512-word mercury delay-line store is exposed as memory[0..511].
    """

    _TIMING_MS: dict[str, float] = {"instruction": 1.5}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: EDSAC) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return self.machine.state.cycle_count

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose first 32 store words as visible column values.
        return [self.machine.get_value(i) for i in range(32)]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "accumulator": self.machine.state.accumulator,
            "multiplier_register": self.machine.state.multiplier_register,
            "ci": self.machine.state.ci,
            "halted": self.machine.state.halted,
        }

    def get_memory_value(self, address: int) -> Any:
        if 0 <= address < 512:
            return self.machine.get_value(address)
        return 0

    def step(self) -> None:
        self.machine.step()

    def get_snapshot(self) -> Any:
        return self.machine.state_snapshot()


class JacquardAdapter(MachineAdapter):
    """Adapter for JacquardLoom (France, 1804).

    One step = one card advance (one weft row woven).
    The loom has no program counter; step() reads the next card in the deck.
    """

    _TIMING_MS: dict[str, float] = {"step": 1000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: JacquardLoom) -> None:
        self.machine = machine
        self._last_card: list[int] = [0] * machine.num_hooks

    def load_deck(self, cards: list[list[int]]) -> None:
        """Load a punch card deck and reset the loom."""
        self.machine.load_deck(cards)
        self._last_card = [0] * self.machine.num_hooks

    def get_cycle_count(self) -> int:
        return self.machine.weft_count

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        return list(self._last_card)

    def get_register_values(self) -> dict[str, Any]:
        return {
            "weft_count": self.machine.weft_count,
            "card_index": self.machine.card_index,
            "num_hooks": self.machine.num_hooks,
            "deck_size": len(self.machine.state.card_deck),
        }

    def get_memory_value(self, address: int) -> Any:
        pattern = self.machine.get_pattern()
        if 0 <= address < len(pattern):
            return pattern[address]
        return [0] * self.machine.num_hooks

    def step(self) -> None:
        card = self.machine.step()
        if card is not None:
            self._last_card = card

    def get_snapshot(self) -> Any:
        return {
            "weft_count": self.machine.weft_count,
            "card_index": self.machine.card_index,
            "last_card": list(self._last_card),
            "pattern": self.machine.get_pattern(),
            "num_hooks": self.machine.num_hooks,
        }


class AbacusAdapter(MachineAdapter):
    """Adapter for AbacusEmulator (China, ~200 BCE).

    One step = one bead push (adds 1 to the current value).
    Demonstrates bead-column arithmetic; columns represent decimal digits.

    ALGOL 68 backend (execute_algol68_ops):
      For sequences of add/sub/load/reset operations, the adapter can delegate
      to the ALGOL68 suanpan program (tools/algol68/abacus.a68) which models
      the physical bead state (heaven/earth columns) in addition to the total.
      The Python AbacusEmulator is then synced to the ALGOL68 result so that
      subsequent step() calls remain consistent.
    """

    _TIMING_MS: dict[str, float] = {"step": 300.0, "add": 300.0, "sub": 300.0}

    # Path to the resident ALGOL68 suanpan program
    _A68_SOURCE: str = str(
        __import__("pathlib").Path(__file__).resolve().parents[3]
        / "tools"
        / "algol68"
        / "abacus.a68"
    )

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: AbacusEmulator) -> None:
        self.machine = machine
        self._ops = 0
        # Full bead-state from the last ALGOL68 execution (None until first run)
        self._heaven: list[int] | None = None
        self._earth: list[int] | None = None

    def get_cycle_count(self) -> int:
        return self._ops

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Each digit of the current value is one column.
        return self.machine._digits()  # noqa: SLF001

    def get_register_values(self) -> dict[str, Any]:
        rv: dict[str, Any] = {"value": self.machine.state()["value"]}
        if self._heaven is not None:
            rv["heaven"] = self._heaven
        if self._earth is not None:
            rv["earth"] = self._earth
        return rv

    def get_memory_value(self, address: int) -> Any:
        digits = self.machine._digits()  # noqa: SLF001
        if 0 <= address < len(digits):
            return digits[address]
        return 0

    def step(self) -> None:
        self.machine.add(1)
        self._ops += 1
        # Invalidate the ALGOL68 bead state since Python stepped it independently
        self._heaven = None
        self._earth = None

    def execute_algol68_ops(
        self, operations: list[dict[str, Any]]
    ) -> dict[str, Any]:
        """Run a sequence of operations through the ALGOL68 suanpan backend.

        Each operation: {"op": "load"|"add"|"sub"|"reset", "value": <int>}
        The "value" key is required for load/add/sub; ignored for reset.

        Encodes the sequence as the abacus.a68 wire protocol (int opcode pairs),
        runs a68g synchronously, parses output, syncs the internal AbacusEmulator,
        and returns the parsed bead state.

        Returns {"total": int, "heaven": list[int], "earth": list[int]}.
        Raises RuntimeError if a68g is not found or the program fails.
        """
        import subprocess

        _OP_CODES = {"load": 1, "add": 2, "sub": 3, "reset": 4}
        lines: list[str] = []
        # If the first operation is not a "load" or "reset", seed the board
        # with the current machine value so that sequential calls compose.
        # A leading "load" or "reset" in the operations list is authoritative.
        first_op = str(operations[0].get("op", "")).lower() if operations else ""
        if first_op not in ("load", "reset"):
            lines.append(f"1 {self.machine.state()['value']}")
        for op_dict in operations:
            op = str(op_dict.get("op", "")).lower()
            code = _OP_CODES.get(op)
            if code is None:
                continue
            if op == "reset":
                lines.append("4")
            else:
                val = int(op_dict.get("value", 0))
                lines.append(f"{code} {val}")
        lines.append("0")  # print + quit
        stdin_data = "\n".join(lines) + "\n"

        try:
            result = subprocess.run(
                ["/usr/bin/a68g", self._A68_SOURCE],
                input=stdin_data,
                capture_output=True,
                text=True,
                timeout=5,
            )
        except FileNotFoundError as exc:
            raise RuntimeError("a68g not found at /usr/bin/a68g") from exc
        except subprocess.TimeoutExpired as exc:
            raise RuntimeError("ALGOL68 abacus timed out") from exc

        if result.returncode != 0:
            raise RuntimeError(
                f"ALGOL68 abacus error: {result.stderr.strip()}"
            )

        parsed = _parse_abacus_a68_output(result.stdout)
        # Sync the Python emulator so step() calls remain consistent
        self.machine.set_value(parsed["total"])
        self._heaven = parsed["heaven"]
        self._earth = parsed["earth"]
        self._ops += len(operations)
        return parsed

    def get_snapshot(self) -> Any:
        snap: dict[str, Any] = {
            "value": self.machine.state()["value"],
            "digits": self.machine.state()["digits"],
            "operations": self._ops,
        }
        if self._heaven is not None:
            snap["heaven"] = self._heaven
        if self._earth is not None:
            snap["earth"] = self._earth
        return snap


def _parse_abacus_a68_output(output: str) -> dict[str, Any]:
    """Parse the abacus.a68 print_state output into a Python dict.

    Expected format (3 lines):
      total=<int>
      heaven=<h1> <h2> ... <h9>
      earth=<e1> <e2> ... <e9>

    Returns {"total": int, "heaven": list[int], "earth": list[int]}.
    Only the LAST total= block is returned (in case multiple PRINT ops ran).
    """
    total = 0
    heaven: list[int] = []
    earth: list[int] = []
    import contextlib

    for line in output.splitlines():
        line = line.strip()
        if line.startswith("total="):
            with contextlib.suppress(ValueError):
                total = int(line[6:])
        elif line.startswith("heaven="):
            with contextlib.suppress(ValueError):
                heaven = [int(x) for x in line[7:].split()]
        elif line.startswith("earth="):
            with contextlib.suppress(ValueError):
                earth = [int(x) for x in line[6:].split()]
    return {"total": total, "heaven": heaven, "earth": earth}


class SlideRuleAdapter(MachineAdapter):
    """Adapter for SlideRuleEmulator (England, ~1620).

    One step = multiply current result by 2 (demonstrates log-scale doubling).
    The slide rule has no internal state; the adapter tracks the running result.
    """

    _TIMING_MS: dict[str, float] = {"multiply": 3000.0, "divide": 3000.0}
    _A68_SOURCE: str = str(
        __import__("pathlib").Path(__file__).resolve().parents[3]
        / "tools"
        / "algol68"
        / "slide_rule.a68"
    )

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: SlideRuleEmulator) -> None:
        self.machine = machine
        self._result: float = 1.0
        self._ops = 0

    def get_cycle_count(self) -> int:
        return self._ops

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose integer part of result as a single column.
        return [int(self._result)]

    def get_register_values(self) -> dict[str, Any]:
        return {"result": self._result, "log_result": self._ops}

    def get_memory_value(self, address: int) -> Any:
        return self._result if address == 0 else 0.0

    def step(self) -> None:
        # Each step doubles the running result via log-scale addition.
        self._result = self.machine.multiply(self._result, 2.0)
        self._ops += 1

    def get_snapshot(self) -> Any:
        return {
            "result": self._result,
            "operations": self._ops,
        }

    def execute_algol68_ops(
        self, operations: list[dict[str, Any]]
    ) -> dict[str, Any]:
        """Run slide rule operations through the ALGOL68 slide_rule.a68 backend.

        Supported operation shapes:
          {"op": "multiply", "a": <float>, "b": <float>}   -- a * b via log addition
          {"op": "divide",   "a": <float>, "b": <float>}   -- a / b via log subtraction
          {"op": "power",    "a": <float>, "n": <int>}     -- a^n via repeated log addition
          {"op": "sqrt",     "a": <float>}                  -- sqrt(a) via log halving
          {"op": "print"}                                    -- emit current state (opcode 5)

        Returns {"result": float, "ops": int}.
        Raises RuntimeError if a68g is unavailable or returns a non-zero exit code.
        """
        import subprocess

        lines: list[str] = []
        for op_dict in operations:
            op = str(op_dict.get("op", "")).lower()
            if op == "multiply":
                a = float(op_dict.get("a", 1.0))
                b = float(op_dict.get("b", 1.0))
                lines.append(f"1 {a} {b}")
            elif op == "divide":
                a = float(op_dict.get("a", 1.0))
                b = float(op_dict.get("b", 1.0))
                lines.append(f"2 {a} {b}")
            elif op == "power":
                a = float(op_dict.get("a", 1.0))
                n = int(op_dict.get("n", 1))
                lines.append(f"3 {a} {n}")
            elif op == "sqrt":
                a = float(op_dict.get("a", 1.0))
                lines.append(f"4 {a}")
            elif op == "print":
                lines.append("5")
        lines.append("0")  # print final state + quit
        stdin_data = "\n".join(lines) + "\n"

        try:
            proc = subprocess.run(
                ["/usr/bin/a68g", self._A68_SOURCE],
                input=stdin_data,
                capture_output=True,
                text=True,
                timeout=5,
            )
        except FileNotFoundError as exc:
            raise RuntimeError("a68g not found at /usr/bin/a68g") from exc
        except subprocess.TimeoutExpired as exc:
            raise RuntimeError("ALGOL68 slide_rule timed out") from exc

        if proc.returncode != 0:
            raise RuntimeError(f"ALGOL68 slide_rule error: {proc.stderr.strip()}")

        parsed = _parse_slide_rule_a68_output(proc.stdout)
        # Sync the adapter state so step() and get_snapshot() remain consistent
        self._result = parsed["result"]
        self._ops = parsed["ops"]
        return parsed


def _parse_slide_rule_a68_output(output: str) -> dict[str, Any]:
    """Parse slide_rule.a68 print_state output.

    Expected format (last block wins if PRINT opcode was used multiple times):
      result=<REAL>    -- a68g REAL format e.g. "+4.20000000000000e  +1"
      ops=<int>

    Returns {"result": float, "ops": int}.
    """
    import contextlib

    result = 0.0
    ops = 0
    for line in output.splitlines():
        if line.startswith("result="):
            with contextlib.suppress(ValueError):
                # a68g REAL output: "+4.20000000000000e  +1"; collapse whitespace
                raw = line.split("=", 1)[1]
                result = float("".join(raw.split()))
        elif line.startswith("ops="):
            with contextlib.suppress(ValueError):
                ops = int(line.split("=", 1)[1].strip())
    return {"result": result, "ops": ops}


class QuipuAdapter(MachineAdapter):
    """Adapter for QuipuEmulator (Inca, ~900-1532 CE).

    One step = record one tally unit in the "step" category.
    Demonstrates knotted-cord encoding; each category is a pendant cord.
    """

    _TIMING_MS: dict[str, float] = {"step": 1000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: QuipuEmulator) -> None:
        self.machine = machine
        self._ops = 0

    def get_cycle_count(self) -> int:
        return self._ops

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Expose per-category totals as column values (up to 8 categories).
        state = self.machine.state()
        cats: dict[str, int] = {}
        for r in state["records"]:
            cats[str(r["category"])] = cats.get(str(r["category"]), 0) + int(r["value"])
        return [v for v in list(cats.values())[:8]]

    def get_register_values(self) -> dict[str, Any]:
        state = self.machine.state()
        return {
            "record_count": len(state["records"]),
            "operations": self._ops,
        }

    def get_memory_value(self, address: int) -> Any:
        state = self.machine.state()
        if 0 <= address < len(state["records"]):
            return state["records"][address]
        return 0

    def step(self) -> None:
        self.machine.encode_number("step", 1)
        self._ops += 1

    def get_snapshot(self) -> Any:
        return {
            "records": self.machine.state()["records"],
            "operations": self._ops,
        }


class AstrolabeAdapter(MachineAdapter):
    """Adapter for AstrolabeEmulator (Greece/Islam, ~200 BCE - 1600 CE).

    One step = advance by 1 hour and recompute solar altitude.
    Demonstrates the reticular projection used to read altitude angles from
    the rete (star map) against the tympan (latitude plate).
    """

    _TIMING_MS: dict[str, float] = {"step": 5000.0}

    def get_operation_time_ms(self) -> dict[str, float]:
        return self._TIMING_MS

    def __init__(self, machine: AstrolabeEmulator) -> None:
        self.machine = machine
        self._date = "2026-03-21"
        self._latitude = 51.5  # London (typical Islamic/European astrolabe latitude)
        self._hour: float = 6.0  # start at dawn
        self._last_altitude: float = 0.0
        self._steps = 0

    def get_cycle_count(self) -> int:
        return self._steps

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        # Altitude in whole degrees as a single column value.
        return [max(0, int(self._last_altitude))]

    def get_register_values(self) -> dict[str, Any]:
        return {
            "hour": self._hour,
            "altitude_deg": round(self._last_altitude, 2),
            "latitude_deg": self._latitude,
            "date": self._date,
        }

    def get_memory_value(self, address: int) -> Any:
        return self._last_altitude if address == 0 else 0.0

    def step(self) -> None:
        from .astrolabe import AstrolabeQuery

        self._hour = (self._hour + 1.0) % 24.0
        hh = int(self._hour)
        mm = int((self._hour - hh) * 60)
        time_str = f"{hh:02d}:{mm:02d}"
        try:
            self._last_altitude = self.machine.read_altitude(
                AstrolabeQuery(self._latitude, self._date, time_str, "sun")
            )
        except (KeyError, ValueError):
            self._last_altitude = 0.0
        self._steps += 1

    def get_snapshot(self) -> Any:
        return {
            "date": self._date,
            "latitude_deg": self._latitude,
            "hour": self._hour,
            "altitude_deg": round(self._last_altitude, 2),
            "steps": self._steps,
        }


class TallyMarksAdapter(MachineAdapter):
    """Adapter for TallyMarksEmulator (Prehistoric, ~20,000 BCE).

    One step = add 1 tally mark.  Every 5th mark completes a gate (||||/).
    Represents the oldest known form of numerical recording (Ishango bone).
    """

    def __init__(self, machine: TallyMarksEmulator) -> None:
        self.machine = machine

    def get_cycle_count(self) -> int:
        return int(self.machine.state()["count"])

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        count = int(self.machine.state()["count"])
        # Each group of 5 is one column; remainder fills the last column.
        groups = count // 5
        remainder = count % 5
        return ([5] * groups + [remainder]) if remainder else ([5] * groups or [0])

    def get_register_values(self) -> dict[str, Any]:
        count = int(self.machine.state()["count"])
        return {
            "count": count,
            "groups": count // 5,
            "remainder": count % 5,
            "tally": self.machine.render(),
        }

    def get_memory_value(self, address: int) -> Any:
        cols = self.get_column_values()
        if 0 <= address < len(cols):
            return cols[address]
        return 0

    def step(self) -> None:
        self.machine.step(1)

    def get_snapshot(self) -> Any:
        return {
            "count": int(self.machine.state()["count"]),
            "tally": self.machine.render(),
        }


class ClayTokensAdapter(MachineAdapter):
    """Adapter for ClayTokensEmulator (Mesopotamia, ~8,000 BCE).

    One step = add one 'goods' token to the collection.
    Demonstrates the bulla/envelope accounting system predating cuneiform.
    """

    def __init__(self, machine: ClayTokensEmulator) -> None:
        self.machine = machine
        self._ops = 0

    def get_cycle_count(self) -> int:
        return self._ops

    def get_current_phase(self) -> MechanicalPhase | None:
        return None

    def get_column_values(self) -> list[int]:
        tokens = dict(self.machine.state()["tokens"])
        return [v for v in list(tokens.values())[:8]]

    def get_register_values(self) -> dict[str, Any]:
        state = self.machine.state()
        return {
            "tokens": dict(state["tokens"]),
            "sealed": bool(state["sealed"]),
            "operations": self._ops,
        }

    def get_memory_value(self, address: int) -> Any:
        tokens = list(dict(self.machine.state()["tokens"]).values())
        if 0 <= address < len(tokens):
            return tokens[address]
        return 0

    def step(self) -> None:
        state = self.machine.state()
        if not state["sealed"]:
            self.machine.add_token("goods", 1)
        self._ops += 1

    def get_snapshot(self) -> Any:
        state = self.machine.state()
        return {
            "tokens": dict(state["tokens"]),
            "sealed": bool(state["sealed"]),
            "impression": dict(state["impression"]),
            "operations": self._ops,
        }
