"""Machines API -- REST interface for all 24 historical machine emulators.

Endpoints:
  GET  /machines              -- list all machines (metadata only)
  GET  /machines/{id}         -- single machine metadata + manual
  POST /machines/{id}/reset   -- destroy and rebuild a fresh machine session
  POST /machines/{id}/load    -- load a program/tape/data into the machine
  POST /machines/{id}/step    -- execute one atomic step
  POST /machines/{id}/run     -- run to completion (up to max_steps)
  GET  /machines/{id}/state   -- current register/memory snapshot

WHY session-per-machine (not per-request): machines carry state (registers,
program counter, memory) that must persist across multiple API calls in a
debugger workflow.  A module-level dict keyed by machine ID holds one live
adapter per machine.  Calling /reset rebuilds the factory instance.
"""

from __future__ import annotations

import logging
from typing import Any

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel

from ..emulator.adapter import MachineAdapter
from ..emulator.machine_registry import (
    MACHINES,
    MachineEntry,
    build_machine,
    get_machine,
)

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/machines", tags=["machines"])

# ---------------------------------------------------------------------------
# Session store  (machine_id -> (machine_obj, adapter))
# ---------------------------------------------------------------------------

_sessions: dict[str, tuple[Any, MachineAdapter]] = {}


def _get_session(machine_id: str) -> tuple[Any, MachineAdapter]:
    """Return the live session, creating it lazily on first access."""
    if machine_id not in _sessions:
        result = build_machine(machine_id)
        if result is None:
            raise HTTPException(status_code=404, detail=f"Unknown machine: {machine_id!r}")
        _sessions[machine_id] = result
    return _sessions[machine_id]


def _reset_session(machine_id: str) -> tuple[Any, MachineAdapter]:
    """Destroy the current session and build a fresh one."""
    result = build_machine(machine_id)
    if result is None:
        raise HTTPException(status_code=404, detail=f"Unknown machine: {machine_id!r}")
    _sessions[machine_id] = result
    return result


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------


class MachineListItem(BaseModel):
    id: str
    name: str
    year: int
    country: str
    inventor: str
    category: str
    brief: str
    program_input_type: str
    tags: list[str]


class MachineDetail(MachineListItem):
    manual: str
    example_payload: dict[str, Any]


class LoadPayload(BaseModel):
    payload: dict[str, Any]


class StepResponse(BaseModel):
    cycle_count: int
    registers: dict[str, Any]
    columns: list[int]
    snapshot: Any
    halted: bool = False


class RunResponse(BaseModel):
    steps_taken: int
    registers: dict[str, Any]
    snapshot: Any
    halted: bool


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _entry_to_list_item(e: MachineEntry) -> MachineListItem:
    return MachineListItem(
        id=e.id,
        name=e.name,
        year=e.year,
        country=e.country,
        inventor=e.inventor,
        category=e.category,
        brief=e.brief,
        program_input_type=e.program_input_type,
        tags=e.tags,
    )


def _entry_to_detail(e: MachineEntry) -> MachineDetail:
    return MachineDetail(
        id=e.id,
        name=e.name,
        year=e.year,
        country=e.country,
        inventor=e.inventor,
        category=e.category,
        brief=e.brief,
        program_input_type=e.program_input_type,
        tags=e.tags,
        manual=e.manual,
        example_payload=e.example_payload,
    )


def _build_step_response(adapter: MachineAdapter) -> StepResponse:
    snap = adapter.get_snapshot()
    halted = snap.get("halted", False) if isinstance(snap, dict) else False
    return StepResponse(
        cycle_count=adapter.get_cycle_count(),
        registers=adapter.get_register_values(),
        columns=adapter.get_column_values(),
        snapshot=snap,
        halted=halted,
    )


# ---------------------------------------------------------------------------
# Endpoints
# ---------------------------------------------------------------------------


@router.get("", response_model=list[MachineListItem])
async def list_all_machines(category: str | None = None) -> list[MachineListItem]:
    """List all emulated machines with their metadata."""
    machines = [m for m in MACHINES if category is None or m.category == category]
    return [_entry_to_list_item(m) for m in machines]


@router.get("/{machine_id}", response_model=MachineDetail)
async def get_machine_detail(machine_id: str) -> MachineDetail:
    """Get full machine metadata including the programming manual."""
    entry = get_machine(machine_id)
    if entry is None:
        raise HTTPException(status_code=404, detail=f"Unknown machine: {machine_id!r}")
    return _entry_to_detail(entry)


@router.post("/{machine_id}/reset", response_model=StepResponse)
async def reset_machine(machine_id: str) -> StepResponse:
    """Reset the machine to its initial state."""
    _machine, adapter = _reset_session(machine_id)
    return _build_step_response(adapter)


@router.post("/{machine_id}/load")
async def load_program(machine_id: str, body: LoadPayload) -> dict[str, Any]:
    """Load a program, tape, or data into the machine.

    The payload structure depends on `program_input_type` for each machine.
    See GET /machines/{id} example_payload for the expected format.
    """
    entry = get_machine(machine_id)
    if entry is None:
        raise HTTPException(status_code=404, detail=f"Unknown machine: {machine_id!r}")

    machine, adapter = _get_session(machine_id)
    payload = body.payload
    pit = entry.program_input_type

    try:
        _apply_load(machine_id, machine, adapter, pit, payload)
    except Exception as exc:
        logger.exception("load failed for %s", machine_id)
        raise HTTPException(status_code=400, detail=str(exc)) from exc

    return {"ok": True, "machine_id": machine_id, "program_input_type": pit}


def _apply_load(
    machine_id: str,
    machine: Any,
    adapter: MachineAdapter,
    pit: str,
    payload: dict[str, Any],
) -> None:
    """Dispatch the load payload to the appropriate machine API."""
    if pit == "assembly":
        # AE: load assembly source text
        source: str = payload.get("source", "")
        machine.load_program_from_text(source)

    elif pit == "instructions_json" and machine_id in ("zuse-z3",):
        from ..emulator.zuse_z3 import Z3Instruction, Z3Op

        if "memory" in payload:
            for addr_str, val in payload["memory"].items():
                machine.load_memory(int(addr_str), float(val))
        if "input_tape" in payload:
            machine.load_input_tape([float(v) for v in payload["input_tape"]])
        if "program" in payload:
            instrs = []
            for instr_dict in payload["program"]:
                op = Z3Op(instr_dict["op"])
                addr = instr_dict.get("address", 0)
                instrs.append(Z3Instruction(op=op, address=addr))
            machine.load_program(instrs)

    elif pit == "instructions_json" and machine_id in ("zuse-z1",):
        from ..emulator.zuse_z1 import Z1Instruction, Z1Op

        if "memory" in payload:
            for addr_str, val in payload["memory"].items():
                machine.load_memory(int(addr_str), float(val))
        if "program" in payload:
            instrs = []
            for instr_dict in payload["program"]:
                op = Z1Op(instr_dict["op"])
                addr = instr_dict.get("address", 0)
                instrs.append(Z1Instruction(op=op, address=addr))
            machine.load_program(instrs)

    elif pit == "instructions_json" and machine_id in ("harvard-mark-i",):
        from decimal import Decimal

        from ..emulator.harvard_mark_i import MarkIInstruction

        if "counters" in payload:
            for idx_str, val in payload["counters"].items():
                machine.set_counter(int(idx_str), Decimal(str(val)))
        if "constants" in payload:
            for idx_str, val in payload["constants"].items():
                machine.set_constant(int(idx_str), Decimal(str(val)))
        if "program" in payload:
            instrs = []
            for instr_dict in payload["program"]:
                op = instr_dict["op"]
                args = tuple(instr_dict.get("args", []))
                label = instr_dict.get("label", "")
                instrs.append(MarkIInstruction(op=op, args=args, label=label))
            machine.load_program(instrs)

    elif pit == "instructions_json" and machine_id in ("eniac",):
        from ..emulator.eniac import ENIACInstruction

        if "accumulators" in payload:
            for idx_str, val in payload["accumulators"].items():
                machine.set_accumulator(int(idx_str), float(val))
        if "program" in payload:
            instrs = []
            for instr_dict in payload["program"]:
                op = instr_dict["op"]
                args = tuple(instr_dict.get("args", []))
                instrs.append(ENIACInstruction(op=op, args=args))
            machine.load_program(instrs)

    elif pit == "instructions_json" and machine_id == "ludgate":
        # Ludgate: load program tape as list of tuples
        if "variables" in payload:
            for idx_str, val in payload["variables"].items():
                machine.state.store[int(idx_str)] = int(val)
        if "program" in payload:
            tape = [tuple(step) for step in payload["program"]]
            machine.load_program(tape)

    elif pit == "instructions_json" and machine_id == "torres-quevedo":
        if "registers" in payload:
            from ..emulator.torres_quevedo import FloatingPointNumber

            for idx_str, val in payload["registers"].items():
                machine.state.registers[int(idx_str)] = FloatingPointNumber.from_float(float(val))
        if "program" in payload:
            tape = [tuple(step) for step in payload["program"]]
            machine.load_program(tape)

    elif pit == "plaintext" and machine_id == "enigma":
        rotors = payload.get("rotors", ["I", "II", "III"])
        reflector = payload.get("reflector", "B")
        positions = payload.get("positions", "AAA")
        plaintext = payload.get("plaintext", "")
        # Rebuild Enigma with requested config
        from ..emulator.enigma import EnigmaMachine

        new_machine = EnigmaMachine(rotors=rotors, reflector=reflector)
        new_machine.set_rotor_positions(positions)
        # Swap underlying machine on the adapter
        adapter.machine = new_machine  # type: ignore[attr-defined]
        adapter.load_input(plaintext)  # type: ignore[attr-defined]

    elif pit == "crib_cipher" and machine_id == "bombe":
        from ..emulator.bombe import Bombe, BombeMenu

        crib = payload.get("crib", "ABCDEFG")
        cipher = payload.get("cipher", "XXXXXXX")
        rotors = payload.get("rotors", ["I", "II", "III"])
        reflector = payload.get("reflector", "B")
        menu = BombeMenu.from_crib(crib, cipher)
        new_bombe = Bombe(rotor_order=rotors, reflector=reflector)
        # Swap adapter state
        adapter.machine = new_bombe  # type: ignore[attr-defined]
        adapter.menu = menu  # type: ignore[attr-defined]
        adapter._pos = 0  # type: ignore[attr-defined]
        adapter._stops = []  # type: ignore[attr-defined]

    elif pit == "lorenz_tape" and machine_id in ("colossus", "colossus-2"):
        tape = [[int(b) for b in row] for row in payload.get("tape", [])]
        adapter.load_tape(tape)  # type: ignore[attr-defined]

    elif pit == "coefficients" and machine_id == "scheutz-de":
        vals = payload.get("initial_values", [0, 6, 2, 0])
        machine.set_initial_values([int(v) for v in vals])

    elif pit == "coefficients" and machine_id == "grant-de":
        vals = payload.get("initial_values", [0.0, 6.0, 2.0, 0.0])
        machine.set_initial_values([float(v) for v in vals])

    elif pit == "words" and machine_id == "manchester-baby":
        for item in payload.get("program_words", []):
            machine.store_program(item["address"], item["value"])

    elif pit == "words" and machine_id == "edsac":
        from ..emulator.edsac import EDSACInstruction

        instrs = []
        for item in payload.get("instructions", []):
            instrs.append(
                EDSACInstruction(
                    address=item["address"],
                    mnemonic=item["mnemonic"],
                    operand=item.get("operand", 0),
                )
            )
        if instrs:
            machine.store_instructions(instrs)
        if "data" in payload:
            for addr_str, val in payload["data"].items():
                machine.set_value(int(addr_str), int(val))

    elif pit == "data_deck":
        from ..emulator.hollerith_tabulator import PunchedCard

        cards = []
        for card_dict in payload.get("cards", []):
            cols = card_dict.get("columns", [])
            c = PunchedCard(num_columns=80)
            for col in cols:
                c.punch(card_dict.get("row", 0), col)
            cards.append(c)
        adapter.load_deck(cards)  # type: ignore[attr-defined]

    elif pit == "none":
        # Calculators: optionally set input value
        if "input" in payload:
            val = int(payload["input"])
            if hasattr(machine, "set_input"):
                machine.set_input(val)
            elif hasattr(machine, "set_number"):
                machine.set_number(val)
        if "sliders" in payload and hasattr(machine, "set_slider"):
            for i, v in enumerate(payload["sliders"]):
                machine.set_slider(i, int(v))
        if "multiplier_lever" in payload and hasattr(machine, "set_multiplier_lever"):
            machine.set_multiplier_lever(int(payload["multiplier_lever"]))

    elif pit == "date":
        # Antikythera: reset date
        adapter._date = float(payload.get("years", 0.0))  # type: ignore[attr-defined]
        adapter._steps = 0  # type: ignore[attr-defined]


@router.post("/{machine_id}/step", response_model=StepResponse)
async def step_machine(machine_id: str) -> StepResponse:
    """Execute one atomic step on the machine."""
    _machine, adapter = _get_session(machine_id)
    try:
        adapter.step()
    except Exception as exc:
        logger.exception("step failed for %s", machine_id)
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return _build_step_response(adapter)


@router.post("/{machine_id}/run", response_model=RunResponse)
async def run_machine(machine_id: str, max_steps: int = 1000) -> RunResponse:
    """Run the machine until halted or max_steps reached."""
    _machine, adapter = _get_session(machine_id)
    steps = 0
    halted = False
    while steps < max_steps:
        snap = adapter.get_snapshot()
        if isinstance(snap, dict) and snap.get("halted"):
            halted = True
            break
        try:
            adapter.step()
            steps += 1
        except StopIteration:
            halted = True
            break
        except Exception as exc:
            logger.exception("run step %d failed for %s", steps, machine_id)
            raise HTTPException(status_code=400, detail=str(exc)) from exc
        # Re-check halted after step
        snap = adapter.get_snapshot()
        if isinstance(snap, dict) and snap.get("halted"):
            halted = True
            break

    return RunResponse(
        steps_taken=steps,
        registers=adapter.get_register_values(),
        snapshot=adapter.get_snapshot(),
        halted=halted,
    )


@router.get("/{machine_id}/state", response_model=StepResponse)
async def get_state(machine_id: str) -> StepResponse:
    """Return the current machine state without executing any steps."""
    _machine, adapter = _get_session(machine_id)
    return _build_step_response(adapter)
