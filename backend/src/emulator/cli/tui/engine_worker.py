"""Textual Worker wrapping Engine execution.

The worker runs the engine in the background and posts EngineStateUpdate
messages to the app. The UI never reads engine state directly -- always
via captured snapshots posted from the worker thread.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from textual.message import Message

from ...analytical_engine import Engine


@dataclass
class EngineSnapshot:
    """Snapshot of engine state at one point in time."""

    pc: int
    clock_time: float
    running: bool
    registers: dict[str, Any]
    flags: dict[str, Any]
    barrel: dict[str, Any]
    mill_operand_buffer: float
    mill_result_buffer: float
    last_opcode: str
    execution_trace: list[Any]


class EngineStateUpdate(Message):
    """Posted by the worker when a new snapshot is available."""

    def __init__(self, snapshot: EngineSnapshot) -> None:
        super().__init__()
        self.snapshot = snapshot


def make_snapshot(engine: Engine, last_opcode: str = "NOP") -> EngineSnapshot:
    """Capture current engine state into an immutable snapshot dict."""
    return EngineSnapshot(
        pc=engine.PC,
        clock_time=engine.clock_time,
        running=engine.running,
        registers={k: v.to_decimal() for k, v in engine.registers.items()},
        flags=dict(engine.flags),
        barrel={
            "active": engine.barrels.active_barrel,
            "step": engine.barrels.step_index,
        },
        mill_operand_buffer=engine.mill_operand_buffer.to_decimal(),
        mill_result_buffer=engine.mill_result_buffer.to_decimal(),
        last_opcode=last_opcode,
        execution_trace=list(engine.execution_trace[-50:]),  # last 50 entries
    )


def snapshot_as_dict(snap: EngineSnapshot) -> dict[str, Any]:
    """Convert EngineSnapshot to dict for widget consumption."""
    return {
        "pc": snap.pc,
        "clock_time": snap.clock_time,
        "running": snap.running,
        "registers": snap.registers,
        "flags": snap.flags,
        "barrel": snap.barrel,
        "mill_operand_buffer": snap.mill_operand_buffer,
        "mill_result_buffer": snap.mill_result_buffer,
        "last_opcode": snap.last_opcode,
        "execution_trace": snap.execution_trace,
    }
