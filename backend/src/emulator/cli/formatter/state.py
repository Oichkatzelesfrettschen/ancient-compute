"""Rich-formatted engine state display."""

from typing import Any

from rich.table import Table
from rich.text import Text


def format_registers(engine: Any) -> Table:
    """Return a Rich Table showing register values."""
    tbl = Table(title="Registers", show_header=True, header_style="bold cyan")
    tbl.add_column("Reg", style="bold yellow", width=4)
    tbl.add_column("Value", style="white")
    for name, val in engine.registers.items():
        tbl.add_row(name, str(val.to_decimal()))
    return tbl


def format_flags(engine: Any) -> Table:
    """Return a Rich Table showing flag values."""
    tbl = Table(title="Flags", show_header=True, header_style="bold cyan")
    tbl.add_column("Flag", style="bold yellow", width=8)
    tbl.add_column("Value", width=5)
    for name, val in engine.flags.items():
        color = "green" if val else "red"
        tbl.add_row(name, Text(str(int(val)), style=color))
    return tbl


def format_memory(engine: Any, start: int = 0, count: int = 16) -> Table:
    """Return a Rich Table showing a memory window."""
    tbl = Table(
        title=f"Memory [{start}..{start + count - 1}]", show_header=True, header_style="bold cyan"
    )
    tbl.add_column("Addr", style="bold yellow", width=6)
    tbl.add_column("Value", style="white")
    for i in range(start, min(start + count, len(engine.memory))):
        val = engine.memory[i].to_decimal()
        style = "bright_white" if val != 0 else "dim"
        tbl.add_row(f"[{i}]", Text(str(val), style=style))
    return tbl


def format_state(engine: Any) -> str:
    """Return a plain-text state summary string."""
    lines = [
        f"PC: {engine.PC}  Clock: {engine.clock_time}s  Running: {engine.running}",
        "Registers: " + "  ".join(f"{k}={v.to_decimal()}" for k, v in engine.registers.items()),
        "Flags: " + "  ".join(f"{k}={int(v)}" for k, v in engine.flags.items()),
    ]
    if engine.barrels.active_barrel:
        lines.append(f"Barrel: {engine.barrels.active_barrel}  Step: {engine.barrels.step_index}")
    return "\n".join(lines)
