"""Rich-formatted trace output."""

import json
from typing import Any

from rich.table import Table


def format_trace_entry(entry: dict[str, Any]) -> str:
    """Format a single trace entry as a compact string."""
    regs = "  ".join(f"{k}={v:.6g}" for k, v in entry.get("registers", {}).items())
    flags = "".join(k[0] for k, v in entry.get("flags", {}).items() if v)
    return (
        f"PC={entry['pc']:4d} | {entry['opcode']:8s} {' '.join(entry.get('operands', [])):<20s}"
        f"| [{flags or '-':6s}] | {regs}"
    )


def format_trace_table(trace: list[dict[str, Any]]) -> Table:
    """Return a Rich Table of the full execution trace."""
    tbl = Table(title="Execution Trace", show_header=True, header_style="bold cyan", expand=True)
    tbl.add_column("PC", style="bold yellow", width=5)
    tbl.add_column("Opcode", width=8)
    tbl.add_column("Operands", width=20)
    tbl.add_column("Clock", width=10)
    tbl.add_column("A", width=14)
    tbl.add_column("Flags", width=8)

    for entry in trace:
        flags_str = "".join(k[0] for k, v in entry.get("flags", {}).items() if v) or "-"
        a_val = entry.get("registers", {}).get("A", 0)
        operands = " ".join(str(o) for o in entry.get("operands", []))
        tbl.add_row(
            str(entry["pc"]),
            entry["opcode"],
            operands,
            str(entry.get("clock_time", "")),
            f"{a_val:.6g}",
            flags_str,
        )
    return tbl


def format_trace_json(trace: list[dict[str, Any]]) -> str:
    """Return trace as JSON string."""
    return json.dumps(trace, indent=2, default=str)
