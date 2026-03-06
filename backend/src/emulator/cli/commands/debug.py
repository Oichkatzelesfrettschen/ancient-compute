"""ancient-compute debug -- interactive debugger wrapping the Debugger class."""

import sys

import click
from rich.console import Console

from ...analytical_engine import Engine
from ...adapter import AEMachineAdapter
from ...debugger import Debugger
from ..formatter.state import format_state

console = Console()

_DEBUG_HELP = """\
Debugger commands:
  s / step        Step one instruction
  r / run         Run to completion (or next breakpoint)
  b <PC>          Set breakpoint at PC
  bl              List breakpoints
  bc              Clear all breakpoints
  p               Print current state
  m [start] [n]   Print memory window
  q / quit        Quit debugger
"""


@click.command("debug")
@click.argument("program", type=click.Path(exists=True))
def debug_cmd(program):
    """Launch interactive debugger for PROGRAM."""
    engine = Engine()

    try:
        engine.load_program(program)
    except Exception as exc:
        console.print(f"[bold red]Assembly error:[/] {exc}")
        sys.exit(1)

    adapter = AEMachineAdapter(engine)
    dbg = Debugger(adapter)

    console.print(f"[bold green]Debugger[/] -- {program} ({len(engine.instruction_cards)} instructions)")
    console.print("Type [cyan]help[/] for commands.\n")

    while True:
        try:
            raw = input("dbg> ").strip().lower()
        except (EOFError, KeyboardInterrupt):
            console.print("\n[dim]Exiting debugger.[/]")
            break

        if not raw:
            continue

        if raw == "help":
            console.print(_DEBUG_HELP)
        elif raw in ("s", "step"):
            engine.step_one_instruction()
            console.print(format_state(engine))
        elif raw in ("r", "run"):
            engine.run()
            console.print("[dim]Execution complete.[/]")
            console.print(format_state(engine))
        elif raw.startswith("b "):
            try:
                pc = int(raw.split()[1])
                engine.set_breakpoint("address", pc)
                console.print(f"[dim]Breakpoint set at PC={pc}[/]")
            except (IndexError, ValueError):
                console.print("[red]Usage: b <PC>[/]")
        elif raw == "bl":
            for i, bp in enumerate(engine.breakpoints):
                console.print(f"  {i}: {bp}")
        elif raw == "bc":
            engine.breakpoints.clear()
            console.print("[dim]Breakpoints cleared.[/]")
        elif raw == "p":
            console.print(format_state(engine))
        elif raw.startswith("m"):
            parts = raw.split()
            start = int(parts[1]) if len(parts) > 1 else 0
            n = int(parts[2]) if len(parts) > 2 else 8
            from ..formatter.state import format_memory
            console.print(format_memory(engine, start, n))
        elif raw in ("q", "quit"):
            break
        else:
            console.print(f"[red]Unknown command:[/] {raw}  (type 'help')")
