"""ancient-compute repl -- interactive engine REPL."""

import click
from rich.console import Console

from ...analytical_engine import Engine, Instruction
from ..assembler.syntax import tokenize_line
from ..formatter.state import format_state

console = Console()

_HELP_TEXT = """\
Babbage Analytical Engine Interactive REPL
Commands:
  OPCODE [operands]   Execute one instruction (e.g. LOAD A 42)
  .state              Print current engine state
  .mem [start] [n]    Print memory window
  .reset              Reset engine to initial state
  .help               Show this help
  .quit / Ctrl-D      Exit
"""


@click.command("repl")
def repl_cmd():
    """Launch an interactive engine REPL."""
    engine = Engine()
    console.print("[bold green]Babbage Analytical Engine REPL[/]")
    console.print("Type [cyan].help[/] for commands, [cyan].quit[/] to exit.\n")

    while True:
        try:
            raw = input("ae> ").strip()
        except (EOFError, KeyboardInterrupt):
            console.print("\n[dim]Goodbye.[/]")
            break

        if not raw:
            continue

        if raw.lower() in (".quit", "quit", "exit"):
            console.print("[dim]Goodbye.[/]")
            break

        if raw.lower() == ".help":
            console.print(_HELP_TEXT)
            continue

        if raw.lower() == ".state":
            console.print(format_state(engine))
            continue

        if raw.lower().startswith(".mem"):
            parts = raw.split()
            start = int(parts[1]) if len(parts) > 1 else 0
            n = int(parts[2]) if len(parts) > 2 else 8
            from ..formatter.state import format_memory
            console.print(format_memory(engine, start, n))
            continue

        if raw.lower() == ".reset":
            engine = Engine()
            console.print("[dim]Engine reset.[/]")
            continue

        # Try to parse as an instruction
        _, opcode, operands = tokenize_line(raw)
        if not opcode:
            console.print(f"[red]Unknown command:[/] {raw}")
            continue

        instr = Instruction(opcode, operands)
        engine.instruction_cards = [instr]
        engine.PC = 0
        engine.running = True
        try:
            engine.execute_instruction(instr)
            engine.PC = 0  # Reset for next instruction
        except Exception as exc:
            console.print(f"[red]Error:[/] {exc}")
