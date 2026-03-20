"""ancient-compute step -- step-through execution."""

import sys

import click
from rich.console import Console

from ...analytical_engine import Engine
from ..formatter.state import format_state

console = Console()


@click.command("step")
@click.argument("program", type=click.Path(exists=True))
@click.option("--breakpoint", "bp", type=int, default=None, help="Break at this PC address.")
def step_cmd(program, bp):
    """Step through PROGRAM one instruction at a time."""
    engine = Engine()

    try:
        engine.load_program(program)
    except Exception as exc:
        console.print(f"[bold red]Assembly error:[/] {exc}")
        sys.exit(1)

    console.print(
        f"[bold green]Stepping[/] {program} ({len(engine.instruction_cards)} instructions)"
    )
    console.print("Press Enter to step, 'r' to run, 'q' to quit.")

    while engine.running and len(engine.instruction_cards) > engine.PC:
        if bp is not None and bp == engine.PC:
            console.print(f"[bold yellow]Breakpoint hit at PC={engine.PC}[/]")

        console.print(f"\n[bold]PC={engine.PC}[/] | {format_state(engine)}")

        if len(engine.instruction_cards) > engine.PC:
            instr = engine.instruction_cards[engine.PC]
            console.print(f"  Next: [cyan]{instr.opcode}[/] {' '.join(instr.operands)}")

        try:
            user = input("> ").strip().lower()
        except (EOFError, KeyboardInterrupt):
            break

        if user == "q":
            break
        elif user == "r":
            engine.run()
            break
        else:
            engine.step_one_instruction()

    console.print("\n[dim]Final state:[/]")
    console.print(format_state(engine))
