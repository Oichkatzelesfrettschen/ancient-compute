"""ancient-compute assemble -- assemble a .basm file."""

import json
import sys

import click
from rich.console import Console

from ..assembler.parser import assemble_file

console = Console()


@click.command("assemble")
@click.argument("input_file", type=click.Path(exists=True))
@click.option("--output", "-o", type=click.Path(), default=None,
              help="Write assembled output to FILE.")
@click.option("--format", "fmt", type=click.Choice(["text", "json"]), default="text")
def assemble_cmd(input_file, output, fmt):
    """Assemble INPUT_FILE and display the instruction list."""
    try:
        instructions = assemble_file(input_file)
    except Exception as exc:
        console.print(f"[bold red]Error:[/] {exc}")
        sys.exit(1)

    if fmt == "json":
        data = [{"pc": i, "opcode": instr.opcode, "operands": instr.operands}
                for i, instr in enumerate(instructions)]
        result = json.dumps(data, indent=2)
        if output:
            with open(output, "w", encoding="utf-8") as fh:
                fh.write(result)
            console.print(f"Written to {output}")
        else:
            click.echo(result)
    else:
        for i, instr in enumerate(instructions):
            operands = " ".join(instr.operands)
            line = f"{i:4d}: {instr.opcode:<10s} {operands}"
            if output is None:
                console.print(line)
            else:
                pass  # collected below

        if output:
            with open(output, "w", encoding="utf-8") as fh:
                for i, instr in enumerate(instructions):
                    fh.write(f"{i:4d}: {instr.opcode:<10s} {' '.join(instr.operands)}\n")
            console.print(f"Written to {output}")
