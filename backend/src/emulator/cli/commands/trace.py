"""ancient-compute trace -- export execution trace."""

import sys

import click
from rich.console import Console

from ...analytical_engine import Engine
from ..formatter.trace import format_trace_table, format_trace_json

console = Console()


@click.command("trace")
@click.argument("program", type=click.Path(exists=True))
@click.option("--output", "-o", type=click.Path(), default=None,
              help="Write trace to FILE instead of stdout.")
@click.option("--format", "fmt", type=click.Choice(["json", "table"]), default="table")
def trace_cmd(program, output, fmt):
    """Run PROGRAM and export its execution trace."""
    engine = Engine()

    try:
        engine.load_program(program)
    except Exception as exc:
        console.print(f"[bold red]Assembly error:[/] {exc}")
        sys.exit(1)

    engine.run()

    if fmt == "json":
        result = format_trace_json(engine.execution_trace)
        if output:
            with open(output, "w", encoding="utf-8") as fh:
                fh.write(result)
            console.print(f"Trace written to {output}")
        else:
            click.echo(result)
    else:
        tbl = format_trace_table(engine.execution_trace)
        if output:
            file_console = Console(file=open(output, "w", encoding="utf-8"), markup=False)
            file_console.print(tbl)
            console.print(f"Trace written to {output}")
        else:
            console.print(tbl)
