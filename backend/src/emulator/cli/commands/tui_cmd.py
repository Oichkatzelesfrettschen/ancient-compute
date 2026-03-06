"""ancient-compute tui -- launch TUI dashboard."""

import sys

import click
from rich.console import Console

console = Console()


@click.command("tui")
@click.argument("program", type=click.Path(exists=True), required=False)
def tui_cmd(program):
    """Launch the Textual TUI dashboard, optionally loading PROGRAM."""
    try:
        from ..tui.app import BabbageApp
    except ImportError as exc:
        console.print(f"[red]TUI unavailable:[/] {exc}")
        sys.exit(1)

    app = BabbageApp(program=program)
    app.run()
