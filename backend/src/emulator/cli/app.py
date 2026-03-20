"""ancient-compute CLI -- main Click group."""

import click

from .commands.assemble import assemble_cmd
from .commands.debug import debug_cmd
from .commands.deck import deck_cmd
from .commands.repl import repl_cmd
from .commands.run import run_cmd
from .commands.step import step_cmd
from .commands.trace import trace_cmd
from .commands.tui_cmd import tui_cmd


@click.group()
@click.version_option(version="0.1.0", prog_name="ancient-compute")
def cli() -> None:
    """Ancient Compute -- Babbage Analytical Engine emulator CLI.

    Run historical mechanical computer programs, explore execution traces,
    and visualize engine state via the TUI dashboard.
    """


cli.add_command(run_cmd, name="run")
cli.add_command(step_cmd, name="step")
cli.add_command(debug_cmd, name="debug")
cli.add_command(trace_cmd, name="trace")
cli.add_command(repl_cmd, name="repl")
cli.add_command(assemble_cmd, name="assemble")
cli.add_command(deck_cmd, name="deck")
cli.add_command(tui_cmd, name="tui")
