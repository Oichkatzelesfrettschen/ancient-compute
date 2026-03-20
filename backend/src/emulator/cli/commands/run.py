"""ancient-compute run -- execute a .basm program."""

import sys

import click
from rich.console import Console

from ...analytical_engine import Engine
from ...simulation.bridge import SimulationBridge
from ...simulation.engine import SimulationEngine
from ...simulation.state import SimulationConfig
from ..assembler.fourmilab import parse_fourmilab_source, translate_fourmilab
from ..formatter.state import format_state
from ..formatter.trace import format_trace_json, format_trace_table

console = Console()

_MACHINE_CHOICES = click.Choice(
    ["analytical-engine", "scheutz", "ludgate", "torres", "zuse"],
    case_sensitive=False,
)


@click.command("run")
@click.argument("program", type=click.Path(exists=True))
@click.option("--trace", is_flag=True, help="Print execution trace after completion.")
@click.option("--dump", is_flag=True, help="Dump engine state after completion.")
@click.option("--physics", is_flag=True, help="Enable physics coupling.")
@click.option(
    "--format",
    "fmt",
    type=click.Choice(["basm", "fourmilab"]),
    default="basm",
    help="Source format (default: basm).",
)
@click.option(
    "--trace-format",
    type=click.Choice(["table", "json"]),
    default="table",
    help="Trace output format.",
)
@click.option(
    "--machine", type=_MACHINE_CHOICES, default="analytical-engine", help="Emulator machine type."
)
def run_cmd(program: str, trace: bool, dump: bool, physics: bool, fmt: str, trace_format: str, machine: str) -> None:
    """Execute PROGRAM on the Analytical Engine emulator."""
    output_lines = []

    def capture(msg: str) -> None:
        output_lines.append(msg)
        console.print(msg)

    if machine != "analytical-engine":
        _run_historical(machine, program)
        return

    physical_engine = None
    if physics:
        cfg = SimulationConfig(rpm=30.0)
        phys = SimulationEngine(cfg)
        physical_engine = SimulationBridge(phys)
        console.print("[dim]Physics coupling enabled (rpm=30, wear+thermal+lubrication)[/]")

    engine = Engine(output_callback=capture, physical_engine=physical_engine)

    try:
        if fmt == "fourmilab":
            with open(program, encoding="utf-8") as fh:
                cards = parse_fourmilab_source(fh.read())
            instructions = translate_fourmilab(cards)
            engine.instruction_cards = instructions
        else:
            engine.load_program(program)
    except Exception as exc:
        console.print(f"[bold red]Assembly error:[/] {exc}")
        sys.exit(1)

    console.print(
        f"[bold green]Running[/] {program} ({len(engine.instruction_cards)} instructions)"
    )

    try:
        engine.run()
    except Exception as exc:
        console.print(f"[bold red]Runtime error:[/] {exc}")
        sys.exit(1)

    console.print(f"[dim]Completed. Clock: {engine.clock_time}s[/]")

    if physics and physical_engine is not None:
        report = physical_engine.physics_report()
        console.print("\n[bold]Physics Report:[/]")
        console.print(f"  Simulated time: {report['simulated_time_s']:.4f}s")
        console.print(f"  Temperature:    {report['temperature_C']:.1f}C")
        console.print(f"  Shaft deflection: {report['shaft_deflection_mm']:.4f}mm")
        console.print(f"  Max bearing clearance: {report['max_bearing_clearance_mm']:.4f}mm")
        console.print(f"  Lubrication regime: {report['lubrication_regime']}")
        console.print(f"  Energy consumed: {report['energy_consumed_J']:.2f}J")
        if report["failed"]:
            console.print(f"  [red]MECHANICAL FAILURE: {report['failure_reason']}[/]")

    if dump:
        console.print(format_state(engine))

    if trace:
        if trace_format == "json":
            click.echo(format_trace_json(engine.execution_trace))
        else:
            console.print(format_trace_table(engine.execution_trace))


def _run_historical(machine: str, program: str) -> None:
    """Dispatch to a historical machine runner."""
    console.print(
        f"[yellow]Historical machine '{machine}' not yet integrated in run command. "
        "Use the dedicated machine module directly.[/]"
    )
