"""ancient-compute deck -- Lovelace / Note G deck runner."""

import click
from rich.console import Console

from ...note_g_deck import run_note_g_exact

console = Console()


@click.command("deck")
@click.option("--note", type=click.Choice(["b", "c", "d", "g"], case_sensitive=False),
              default="g", show_default=True, help="Which Lovelace note to run.")
@click.option("--n", "n_count", type=int, default=3, show_default=True,
              help="Number of Bernoulli numbers to compute (note g), or iteration count.")
@click.option("--physics", is_flag=True, help="Enable physics coupling (note g only).")
def deck_cmd(note, n_count, physics):
    """Run a Lovelace card deck (Note B, C, D, or G)."""
    if note == "g":
        _run_note_g(n_count)
    elif note in ("b", "c", "d"):
        _run_lovelace_note(note, n_count)
    else:
        console.print(f"[red]Unknown note: {note}[/]")


def _run_note_g(n_count: int) -> None:
    """Run Note G (Bernoulli numbers)."""
    console.print(f"[bold green]Note G:[/] Computing {n_count} Bernoulli number(s)")
    results = run_note_g_exact(n_count)
    for i, b in enumerate(results, 1):
        console.print(f"  B_{2*i - 1} = [cyan]{b}[/]")


def _run_lovelace_note(note: str, n: int) -> None:
    """Run Notes B, C, or D via lovelace_notes module."""
    try:
        from ...lovelace_notes import run_note_b, run_note_c, run_note_d
    except ImportError:
        console.print(f"[red]lovelace_notes module not found. Run Phase 4 first.[/]")
        return

    runners = {"b": run_note_b, "c": run_note_c, "d": run_note_d}
    runner = runners[note]
    console.print(f"[bold green]Note {note.upper()}:[/] Running with n={n}")
    result = runner(n)
    console.print(f"  Result: [cyan]{result}[/]")
