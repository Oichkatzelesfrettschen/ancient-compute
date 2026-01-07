"""Integration-style tests for simple AE programs."""

from pathlib import Path

from backend.src.emulator.analytical_engine import Engine


def test_babbage_addition_program():
    repo_root = Path(__file__).resolve().parents[3]
    program = repo_root / "docs/simulation/programs/babbage_addition.ae"

    engine = Engine()
    engine.load_program(str(program))
    engine.run()

    assert engine.result_cards[-1]["value"].to_decimal() == 5.0
