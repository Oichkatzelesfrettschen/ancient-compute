"""Thin shim: delegates to canonical backend.src.emulator modules.

This file was the original standalone emulator. All functionality has been
absorbed into backend/src/emulator/ (analytical_engine.py, types.py, etc.).
Import from there directly; this shim exists for backwards compatibility only.

WHY: The canonical emulator (analytical_engine.py) has micro-op barrels,
     physics coupling, and 1100+ tests. The legacy code here was a duplicate.
WHAT: Re-exports the key public names that external scripts may reference.
HOW: Replace any `from tools.babbage_emulator import X` with
     `from backend.src.emulator import X` or equivalent.
"""

from backend.src.emulator.analytical_engine import Engine, Instruction  # noqa: F401
from backend.src.emulator.types import BabbageNumber  # noqa: F401

# Legacy CLI entry point -- delegates to the canonical Click app.
if __name__ == "__main__":
    from backend.src.emulator.cli.app import cli

    cli()
