"""Babbage Analytical Engine emulator package.

Re-exports the public API from engine.py for backward compatibility.
All existing ``from backend.src.emulator.analytical_engine import X``
imports continue to work unchanged.
"""

from .engine import TIMING_TABLE, BabbageNumber, Engine, Instruction, MechanicalFailureError

__all__ = [
    "BabbageNumber",
    "Engine",
    "Instruction",
    "MechanicalFailureError",
    "TIMING_TABLE",
]
