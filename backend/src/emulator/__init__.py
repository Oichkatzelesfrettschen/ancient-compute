"""
Difference Engine No. 2 (DE2) Emulator

A faithful mechanical simulation of Charles Babbage's Difference Engine No. 2,
based on Science Museum Group (SMG) technical documentation and working hardware
(1991 calculator, 2002 printer/stereotyper).

Modules:
  types.py       - Dataclasses and type definitions
  columns.py     - DigitColumn, ColumnBank (digit storage and operations)
  carry.py       - AnticipatingCarriage (overlapped carry logic)
  timing.py      - TimingController (main shaft angle → event dispatch)
  core.py        - DEMachine (top-level emulator orchestrator)
  printer.py     - PrinterApparatus, StereotypeFrame (I/O)
  cards.py       - CardReader, AECard (Analytical Engine support)
  debugger.py    - Debugger, SymbolTable, Stepper (inspection & breakpoints)

Example usage:
  from backend.src.emulator import DEMachine

  # Create emulator with initial difference values
  de = DEMachine(initial_differences=[1, 2, 2, 0, 0, 0, 0, 0])

  # Execute 6 cycles (polynomial evaluation)
  for _ in range(6):
    de.run_cycle()

  # Get results
  page = de.printer.get_printed_page()
  molds = de.stereotyper.get_completed_molds()

  # Debug with breakpoints
  debugger = Debugger(de)
  bp_id = debugger.set_breakpoint("main_shaft_angle == 240")
  debugger.step_until_breakpoint(bp_id)
  print(debugger.inspect_variable("x", debugger.get_snapshot()))
"""

from backend.src.emulator.types import (
    DebugSnapshot,
    TimeEvent,
    CarryState,
    ColumnSnapshot,
    PrinterSnapshot,
    StereotyperSnapshot,
    OperationResult,
    TimingSpec,
    MachineConfig,
    DEFAULT_TIMING_SPEC,
)

from backend.src.emulator.analytical_engine import (
    BabbageNumber,
    Instruction,
    Engine,
    TIMING_TABLE,
)

from backend.src.emulator.note_g_deck import (
    load_deck as load_note_g_deck,
    run_once as run_note_g_once,
    run_series as run_note_g_series,
)
from backend.src.emulator.tally_marks import TallyMarksEmulator
from backend.src.emulator.abacus import AbacusEmulator
from backend.src.emulator.clay_tokens import ClayTokensEmulator
from backend.src.emulator.quipu import QuipuEmulator
from backend.src.emulator.slide_rule import SlideRuleEmulator
from backend.src.emulator.pascaline import PascalineEmulator
from backend.src.emulator.jacquard import JacquardEmulator
from backend.src.emulator.leibniz_reckoner import LeibnizReckonerEmulator
from backend.src.emulator.antikythera import GearEdge, GearTrain
from backend.src.emulator.astrolabe import AstrolabeEmulator, AstrolabeQuery

__all__ = [
    # Type system
    "DebugSnapshot",
    "TimeEvent",
    "CarryState",
    "ColumnSnapshot",
    "PrinterSnapshot",
    "StereotyperSnapshot",
    "OperationResult",
    "TimingSpec",
    "MachineConfig",
    "DEFAULT_TIMING_SPEC",
    # Analytical Engine
    "BabbageNumber",
    "Instruction",
    "Engine",
    "TIMING_TABLE",
    "load_note_g_deck",
    "run_note_g_once",
    "run_note_g_series",
    "TallyMarksEmulator",
    "AbacusEmulator",
    "ClayTokensEmulator",
    "QuipuEmulator",
    "SlideRuleEmulator",
    "PascalineEmulator",
    "JacquardEmulator",
    "LeibnizReckonerEmulator",
    "GearEdge",
    "GearTrain",
    "AstrolabeEmulator",
    "AstrolabeQuery",
]

__version__ = "0.1.0"
__author__ = "Ancient Compute Project"
__doc_reference__ = "SMG Technical Description: Charles Babbage's Difference Engine No. 2 (232 pp.)"
