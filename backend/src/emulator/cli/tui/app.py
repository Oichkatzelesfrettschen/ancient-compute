"""BabbageApp -- Textual TUI for the Analytical Engine emulator.

Layout (80x24 terminal):

+----------------------------------------------------------+
|  BABBAGE ANALYTICAL ENGINE                    [S] [R] [Q]|
+---------------------------+------------------------------+
| REGISTERS                 | MECHANICAL ANIMATION         |
|  A: ...                   |    gears / mill / carry      |
|  PC: ...  Clock: ...      |                              |
+---------------------------+------------------------------+
| MEMORY [0..15]            | TRACE                        |
|  [0]: ...                 |  0: LOAD A [0]  A=5          |
|  ...                      | >7: CMP  C [0]               |
+---------------------------+------------------------------+
| PC=7  Clock=498s  Barrel=ADD  [HALT]  [S]tep [R]un [Q]uit|
+----------------------------------------------------------+
"""

from __future__ import annotations

import threading

from textual.app import App, ComposeResult
from textual.binding import Binding
from textual.containers import Horizontal, Vertical
from textual.timer import Timer
from textual.widgets import Footer, Header, Label

from ...analytical_engine import Engine
from ..assembler.parser import assemble_file
from .engine_worker import EngineStateUpdate, make_snapshot, snapshot_as_dict
from .widgets.animation_panel import AnimationPanel
from .widgets.card_deck_view import CardDeckView
from .widgets.memory_inspector import MemoryInspector
from .widgets.register_panel import RegisterPanel
from .widgets.status_bar import StatusBar
from .widgets.trace_panel import TracePanel

_ANIMATION_FPS = 8
_ANIMATION_INTERVAL = 1.0 / _ANIMATION_FPS


class BabbageApp(App):
    """Textual TUI dashboard for the Babbage Analytical Engine."""

    TITLE = "Babbage Analytical Engine Emulator"
    CSS = """
    Screen {
        layout: vertical;
    }
    #top-row {
        layout: horizontal;
        height: 1fr;
    }
    #bottom-row {
        layout: horizontal;
        height: 1fr;
    }
    RegisterPanel, AnimationPanel, MemoryInspector, TracePanel {
        width: 1fr;
    }
    """

    BINDINGS = [
        Binding("s", "step", "Step"),
        Binding("r", "run", "Run"),
        Binding("p", "pause", "Pause"),
        Binding("b", "set_breakpoint", "Break"),
        Binding("m", "toggle_memory", "Memory"),
        Binding("t", "toggle_trace", "Trace"),
        Binding("q", "quit", "Quit"),
        Binding("space", "step", "Step", show=False),
    ]

    def __init__(self, program: str | None = None) -> None:
        super().__init__()
        self.program = program
        self.engine = Engine()
        self._running_bg = False
        self._bg_thread: threading.Thread | None = None
        self._frame_idx = 0
        self._anim_timer: Timer | None = None

        if program:
            try:
                self.engine.load_program(program)
            except Exception as exc:
                self._load_error = str(exc)
            else:
                self._load_error = None
        else:
            self._load_error = None

    def compose(self) -> ComposeResult:
        yield Header()
        with Horizontal(id="top-row"):
            yield RegisterPanel()
            yield AnimationPanel()
        with Horizontal(id="bottom-row"):
            yield MemoryInspector()
            yield TracePanel()
        yield StatusBar()
        yield Footer()

    def on_mount(self) -> None:
        self._anim_timer = self.set_interval(_ANIMATION_INTERVAL, self._tick_animation)
        self._refresh_all()

    # --- Engine control actions ---

    def action_step(self) -> None:
        """Execute one instruction."""
        if self.engine.running and self.engine.PC < len(self.engine.instruction_cards):
            last_op = "NOP"
            if self.engine.PC < len(self.engine.instruction_cards):
                last_op = self.engine.instruction_cards[self.engine.PC].opcode
            self.engine.step_one_instruction()
            self._post_snapshot(last_op)

    def action_run(self) -> None:
        """Run engine in background thread until halt or breakpoint."""
        if self._running_bg:
            return
        self._running_bg = True
        self._bg_thread = threading.Thread(target=self._run_background, daemon=True)
        self._bg_thread.start()

    def action_pause(self) -> None:
        """Pause background execution."""
        self._running_bg = False
        self.engine.paused = True

    def action_set_breakpoint(self) -> None:
        """Set breakpoint at current PC + 5."""
        bp_pc = self.engine.PC + 5
        self.engine.set_breakpoint("address", bp_pc)

    def action_toggle_memory(self) -> None:
        """Scroll memory window by 16."""
        mem = self.query_one(MemoryInspector)
        mem.mem_start = (mem.mem_start + 16) % 2000
        self._refresh_memory()

    def action_toggle_trace(self) -> None:
        """No-op -- trace always visible."""
        pass

    # --- Internal helpers ---

    def _run_background(self) -> None:
        """Background thread: run engine, posting snapshots periodically."""
        while (self._running_bg and self.engine.running
               and self.engine.PC < len(self.engine.instruction_cards)):
            last_op = "NOP"
            if self.engine.PC < len(self.engine.instruction_cards):
                last_op = self.engine.instruction_cards[self.engine.PC].opcode
            self.engine.step_one_instruction()
            # Post snapshot via call_from_thread for thread safety
            snap = make_snapshot(self.engine, last_op)
            self.call_from_thread(self._apply_snapshot, snap)
        self._running_bg = False

    def _post_snapshot(self, last_op: str = "NOP") -> None:
        snap = make_snapshot(self.engine, last_op)
        self._apply_snapshot(snap)

    def _apply_snapshot(self, snap) -> None:
        """Update all widgets from a snapshot."""
        snap_dict = snapshot_as_dict(snap)

        reg_panel = self.query_one(RegisterPanel)
        reg_panel.snapshot = snap_dict
        reg_panel.refresh()

        anim = self.query_one(AnimationPanel)
        anim.snapshot = snap_dict
        anim.refresh()

        status = self.query_one(StatusBar)
        status.snapshot = snap_dict
        status.refresh()

        trace = self.query_one(TracePanel)
        trace.trace = snap.execution_trace
        trace.refresh()

        self._refresh_memory()

        # CardDeckView is optional; only update if present
        deck_nodes = self.query(CardDeckView)
        if deck_nodes:
            deck = deck_nodes.first()
            deck.current_pc = snap.pc
            deck.instructions = self.engine.instruction_cards
            deck.refresh()

    def _refresh_memory(self) -> None:
        mem = self.query_one(MemoryInspector)
        start = mem.mem_start
        window = [self.engine.memory[i].to_decimal()
                  for i in range(start, min(start + 16, len(self.engine.memory)))]
        mem.memory_window = window
        mem.refresh()

    def _refresh_all(self) -> None:
        self._post_snapshot()

    def _tick_animation(self) -> None:
        """Timer callback: advance animation frame."""
        self._frame_idx += 1
        anim = self.query_one(AnimationPanel)
        anim.frame_idx = self._frame_idx
        anim.refresh()
