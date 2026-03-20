"""Status bar widget: cycle count, phase, engine state summary."""

from textual.reactive import reactive
from textual.widget import Widget


class StatusBar(Widget):
    """Bottom status bar showing engine summary."""

    DEFAULT_CSS = """
    StatusBar {
        height: 1;
        background: $primary;
        color: $text;
        padding: 0 1;
    }
    """

    snapshot: reactive[dict] = reactive({})

    def render(self) -> str:
        snap = self.snapshot
        clock = snap.get("clock_time", 0)
        barrel = snap.get("barrel", {}).get("active", "IDLE")
        pc = snap.get("pc", "-")
        running = "RUN" if snap.get("running", False) else "HALT"
        return (
            f" PC={pc}  Clock={clock:.1f}s  Barrel={barrel}"
            f"  [{running}]  [S]tep [R]un [B]reak [Q]uit"
        )
