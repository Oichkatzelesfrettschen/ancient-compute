"""Register panel widget: shows A, B, C, D registers, flags, PC, clock."""

from typing import Any

from textual.reactive import reactive
from textual.widget import Widget


class RegisterPanel(Widget):
    """Displays engine register state."""

    DEFAULT_CSS = """
    RegisterPanel {
        border: solid cyan;
        height: 14;
        padding: 0 1;
    }
    """

    snapshot: reactive[dict[str, Any]] = reactive({})

    def render(self) -> str:
        snap = self.snapshot
        if not snap:
            return "No engine state"

        regs = snap.get("registers", {})
        flags = snap.get("flags", {})
        lines = [
            f"  PC: {snap.get('pc', '-'):>4}   Clock: {snap.get('clock_time', 0):.1f}s",
            "",
            *[f"  {k}: {v:.10g}" for k, v in regs.items()],
            "",
            "  Flags: " + " ".join(f"{k[0]}={int(v)}" for k, v in flags.items()),
        ]
        barrel = snap.get("barrel", {})
        if barrel.get("active"):
            lines.append(f"  Barrel: {barrel['active']}  Step: {barrel.get('step', '?')}")
        return "\n".join(lines)
