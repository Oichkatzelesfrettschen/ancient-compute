"""Trace panel widget: instruction history with color coding."""

from textual.reactive import reactive
from textual.widget import Widget


class TracePanel(Widget):
    """Shows last N executed instructions."""

    DEFAULT_CSS = """
    TracePanel {
        border: solid cyan;
        height: 14;
        padding: 0 1;
    }
    """

    trace: reactive[list] = reactive([])
    max_lines: int = 12

    def render(self) -> str:
        recent = self.trace[-self.max_lines :]
        lines = ["  Trace (recent):"]
        for entry in recent:
            ops = " ".join(str(o) for o in entry.get("operands", []))
            line = f"  {entry['pc']:4d}: {entry['opcode']:<8s} {ops:<20s}"
            # Mark current (last) instruction
            if entry is recent[-1]:
                line = "> " + line[2:]
            lines.append(line)
        return "\n".join(lines)
