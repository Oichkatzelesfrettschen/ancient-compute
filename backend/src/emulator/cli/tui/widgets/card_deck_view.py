"""Card deck view widget: program listing with current instruction highlighted."""

from textual.reactive import reactive
from textual.widget import Widget


class CardDeckView(Widget):
    """Shows the loaded program with current PC highlighted."""

    DEFAULT_CSS = """
    CardDeckView {
        border: solid cyan;
        height: 14;
        padding: 0 1;
    }
    """

    instructions: reactive[list] = reactive([])
    current_pc: reactive[int] = reactive(0)

    def render(self) -> str:
        lines = ["  Program:"]
        window_start = max(0, self.current_pc - 4)
        window_end = min(len(self.instructions), window_start + 10)

        for i in range(window_start, window_end):
            instr = self.instructions[i]
            ops = " ".join(str(o) for o in instr.operands)
            marker = ">" if i == self.current_pc else " "
            lines.append(f" {marker} {i:4d}: {instr.opcode:<8s} {ops}")

        return "\n".join(lines)
