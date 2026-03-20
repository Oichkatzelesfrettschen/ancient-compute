"""Memory inspector widget: scrollable memory view with change highlighting."""

from typing import Any

from textual.reactive import reactive
from textual.widget import Widget


class MemoryInspector(Widget):
    """Scrollable memory view showing 16 words."""

    DEFAULT_CSS = """
    MemoryInspector {
        border: solid cyan;
        height: 14;
        padding: 0 1;
    }
    """

    memory_window: reactive[list[Any]] = reactive([])
    mem_start: reactive[int] = reactive(0)
    changed_addrs: reactive[set[int]] = reactive(set())

    def render(self) -> str:
        lines = [f"  Memory [{self.mem_start}..{self.mem_start + len(self.memory_window) - 1}]"]
        for i, val in enumerate(self.memory_window):
            addr = self.mem_start + i
            marker = "*" if addr in self.changed_addrs else " "
            lines.append(f" {marker}[{addr:4d}]: {val:.10g}")
        return "\n".join(lines)
