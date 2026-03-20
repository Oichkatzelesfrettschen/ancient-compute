"""Animation panel widget: composite mechanical visualization."""

from textual.reactive import reactive
from textual.widget import Widget

from ..animation.barrel_rotation import render_barrel
from ..animation.carry_propagation import render_carry
from ..animation.gear_wheels import render_gear
from ..animation.mill_operation import render_mill


class AnimationPanel(Widget):
    """Composite animated mechanical view."""

    DEFAULT_CSS = """
    AnimationPanel {
        border: solid cyan;
        height: 14;
        padding: 0 1;
    }
    """

    frame_idx: reactive[int] = reactive(0)
    snapshot: reactive[dict] = reactive({})

    def render(self) -> str:
        snap = self.snapshot
        barrel = snap.get("barrel", {})
        barrel_name = barrel.get("active") or "IDLE"
        barrel_step = barrel.get("step", 0)
        opcode = snap.get("last_opcode", "NOP")
        ingress = snap.get("mill_operand_buffer", 0.0)
        egress = snap.get("mill_result_buffer", 0.0)
        fi = self.frame_idx

        lines = []

        # Gear row
        gear_line = "  ".join(render_gear(g, fi) for g in ["MLL", "ADD", "CAR"])
        lines.append(gear_line.split("\n")[0])

        # Mill operation
        mill_art = render_mill(opcode, ingress, egress, fi)
        lines.extend(mill_art.split("\n"))

        lines.append("")

        # Barrel rotation
        barrel_art = render_barrel(barrel_name, 6, barrel_step, fi)
        for line in barrel_art.split("\n")[:4]:
            lines.append(line)

        lines.append("")

        # Carry propagation
        lines.append(render_carry(fi, active=(barrel_name != "IDLE")))

        return "\n".join(lines)
