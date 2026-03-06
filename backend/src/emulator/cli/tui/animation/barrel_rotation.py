"""Barrel drum visualization with stud pattern and active row marker."""

_STUD = "[|]"
_EMPTY = "[.]"
_ACTIVE_PREFIX = ">> "
_INACTIVE_PREFIX = "   "


def render_barrel(barrel_name: str, num_steps: int, active_step: int, frame_idx: int) -> str:
    """Render a barrel drum showing its stud rows.

    Args:
        barrel_name:  Name of the active barrel (e.g. "ADD", "MULT").
        num_steps:    Total number of steps in the barrel.
        active_step:  Current step index being executed.
        frame_idx:    Animation frame counter (for scrolling effect).

    Returns:
        Multi-line ASCII art string.
    """
    lines = [f"Barrel: {barrel_name}  ({num_steps} steps)"]
    lines.append("-" * 28)

    # Show up to 5 rows around the active step
    window_start = max(0, active_step - 2)
    window_end = min(num_steps, window_start + 5)

    for i in range(window_start, window_end):
        # Alternating stud pattern per row (simplified visual)
        pattern = "".join(_STUD if (j + i + frame_idx) % 2 == 0 else _EMPTY for j in range(5))
        prefix = _ACTIVE_PREFIX if i == active_step else _INACTIVE_PREFIX
        step_label = f"Step {i:2d}:"
        lines.append(f"{prefix}{step_label} {pattern}")

    lines.append("-" * 28)
    return "\n".join(lines)
