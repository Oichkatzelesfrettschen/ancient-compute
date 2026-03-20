"""Rotating gear ASCII art visualization.

Renders a named gear (mill, carry, timing) at a given rotation angle.
"""

# 8-step rotation frames for a gear labeled with the given name
_GEAR_TEMPLATE = [
    "   ___\n  / | \\\n | {name:^5} |\n  \\_|_/",
    "   _|_\n  / - \\\n | {name:^5} |\n  \\___/",
    "   ___\n  \\ | /\n | {name:^5} |\n  /___\\",
    "   _|_\n  \\ - /\n | {name:^5} |\n  /   \\",
]


def render_gear(name: str, frame_idx: int) -> str:
    """Render a named gear at the given animation frame index."""
    template = _GEAR_TEMPLATE[frame_idx % len(_GEAR_TEMPLATE)]
    label = name[:5]
    return template.format(name=label)
