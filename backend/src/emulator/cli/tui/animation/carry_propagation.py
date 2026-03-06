"""Carry chain sweep animation -- right-to-left propagation."""

_WIDTH = 10  # Number of digit positions to show


def render_carry(frame_idx: int, active: bool = True) -> str:
    """Render carry propagation sweep.

    Args:
        frame_idx:  Animation frame counter.
        active:     Whether a carry is propagating.

    Returns:
        Single-line ASCII art carry chain.
    """
    if not active:
        return "Carry: " + "[ ] " * _WIDTH

    # Sweep position: right to left (10 -> 0 over _WIDTH+2 frames)
    pos = _WIDTH - (frame_idx % (_WIDTH + 2))
    pos = max(0, min(pos, _WIDTH - 1))

    cells = []
    for i in range(_WIDTH):
        if i == pos:
            cells.append("[C]")
        elif i < pos:
            cells.append("[1]")
        else:
            cells.append("[ ]")

    return "Carry: " + " ".join(cells)
