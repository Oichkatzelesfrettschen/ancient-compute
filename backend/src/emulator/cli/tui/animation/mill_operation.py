"""Mill ALU visualization: ingress -> operation -> egress."""


def render_mill(opcode: str, ingress: float, egress: float, frame_idx: int) -> str:
    """Render the mill operation in progress.

    Args:
        opcode:     Current operation (ADD, SUB, MULT, DIV, etc.).
        ingress:    Ingress axis value (operand).
        egress:     Egress axis value (result so far).
        frame_idx:  Animation frame counter (0-3).

    Returns:
        Multi-line ASCII art string.
    """
    phase = frame_idx % 4

    op_sym = {
        "ADD": "+", "SUB": "-", "MULT": "*", "DIV": "/",
        "SQRT": "sqrt", "LOAD": "<-", "STOR": "->",
    }.get(opcode, "?")

    # Four-phase animation: load -> engage -> operate -> egress
    if phase == 0:
        mid_line = f"  [{op_sym:^6}]   <<< {ingress:.4g}"
    elif phase == 1:
        mid_line = f"  [{op_sym:^6}]   ..."
    elif phase == 2:
        mid_line = f"  [{op_sym:^6}]   >>> {egress:.4g}"
    else:
        mid_line = f"  [{op_sym:^6}]      "

    lines = [
        f"Ingress: {ingress:.8g}",
        mid_line,
        f"Egress:  {egress:.8g}",
    ]
    return "\n".join(lines)
