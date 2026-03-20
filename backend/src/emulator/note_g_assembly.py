"""Generate AE assembly instructions for Note G (Bernoulli numbers).

Produces a flat list of AE assembly text that can be loaded via
Engine.load_program_from_text() and executed natively on the emulated
Analytical Engine.

Variable mapping (V_k -> memory[k-1]):
  V1 = mem[0]  (constant 1)
  V2 = mem[1]  (constant 2)
  V3 = mem[2]  (outer loop counter n)
  V4 = mem[3]  (scratch: 2n)
  V5 = mem[4]  (scratch: 2n+1)
  V6 = mem[5]  (decreasing numerator)
  V7 = mem[6]  (increasing denominator)
  V8 = mem[7]  (scratch quotient)
  V9 = mem[8]  (scratch quotient)
  V10 = mem[9]  (inner loop counter n-1)
  V11 = mem[10] (running A product)
  V12 = mem[11] (B*A product)
  V13 = mem[12] (running sum A0 + B1*A1 + ...)
  V21 = mem[20] (B value slot 0 = B1)
  V22 = mem[21] (B value slot 1 = B3; op 21 always reads here)
  V23 = mem[22] (B value slot 2 = B5)
  V24 = mem[23] (output: B_{2n-1})

Extended B storage (for n_target > 3):
  mem[25] = B7, mem[26] = B9, ...

Loop unrolling (matching Ada's own Table A.2 static instantiation):
  n=1: ops 1-7, 24-25
  n=2: ops 1-7, 8-12, 24-25
  n=3: ops 1-7, 8-12, [ops 13-23 x1], 24-25
  n=k: ops 1-7, 8-12, [ops 13-23 x(k-2)], 24-25

References:
  - Menabrea, L. F., & Lovelace, A. A. (1843). Note G, Table A.2.
  - Note G deck in docs/simulation/NOTE_G_DECK.yaml.
"""

from __future__ import annotations

# Address of each V-variable in engine memory (0-indexed).
_V = {i: i - 1 for i in range(1, 25)}  # V1=mem[0], ..., V24=mem[23]

# B-value storage addresses (indexed from 0):
# B_store[0]=B1 -> mem[20]=V21
# B_store[1]=B3 -> mem[21]=V22
# B_store[2]=B5 -> mem[22]=V23
# B_store[3]=B7 -> mem[25] (extended, avoids collision with V24=mem[23])
# B_store[k] -> mem[25+k-3] for k>=3
_B_STORE = [20, 21, 22] + list(range(25, 50))


def _addr(vk: int) -> int:
    """Return the memory address for variable Vk (1-indexed)."""
    return _V[vk]


def _reset_working_vars() -> list[str]:
    """Emit CLR instructions to zero working variables for a fresh outer iteration.

    WHY: run_note_g() calls init_state(n) before each outer n iteration, which
    resets all variables to 0. The assembly must replicate this by zeroing the
    working registers before each unrolled n-block. Constants V1 (mem[0]) and
    V2 (mem[1]) and B-slots (mem[20..]) are NOT reset here.
    """
    lines: list[str] = []
    # Zero: V3-V16, V24 (working and output)
    zero_slots = list(range(2, 16)) + [23]  # mem indices
    for addr in zero_slots:
        lines += [
            "LOAD A, 0",
            f"STOR A, [{addr}]",
        ]
    return lines


def _op1(lines: list[str]) -> None:
    """Op 1: V4 = V5 = V6 = V2 * V3  (result = 2n)."""
    lines += [
        f"LOAD A, [{_addr(2)}]",  # A = V2 = 2
        f"LOAD B, [{_addr(3)}]",  # B = V3 = n
        "MULT A, B",  # A = 2 * n
        f"STOR A, [{_addr(4)}]",  # V4 = 2n
        f"STOR A, [{_addr(5)}]",  # V5 = 2n
        f"STOR A, [{_addr(6)}]",  # V6 = 2n
    ]


def _op2(lines: list[str]) -> None:
    """Op 2: V4 = V4 - V1  (2n - 1)."""
    lines += [
        f"LOAD A, [{_addr(4)}]",  # A = V4
        f"LOAD B, [{_addr(1)}]",  # B = V1 = 1
        "SUB A, B",  # A = 2n - 1
        f"STOR A, [{_addr(4)}]",  # V4 = 2n-1
    ]


def _op3(lines: list[str]) -> None:
    """Op 3: V5 = V5 + V1  (2n + 1)."""
    lines += [
        f"LOAD A, [{_addr(5)}]",  # A = V5
        f"LOAD B, [{_addr(1)}]",  # B = V1 = 1
        "ADD A, B",  # A = 2n + 1
        f"STOR A, [{_addr(5)}]",  # V5 = 2n+1
    ]


def _op4(lines: list[str]) -> None:
    """Op 4: V11 = V4 / V5  ((2n-1) / (2n+1))."""
    lines += [
        f"LOAD A, [{_addr(4)}]",  # A = V4
        f"LOAD B, [{_addr(5)}]",  # B = V5
        "DIV A, B",  # A = V4 / V5
        f"STOR A, [{_addr(11)}]",  # V11 = result
    ]


def _op5(lines: list[str]) -> None:
    """Op 5: V11 = V11 / V2  (0.5 * (2n-1)/(2n+1))."""
    lines += [
        f"LOAD A, [{_addr(11)}]",  # A = V11
        f"LOAD B, [{_addr(2)}]",  # B = V2 = 2
        "DIV A, B",  # A = V11 / 2
        f"STOR A, [{_addr(11)}]",  # V11 = result
    ]


def _op6(lines: list[str]) -> None:
    """Op 6: V13 = V13 - V11  (A0 = -(0.5*(2n-1)/(2n+1)))."""
    lines += [
        f"LOAD A, [{_addr(13)}]",  # A = V13 (initially 0)
        f"LOAD B, [{_addr(11)}]",  # B = V11
        "SUB A, B",  # A = V13 - V11
        f"STOR A, [{_addr(13)}]",  # V13 = A0
    ]


def _op7(lines: list[str]) -> None:
    """Op 7: V10 = V3 - V1  (n - 1, inner loop counter)."""
    lines += [
        f"LOAD A, [{_addr(3)}]",  # A = V3 = n
        f"LOAD B, [{_addr(1)}]",  # B = V1 = 1
        "SUB A, B",  # A = n - 1
        f"STOR A, [{_addr(10)}]",  # V10 = n-1
    ]


def _ops_8_12(lines: list[str]) -> None:
    """Ops 8-12: B1*A1 term (run when n >= 2)."""
    # Op 8: V7 = V2 + V7  (2 + 0 = 2 for first call; 2+... otherwise)
    lines += [
        f"LOAD A, [{_addr(2)}]",  # A = V2 = 2
        f"LOAD B, [{_addr(7)}]",  # B = V7
        "ADD A, B",  # A = 2 + V7
        f"STOR A, [{_addr(7)}]",  # V7 = 2
    ]
    # Op 9: V11 = V6 / V7
    lines += [
        f"LOAD A, [{_addr(6)}]",  # A = V6 = 2n
        f"LOAD B, [{_addr(7)}]",  # B = V7 = 2
        "DIV A, B",  # A = 2n / 2 = n = A1
        f"STOR A, [{_addr(11)}]",  # V11 = A1
    ]
    # Op 10: V12 = V21 * V11  (B1 * A1)
    lines += [
        f"LOAD A, [{_B_STORE[0]}]",  # A = V21 = B1
        f"LOAD B, [{_addr(11)}]",  # B = V11 = A1
        "MULT A, B",  # A = B1 * A1
        f"STOR A, [{_addr(12)}]",  # V12 = B1*A1
    ]
    # Op 11: V13 = V12 + V13
    lines += [
        f"LOAD A, [{_addr(12)}]",  # A = V12
        f"LOAD B, [{_addr(13)}]",  # B = V13
        "ADD A, B",  # A = V12 + V13
        f"STOR A, [{_addr(13)}]",  # V13 = A0 + B1*A1
    ]
    # Op 12: V10 = V10 - V1  (counter: n-2)
    lines += [
        f"LOAD A, [{_addr(10)}]",  # A = V10
        f"LOAD B, [{_addr(1)}]",  # B = V1 = 1
        "SUB A, B",  # A = V10 - 1
        f"STOR A, [{_addr(10)}]",  # V10 = n-2
    ]


def _ops_13_23(lines: list[str], b_slot_addr: int) -> None:
    """Ops 13-23: one pass of the inner loop body.

    Uses the B value at b_slot_addr for op 21. Caller must have already
    copied the correct B into V22 (mem[21]) if b_slot_addr != mem[21].

    WHY: The perforated cylinder would rotate to select the correct B
    value card for this pass; in the unrolled assembly we emit an explicit
    LOAD/STOR copy before each pass that needs a non-default B.
    """
    # Op 13: V6 = V6 - V1
    lines += [
        f"LOAD A, [{_addr(6)}]",
        f"LOAD B, [{_addr(1)}]",
        "SUB A, B",
        f"STOR A, [{_addr(6)}]",
    ]
    # Op 14: V7 = V1 + V7
    lines += [
        f"LOAD A, [{_addr(1)}]",
        f"LOAD B, [{_addr(7)}]",
        "ADD A, B",
        f"STOR A, [{_addr(7)}]",
    ]
    # Op 15: V8 = V6 / V7
    lines += [
        f"LOAD A, [{_addr(6)}]",
        f"LOAD B, [{_addr(7)}]",
        "DIV A, B",
        f"STOR A, [{_addr(8)}]",
    ]
    # Op 16: V11 = V8 * V11
    lines += [
        f"LOAD A, [{_addr(8)}]",
        f"LOAD B, [{_addr(11)}]",
        "MULT A, B",
        f"STOR A, [{_addr(11)}]",
    ]
    # Op 17: V6 = V6 - V1
    lines += [
        f"LOAD A, [{_addr(6)}]",
        f"LOAD B, [{_addr(1)}]",
        "SUB A, B",
        f"STOR A, [{_addr(6)}]",
    ]
    # Op 18: V7 = V1 + V7
    lines += [
        f"LOAD A, [{_addr(1)}]",
        f"LOAD B, [{_addr(7)}]",
        "ADD A, B",
        f"STOR A, [{_addr(7)}]",
    ]
    # Op 19: V9 = V6 / V7
    lines += [
        f"LOAD A, [{_addr(6)}]",
        f"LOAD B, [{_addr(7)}]",
        "DIV A, B",
        f"STOR A, [{_addr(9)}]",
    ]
    # Op 20: V11 = V9 * V11
    lines += [
        f"LOAD A, [{_addr(9)}]",
        f"LOAD B, [{_addr(11)}]",
        "MULT A, B",
        f"STOR A, [{_addr(11)}]",
    ]
    # Op 21: V12 = V22 * V11  (V22 must hold the correct B for this pass)
    lines += [
        f"LOAD A, [{_B_STORE[1]}]",  # A = V22 (current B for this pass)
        f"LOAD B, [{_addr(11)}]",
        "MULT A, B",
        f"STOR A, [{_addr(12)}]",
    ]
    # Op 22: V13 = V12 + V13
    lines += [
        f"LOAD A, [{_addr(12)}]",
        f"LOAD B, [{_addr(13)}]",
        "ADD A, B",
        f"STOR A, [{_addr(13)}]",
    ]
    # Op 23: V10 = V10 - V1
    lines += [
        f"LOAD A, [{_addr(10)}]",
        f"LOAD B, [{_addr(1)}]",
        "SUB A, B",
        f"STOR A, [{_addr(10)}]",
    ]


def _ops_24_25(lines: list[str]) -> None:
    """Ops 24-25: negate sum into V24, increment V3."""
    # Op 24: V24 = V24 - V13  (B_{2n-1} = -(A0 + B1*A1 + ...))
    lines += [
        f"LOAD A, [{_addr(24)}]",  # A = V24 (0 at start of this outer n)
        f"LOAD B, [{_addr(13)}]",  # B = V13
        "SUB A, B",  # A = 0 - V13 = -(sum)
        f"STOR A, [{_addr(24)}]",  # V24 = B_{2n-1}
    ]
    # Op 25: V3 = V1 + V3  (n -> n+1)
    lines += [
        f"LOAD A, [{_addr(1)}]",  # A = V1 = 1
        f"LOAD B, [{_addr(3)}]",  # B = V3 = n
        "ADD A, B",  # A = n + 1
        f"STOR A, [{_addr(3)}]",  # V3 = n+1
    ]


def generate_note_g_assembly_text(n_target: int) -> str:
    """Generate Babbage AE assembly for Note G, computing B_1..B_{2*n_target-1}.

    Args:
        n_target: Number of outer iterations (1 = B1 only; 3 = B1, B3, B5).

    Returns:
        Multi-line AE assembly string compatible with Engine.load_program_from_text().

    WHY: Linear (loop-unrolled) expansion matches Ada Lovelace's own Table A.2
    static instantiation for n=4. No runtime conditional branching is needed;
    each outer n block is emitted inline. This is historically authentic --
    Babbage's engine would advance to the correct card deck position mechanically.
    """
    if n_target < 1:
        raise ValueError("n_target must be >= 1")

    lines: list[str] = []

    # --- Global initialization ---
    # V1 = 1 (constant throughout)
    lines += ["LOAD A, 1", f"STOR A, [{_addr(1)}]"]
    # V2 = 2 (constant throughout)
    lines += ["LOAD A, 2", f"STOR A, [{_addr(2)}]"]

    for n in range(1, n_target + 1):
        lines.append(f"# --- Outer iteration n={n} ---")

        # Reset working variables (fresh init_state(n) equivalent)
        lines += _reset_working_vars()

        # Set V3 = n
        lines += [f"LOAD A, {n}", f"STOR A, [{_addr(3)}]"]

        # Ops 1-7
        _op1(lines)
        _op2(lines)
        _op3(lines)
        _op4(lines)
        _op5(lines)
        _op6(lines)
        _op7(lines)

        # Ops 8-12: only when n >= 2
        if n >= 2:
            _ops_8_12(lines)

        # Ops 13-23: inner loop, n-2 iterations
        loop_count = max(0, n - 2)
        for inner_i in range(loop_count):
            # Before each inner pass, load the correct B into V22 (mem[21]).
            # B for pass inner_i is B_store[inner_i + 1].
            b_slot_index = inner_i + 1
            b_src_addr = _B_STORE[b_slot_index]
            if b_src_addr != _B_STORE[1]:
                # Need to copy B from extended slot to V22
                lines += [
                    f"# Copy B (pass {inner_i}) from mem[{b_src_addr}] to V22",
                    f"LOAD A, [{b_src_addr}]",
                    f"STOR A, [{_B_STORE[1]}]",
                ]
            _ops_13_23(lines, b_src_addr)

        # Ops 24-25
        _ops_24_25(lines)

        # Output V24 = B_{2n-1}
        lines += [f"LOAD A, [{_addr(24)}]", "WRPRN A"]

        # Store B_{2n-1} for use in later inner loop passes
        b_dst_addr = _B_STORE[n - 1]
        lines += [
            f"# Store B result to B_store[{n - 1}] at mem[{b_dst_addr}]",
            f"STOR A, [{b_dst_addr}]",
        ]

    lines.append("HALT")
    return "\n".join(lines)
