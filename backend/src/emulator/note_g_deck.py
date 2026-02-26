"""Note G deck interpreter for the Analytical Engine (logic-level).

This module provides three capabilities:

1. run_note_g(n_target): Full loop-back execution of Ada Lovelace's Note G
   program, producing Bernoulli numbers B_1 through B_{2*n_target-1} from
   deck execution alone (no oracle fallback).

2. run_once(n) / run_table_a2_once(n, b1, b3): Single-pass deck execution
   for testing individual operations.

3. run_series(n_max): Oracle-backed exact Bernoulli series for validation.

Loop-back semantics follow Ada's cycle notation:
  n=1: (1...7),(24,25)
  n=2: (1...7),(8...12),(24,25)
  n=3: (1...7),(8...12),(13...23),(24,25)
  n=4: (1...7),(8...12),2*(13...23),(24,25)
  General: (1...7),(8...12),(n-2)*(13...23),(24,25)

V10 serves as the iteration counter: initialized to n-1, decremented at ops
12 and 23. When V10 reaches 0, the loop body (13-23) is skipped.
"""

from __future__ import annotations

import re
from fractions import Fraction
from pathlib import Path

import yaml

from .analytical_engine import BabbageNumber
from .bernoulli import ada_lovelace_bernoulli_series

_DECK_PATH = Path(__file__).resolve().parents[3] / "docs/simulation/NOTE_G_DECK.yaml"
_TABLE_A2_PATH = Path(__file__).resolve().parents[3] / "docs/simulation/NOTE_G_TABLE_A2.yaml"


def load_deck(path: Path | None = None) -> list[dict]:
    deck_path = path or _DECK_PATH
    with open(deck_path, encoding="utf-8") as handle:
        data = yaml.safe_load(handle)
    return data["deck"]


def load_table_a2(path: Path | None = None) -> list[dict]:
    deck_path = path or _TABLE_A2_PATH
    with open(deck_path, encoding="utf-8") as handle:
        data = yaml.safe_load(handle)
    return data["deck"]


def init_state(n: int) -> dict[str, BabbageNumber]:
    state = {f"V{i}": BabbageNumber(0) for i in range(1, 25)}
    state["V1"] = BabbageNumber(1)
    state["V2"] = BabbageNumber(2)
    state["V3"] = BabbageNumber(n)
    return state


_TOKEN_RE = re.compile(r"^(?:(\d+))?(V\d+)$")


def _parse_token(token: str) -> tuple:
    """Parse a token like '3V11' or 'V11'.

    Returns (version, var) where version=None means 'latest'.
    """
    match = _TOKEN_RE.match(token)
    if not match:
        raise ValueError(f"Invalid token: {token}")
    version_raw, var = match.groups()
    return (int(version_raw) if version_raw is not None else None), var


def init_state_versioned(n: int) -> dict[str, dict[int, BabbageNumber]]:
    """Initialize versioned variables: 0V* = 0, 1V1=1, 1V2=2, 1V3=n."""
    state: dict[str, dict[int, BabbageNumber]] = {
        f"V{i}": {0: BabbageNumber(0)} for i in range(1, 25)
    }
    state["V1"][1] = BabbageNumber(1)
    state["V2"][1] = BabbageNumber(2)
    state["V3"][1] = BabbageNumber(n)
    return state


def _latest_version(state: dict[str, dict[int, BabbageNumber]], var: str) -> int:
    versions = state.get(var)
    if not versions:
        raise KeyError(var)
    return max(versions.keys())


def _get_value(state: dict[str, dict[int, BabbageNumber]], token: str) -> BabbageNumber:
    version, var = _parse_token(token)
    if var.startswith("V"):
        versions = state[var]
        if version is None:
            version = _latest_version(state, var)
        return versions.get(version, BabbageNumber(0))
    raise ValueError(f"Unexpected token: {token}")


def _set_value(
    state: dict[str, dict[int, BabbageNumber]], token: str, value: BabbageNumber
) -> None:
    version, var = _parse_token(token)
    if version is None:
        version = _latest_version(state, var) + 1
    state[var][version] = value


def _resolve_operand(state: dict[str, BabbageNumber], token: str) -> BabbageNumber:
    if token.startswith("V"):
        return state[token]
    return BabbageNumber(float(token))


def _apply_op(state: dict[str, BabbageNumber], step: dict) -> None:
    lhs = _resolve_operand(state, step["lhs"])
    rhs = _resolve_operand(state, step["rhs"])
    value = _exec_opcode(step["opcode"], lhs, rhs)
    for target in step["out"]:
        state[target] = value


def _apply_op_versioned(
    state: dict[str, dict[int, BabbageNumber]], step: dict
) -> None:
    lhs = _get_value(state, step["lhs"])
    rhs = _get_value(state, step["rhs"])
    value = _exec_opcode(step["opcode"], lhs, rhs)
    for target in step["out"]:
        _set_value(state, target, value)


def _exec_opcode(opcode: str, lhs: BabbageNumber, rhs: BabbageNumber) -> BabbageNumber:
    if opcode == "add":
        return lhs + rhs
    elif opcode == "sub":
        return lhs - rhs
    elif opcode == "mult":
        return lhs * rhs
    elif opcode == "div":
        return lhs / rhs
    raise ValueError(f"Unsupported opcode: {opcode}")


# ---------------------------------------------------------------------------
# Single-pass execution (legacy)
# ---------------------------------------------------------------------------

def run_once(n: int, deck_path: Path | None = None) -> dict[str, BabbageNumber]:
    """Execute Note G operations 1..25 once (no looping)."""
    deck = load_deck(deck_path)
    state = init_state(n)
    for step in deck:
        _apply_op(state, step)
    return state


def run_series(n_max: int, deck_path: Path | None = None) -> list[BabbageNumber]:
    """Return Bernoulli series from exact oracle (validation baseline)."""
    return [BabbageNumber(float(b)) for b in ada_lovelace_bernoulli_series(n_max)]


def run_table_a2_once(
    n: int, b1: BabbageNumber, b3: BabbageNumber
) -> dict[str, dict[int, BabbageNumber]]:
    """Execute Table A.2 deck once with preloaded B1, B3."""
    deck = load_table_a2()
    state = init_state_versioned(n)
    state["V21"][1] = b1
    state["V22"][1] = b3
    for step in deck:
        _apply_op_versioned(state, step)
    return state


# ---------------------------------------------------------------------------
# Full loop-back execution: run_note_g(n_target)
# ---------------------------------------------------------------------------

# Bernoulli coefficient slots in the variable store.
# V21 holds B1, V22 holds B3, V23 holds B5, etc.
_B_SLOTS = [21, 22, 23]  # Extendable if we add more preload columns


def run_note_g(n_target: int) -> list[BabbageNumber]:
    """Execute Note G with full loop-back, producing B_1..B_{2*n_target-1}.

    This is the first computer program, fully executed from the card deck
    without oracle fallback. Each Bernoulli number is computed by:

    1. Initializing V1=1, V2=2, V3=n, clearing working variables
    2. Running ops 1-7 (A0 term + counter setup)
    3. If n >= 2: running ops 8-12 (B1*A1 term)
    4. For each additional term (n-2 iterations): running ops 13-23
    5. Running ops 24-25 (negate sum into V24, increment n)

    Previously computed B values are preloaded into V21, V22, V23.

    The deck contains two known errata from the original 1843 publication:
    - Op 4: operand order corrected (V4/V5 not V5/V4)
    - Op 24: subtraction (V24 - V13) to negate the accumulated sum

    Returns:
        List of BabbageNumber: [B1, B3, B5, ...] up to n_target values,
        where Ada's B_{2k-1} = modern B_{2k}.
    """
    if n_target < 1:
        raise ValueError("n_target must be >= 1")

    deck = load_deck()
    ops = {step["op"]: step for step in deck}

    results: list[BabbageNumber] = []
    computed_b: list[BabbageNumber] = []

    for n in range(1, n_target + 1):
        state = init_state(n)

        # Preload previously computed B values into V21, V22, V23
        for i, b_val in enumerate(computed_b):
            if i < len(_B_SLOTS):
                state[f"V{_B_SLOTS[i]}"] = b_val

        # Phase 1: Ops 1-7 (A0 term + counter V10 = n-1)
        for op_num in range(1, 8):
            _apply_op(state, ops[op_num])

        # Phase 2: Ops 8-12 (B1*A1 term) -- only when n >= 2
        if n >= 2:
            for op_num in range(8, 13):
                _apply_op(state, ops[op_num])

        # Phase 3: Loop ops 13-23, repeating (n-2) times
        # Each iteration adds B_{2k+1} * A_{2k+1} to V13.
        # Op 21 reads from V22 in the deck, but the variable card advances
        # to the next B slot each iteration (Lovelace, lines 3966-3968).
        # We simulate this by writing the correct B into V22 before each pass.
        loop_count = max(0, n - 2)
        for iteration in range(loop_count):
            # B coefficient for this iteration: computed_b[iteration + 1]
            # iteration 0 -> B3 (computed_b[1]), already in V22 from preload
            # iteration 1 -> B5 (computed_b[2]), must be rotated into V22
            b_index = iteration + 1
            if b_index < len(computed_b):
                state["V22"] = computed_b[b_index]

            for op_num in range(13, 24):
                _apply_op(state, ops[op_num])

        # Phase 4: Ops 24-25
        # Op 24 negates V13: B_{2n-1} = -(A0 + A1*B1 + A3*B3 + ...)
        # Op 25 increments V3 for the next iteration
        _apply_op(state, ops[24])
        _apply_op(state, ops[25])

        result = state["V24"]
        results.append(result)
        computed_b.append(result)

    return results


def run_note_g_exact(n_target: int) -> list[Fraction]:
    """Run Note G and return results as exact Fractions for validation.

    Wraps run_note_g() and converts BabbageNumber results to Fraction.
    Uses limit_denominator to recover exact rationals from the 50-digit
    fixed-point BabbageNumber representation.
    """
    bn_results = run_note_g(n_target)
    return [
        Fraction(bn.value, 10**40).limit_denominator(10**10)
        for bn in bn_results
    ]
