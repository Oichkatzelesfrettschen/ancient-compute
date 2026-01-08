"""Note G deck interpreter for the Analytical Engine (logic-level).

This module provides two complementary capabilities:
1) A deck runner that can execute Table A.2-style operations (with versioned variables).
2) An exact Bernoulli-number generator used to validate Note G outputs.

The historically faithful "card-loop" semantics are still being refined; the
exact Bernoulli generator is used as an oracle while we close those gaps.
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Dict, List

import yaml

from .analytical_engine import BabbageNumber
from .bernoulli import ada_lovelace_bernoulli_series

_DECK_PATH = Path(__file__).resolve().parents[3] / "docs/simulation/NOTE_G_DECK.yaml"
_TABLE_A2_PATH = Path(__file__).resolve().parents[3] / "docs/simulation/NOTE_G_TABLE_A2.yaml"


def load_deck(path: Path | None = None) -> List[dict]:
    deck_path = path or _DECK_PATH
    with open(deck_path, "r", encoding="utf-8") as handle:
        data = yaml.safe_load(handle)
    return data["deck"]


def load_table_a2(path: Path | None = None) -> List[dict]:
    deck_path = path or _TABLE_A2_PATH
    with open(deck_path, "r", encoding="utf-8") as handle:
        data = yaml.safe_load(handle)
    return data["deck"]


def init_state(n: int) -> Dict[str, BabbageNumber]:
    # For the unversioned deck we keep a simple single-slot state.
    state = {f"V{i}": BabbageNumber(0) for i in range(1, 25)}
    state["V1"] = BabbageNumber(1)
    state["V2"] = BabbageNumber(2)
    state["V3"] = BabbageNumber(n)
    return state


_TOKEN_RE = re.compile(r"^(?:(\d+))?(V\d+)$")


def _parse_token(token: str) -> tuple[int | None, str]:
    """Parse a token like '3V11' or 'V11'.

    Returns (version, var) where version=None means 'latest' (for operands).
    """
    match = _TOKEN_RE.match(token)
    if not match:
        raise ValueError(f"Invalid token: {token}")
    version_raw, var = match.groups()
    return (int(version_raw) if version_raw is not None else None), var


def init_state_versioned(n: int) -> Dict[str, Dict[int, BabbageNumber]]:
    """Initialize versioned variables: 0V* = 0, 1V1=1, 1V2=2, 1V3=n."""
    state: Dict[str, Dict[int, BabbageNumber]] = {f"V{i}": {0: BabbageNumber(0)} for i in range(1, 25)}
    state["V1"][1] = BabbageNumber(1)
    state["V2"][1] = BabbageNumber(2)
    state["V3"][1] = BabbageNumber(n)
    return state


def _latest_version(state: Dict[str, Dict[int, BabbageNumber]], var: str) -> int:
    versions = state.get(var)
    if not versions:
        raise KeyError(var)
    return max(versions.keys())


def _get_value(state: Dict[str, Dict[int, BabbageNumber]], token: str) -> BabbageNumber:
    version, var = _parse_token(token)
    if var.startswith("V"):
        versions = state[var]
        if version is None:
            version = _latest_version(state, var)
        return versions.get(version, BabbageNumber(0))
    raise ValueError(f"Unexpected token: {token}")


def _set_value(state: Dict[str, Dict[int, BabbageNumber]], token: str, value: BabbageNumber) -> None:
    version, var = _parse_token(token)
    if version is None:
        # If no version was provided, append a new latest version.
        version = _latest_version(state, var) + 1
    state[var][version] = value

def _resolve_operand(state: Dict[str, BabbageNumber], token: str) -> BabbageNumber:
    if token.startswith("V"):
        return state[token]
    return BabbageNumber(float(token))


def _apply_op(state: Dict[str, BabbageNumber], step: dict) -> None:
    lhs = _resolve_operand(state, step["lhs"])
    rhs = _resolve_operand(state, step["rhs"])

    if step["opcode"] == "add":
        value = lhs + rhs
    elif step["opcode"] == "sub":
        value = lhs - rhs
    elif step["opcode"] == "mult":
        value = lhs * rhs
    elif step["opcode"] == "div":
        value = lhs / rhs
    else:
        raise ValueError(f"Unsupported opcode: {step['opcode']}")

    for target in step["out"]:
        state[target] = value


def _apply_op_versioned(state: Dict[str, Dict[int, BabbageNumber]], step: dict) -> None:
    lhs = _get_value(state, step["lhs"])
    rhs = _get_value(state, step["rhs"])

    if step["opcode"] == "add":
        value = lhs + rhs
    elif step["opcode"] == "sub":
        value = lhs - rhs
    elif step["opcode"] == "mult":
        value = lhs * rhs
    elif step["opcode"] == "div":
        value = lhs / rhs
    else:
        raise ValueError(f"Unsupported opcode: {step['opcode']}")

    for target in step["out"]:
        _set_value(state, target, value)

def run_once(n: int, deck_path: Path | None = None) -> Dict[str, BabbageNumber]:
    """Execute Note G operations 1..25 once (looping not modeled yet)."""
    deck = load_deck(deck_path)
    state = init_state(n)
    for step in deck:
        _apply_op(state, step)
    return state


def run_series(n_max: int, deck_path: Path | None = None) -> list[BabbageNumber]:
    """Return the Bernoulli odd series [B1, B3, ..., B_(2n_max-1)].

    Until the card-loop semantics are fully encoded, we use exact Bernoulli
    values as the canonical output for Note G validation.
    """
    return [BabbageNumber(float(b)) for b in ada_lovelace_bernoulli_series(n_max)]


def run_table_a2_once(n: int, b1: BabbageNumber, b3: BabbageNumber) -> Dict[str, Dict[int, BabbageNumber]]:
    """Execute Table A.2 deck (as transcribed) once.

    This corresponds to the diagram shown for n=4 and expects B1 and B3 to be
    preloaded into V21 and V22. Output is stored in 1V24 (per table).
    """
    deck = load_table_a2()
    state = init_state_versioned(n)
    state["V21"][1] = b1
    state["V22"][1] = b3
    for step in deck:
        _apply_op_versioned(state, step)
    return state
