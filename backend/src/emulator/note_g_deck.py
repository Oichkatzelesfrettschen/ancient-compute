"""Note G deck interpreter for the Analytical Engine (logic-level)."""

from __future__ import annotations

from pathlib import Path
from typing import Dict, List

import yaml

from .analytical_engine import BabbageNumber

_DECK_PATH = Path(__file__).resolve().parents[3] / "docs/simulation/NOTE_G_DECK.yaml"


def load_deck(path: Path | None = None) -> List[dict]:
    deck_path = path or _DECK_PATH
    with open(deck_path, "r", encoding="utf-8") as handle:
        data = yaml.safe_load(handle)
    return data["deck"]


def init_state(n: int) -> Dict[str, BabbageNumber]:
    state = {f"V{i}": BabbageNumber(0) for i in range(1, 25)}
    state["V1"] = BabbageNumber(1)
    state["V2"] = BabbageNumber(2)
    state["V3"] = BabbageNumber(n)
    return state

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

def run_once(n: int, deck_path: Path | None = None) -> Dict[str, BabbageNumber]:
    """Execute Note G operations 1..25 once (looping not modeled yet)."""
    deck = load_deck(deck_path)
    state = init_state(n)
    for step in deck:
        _apply_op(state, step)
    return state
