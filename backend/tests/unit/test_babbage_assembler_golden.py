"""Deterministic golden-corpus tests for the Babbage assembler."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from backend.src.assembler.assembler import Assembler

FIXTURE_PATH = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "babbage_assembler_golden_corpus.yaml"
)


def _load_cases() -> list[dict]:
    payload = yaml.safe_load(FIXTURE_PATH.read_text(encoding="utf-8"))
    assert isinstance(payload, dict)
    cases = payload.get("cases", [])
    assert isinstance(cases, list)
    return cases


@pytest.mark.parametrize("case", _load_cases(), ids=lambda item: str(item.get("name", "case")))
def test_assembler_golden_corpus(case: dict) -> None:
    source = case["source"]
    expected_hex = case["expected_hex"]
    expected_symbols = case["expected_symbols"]

    first = Assembler(source).assemble()
    second = Assembler(source).assemble()

    assert first.error_count == 0, "\n".join(first.errors)
    assert second.error_count == 0, "\n".join(second.errors)

    first_hex = [f"0x{word:012x}" for word in first.machine_code]
    second_hex = [f"0x{word:012x}" for word in second.machine_code]

    assert first_hex == expected_hex
    assert second_hex == expected_hex
    assert first.symbol_table == expected_symbols
    assert second.symbol_table == expected_symbols
