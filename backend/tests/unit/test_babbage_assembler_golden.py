"""Deterministic golden-corpus tests for the Babbage assembler."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from backend.src.assembler.assembler import Assembler

FIXTURE_PATH = (
    Path(__file__).resolve().parents[1] / "fixtures" / "babbage_assembler_golden_corpus.yaml"
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


class TestAssemblerBasics:
    def test_empty_source_produces_no_machine_code(self) -> None:
        result = Assembler("").assemble()
        assert result.machine_code == []
        assert result.error_count == 0

    def test_single_ret_assembles(self) -> None:
        result = Assembler(".global main\n.text\nmain:\n  RET").assemble()
        assert result.error_count == 0
        assert len(result.machine_code) == 1

    def test_machine_code_words_are_ints(self) -> None:
        result = Assembler(".global main\n.text\nmain:\n  LOAD A, 5\n  RET").assemble()
        assert all(isinstance(w, int) for w in result.machine_code)

    def test_hex_format_is_12_char_per_word(self) -> None:
        result = Assembler(".global main\n.text\nmain:\n  LOAD A, 1\n  RET").assemble()
        for word in result.machine_code:
            hex_str = f"0x{word:012x}"
            # "0x" + 12 hex digits = 14 chars
            assert len(hex_str) == 14
            assert hex_str.startswith("0x")

    def test_get_hex_dump_returns_string(self) -> None:
        result = Assembler(".global main\n.text\nmain:\n  LOAD A, 7\n  RET").assemble()
        dump = result.get_hex_dump()
        assert isinstance(dump, str)
        assert len(dump) > 0

    def test_get_symbol_map_returns_string(self) -> None:
        result = Assembler(".global main\n.text\nmain:\n  RET").assemble()
        sym_map = result.get_symbol_map()
        assert isinstance(sym_map, str)

    def test_idempotent_assembly(self) -> None:
        src = ".global f\n.text\nf:\n  LOAD A, 42\n  WRPRN A\n  RET"
        r1 = Assembler(src).assemble()
        r2 = Assembler(src).assemble()
        assert r1.machine_code == r2.machine_code
        assert r1.symbol_table == r2.symbol_table

    def test_multiple_labels_in_symbol_table(self) -> None:
        src = ".global f\n.text\nf:\n  LOAD A, 1\nlabel2:\n  LOAD B, 2\n  RET"
        result = Assembler(src).assemble()
        assert result.error_count == 0
        assert "f" in result.symbol_table
        assert "label2" in result.symbol_table
        assert result.symbol_table["f"] == 0
        assert result.symbol_table["label2"] == 1

    def test_unknown_opcode_produces_errors(self) -> None:
        result = Assembler("LOAD A, 10\nUNKNOWN_OP X\nRET").assemble()
        assert result.error_count > 0
        assert len(result.errors) > 0

    def test_error_list_contains_strings(self) -> None:
        result = Assembler("BAD_OP A\nRET").assemble()
        assert all(isinstance(e, str) for e in result.errors)

    def test_arithmetic_program_instruction_count(self) -> None:
        # LOAD A + LOAD B + ADD + WRPRN + RET = 5 instructions
        src = (
            ".global main\n.text\nmain:\n" "  LOAD A, 10\n  LOAD B, 5\n  ADD A, B\n  WRPRN A\n  RET"
        )
        result = Assembler(src).assemble()
        assert result.error_count == 0
        assert len(result.machine_code) == 5

    def test_fixture_file_exists_and_has_cases(self) -> None:
        cases = _load_cases()
        assert len(cases) >= 1
        for case in cases:
            assert "name" in case
            assert "source" in case
            assert "expected_hex" in case
            assert "expected_symbols" in case
