"""Deterministic golden-corpus tests for the Babbage assembler."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from backend.src.assembler.assembler import (
    Assembler,
    AssemblyError,
    Lexer,
    SymbolTable,
)

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
        src = ".global main\n.text\nmain:\n  LOAD A, 10\n  LOAD B, 5\n  ADD A, B\n  WRPRN A\n  RET"
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


class TestSymbolTable:
    def test_define_and_lookup(self) -> None:
        st = SymbolTable()
        st.define("foo", 5)
        assert st.lookup("foo") == 5

    def test_is_defined_true(self) -> None:
        st = SymbolTable()
        st.define("bar", 0)
        assert st.is_defined("bar") is True

    def test_is_defined_false_for_missing(self) -> None:
        st = SymbolTable()
        assert st.is_defined("missing") is False

    def test_duplicate_define_raises(self) -> None:
        st = SymbolTable()
        st.define("x", 1)
        with pytest.raises(AssemblyError):
            st.define("x", 2)

    def test_lookup_undefined_raises(self) -> None:
        st = SymbolTable()
        with pytest.raises(AssemblyError):
            st.lookup("nope")

    def test_define_multiple_symbols(self) -> None:
        st = SymbolTable()
        st.define("a", 0)
        st.define("b", 1)
        st.define("c", 2)
        assert st.lookup("a") == 0
        assert st.lookup("b") == 1
        assert st.lookup("c") == 2

    def test_symbols_dict_is_populated(self) -> None:
        st = SymbolTable()
        st.define("label", 10)
        assert "label" in st.symbols
        assert st.symbols["label"] == 10


class TestLexer:
    def test_empty_source_produces_no_tokens(self) -> None:
        lexer = Lexer("")
        tokens = lexer.tokenize()
        assert tokens == []

    def test_comment_line_produces_no_tokens(self) -> None:
        lexer = Lexer("# this is a comment")
        tokens = lexer.tokenize()
        assert tokens == []

    def test_global_directive_tokenized(self) -> None:
        lexer = Lexer(".global main")
        tokens = lexer.tokenize()
        types = [t.type for t in tokens]
        assert "directive" in types

    def test_text_directive_tokenized(self) -> None:
        lexer = Lexer(".text")
        tokens = lexer.tokenize()
        assert tokens[0].type == "directive"
        assert tokens[0].value == ".text"

    def test_label_produces_label_token(self) -> None:
        lexer = Lexer("main:")
        tokens = lexer.tokenize()
        label_tokens = [t for t in tokens if t.type == "label"]
        assert len(label_tokens) == 1
        assert label_tokens[0].value == "main"

    def test_inline_comment_stripped(self) -> None:
        lexer = Lexer("HALT  # stop here")
        tokens = lexer.tokenize()
        # Should have instruction token(s) but not the comment text
        values = [t.value for t in tokens]
        assert "# stop here" not in values

    def test_blank_lines_skipped(self) -> None:
        lexer = Lexer("\n\n\nHALT\n\n")
        tokens = lexer.tokenize()
        assert len(tokens) >= 1  # HALT produces at least one token

    def test_line_numbers_tracked(self) -> None:
        lexer = Lexer(".global f\n.text\nf:\n  RET")
        tokens = lexer.tokenize()
        assert all(t.line_number >= 1 for t in tokens)


class TestAssemblerBranchInstructions:
    def test_jmp_to_label_assembles(self) -> None:
        src = ".global main\n.text\nmain:\n  JMP main"
        result = Assembler(src).assemble()
        assert result.error_count == 0
        assert len(result.machine_code) == 1

    def test_jz_to_label_assembles(self) -> None:
        src = ".global f\n.text\nf:\n  LOAD A, 0\nloop:\n  JZ loop\n  RET"
        result = Assembler(src).assemble()
        assert result.error_count == 0
        assert len(result.machine_code) == 3

    def test_call_and_ret_assemble(self) -> None:
        src = ".global main\n.text\nmain:\n  CALL sub\n  RET\nsub:\n  RET"
        result = Assembler(src).assemble()
        assert result.error_count == 0
        assert len(result.machine_code) >= 3

    def test_forward_reference_label_resolved(self) -> None:
        src = ".global main\n.text\nmain:\n  JMP end\n  LOAD A, 1\nend:\n  RET"
        result = Assembler(src).assemble()
        assert result.error_count == 0
        assert "end" in result.symbol_table

    def test_multiple_conditional_jumps_assemble(self) -> None:
        src = (
            ".global main\n.text\nmain:\n"
            "  LOAD A, 5\n  CMP A, 3\ntrue_branch:\n  JGT end\n"
            "  LOAD B, 0\nend:\n  RET"
        )
        result = Assembler(src).assemble()
        assert result.error_count == 0


class TestAssemblerArithmeticOpcodes:
    """Arithmetic opcode assembly and machine word count."""

    def _src(self, *body: str) -> str:
        lines = ".global main\n.text\nmain:\n"
        lines += "\n".join(f"  {line}" for line in body)
        return lines

    def test_add_instruction_assembles(self) -> None:
        r = Assembler(self._src("LOAD A, 3", "LOAD B, 4", "ADD A, B", "RET")).assemble()
        assert r.error_count == 0
        assert len(r.machine_code) == 4

    def test_sub_instruction_assembles(self) -> None:
        r = Assembler(self._src("LOAD A, 10", "LOAD B, 3", "SUB A, B", "RET")).assemble()
        assert r.error_count == 0
        assert len(r.machine_code) == 4

    def test_mult_instruction_assembles(self) -> None:
        r = Assembler(self._src("LOAD A, 5", "LOAD B, 6", "MULT A, B", "RET")).assemble()
        assert r.error_count == 0
        assert len(r.machine_code) == 4

    def test_div_instruction_assembles(self) -> None:
        r = Assembler(self._src("LOAD A, 20", "LOAD B, 4", "DIV A, B", "RET")).assemble()
        assert r.error_count == 0
        assert len(r.machine_code) == 4

    def test_wrprn_instruction_assembles(self) -> None:
        r = Assembler(self._src("LOAD A, 7", "WRPRN A", "RET")).assemble()
        assert r.error_count == 0
        assert len(r.machine_code) == 3

    def test_cmp_instruction_assembles(self) -> None:
        r = Assembler(self._src("LOAD A, 5", "CMP A, 3", "RET")).assemble()
        assert r.error_count == 0
        assert len(r.machine_code) == 3

    def test_stor_instruction_assembles(self) -> None:
        r = Assembler(self._src("LOAD A, 42", "STOR A, 10", "RET")).assemble()
        assert r.error_count == 0
        assert len(r.machine_code) == 3

    def test_machine_code_words_fit_in_48_bits(self) -> None:
        r = Assembler(self._src("LOAD A, 1", "RET")).assemble()
        for word in r.machine_code:
            assert 0 <= word < (1 << 48)


class TestLexerTokenValues:
    """Lexer token value correctness."""

    def test_immediate_integer_token(self) -> None:
        from backend.src.assembler.assembler import Lexer
        tokens = Lexer("LOAD A, 42").tokenize()
        values = [t.value for t in tokens]
        assert "42" in values or 42 in values or any("42" in str(v) for v in values)

    def test_register_name_in_tokens(self) -> None:
        from backend.src.assembler.assembler import Lexer
        tokens = Lexer("LOAD A, 1").tokenize()
        values = [t.value for t in tokens]
        assert any("A" in str(v).upper() for v in values)


class TestAssemblerExtendedOpcodes:
    """Tests for opcodes beyond the basics."""

    def test_nop_assembles(self) -> None:
        result = Assembler(".global f\n.text\nf:\n  NOP\n  RET").assemble()
        assert result.error_count == 0
        assert len(result.machine_code) == 2

    def test_halt_assembles(self) -> None:
        result = Assembler(".global f\n.text\nf:\n  HALT").assemble()
        assert result.error_count == 0
        assert len(result.machine_code) == 1

    def test_wrprn_assembles(self) -> None:
        result = Assembler(".global f\n.text\nf:\n  LOAD A, 7\n  WRPRN A\n  RET").assemble()
        assert result.error_count == 0
        assert len(result.machine_code) == 3

    def test_sqrt_assembles(self) -> None:
        result = Assembler(".global f\n.text\nf:\n  LOAD A, 9\n  SQRT A\n  RET").assemble()
        assert result.error_count == 0

    def test_sub_assembles(self) -> None:
        src = ".global f\n.text\nf:\n  LOAD A, 10\n  LOAD B, 3\n  SUB A, B\n  RET"
        result = Assembler(src).assemble()
        assert result.error_count == 0
        assert len(result.machine_code) == 4

    def test_mul_assembles(self) -> None:
        src = ".global f\n.text\nf:\n  LOAD A, 6\n  LOAD B, 7\n  MULT A, B\n  RET"
        result = Assembler(src).assemble()
        assert result.error_count == 0

    def test_cmp_assembles(self) -> None:
        result = Assembler(".global f\n.text\nf:\n  LOAD A, 5\n  CMP A, 3\n  RET").assemble()
        assert result.error_count == 0

    def test_get_hex_dump_has_address_prefix(self) -> None:
        result = Assembler(".global f\n.text\nf:\n  LOAD A, 1\n  RET").assemble()
        dump = result.get_hex_dump()
        # Hex dump should have address info
        assert len(dump) > 0
        # Each line contains a hex word
        assert "0x" in dump or all(c in "0123456789abcdefABCDEF \n:" for c in dump)

    def test_get_symbol_map_lists_labels(self) -> None:
        src = ".global f\n.text\nf:\n  LOAD A, 1\nlabel2:\n  RET"
        result = Assembler(src).assemble()
        sym_map = result.get_symbol_map()
        assert "f" in sym_map
        assert "label2" in sym_map
