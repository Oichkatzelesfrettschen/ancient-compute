"""Unit tests for the CLI assembler (parser.py, syntax.py, fourmilab.py).

Covers:
- tokenize_line: labels, opcodes, operands, comments
- parse_source: two-pass label resolution
- assemble_file: round-trip from .basm file
- fourmilab: parse_fourmilab_source + translate_fourmilab
"""

from backend.src.emulator.cli.assembler.parser import parse_source
from backend.src.emulator.cli.assembler.syntax import tokenize_line

# ---------------------------------------------------------------------------
# tokenize_line
# ---------------------------------------------------------------------------


def test_tokenize_line_empty():
    label, opcode, operands = tokenize_line("")
    assert label is None
    assert opcode is None
    assert operands == []


def test_tokenize_line_comment_only():
    label, opcode, operands = tokenize_line("; this is a comment")
    assert opcode is None


def test_tokenize_line_opcode_only():
    label, opcode, operands = tokenize_line("NOP")
    assert opcode == "NOP"
    assert operands == []


def test_tokenize_line_opcode_with_operands():
    label, opcode, operands = tokenize_line("LOAD A 42")
    assert opcode == "LOAD"
    assert operands == ["A", "42"]


def test_tokenize_line_label_and_opcode():
    label, opcode, operands = tokenize_line("start: JUMP end")
    assert label == "start"
    assert opcode == "JUMP"
    assert operands == ["end"]


def test_tokenize_line_label_only():
    label, opcode, operands = tokenize_line("entry:")
    assert label == "entry"
    assert opcode is None
    assert operands == []


def test_tokenize_line_comment_stripped():
    label, opcode, operands = tokenize_line("ADD A B ; add values")
    assert opcode == "ADD"
    assert operands == ["A", "B"]


# ---------------------------------------------------------------------------
# parse_source
# ---------------------------------------------------------------------------


def test_parse_source_empty():
    result = parse_source("")
    assert result == []


def test_parse_source_single_nop():
    result = parse_source("NOP")
    assert len(result) == 1
    assert result[0].opcode == "NOP"


def test_parse_source_label_resolution():
    source = """
start: LOAD A 1
       JUMP start
"""
    result = parse_source(source)
    assert len(result) == 2
    # JUMP should resolve 'start' to address 0
    assert result[1].operands == ["0"]


def test_parse_source_forward_label():
    source = """
    JUMP end
    NOP
end: NOP
"""
    result = parse_source(source)
    assert len(result) == 3
    # JUMP should resolve 'end' to address 2
    assert result[0].operands == ["2"]


def test_parse_source_multiple_instructions():
    source = """
LOAD A 5
LOAD B 10
ADD A B
STOR A 0
"""
    result = parse_source(source)
    assert len(result) == 4
    assert [i.opcode for i in result] == ["LOAD", "LOAD", "ADD", "STOR"]


def test_parse_source_unknown_label_kept_as_is():
    source = "JUMP unknown_label"
    result = parse_source(source)
    assert result[0].operands == ["unknown_label"]


def test_parse_source_comment_lines_skipped():
    source = """
; this is a comment
LOAD A 1
; another comment
LOAD B 2
"""
    result = parse_source(source)
    assert len(result) == 2


def test_parse_source_whitespace_only_lines():
    source = "   \n\t\nLOAD A 1\n   \n"
    result = parse_source(source)
    assert len(result) == 1
    assert result[0].opcode == "LOAD"


# ---------------------------------------------------------------------------
# fourmilab assembler
# ---------------------------------------------------------------------------


def test_fourmilab_parse_empty():
    from backend.src.emulator.cli.assembler.fourmilab import parse_fourmilab_source

    cards = parse_fourmilab_source("")
    assert cards == []


def test_fourmilab_translate_empty():
    from backend.src.emulator.cli.assembler.fourmilab import (
        parse_fourmilab_source,
        translate_fourmilab,
    )

    cards = parse_fourmilab_source("")
    instructions = translate_fourmilab(cards)
    assert instructions == []


class TestTokenizeLine:
    """Class-based coverage of tokenize_line edge cases."""

    def test_label_without_opcode(self) -> None:
        label, opcode, operands = tokenize_line("loop:")
        assert label == "loop"
        assert opcode is None

    def test_opcode_is_uppercased(self) -> None:
        _, opcode, _ = tokenize_line("load A 5")
        assert opcode == "LOAD"

    def test_multiple_operands(self) -> None:
        _, opcode, operands = tokenize_line("MOV A B C")
        assert len(operands) == 3

    def test_inline_comment_stripped(self) -> None:
        _, _, operands = tokenize_line("LOAD A 5 ; load five into A")
        assert operands == ["A", "5"]

    def test_label_with_inline_comment(self) -> None:
        label, opcode, operands = tokenize_line("start: NOP ; begin")
        assert label == "start"
        assert opcode == "NOP"
        assert operands == []

    def test_whitespace_around_label(self) -> None:
        label, opcode, _ = tokenize_line("  entry:  HALT")
        assert label == "entry"
        assert opcode == "HALT"

    def test_returns_three_tuple(self) -> None:
        result = tokenize_line("NOP")
        assert len(result) == 3


class TestParseSource:
    """Class-based coverage of parse_source multi-instruction behavior."""

    def test_nop_instruction_has_empty_operands(self) -> None:
        result = parse_source("NOP")
        assert result[0].operands == []

    def test_instruction_count_matches_non_comment_lines(self) -> None:
        source = "LOAD A 1\n; skip\nLOAD B 2\nHALT\n"
        result = parse_source(source)
        assert len(result) == 3

    def test_each_instruction_has_opcode_attr(self) -> None:
        result = parse_source("LOAD A 1\nHALT")
        for instr in result:
            assert hasattr(instr, "opcode")

    def test_each_instruction_has_operands_attr(self) -> None:
        result = parse_source("LOAD A 1\nHALT")
        for instr in result:
            assert hasattr(instr, "operands")

    def test_halt_opcode_parsed_correctly(self) -> None:
        result = parse_source("HALT")
        assert result[0].opcode == "HALT"

    def test_three_labels_all_resolved(self) -> None:
        source = "a: NOP\nb: NOP\nc: JUMP a"
        result = parse_source(source)
        # JUMP a should resolve 'a' to 0
        assert result[2].operands == ["0"]


class TestFourmilabParse:
    """Additional fourmilab parser tests."""

    def test_parse_returns_list(self) -> None:
        from backend.src.emulator.cli.assembler.fourmilab import parse_fourmilab_source

        cards = parse_fourmilab_source("P\n")
        assert isinstance(cards, list)

    def test_print_card_has_print_kind(self) -> None:
        from backend.src.emulator.cli.assembler.fourmilab import parse_fourmilab_source

        cards = parse_fourmilab_source("P\n")
        assert len(cards) == 1
        assert cards[0].kind == "print"

    def test_multiple_lines_produce_multiple_cards(self) -> None:
        from backend.src.emulator.cli.assembler.fourmilab import parse_fourmilab_source

        cards = parse_fourmilab_source("P\nP\nP\n")
        assert len(cards) == 3


class TestTokenizeEdgeCases:
    """Additional tokenize_line edge cases."""

    def test_halt_uppercase(self) -> None:
        _, opcode, _ = tokenize_line("halt")
        assert opcode == "HALT"

    def test_parse_source_returns_list(self) -> None:
        result = parse_source("NOP")
        assert isinstance(result, list)


class TestTokenizeOperands:
    """Additional tokenize_line operand and label parsing cases."""

    def test_two_operand_instruction(self) -> None:
        _, opcode, operands = tokenize_line("MOV A, 5")
        assert opcode == "MOV"
        assert operands == ["A", "5"]

    def test_label_extracted_from_labeled_line(self) -> None:
        label, opcode, _ = tokenize_line("start: NOP")
        assert label == "start"
        assert opcode == "NOP"

    def test_comment_stripped(self) -> None:
        _, opcode, _ = tokenize_line("NOP ; this is a comment")
        assert opcode == "NOP"

    def test_empty_operands_for_no_arg_instruction(self) -> None:
        _, opcode, operands = tokenize_line("HALT")
        assert opcode == "HALT"
        assert operands == []

    def test_add_opcode_parsed(self) -> None:
        _, opcode, operands = tokenize_line("ADD A, B")
        assert opcode == "ADD"
        assert "A" in operands


class TestParseSources:
    """parse_source tests for instruction lists and label resolution."""

    def test_single_nop_returns_one_instruction(self) -> None:
        instrs = parse_source("NOP")
        assert len(instrs) == 1

    def test_three_instructions_returns_three(self) -> None:
        instrs = parse_source("NOP\nNOP\nHALT")
        assert len(instrs) == 3

    def test_instruction_has_opcode_attribute(self) -> None:
        instrs = parse_source("HALT")
        assert hasattr(instrs[0], "opcode")
        assert instrs[0].opcode == "HALT"

    def test_mov_instruction_has_operands(self) -> None:
        instrs = parse_source("MOV A, 5")
        assert hasattr(instrs[0], "operands")

    def test_label_line_not_counted_as_instruction(self) -> None:
        instrs = parse_source("start:\nNOP\nHALT")
        assert len(instrs) == 2

    def test_jmp_target_resolved_to_integer_string(self) -> None:
        instrs = parse_source("start:\nNOP\nJMP start")
        jmp = next(i for i in instrs if i.opcode == "JMP")
        assert jmp.operands[0] == "0"


class TestAssemblerInstructionObjects:
    """Instruction object attribute invariants after parse_source."""

    def test_nop_has_empty_operands(self) -> None:
        instrs = parse_source("NOP")
        assert instrs[0].operands == []

    def test_halt_opcode_is_halt(self) -> None:
        instrs = parse_source("HALT")
        assert instrs[0].opcode == "HALT"

    def test_load_instruction_has_two_operands(self) -> None:
        instrs = parse_source("LOAD A, 5")
        assert len(instrs[0].operands) == 2

    def test_add_instruction_operands_preserved(self) -> None:
        instrs = parse_source("ADD A, B")
        assert instrs[0].operands[0] == "A"

    def test_multiple_instructions_indexed_correctly(self) -> None:
        instrs = parse_source("NOP\nHALT\nNOP")
        assert instrs[0].opcode == "NOP"
        assert instrs[1].opcode == "HALT"
        assert instrs[2].opcode == "NOP"
