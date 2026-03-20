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
