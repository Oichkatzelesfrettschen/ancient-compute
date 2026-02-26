"""Round-trip tests for the card compiler (tools/card_compiler.py).

Verifies: source -> encode -> decode -> source for all card classes
and opcodes defined in CARD_STANDARD.md v0.1.
"""

import sys
from pathlib import Path

import pytest

# Add tools/ to path so we can import card_compiler
sys.path.insert(0, str(Path(__file__).resolve().parents[3] / "tools"))

from card_compiler import (
    CardClass,
    CardData,
    Opcode,
    card_to_source,
    compile_deck,
    decompile_deck,
    decode_card,
    encode_card,
    parse_line,
    round_trip,
)


# --- Operation card round-trips ---

class TestOperationCards:
    """Operation cards: encode -> decode preserves opcode and modifier."""

    @pytest.mark.parametrize("opcode", list(Opcode))
    def test_all_opcodes_round_trip(self, opcode):
        card = CardData(card_class=CardClass.OPERATION, opcode=opcode, modifier=0)
        enc = encode_card(card)
        dec = decode_card(enc)
        assert dec.card_class == CardClass.OPERATION
        assert dec.opcode == opcode
        assert dec.modifier == 0

    @pytest.mark.parametrize("modifier", [0, 1, 5, 15])
    def test_modifier_preserved(self, modifier):
        card = CardData(card_class=CardClass.OPERATION, opcode=Opcode.ADD, modifier=modifier)
        enc = encode_card(card)
        dec = decode_card(enc)
        assert dec.modifier == modifier

    def test_parse_halt(self):
        card = parse_line("HALT")
        assert card.card_class == CardClass.OPERATION
        assert card.opcode == Opcode.HALT

    def test_parse_add_with_operand(self):
        card = parse_line("ADD V3")
        assert card.opcode == Opcode.ADD
        assert card.modifier == 3


# --- Variable card round-trips ---

class TestVariableCards:
    """Variable cards: encode -> decode preserves direction and column."""

    @pytest.mark.parametrize("direction", ["read", "write"])
    @pytest.mark.parametrize("column", [0, 1, 24, 127, 512, 1023])
    def test_variable_round_trip(self, direction, column):
        card = CardData(card_class=CardClass.VARIABLE, direction=direction, column=column)
        enc = encode_card(card)
        dec = decode_card(enc)
        assert dec.card_class == CardClass.VARIABLE
        assert dec.direction == direction
        assert dec.column == column

    def test_parse_load(self):
        card = parse_line("LOAD V5")
        assert card.card_class == CardClass.VARIABLE
        assert card.direction == "read"
        assert card.column == 5

    def test_parse_store(self):
        card = parse_line("STORE V10")
        assert card.card_class == CardClass.VARIABLE
        assert card.direction == "write"
        assert card.column == 10


# --- Number card round-trips ---

class TestNumberCards:
    """Number cards: encode -> decode preserves sign and magnitude."""

    @pytest.mark.parametrize("sign,magnitude", [
        (0, "0"),
        (0, "1"),
        (0, "12345"),
        (1, "99999"),
        (0, "1234567890"),
        (0, "12345678901234567890123456789012345678901234567890"),  # 50 digits
    ])
    def test_number_round_trip(self, sign, magnitude):
        card = CardData(card_class=CardClass.NUMBER, sign=sign, magnitude=magnitude)
        enc = encode_card(card)
        dec = decode_card(enc)
        assert dec.card_class == CardClass.NUMBER
        assert dec.sign == sign
        assert dec.magnitude == magnitude

    def test_parse_positive_number(self):
        card = parse_line("NUM +42")
        assert card.card_class == CardClass.NUMBER
        assert card.sign == 0
        assert card.magnitude == "42"

    def test_parse_negative_number(self):
        card = parse_line("NUM -100")
        assert card.card_class == CardClass.NUMBER
        assert card.sign == 1
        assert card.magnitude == "100"


# --- Deck-level round-trips ---

class TestDeckRoundTrip:
    """Full deck: source -> compile -> decompile -> source."""

    def test_simple_deck(self):
        source = "ADD\nSUB\nHALT"
        assert round_trip(source)

    def test_mixed_deck(self):
        source = "LOAD V1\nADD\nSTORE V2\nNUM +42\nHALT"
        assert round_trip(source)

    def test_deck_with_comments(self):
        source = "ADD  # add values\nHALT  # stop"
        assert round_trip(source)

    def test_all_opcodes_deck(self):
        lines = [op.name for op in Opcode]
        source = "\n".join(lines)
        assert round_trip(source)

    def test_note_g_style_deck(self):
        """A deck resembling Ada's Note G operations."""
        source = "\n".join([
            "LOAD V1",
            "LOAD V2",
            "MUL",
            "STORE V4",
            "LOAD V3",
            "SUB",
            "STORE V5",
            "HALT",
        ])
        assert round_trip(source)

    def test_empty_lines_ignored(self):
        source = "\n\nADD\n\nHALT\n\n"
        encoded = compile_deck(source)
        assert len(encoded) == 2

    def test_compile_decompile_count(self):
        source = "ADD\nSUB\nMUL\nDIV\nHALT"
        encoded = compile_deck(source)
        assert len(encoded) == 5
        recovered = decompile_deck(encoded)
        assert len(recovered.splitlines()) == 5


# --- Error handling ---

class TestErrors:
    """Invalid inputs produce clear errors."""

    def test_unknown_mnemonic(self):
        with pytest.raises(ValueError, match="Unknown mnemonic"):
            parse_line("BOGUS")

    def test_invalid_variable(self):
        with pytest.raises(ValueError, match="Invalid variable"):
            parse_line("LOAD XYZ")

    def test_operation_card_requires_opcode(self):
        card = CardData(card_class=CardClass.OPERATION, opcode=None)
        with pytest.raises(ValueError, match="requires opcode"):
            encode_card(card)
