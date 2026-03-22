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
    decode_card,
    decompile_deck,
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

    @pytest.mark.parametrize(
        "sign,magnitude",
        [
            (0, "0"),
            (0, "1"),
            (0, "12345"),
            (1, "99999"),
            (0, "1234567890"),
            (0, "12345678901234567890123456789012345678901234567890"),  # 50 digits
        ],
    )
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
        source = "\n".join(
            [
                "LOAD V1",
                "LOAD V2",
                "MUL",
                "STORE V4",
                "LOAD V3",
                "SUB",
                "STORE V5",
                "HALT",
            ]
        )
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


class TestCardToSource:
    """card_to_source() converts CardData back to canonical source text."""

    def test_halt_to_source(self) -> None:
        card = CardData(card_class=CardClass.OPERATION, opcode=Opcode.HALT)
        assert card_to_source(card) == "HALT"

    def test_add_to_source(self) -> None:
        card = CardData(card_class=CardClass.OPERATION, opcode=Opcode.ADD)
        assert card_to_source(card) == "ADD"

    def test_mul_to_source(self) -> None:
        card = CardData(card_class=CardClass.OPERATION, opcode=Opcode.MUL)
        assert card_to_source(card) == "MUL"

    def test_load_v5_to_source(self) -> None:
        card = CardData(card_class=CardClass.VARIABLE, direction="read", column=5)
        assert card_to_source(card) == "LOAD V5"

    def test_store_v10_to_source(self) -> None:
        card = CardData(card_class=CardClass.VARIABLE, direction="write", column=10)
        assert card_to_source(card) == "STORE V10"

    def test_num_positive_to_source(self) -> None:
        card = CardData(card_class=CardClass.NUMBER, sign=0, magnitude="42")
        assert card_to_source(card) == "NUM +42"

    def test_num_negative_to_source(self) -> None:
        card = CardData(card_class=CardClass.NUMBER, sign=1, magnitude="100")
        assert card_to_source(card) == "NUM -100"


class TestCardDataFields:
    """CardData dataclass field defaults and structure."""

    def test_blank_line_returns_nop(self) -> None:
        card = parse_line("")
        assert card.card_class == CardClass.OPERATION
        assert card.opcode == Opcode.NOP

    def test_decode_returns_card_data_instance(self) -> None:
        enc = encode_card(CardData(card_class=CardClass.OPERATION, opcode=Opcode.ADD))
        dec = decode_card(enc)
        assert isinstance(dec, CardData)

    def test_operation_card_class_value_is_o(self) -> None:
        assert CardClass.OPERATION.value == "O"

    def test_variable_card_class_value_is_v(self) -> None:
        assert CardClass.VARIABLE.value == "V"

    def test_number_card_class_value_is_n(self) -> None:
        assert CardClass.NUMBER.value == "N"

    def test_card_defaults_to_zero_modifier(self) -> None:
        card = CardData(card_class=CardClass.OPERATION, opcode=Opcode.ADD)
        assert card.modifier == 0

    def test_parse_nop_is_operation_card(self) -> None:
        card = parse_line("NOP")
        assert card.card_class == CardClass.OPERATION
        assert card.opcode == Opcode.NOP

    def test_card_to_source_round_trip_for_store_v3(self) -> None:
        original = "STORE V3"
        card = parse_line(original)
        assert card_to_source(card) == original


class TestCompileDecompileRoundTrip:
    """compile_deck + decompile_deck round-trip tests."""

    def test_halt_only_program_round_trips(self) -> None:
        src = "HALT"
        assert decompile_deck(compile_deck(src)) == src

    def test_load_halt_round_trips(self) -> None:
        src = "LOAD V1\nHALT"
        assert decompile_deck(compile_deck(src)) == src

    def test_store_halt_round_trips(self) -> None:
        src = "STORE V2\nHALT"
        assert decompile_deck(compile_deck(src)) == src

    def test_add_instruction_round_trips(self) -> None:
        src = "ADD\nHALT"
        assert decompile_deck(compile_deck(src)) == src

    def test_compile_multi_line_gives_multiple_cards(self) -> None:
        src = "LOAD V1\nLOAD V2\nADD\nSTORE V3\nHALT"
        cards = compile_deck(src)
        assert len(cards) == 5

    def test_decompile_preserves_line_count(self) -> None:
        src = "LOAD V1\nLOAD V2\nADD\nSTORE V3\nHALT"
        back = decompile_deck(compile_deck(src))
        assert back.count("\n") == 4

    def test_compile_produces_list_of_encoded_cards(self) -> None:
        cards = compile_deck("NOP\nHALT")
        assert isinstance(cards, list)
        assert len(cards) == 2


class TestCardEncodingProperties:
    """Property checks for the card encoding layer."""

    def test_operation_card_has_class_byte_zero(self) -> None:
        card = CardData(card_class=CardClass.OPERATION, opcode=Opcode.HALT)
        enc = encode_card(card)
        assert enc.class_byte == 0

    def test_variable_card_has_class_byte_one(self) -> None:
        card = CardData(card_class=CardClass.VARIABLE, direction="read", column=1)
        enc = encode_card(card)
        assert enc.class_byte == 1

    def test_number_card_has_class_byte_two(self) -> None:
        card = CardData(card_class=CardClass.NUMBER, sign=0, magnitude="1")
        enc = encode_card(card)
        assert enc.class_byte == 2

    def test_encoded_payload_is_bytes(self) -> None:
        enc = encode_card(CardData(card_class=CardClass.OPERATION, opcode=Opcode.ADD))
        assert isinstance(enc.payload, bytes)

    def test_all_operation_opcodes_encode_without_error(self) -> None:
        for op in Opcode:
            card = CardData(card_class=CardClass.OPERATION, opcode=op)
            enc = encode_card(card)
            assert enc is not None

    def test_decode_after_encode_preserves_opcode(self) -> None:
        for op in Opcode:
            card = CardData(card_class=CardClass.OPERATION, opcode=op)
            enc = encode_card(card)
            dec = decode_card(enc)
            assert dec.opcode == op
