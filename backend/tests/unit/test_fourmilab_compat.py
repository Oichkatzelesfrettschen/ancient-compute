"""Tests for the Fourmilab AE card deck compatibility layer."""

from backend.src.emulator.analytical_engine import Engine
from backend.src.emulator.fourmilab_compat import (
    fourmilab_to_instructions,
    parse_fourmilab_deck,
    run_fourmilab_deck,
)

# ---------------------------------------------------------------------------
# Parser tests
# ---------------------------------------------------------------------------


class TestParseFourmilabDeck:
    def test_number_card(self):
        cards = parse_fourmilab_deck("N000 +42")
        assert len(cards) == 1
        assert cards[0].kind == "number"
        assert cards[0].payload == (0, 42)

    def test_negative_number_card(self):
        cards = parse_fourmilab_deck("N000 -7")
        assert cards[0].kind == "number"
        assert cards[0].payload == (0, -7)

    def test_load_card(self):
        cards = parse_fourmilab_deck("L003")
        assert cards[0].kind == "load"
        col, destructive = cards[0].payload
        assert col == 3
        assert destructive is False

    def test_destructive_load(self):
        cards = parse_fourmilab_deck("L003'")
        col, destructive = cards[0].payload
        assert destructive is True

    def test_store_card(self):
        cards = parse_fourmilab_deck("S005")
        assert cards[0].kind == "stor"
        col, additive = cards[0].payload
        assert col == 5
        assert additive is False

    def test_additive_store(self):
        cards = parse_fourmilab_deck("S005'")
        col, additive = cards[0].payload
        assert additive is True

    def test_operation_cards(self):
        for op in ["+", "-", "*", "/"]:
            cards = parse_fourmilab_deck(op)
            assert cards[0].kind == "op"
            assert cards[0].payload == op

    def test_conditional_forward(self):
        cards = parse_fourmilab_deck("CF+005")
        assert cards[0].kind == "cf"
        assert cards[0].payload == 5

    def test_conditional_backward(self):
        cards = parse_fourmilab_deck("CB-003")
        assert cards[0].kind == "cb"
        assert cards[0].payload == -3

    def test_unconditional_jump(self):
        cards = parse_fourmilab_deck("B+010")
        assert cards[0].kind == "b"
        assert cards[0].payload == 10

    def test_print_card(self):
        cards = parse_fourmilab_deck("P")
        assert cards[0].kind == "print"

    def test_halt_card(self):
        cards = parse_fourmilab_deck("H")
        assert cards[0].kind == "halt"

    def test_comment_ignored(self):
        cards = parse_fourmilab_deck("# this is a comment\nN000 +1")
        assert len(cards) == 1
        assert cards[0].kind == "number"

    def test_blank_lines_ignored(self):
        src = "\n\n\nN000 +1\n\n"
        cards = parse_fourmilab_deck(src)
        assert len(cards) == 1

    def test_inline_comment_ignored(self):
        cards = parse_fourmilab_deck("N000 +1  # load 1")
        assert cards[0].payload == (0, 1)

    def test_multiple_cards(self):
        src = "N000 +3\nN001 +5\nL000\nL001\n+\nP\nH"
        cards = parse_fourmilab_deck(src)
        assert len(cards) == 7


# ---------------------------------------------------------------------------
# Translator tests
# ---------------------------------------------------------------------------


class TestFourmilabToInstructions:
    def test_number_card_produces_load_stor(self):
        cards = parse_fourmilab_deck("N000 +42")
        instrs = fourmilab_to_instructions(cards)
        # Should produce LOAD A 42, STOR A [0]
        assert any(i.opcode == "LOAD" for i in instrs)
        assert any(i.opcode == "STOR" for i in instrs)

    def test_load_card_produces_load(self):
        cards = parse_fourmilab_deck("L003")
        instrs = fourmilab_to_instructions(cards)
        assert any(i.opcode == "LOAD" for i in instrs)

    def test_stor_card_produces_stor(self):
        cards = parse_fourmilab_deck("S002")
        instrs = fourmilab_to_instructions(cards)
        assert any(i.opcode == "STOR" for i in instrs)

    def test_add_op(self):
        cards = parse_fourmilab_deck("+")
        instrs = fourmilab_to_instructions(cards)
        assert any(i.opcode == "ADD" for i in instrs)

    def test_sub_op(self):
        cards = parse_fourmilab_deck("-")
        instrs = fourmilab_to_instructions(cards)
        assert any(i.opcode == "SUB" for i in instrs)

    def test_mult_op(self):
        cards = parse_fourmilab_deck("*")
        instrs = fourmilab_to_instructions(cards)
        assert any(i.opcode == "MULT" for i in instrs)

    def test_div_op(self):
        cards = parse_fourmilab_deck("/")
        instrs = fourmilab_to_instructions(cards)
        assert any(i.opcode == "DIV" for i in instrs)

    def test_print_card_produces_wrprn(self):
        cards = parse_fourmilab_deck("P")
        instrs = fourmilab_to_instructions(cards)
        assert any(i.opcode == "WRPRN" for i in instrs)

    def test_halt_card_produces_halt(self):
        cards = parse_fourmilab_deck("H")
        instrs = fourmilab_to_instructions(cards)
        assert any(i.opcode == "HALT" for i in instrs)

    def test_branch_patches_target(self):
        # B+001 from card 0 should jump to card 1's PC
        src = "N000 +1\nB+001\nP\nH"
        cards = parse_fourmilab_deck(src)
        instrs = fourmilab_to_instructions(cards)
        jmp_instrs = [i for i in instrs if i.opcode == "JMP"]
        assert len(jmp_instrs) >= 1
        # Target should be a numeric string (resolved PC)
        target = jmp_instrs[0].operands[0]
        assert target.isdigit() or (target.startswith("-") and target[1:].isdigit())

    def test_empty_source(self):
        cards = parse_fourmilab_deck("")
        instrs = fourmilab_to_instructions(cards)
        assert instrs == []


# ---------------------------------------------------------------------------
# End-to-end run tests
# ---------------------------------------------------------------------------


class TestRunFourmilabDeck:
    def test_number_card_sets_memory(self):
        engine = run_fourmilab_deck("N000 +99\nH")
        # Memory address 0 should hold 99
        assert abs(engine.memory[0].to_decimal() - 99.0) < 1e-6

    def test_load_and_print(self):
        output = []
        run_fourmilab_deck("N000 +7\nL000\nP\nH", output_callback=output.append)
        assert any("7" in line for line in output)

    def test_returns_engine(self):
        engine = run_fourmilab_deck("H")
        assert isinstance(engine, Engine)

    def test_halt_stops_engine(self):
        engine = run_fourmilab_deck("H")
        assert not engine.running

    def test_add_two_numbers(self):
        src = "N000 +3\nN001 +5\nL000\nL001\n+\nP\nH"
        output = []
        run_fourmilab_deck(src, output_callback=output.append)
        # Result 3+5=8 should appear in output
        assert any("8" in line for line in output)

    def test_example_add_file(self):
        from pathlib import Path

        path = Path(__file__).parents[3] / "docs/simulation/fourmilab_examples/add_two_numbers.ae"
        with open(path, encoding="utf-8") as fh:
            source = fh.read()
        output = []
        engine = run_fourmilab_deck(source, output_callback=output.append)
        assert isinstance(engine, Engine)


class TestParseFourmilabEdgeCases:
    """Additional parse edge cases."""

    def test_load_col_ten(self) -> None:
        cards = parse_fourmilab_deck("L010")
        col, _ = cards[0].payload
        assert col == 10

    def test_number_card_large_value(self) -> None:
        cards = parse_fourmilab_deck("N005 +9999")
        assert cards[0].payload == (5, 9999)


class TestParseFourmilabCardTypes:
    """All card kinds parsed correctly."""

    def test_store_card_kind(self) -> None:
        cards = parse_fourmilab_deck("S003")
        assert cards[0].kind == "stor"

    def test_store_card_column(self) -> None:
        cards = parse_fourmilab_deck("S007")
        col, _ = cards[0].payload
        assert col == 7

    def test_print_card_no_payload(self) -> None:
        cards = parse_fourmilab_deck("P")
        assert cards[0].kind == "print"
        assert cards[0].payload is None

    def test_number_card_negative_value(self) -> None:
        cards = parse_fourmilab_deck("N002 -99")
        assert cards[0].payload == (2, -99)

    def test_number_card_zero(self) -> None:
        cards = parse_fourmilab_deck("N000 +0")
        assert cards[0].payload == (0, 0)

    def test_multiple_cards_all_parsed(self) -> None:
        cards = parse_fourmilab_deck("N000 +5\nL000\nP")
        assert len(cards) == 3

    def test_load_card_non_destructive_default(self) -> None:
        cards = parse_fourmilab_deck("L000")
        _, destructive = cards[0].payload
        assert destructive is False

    def test_load_card_column_zero(self) -> None:
        cards = parse_fourmilab_deck("L000")
        col, _ = cards[0].payload
        assert col == 0


class TestFourmilabToInstructionsExtended:
    """fourmilab_to_instructions translation tests."""

    def test_number_card_gives_instructions(self) -> None:
        cards = parse_fourmilab_deck("N000 +42")
        instrs = fourmilab_to_instructions(cards)
        assert len(instrs) >= 1

    def test_load_card_gives_instructions(self) -> None:
        cards = parse_fourmilab_deck("L000")
        instrs = fourmilab_to_instructions(cards)
        assert len(instrs) >= 1

    def test_print_card_gives_wrprn_instruction(self) -> None:
        cards = parse_fourmilab_deck("P")
        instrs = fourmilab_to_instructions(cards)
        assert any(i.opcode in ("WRPRN", "PRINT") for i in instrs)

    def test_deck_with_number_gives_instructions(self) -> None:
        cards = parse_fourmilab_deck("N000 +5\nP")
        instrs = fourmilab_to_instructions(cards)
        # At least the number load and print instructions
        assert len(instrs) >= 1

    def test_instructions_are_list(self) -> None:
        cards = parse_fourmilab_deck("N000 +1\nP")
        instrs = fourmilab_to_instructions(cards)
        assert isinstance(instrs, list)
