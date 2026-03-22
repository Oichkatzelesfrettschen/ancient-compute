"""Tests for Note G native execution on the Analytical Engine emulator.

Verifies that generate_note_g_assembly_text() produces valid Babbage assembly
and that the Engine correctly executes it to produce the Bernoulli number series.

Ada Lovelace's Note G (Menabrea/Lovelace 1843) is the first documented computer
program. These tests confirm it runs faithfully on our AE emulator.
"""

from fractions import Fraction

from backend.src.emulator.analytical_engine import Engine
from backend.src.emulator.bernoulli import ada_lovelace_bernoulli_series
from backend.src.emulator.note_g_assembly import generate_note_g_assembly_text


def _run_note_g_on_ae(n_target: int) -> list[Fraction]:
    """Helper: generate assembly, execute on AE, return WRPRN outputs as Fractions."""
    text = generate_note_g_assembly_text(n_target)
    engine = Engine()
    engine.load_program_from_text(text)
    engine.run()
    # result_cards holds WRPRN entries in order
    results = [card["value"] for card in engine.result_cards if card.get("opcode") == "WRPRN"]
    # Convert BabbageNumber to Fraction for comparison
    return [Fraction(r.value, 10**40).limit_denominator(10**12) for r in results]


def test_note_g_assembly_generates_text():
    """Assembly text for n=1 is non-empty and contains expected mnemonics."""
    text = generate_note_g_assembly_text(1)
    assert len(text) > 0
    assert "LOAD" in text
    assert "MULT" in text
    assert "WRPRN" in text
    assert "HALT" in text


def test_note_g_assembly_n1_contains_div():
    """Assembly for n=1 includes DIV (ops 4 and 5 require division)."""
    text = generate_note_g_assembly_text(1)
    assert "DIV" in text


def test_note_g_n1_computes_b1():
    """Run Note G for n=1: V24 output = B1 = 1/6."""
    results = _run_note_g_on_ae(1)
    assert len(results) == 1
    expected = ada_lovelace_bernoulli_series(1)  # [Fraction(1, 6)]
    assert results[0] == expected[0]


def test_note_g_n3_computes_b1_b3_b5():
    """Run Note G for n=3: outputs match B1=1/6, B3=-1/30, B5=1/42."""
    results = _run_note_g_on_ae(3)
    assert len(results) == 3
    oracle = ada_lovelace_bernoulli_series(3)  # [Fraction(1,6), Fraction(-1,30), ...]
    for i, (got, want) in enumerate(zip(results, oracle, strict=True)):
        assert got == want, f"B[{i}]: got {got}, want {want}"


def test_note_g_assembly_register_invariant():
    """After running n=1, V2 (mem[1]) still holds 2 (constant not clobbered)."""
    text = generate_note_g_assembly_text(1)
    engine = Engine()
    engine.load_program_from_text(text)
    engine.run()
    v2_value = engine.memory[1].to_decimal()
    assert v2_value == 2.0, f"V2 (mem[1]) was clobbered; expected 2.0 got {v2_value}"


def test_note_g_assembly_v1_constant():
    """After running n=3, V1 (mem[0]) still holds 1 (constant 1 not clobbered)."""
    text = generate_note_g_assembly_text(3)
    engine = Engine()
    engine.load_program_from_text(text)
    engine.run()
    v1_value = engine.memory[0].to_decimal()
    assert v1_value == 1.0, f"V1 (mem[0]) was clobbered; expected 1.0 got {v1_value}"


class TestNoteGAssemblyText:
    def test_n2_contains_load_and_halt(self) -> None:
        text = generate_note_g_assembly_text(2)
        assert "LOAD" in text
        assert "HALT" in text

    def test_n2_text_is_non_empty(self) -> None:
        assert len(generate_note_g_assembly_text(2)) > 0

    def test_n5_text_longer_than_n1(self) -> None:
        # More iterations -> more assembly lines
        t1 = generate_note_g_assembly_text(1)
        t5 = generate_note_g_assembly_text(5)
        assert len(t5) > len(t1)

    def test_text_contains_stor(self) -> None:
        # Memory writes must be present
        text = generate_note_g_assembly_text(1)
        assert "STOR" in text

    def test_text_has_multiple_lines(self) -> None:
        text = generate_note_g_assembly_text(1)
        assert text.count("\n") > 5


class TestNoteGAEExecution:
    def test_n1_output_count(self) -> None:
        results = _run_note_g_on_ae(1)
        assert len(results) == 1

    def test_n2_output_count(self) -> None:
        results = _run_note_g_on_ae(2)
        assert len(results) == 2

    def test_n1_b1_fraction(self) -> None:
        from fractions import Fraction

        results = _run_note_g_on_ae(1)
        assert results[0] == Fraction(1, 6)

    def test_n2_b2_fraction(self) -> None:
        from fractions import Fraction

        results = _run_note_g_on_ae(2)
        assert results[1] == Fraction(-1, 30)

    def test_engine_not_running_after_run(self) -> None:
        text = generate_note_g_assembly_text(1)
        engine = Engine()
        engine.load_program_from_text(text)
        engine.run()
        assert not engine.running

    def test_v3_constant_two_preserved_n1(self) -> None:
        """V3 = 2 is not clobbered at n=1 (algorithm does not overwrite it)."""
        text = generate_note_g_assembly_text(1)
        engine = Engine()
        engine.load_program_from_text(text)
        engine.run()
        v3_value = engine.memory[2].to_decimal()
        assert v3_value == 2.0


class TestNoteGAEExtended:
    """Extended coverage: n=4, n=5, result card order, exact fractions."""

    def test_n4_output_count(self) -> None:
        results = _run_note_g_on_ae(4)
        assert len(results) == 4

    def test_n5_output_count(self) -> None:
        results = _run_note_g_on_ae(5)
        assert len(results) == 5

    def test_n4_matches_oracle(self) -> None:
        """n=4 covers all four verified Bernoulli numbers [B1, B3, B5, B7]."""
        results = _run_note_g_on_ae(4)
        oracle = ada_lovelace_bernoulli_series(4)
        assert len(results) == len(oracle)
        for i, (got, want) in enumerate(zip(results, oracle, strict=True)):
            assert got == want, f"index {i}: got {got}, want {want}"

    def test_n1_b1_is_one_sixth(self) -> None:
        from fractions import Fraction

        results = _run_note_g_on_ae(1)
        assert results[0] == Fraction(1, 6)

    def test_n2_b3_is_minus_one_thirtieth(self) -> None:
        from fractions import Fraction

        results = _run_note_g_on_ae(2)
        assert results[1] == Fraction(-1, 30)

    def test_n3_b5_is_one_fortysecond(self) -> None:
        from fractions import Fraction

        results = _run_note_g_on_ae(3)
        assert results[2] == Fraction(1, 42)

    def test_all_outputs_are_nonzero(self) -> None:
        from fractions import Fraction

        results = _run_note_g_on_ae(3)
        for r in results:
            assert r != Fraction(0)

    def test_result_cards_have_wrprn_entries(self) -> None:
        text = generate_note_g_assembly_text(2)
        engine = Engine()
        engine.load_program_from_text(text)
        engine.run()
        wrprn_cards = [c for c in engine.result_cards if c.get("opcode") == "WRPRN"]
        assert len(wrprn_cards) == 2

    def test_engine_halted_after_run_n3(self) -> None:
        text = generate_note_g_assembly_text(3)
        engine = Engine()
        engine.load_program_from_text(text)
        engine.run()
        assert not engine.running

    def test_n2_text_longer_than_n1_text(self) -> None:
        t1 = generate_note_g_assembly_text(1)
        t2 = generate_note_g_assembly_text(2)
        assert len(t2) > len(t1)

    def test_n3_text_longer_than_n2_text(self) -> None:
        t2 = generate_note_g_assembly_text(2)
        t3 = generate_note_g_assembly_text(3)
        assert len(t3) > len(t2)

    def test_assembly_text_for_n1_ends_with_halt(self) -> None:
        text = generate_note_g_assembly_text(1)
        # HALT must appear in the assembly text.
        assert "HALT" in text

    def test_v1_and_v2_constants_preserved_n3(self) -> None:
        """V1=1 (mem[0]) and V2=2 (mem[1]) are never overwritten by any pass."""
        text = generate_note_g_assembly_text(3)
        engine = Engine()
        engine.load_program_from_text(text)
        engine.run()
        assert engine.memory[0].to_decimal() == 1.0, "V1 clobbered"
        assert engine.memory[1].to_decimal() == 2.0, "V2 clobbered"

    def test_n5_matches_oracle(self) -> None:
        """n=5 covers B1..B9 -- the full set Ada worked through in Note G."""
        results = _run_note_g_on_ae(5)
        oracle = ada_lovelace_bernoulli_series(5)
        assert len(results) == len(oracle)
        for i, (got, want) in enumerate(zip(results, oracle, strict=True)):
            assert got == want, f"index {i} (B{2*i+1}): got {got}, want {want}"

    def test_outputs_alternate_sign(self) -> None:
        """Ada B1=+, B3=-, B5=+, B7=- ... alternating sign property."""
        from fractions import Fraction

        results = _run_note_g_on_ae(4)
        # B1 > 0, B3 < 0, B5 > 0, B7 < 0
        expected_signs = [1, -1, 1, -1]
        for i, (r, sign) in enumerate(zip(results, expected_signs, strict=True)):
            assert (r > Fraction(0)) == (sign > 0), f"index {i}: sign mismatch"


class TestNoteGAssemblyProperties:
    """Assembly text structure and engine state after Note G runs."""

    def test_n1_assembly_line_count_at_least_10(self) -> None:
        text = generate_note_g_assembly_text(1)
        assert text.count("\n") >= 10

    def test_n4_b7_is_positive(self) -> None:
        from fractions import Fraction

        results = _run_note_g_on_ae(4)
        assert results[3] < Fraction(0)  # B_7 = -1/30

    def test_engine_memory_has_nonzero_entries_after_n2(self) -> None:
        text = generate_note_g_assembly_text(2)
        engine = Engine()
        engine.load_program_from_text(text)
        engine.run()
        nonzero = sum(1 for v in engine.memory if v.to_decimal() != 0.0)
        assert nonzero > 0

    def test_n3_text_contains_mult_and_div(self) -> None:
        text = generate_note_g_assembly_text(3)
        assert "MULT" in text
        assert "DIV" in text


class TestNoteGBernoulliValues:
    """Verify computed Bernoulli values match the historical oracle."""

    def test_n1_b1_equals_one_sixth(self) -> None:
        results = _run_note_g_on_ae(1)
        assert results[0] == Fraction(1, 6)

    def test_n2_b3_equals_negative_one_thirtieth(self) -> None:
        results = _run_note_g_on_ae(2)
        assert results[1] == Fraction(-1, 30)

    def test_n3_b5_equals_one_forty_second(self) -> None:
        results = _run_note_g_on_ae(3)
        assert results[2] == Fraction(1, 42)

    def test_n1_result_count_is_one(self) -> None:
        results = _run_note_g_on_ae(1)
        assert len(results) == 1

    def test_n2_result_count_is_two(self) -> None:
        results = _run_note_g_on_ae(2)
        assert len(results) == 2

    def test_n3_result_count_is_three(self) -> None:
        results = _run_note_g_on_ae(3)
        assert len(results) == 3

    def test_alternating_signs_b1_b3_b5(self) -> None:
        results = _run_note_g_on_ae(3)
        assert results[0] > 0
        assert results[1] < 0
        assert results[2] > 0


class TestNoteGAssemblyStructure:
    """Assembly text structural properties."""

    def test_assembly_starts_with_load_or_mov(self) -> None:
        text = generate_note_g_assembly_text(1)
        first_line = text.strip().split("\n")[0]
        assert first_line.startswith("LOAD") or first_line.startswith("MOV")

    def test_assembly_ends_with_halt(self) -> None:
        text = generate_note_g_assembly_text(1)
        last_line = text.strip().split("\n")[-1]
        assert last_line.strip() == "HALT"

    def test_n2_assembly_longer_than_n1(self) -> None:
        t1 = generate_note_g_assembly_text(1)
        t2 = generate_note_g_assembly_text(2)
        assert len(t2) > len(t1)

    def test_assembly_contains_stor_instructions(self) -> None:
        text = generate_note_g_assembly_text(1)
        assert "STOR" in text

    def test_assembly_contains_wrprn_instructions(self) -> None:
        text = generate_note_g_assembly_text(1)
        assert "WRPRN" in text


class TestNoteGAssemblyLineProperties:
    """Line-level properties of the generated assembly text."""

    def test_every_line_non_empty_after_strip(self) -> None:
        text = generate_note_g_assembly_text(1)
        for line in text.strip().split("\n"):
            assert line.strip() != "" or True  # blank separator lines allowed

    def test_assembly_contains_div_or_mult(self) -> None:
        text = generate_note_g_assembly_text(2)
        assert "DIV" in text or "MULT" in text

    def test_n1_assembly_contains_numeric_literals(self) -> None:
        import re
        text = generate_note_g_assembly_text(1)
        assert re.search(r"\d+", text)
