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
