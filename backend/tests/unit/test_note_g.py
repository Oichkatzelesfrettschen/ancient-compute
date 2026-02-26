"""Tests for Note G deck runner -- Ada Lovelace's first computer program.

Validates that the corrected deck (with two known errata fixed) produces
correct Bernoulli numbers via pure card execution, matching the oracle
derived from the Akiyama-Tanigawa algorithm.

Errata in original 1843 publication:
1. Op 4: division operand order reversed (V5/V4 should be V4/V5)
2. Op 24: addition should be subtraction (negate accumulated sum)

References:
- Menabrea/Lovelace, "Sketch of the Analytical Engine" (1843), Note G
- Glaschick, "Ada Lovelace's Calculation of Bernoulli's Numbers" (2016)
"""

import pytest
from fractions import Fraction

from backend.src.emulator.note_g_deck import (
    run_note_g,
    run_note_g_exact,
    run_once,
    load_deck,
    init_state,
    _apply_op,
    _exec_opcode,
)
from backend.src.emulator.bernoulli import (
    ada_lovelace_bernoulli_series,
    bernoulli_numbers,
)
from backend.src.emulator.analytical_engine import BabbageNumber


# Ada's B_{2k-1} = modern B_{2k}
KNOWN_VALUES = {
    1: Fraction(1, 6),       # B1 = modern B_2
    3: Fraction(-1, 30),     # B3 = modern B_4
    5: Fraction(1, 42),      # B5 = modern B_6
    7: Fraction(-1, 30),     # B7 = modern B_8
    9: Fraction(5, 66),      # B9 = modern B_10
}


class TestOracleMapping:
    """Verify oracle produces correct Ada-convention Bernoulli numbers."""

    def test_oracle_b1(self):
        series = ada_lovelace_bernoulli_series(1)
        assert series[0] == Fraction(1, 6)

    def test_oracle_b3(self):
        series = ada_lovelace_bernoulli_series(2)
        assert series[1] == Fraction(-1, 30)

    def test_oracle_series_5(self):
        series = ada_lovelace_bernoulli_series(5)
        for i, expected in enumerate(KNOWN_VALUES.values()):
            assert series[i] == expected, f"Mismatch at index {i}"

    def test_oracle_maps_to_modern_even_bernoulli(self):
        """Ada's B_{2k-1} must equal modern B_{2k}."""
        modern = bernoulli_numbers(10)
        ada = ada_lovelace_bernoulli_series(5)
        for k in range(1, 6):
            assert ada[k - 1] == modern[2 * k], (
                f"Ada B_{2*k-1} != modern B_{2*k}"
            )


class TestDeckStructure:
    """Verify deck YAML is well-formed and contains corrected errata."""

    def test_deck_has_25_operations(self):
        deck = load_deck()
        assert len(deck) == 25

    def test_op4_corrected_division_order(self):
        """Op 4 must divide V4/V5 (not V5/V4 as in original)."""
        deck = load_deck()
        op4 = [s for s in deck if s["op"] == 4][0]
        assert op4["lhs"] == "V4", "Op 4 lhs should be V4 (2n-1)"
        assert op4["rhs"] == "V5", "Op 4 rhs should be V5 (2n+1)"
        assert op4["opcode"] == "div"

    def test_op24_corrected_negation(self):
        """Op 24 must subtract V13 from V24 (not add)."""
        deck = load_deck()
        op24 = [s for s in deck if s["op"] == 24][0]
        assert op24["opcode"] == "sub", "Op 24 must negate V13"
        assert op24["lhs"] == "V24"
        assert op24["rhs"] == "V13"

    def test_all_ops_have_required_fields(self):
        deck = load_deck()
        for step in deck:
            assert "op" in step
            assert "opcode" in step
            assert "lhs" in step
            assert "rhs" in step
            assert "out" in step


class TestRunNoteGSingle:
    """Test individual Bernoulli number computations."""

    def test_b1(self):
        """n=1: B1 = 1/6 from A0 term alone (no loop body)."""
        results = run_note_g_exact(1)
        assert results[0] == Fraction(1, 6)

    def test_b3(self):
        """n=2: B3 = -1/30 using B1*A1 term."""
        results = run_note_g_exact(2)
        assert results[1] == Fraction(-1, 30)

    def test_b5(self):
        """n=3: B5 = 1/42 using one loop iteration."""
        results = run_note_g_exact(3)
        assert results[2] == Fraction(1, 42)

    def test_b7(self):
        """n=4: B7 = -1/30 -- the example worked in Ada's diagram."""
        results = run_note_g_exact(4)
        assert results[3] == Fraction(-1, 30)

    def test_b9(self):
        """n=5: B9 = 5/66 using three loop iterations."""
        results = run_note_g_exact(5)
        assert results[4] == Fraction(5, 66)


class TestRunNoteGFull:
    """Test full series computation against oracle."""

    def test_series_matches_oracle_5(self):
        deck_results = run_note_g_exact(5)
        oracle = ada_lovelace_bernoulli_series(5)
        for i in range(5):
            assert deck_results[i] == oracle[i], (
                f"B_{2*i+1}: deck={deck_results[i]} != oracle={oracle[i]}"
            )

    def test_series_matches_oracle_7(self):
        """Extended test through B13."""
        deck_results = run_note_g_exact(7)
        oracle = ada_lovelace_bernoulli_series(7)
        for i in range(7):
            assert deck_results[i] == oracle[i], (
                f"B_{2*i+1}: deck={deck_results[i]} != oracle={oracle[i]}"
            )

    def test_invalid_n_target(self):
        with pytest.raises(ValueError):
            run_note_g(0)

    def test_returns_babbage_numbers(self):
        results = run_note_g(1)
        assert isinstance(results[0], BabbageNumber)


class TestLoopBackSemantics:
    """Verify Ada's cycle notation is implemented correctly."""

    def test_n1_no_loop_body(self):
        """n=1: ops (1..7),(24,25) only -- no loop at all."""
        deck = load_deck()
        ops = {step["op"]: step for step in deck}
        state = init_state(1)

        for op_num in range(1, 8):
            _apply_op(state, ops[op_num])

        # V10 = n-1 = 0, so loop should be skipped
        v10 = Fraction(state["V10"].value, 10**40)
        assert v10 == 0, "V10 should be 0 for n=1"

    def test_n2_no_inner_loop(self):
        """n=2: ops (1..7),(8..12),(24,25) -- no inner loop."""
        results = run_note_g_exact(2)
        assert len(results) == 2
        assert results[0] == Fraction(1, 6)
        assert results[1] == Fraction(-1, 30)

    def test_n4_two_loop_iterations(self):
        """n=4: ops (1..7),(8..12),2*(13..23),(24,25)."""
        results = run_note_g_exact(4)
        assert len(results) == 4
        assert results[3] == Fraction(-1, 30), "B7 should be -1/30"

    def test_accumulated_results_feed_forward(self):
        """Each B value feeds into subsequent computations."""
        results = run_note_g_exact(3)
        # B5 depends on B1 and B3 being correct
        assert results[0] == Fraction(1, 6), "B1 must be correct for B5"
        assert results[1] == Fraction(-1, 30), "B3 must be correct for B5"
        assert results[2] == Fraction(1, 42), "B5 depends on B1 and B3"


class TestRunOnce:
    """Test single-pass execution (no looping)."""

    def test_run_once_n4(self):
        """Single pass through all 25 ops for n=4."""
        state = run_once(4)
        # V24 should contain something (we don't check correctness since
        # run_once doesn't handle looping or B preloading)
        assert "V24" in state

    def test_run_once_sets_v3(self):
        """After op 25, V3 should be n+1."""
        state = run_once(4)
        v3 = Fraction(state["V3"].value, 10**40).limit_denominator(100)
        assert v3 == 5, "V3 should be n+1=5 after op 25"
