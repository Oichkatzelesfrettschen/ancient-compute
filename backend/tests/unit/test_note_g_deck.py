"""Tests for Note G outputs and Table A.2 runner."""

from backend.src.emulator.analytical_engine import BabbageNumber
from backend.src.emulator.note_g_deck import load_deck, run_series, run_table_a2_once


def test_note_g_deck_loads():
    deck = load_deck()
    assert len(deck) == 25
    assert deck[0]["op"] == 1


def test_note_g_bernoulli_odd_series_matches_modern_convention():
    results = run_series(4)
    assert [r.to_decimal() for r in results] == [
        -0.5,  # B1
        1 / 6,  # B3
        -1 / 30,  # B5
        1 / 42,  # B7
    ]


def test_note_g_table_a2_runs_for_n4():
    state = run_table_a2_once(n=4, b1=BabbageNumber(-0.5), b3=BabbageNumber(1 / 6))
    # The full repetition/cycle semantics (13..23 repeated) are not yet modeled.
    # This test ensures the transcribed Table A.2 operations execute without error
    # and populate the expected versioned variables for the first pass.
    assert "V13" in state
    assert 1 in state["V13"]
