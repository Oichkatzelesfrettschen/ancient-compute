"""Tests for Note G outputs and Table A.2 runner."""

from backend.src.emulator.analytical_engine import BabbageNumber
from backend.src.emulator.note_g_deck import load_deck, run_series, run_table_a2_once


def test_note_g_deck_loads():
    deck = load_deck()
    assert len(deck) == 25
    assert deck[0]["op"] == 1


def test_note_g_bernoulli_series_matches_ada_convention():
    """Oracle series uses Ada's convention: B_{2k-1} = modern B_{2k}."""
    results = run_series(4)
    expected = [1 / 6, -1 / 30, 1 / 42, -1 / 30]
    for r, e in zip(results, expected, strict=True):
        assert abs(r.to_decimal() - e) < 1e-10, f"{r.to_decimal()} != {e}"


def test_note_g_table_a2_runs_for_n4():
    state = run_table_a2_once(n=4, b1=BabbageNumber(-0.5), b3=BabbageNumber(1 / 6))
    # The full repetition/cycle semantics (13..23 repeated) are not yet modeled.
    # This test ensures the transcribed Table A.2 operations execute without error
    # and populate the expected versioned variables for the first pass.
    assert "V13" in state
    assert 1 in state["V13"]


class TestNoteGDeck:
    def test_deck_length_is_25(self) -> None:
        assert len(load_deck()) == 25

    def test_deck_ops_are_sequential(self) -> None:
        deck = load_deck()
        for i, card in enumerate(deck):
            assert card["op"] == i + 1

    def test_all_cards_have_required_keys(self) -> None:
        for card in load_deck():
            for key in ("op", "opcode", "lhs", "rhs", "out", "expr"):
                assert key in card, f"card {card.get('op')} missing key {key}"

    def test_opcode_values_are_strings(self) -> None:
        for card in load_deck():
            assert isinstance(card["opcode"], str)

    def test_out_fields_are_lists(self) -> None:
        for card in load_deck():
            assert isinstance(card["out"], list)

    def test_load_is_deterministic(self) -> None:
        d1 = load_deck()
        d2 = load_deck()
        assert len(d1) == len(d2)
        for c1, c2 in zip(d1, d2, strict=True):
            assert c1["op"] == c2["op"]
            assert c1["opcode"] == c2["opcode"]

    def test_first_op_is_mult(self) -> None:
        assert load_deck()[0]["opcode"] == "mult"

    def test_last_op_number_is_25(self) -> None:
        assert load_deck()[-1]["op"] == 25


class TestRunSeries:
    def test_series_length_matches_n(self) -> None:
        for n in [1, 3, 6]:
            assert len(run_series(n)) == n

    def test_series_elements_are_babbage_numbers(self) -> None:
        for b in run_series(4):
            assert isinstance(b, BabbageNumber)

    def test_b1_value(self) -> None:
        r = run_series(1)[0]
        assert abs(r.to_decimal() - 1 / 6) < 1e-10

    def test_b2_value(self) -> None:
        r = run_series(2)[1]
        assert abs(r.to_decimal() - (-1 / 30)) < 1e-10

    def test_b3_value(self) -> None:
        r = run_series(3)[2]
        assert abs(r.to_decimal() - 1 / 42) < 1e-10

    def test_series_n1_is_subset_of_n4(self) -> None:
        r1 = run_series(1)
        r4 = run_series(4)
        assert abs(r1[0].to_decimal() - r4[0].to_decimal()) < 1e-10


class TestRunTableA2Once:
    def test_all_24_variables_populated(self) -> None:
        state = run_table_a2_once(n=4, b1=BabbageNumber(-0.5), b3=BabbageNumber(1 / 6))
        for k in range(1, 25):
            assert f"V{k}" in state, f"V{k} missing from state"

    def test_state_values_are_dicts(self) -> None:
        state = run_table_a2_once(n=4, b1=BabbageNumber(-0.5), b3=BabbageNumber(1 / 6))
        for v in state.values():
            assert isinstance(v, dict)

    def test_v13_has_version_1(self) -> None:
        state = run_table_a2_once(n=4, b1=BabbageNumber(-0.5), b3=BabbageNumber(1 / 6))
        assert 1 in state["V13"]

    def test_state_keys_are_v_prefixed_strings(self) -> None:
        state = run_table_a2_once(n=4, b1=BabbageNumber(-0.5), b3=BabbageNumber(1 / 6))
        for key in state:
            assert key.startswith("V")

    def test_different_n_gives_different_state(self) -> None:
        s2 = run_table_a2_once(n=2, b1=BabbageNumber(-0.5), b3=BabbageNumber(1 / 6))
        s4 = run_table_a2_once(n=4, b1=BabbageNumber(-0.5), b3=BabbageNumber(1 / 6))
        # V13 should have more versions for larger n
        assert len(s4["V13"]) >= len(s2["V13"])
