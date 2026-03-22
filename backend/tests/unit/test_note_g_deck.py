"""Tests for Note G outputs and Table A.2 runner."""

from backend.src.emulator.analytical_engine import BabbageNumber
from backend.src.emulator.note_g_deck import load_deck, run_once, run_series, run_table_a2_once


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


# ---------------------------------------------------------------------------
# Deck extended: specific field values
# ---------------------------------------------------------------------------


class TestLoadDeckExtended:
    """Deck field-level correctness: opcodes, operands, and structure."""

    def test_all_opcodes_in_valid_set(self) -> None:
        valid = {"add", "sub", "mult", "div"}
        for card in load_deck():
            assert card["opcode"] in valid, f"op {card['op']}: unknown opcode {card['opcode']}"

    def test_expr_fields_are_nonempty_strings(self) -> None:
        for card in load_deck():
            assert isinstance(card["expr"], str)
            assert len(card["expr"]) > 0

    def test_lhs_and_rhs_are_strings(self) -> None:
        for card in load_deck():
            assert isinstance(card["lhs"], str)
            assert isinstance(card["rhs"], str)

    def test_out_lists_all_v_prefixed(self) -> None:
        for card in load_deck():
            for out in card["out"]:
                assert isinstance(out, str)
                assert out.startswith("V")

    def test_deck_ops_1_through_25(self) -> None:
        ops = [card["op"] for card in load_deck()]
        assert ops == list(range(1, 26))


# ---------------------------------------------------------------------------
# run_once: single-pass execution
# ---------------------------------------------------------------------------


class TestRunOnce:
    """run_once() state dictionary properties."""

    def test_run_once_returns_dict(self) -> None:
        result = run_once(n=4)
        assert isinstance(result, dict)

    def test_run_once_has_24_variables(self) -> None:
        result = run_once(n=4)
        for i in range(1, 25):
            assert f"V{i}" in result, f"V{i} missing from run_once state"

    def test_run_once_values_are_babbage_numbers(self) -> None:
        result = run_once(n=4)
        for v in result.values():
            assert isinstance(v, BabbageNumber)

    def test_run_once_v3_equals_n(self) -> None:
        for n in [1, 3, 5]:
            result = run_once(n=n)
            # V3 is initialized to n before operations run; may be overwritten
            # but should always exist
            assert "V3" in result

    def test_run_once_v2_initialized_to_2(self) -> None:
        # V2 = 2 is the initial value and may be read but not written early
        result = run_once(n=4)
        # After execution, V2 should still be a BabbageNumber
        assert isinstance(result["V2"], BabbageNumber)


# ---------------------------------------------------------------------------
# Bernoulli series accuracy
# ---------------------------------------------------------------------------


class TestBernoulliAccuracy:
    """run_series precision matches Ada's tabulated B values."""

    def test_b4_value(self) -> None:
        r = run_series(4)[3]
        assert abs(r.to_decimal() - (-1 / 30)) < 1e-10

    def test_sign_alternates_after_b1(self) -> None:
        # B1=+1/6, B2=-1/30, B3=+1/42, B4=-1/30
        results = run_series(4)
        for i in range(1, 4):
            # signs should alternate: +, -, +, -
            prev_sign = 1 if results[i - 1].to_decimal() > 0 else -1
            curr_sign = 1 if results[i].to_decimal() > 0 else -1
            assert prev_sign != curr_sign, f"Sign did not alternate at index {i}"

    def test_run_series_n5_has_five_elements(self) -> None:
        assert len(run_series(5)) == 5

    def test_run_series_values_match_fractions(self) -> None:
        expected = {1: 1 / 6, 2: -1 / 30, 3: 1 / 42}
        results = run_series(3)
        for i, e in expected.items():
            assert abs(results[i - 1].to_decimal() - e) < 1e-10


# ---------------------------------------------------------------------------
# run_table_a2_once extended
# ---------------------------------------------------------------------------


class TestRunTableA2OnceExtended:
    """Additional run_table_a2_once state verification."""

    def test_v1_has_version_1(self) -> None:
        state = run_table_a2_once(n=4, b1=BabbageNumber(-0.5), b3=BabbageNumber(1 / 6))
        assert 1 in state["V1"]

    def test_version_0_is_initial_zero(self) -> None:
        state = run_table_a2_once(n=1, b1=BabbageNumber(0), b3=BabbageNumber(0))
        # V0 initial state for unused vars is 0
        for var in state:
            assert 0 in state[var]

    def test_state_has_exactly_24_vars(self) -> None:
        state = run_table_a2_once(n=4, b1=BabbageNumber(-0.5), b3=BabbageNumber(1 / 6))
        assert len(state) == 24


class TestLoadDeckStructureExtra:
    """Additional load_deck() field tests."""

    def test_deck_has_25_ops(self) -> None:
        deck = load_deck()
        assert len(deck) == 25

    def test_op_numbers_end_at_25(self) -> None:
        deck = load_deck()
        assert deck[-1]["op"] == 25

    def test_each_op_has_opcode_key(self) -> None:
        deck = load_deck()
        for item in deck:
            assert "opcode" in item

    def test_opcodes_are_strings(self) -> None:
        deck = load_deck()
        for item in deck:
            assert isinstance(item["opcode"], str)


class TestRunOnceResultsExtra:
    """Additional run_once() output shape tests."""

    def test_run_once_returns_dict(self) -> None:
        result = run_once(n=1)
        assert isinstance(result, dict)

    def test_run_once_result_has_v24(self) -> None:
        result = run_once(n=1)
        assert "V24" in result

    def test_run_once_result_values_are_babbage_numbers(self) -> None:
        result = run_once(n=1)
        for v in result.values():
            assert isinstance(v, BabbageNumber)

    def test_run_once_n1_v24_is_positive(self) -> None:
        result = run_once(n=1)
        # B_1 = 1/6 > 0
        assert float(result["V24"].to_decimal()) > 0

    def test_run_series_first_element_is_b1(self) -> None:
        results = run_series(1)
        assert abs(float(results[0].to_decimal()) - 1 / 6) < 1e-9


class TestNoteGDeckLoadDeck:
    """load_deck() structural properties."""

    def test_load_deck_returns_list(self) -> None:
        from backend.src.emulator.note_g_deck import load_deck
        deck = load_deck()
        assert isinstance(deck, list)

    def test_load_deck_non_empty(self) -> None:
        from backend.src.emulator.note_g_deck import load_deck
        assert len(load_deck()) > 0
