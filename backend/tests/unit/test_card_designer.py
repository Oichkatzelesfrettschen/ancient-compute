"""Tests for HollerithCardDesigner, JacquardCardDesigner, and AECardDesigner.

Validates:
- Card layout construction and mutation
- Payload format matching machine load handler expectations
- ASCII rendering
- JSON serialization round-trips
- Direct integration with the corresponding machine emulators
- Error handling for invalid inputs
"""

import json
from typing import Any

import pytest

from backend.src.emulator.card_designer import (
    AECardDesigner,
    HollerithCardDesigner,
    JacquardCardDesigner,
)

pytestmark = pytest.mark.unit


@pytest.fixture
def api_client() -> Any:  # type: ignore[return]
    from fastapi import FastAPI
    from fastapi.testclient import TestClient

    from backend.src.api.machines import router

    app = FastAPI()
    app.include_router(router)
    return TestClient(app)


# ---------------------------------------------------------------------------
# HollerithCardDesigner
# ---------------------------------------------------------------------------


class TestHollerithCardDesigner:
    def test_default_cols(self) -> None:
        d = HollerithCardDesigner()
        assert d.cols == 80

    def test_custom_cols(self) -> None:
        d = HollerithCardDesigner(cols=40)
        assert d.cols == 40

    def test_empty_payload(self) -> None:
        d = HollerithCardDesigner()
        payload = d.to_payload()
        assert payload == {"cards": []}

    def test_punch_single_hole(self) -> None:
        d = HollerithCardDesigner()
        d.punch(0, 0)
        payload = d.to_payload()
        assert len(payload["cards"]) == 1
        assert payload["cards"][0] == {"row": 0, "columns": [0]}

    def test_punch_returns_self(self) -> None:
        d = HollerithCardDesigner()
        result = d.punch(0, 0)
        assert result is d

    def test_punch_out_of_col_range(self) -> None:
        d = HollerithCardDesigner(cols=10)
        with pytest.raises(ValueError, match="Column"):
            d.punch(10, 0)

    def test_punch_out_of_row_range(self) -> None:
        d = HollerithCardDesigner()
        with pytest.raises(ValueError, match="Row"):
            d.punch(0, 12)

    def test_encode_char_A(self) -> None:
        # A = rows 0 (zone-12) and 3 (digit-1) in 0-based indexing
        d = HollerithCardDesigner()
        d.encode_char(0, "A")
        payload = d.to_payload()
        assert len(payload["cards"]) == 1
        col_entry = payload["cards"][0]
        assert col_entry["row"] == 0
        assert 0 in col_entry["columns"]
        assert 3 in col_entry["columns"]

    def test_encode_char_digit(self) -> None:
        # "5" = row 7 only
        d = HollerithCardDesigner()
        d.encode_char(0, "5")
        payload = d.to_payload()
        assert payload["cards"][0]["columns"] == [7]

    def test_encode_char_space_is_blank(self) -> None:
        # Space encodes to no holes
        d = HollerithCardDesigner()
        d.encode_char(0, " ")
        payload = d.to_payload()
        assert payload["cards"] == []

    def test_encode_char_lowercase_same_as_upper(self) -> None:
        d1 = HollerithCardDesigner()
        d2 = HollerithCardDesigner()
        d1.encode_char(0, "a")
        d2.encode_char(0, "A")
        assert d1.to_payload() == d2.to_payload()

    def test_encode_text_hello(self) -> None:
        d = HollerithCardDesigner()
        d.encode_text("HELLO", start_col=0)
        payload = d.to_payload()
        # H, E, L, L, O -- all non-space, so 5 non-empty columns
        assert len(payload["cards"]) == 5
        # Columns are 0, 1, 2, 3, 4
        cols = [c["row"] for c in payload["cards"]]
        assert cols == [0, 1, 2, 3, 4]

    def test_encode_text_with_offset(self) -> None:
        d = HollerithCardDesigner()
        d.encode_text("AB", start_col=10)
        payload = d.to_payload()
        assert payload["cards"][0]["row"] == 10
        assert payload["cards"][1]["row"] == 11

    def test_encode_text_truncates_at_cols(self) -> None:
        d = HollerithCardDesigner(cols=5)
        d.encode_text("HELLO WORLD", start_col=0)
        payload = d.to_payload()
        # Only 5 cols; space at col 5 is out of range anyway
        assert all(c["row"] < 5 for c in payload["cards"])

    def test_clear_column(self) -> None:
        d = HollerithCardDesigner()
        d.encode_char(0, "A")
        d.clear_column(0)
        assert d.to_payload()["cards"] == []

    def test_columns_sorted_in_payload(self) -> None:
        d = HollerithCardDesigner()
        d.punch(0, 5).punch(0, 2).punch(0, 9)
        cols = d.to_payload()["cards"][0]["columns"]
        assert cols == sorted(cols)

    def test_to_json_round_trip(self) -> None:
        d = HollerithCardDesigner()
        d.encode_text("AB")
        j = json.loads(d.to_json())
        assert "payload" in j
        assert "cards" in j["payload"]
        assert len(j["payload"]["cards"]) == 2

    def test_render_ascii_row_count(self) -> None:
        d = HollerithCardDesigner(cols=10)
        lines = d.render_ascii().splitlines()
        # Header line + column-index line + 12 data rows = 14 total
        assert any("Hollerith" in line for line in lines)
        # Data rows start with "R" followed by a row label and ":"
        row_lines = [ln for ln in lines if ln.startswith("R") and ":" in ln]
        assert len(row_lines) == 12

    def test_render_ascii_shows_hole(self) -> None:
        d = HollerithCardDesigner(cols=5)
        d.punch(0, 0)
        rendered = d.render_ascii()
        # Row 0 should show O at position 0
        row_line = [ln for ln in rendered.splitlines() if "R12" in ln][0]
        assert "O" in row_line

    def test_payload_format_matches_hollerith_tabulator(self) -> None:
        # Verify the payload structure matches what _apply_load expects:
        # {"cards": [{"row": col_idx, "columns": [row_idx, ...]}]}
        d = HollerithCardDesigner()
        d.punch(3, 5).punch(3, 7)
        payload = d.to_payload()
        assert "cards" in payload
        for card in payload["cards"]:
            assert "row" in card
            assert "columns" in card
            assert isinstance(card["columns"], list)

    def test_chaining(self) -> None:
        # punch, encode_char, encode_text all return self
        d = HollerithCardDesigner()
        result = d.punch(0, 0).encode_char(1, "A").encode_text("BC", 2)
        assert result is d


# ---------------------------------------------------------------------------
# JacquardCardDesigner
# ---------------------------------------------------------------------------


class TestJacquardCardDesigner:
    def test_default_hooks(self) -> None:
        d = JacquardCardDesigner()
        assert d.hooks == 8

    def test_custom_hooks(self) -> None:
        d = JacquardCardDesigner(hooks=16)
        assert d.hooks == 16

    def test_empty_payload(self) -> None:
        d = JacquardCardDesigner(hooks=4)
        assert d.to_payload() == {"cards": []}

    def test_add_card_valid(self) -> None:
        d = JacquardCardDesigner(hooks=8)
        d.add_card([1, 0, 1, 0, 1, 0, 1, 0])
        assert d.to_payload()["cards"] == [[1, 0, 1, 0, 1, 0, 1, 0]]

    def test_add_card_coerces_to_binary(self) -> None:
        d = JacquardCardDesigner(hooks=4)
        d.add_card([5, 0, 3, 0])
        assert d.to_payload()["cards"][0] == [1, 0, 1, 0]

    def test_add_card_wrong_length_raises(self) -> None:
        d = JacquardCardDesigner(hooks=8)
        with pytest.raises(ValueError, match="hooks"):
            d.add_card([1, 0, 1])

    def test_add_card_returns_self(self) -> None:
        d = JacquardCardDesigner(hooks=4)
        result = d.add_card([1, 0, 1, 0])
        assert result is d

    def test_add_row_from_string(self) -> None:
        d = JacquardCardDesigner(hooks=8)
        d.add_row_from_string("11001100")
        assert d.to_payload()["cards"][0] == [1, 1, 0, 0, 1, 1, 0, 0]

    def test_add_row_from_string_ignores_non_binary(self) -> None:
        d = JacquardCardDesigner(hooks=4)
        # Non-binary chars stripped; only 0/1 kept
        d.add_row_from_string("1x0x1x0")
        assert d.to_payload()["cards"][0] == [1, 0, 1, 0]

    def test_stripe_count(self) -> None:
        d = JacquardCardDesigner(hooks=4)
        d.stripe(4)
        assert len(d.to_payload()["cards"]) == 4

    def test_stripe_alternating_pattern(self) -> None:
        d = JacquardCardDesigner(hooks=4)
        d.stripe(2)
        cards = d.to_payload()["cards"]
        # Cards must alternate: first and second should be complements
        assert cards[0] != cards[1]
        for bit_a, bit_b in zip(cards[0], cards[1], strict=True):
            assert bit_a + bit_b == 1

    def test_clear_removes_all_cards(self) -> None:
        d = JacquardCardDesigner(hooks=4)
        d.stripe(4)
        d.clear()
        assert d.to_payload()["cards"] == []

    def test_multiple_cards_in_order(self) -> None:
        d = JacquardCardDesigner(hooks=4)
        d.add_card([1, 0, 0, 0])
        d.add_card([0, 1, 0, 0])
        d.add_card([0, 0, 1, 0])
        cards = d.to_payload()["cards"]
        assert len(cards) == 3
        assert cards[0] == [1, 0, 0, 0]
        assert cards[1] == [0, 1, 0, 0]
        assert cards[2] == [0, 0, 1, 0]

    def test_to_json_format(self) -> None:
        d = JacquardCardDesigner(hooks=4)
        d.add_card([1, 0, 1, 0])
        j = json.loads(d.to_json())
        assert j["payload"]["cards"] == [[1, 0, 1, 0]]

    def test_render_ascii_header(self) -> None:
        d = JacquardCardDesigner(hooks=8)
        rendered = d.render_ascii()
        assert "Jacquard" in rendered
        assert "8" in rendered

    def test_render_ascii_shows_card_rows(self) -> None:
        d = JacquardCardDesigner(hooks=4)
        d.add_card([1, 0, 0, 1])
        rendered = d.render_ascii()
        assert "Card" in rendered
        assert "#..#" in rendered

    def test_payload_loadable_by_jacquard_loom(self) -> None:
        from backend.src.emulator.jacquard import JacquardLoom

        d = JacquardCardDesigner(hooks=8)
        d.add_card([1, 0, 1, 0, 1, 0, 1, 0])
        d.add_card([0, 1, 0, 1, 0, 1, 0, 1])
        loom = JacquardLoom(num_hooks=8)
        loom.load_deck(d.to_payload()["cards"])
        loom.step()
        # get_pattern() returns the list of cards processed so far
        pattern = loom.get_pattern()
        assert pattern[0] == [1, 0, 1, 0, 1, 0, 1, 0]


# ---------------------------------------------------------------------------
# AECardDesigner
# ---------------------------------------------------------------------------


class TestAECardDesigner:
    def test_empty_source(self) -> None:
        d = AECardDesigner()
        payload = d.to_payload()
        assert payload == {"source": ""}

    def test_source_roundtrip(self) -> None:
        d = AECardDesigner("LOAD A, 5\nHALT")
        payload = d.to_payload()
        assert payload["source"] == "LOAD A, 5\nHALT"

    def test_source_stripped(self) -> None:
        # Leading/trailing whitespace trimmed
        d = AECardDesigner("  HALT  \n")
        assert d.to_payload()["source"] == "HALT"

    def test_set_source_replaces(self) -> None:
        d = AECardDesigner("LOAD A, 1")
        d.set_source("HALT")
        assert d.to_payload()["source"] == "HALT"

    def test_set_source_returns_self(self) -> None:
        d = AECardDesigner()
        assert d.set_source("HALT") is d

    def test_append_instruction(self) -> None:
        d = AECardDesigner()
        d.append_instruction("LOAD A, 3")
        d.append_instruction("HALT")
        source = d.to_payload()["source"]
        assert "LOAD A, 3" in source
        assert "HALT" in source

    def test_append_instruction_returns_self(self) -> None:
        d = AECardDesigner()
        assert d.append_instruction("HALT") is d

    def test_to_json_payload_key(self) -> None:
        d = AECardDesigner("HALT")
        j = json.loads(d.to_json())
        assert "payload" in j
        assert "source" in j["payload"]
        assert j["payload"]["source"] == "HALT"

    def test_render_ascii_fallback_shows_source(self) -> None:
        d = AECardDesigner("LOAD A, 7\nHALT")
        rendered = d.render_ascii()
        # Falls back to raw source display (card_compiler may not be available)
        assert "HALT" in rendered or "AE" in rendered

    def test_payload_loadable_by_ae_via_api(self, api_client: Any) -> None:
        d = AECardDesigner("LOAD A, 7\nHALT")
        api_client.post("/machines/analytical-engine/reset")
        resp = api_client.post(
            "/machines/analytical-engine/load",
            json=json.loads(d.to_json()),
        )
        assert resp.status_code == 200

    def test_multiline_append_no_double_newline(self) -> None:
        d = AECardDesigner("LOAD A, 1")
        d.append_instruction("LOAD B, 2")
        d.append_instruction("HALT")
        source = d.to_payload()["source"]
        # Should not have double blank lines between instructions
        assert "\n\n" not in source

    def test_chaining(self) -> None:
        d = AECardDesigner()
        result = d.set_source("LOAD A, 1").append_instruction("HALT")
        assert result is d
