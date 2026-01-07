"""Tests for Note G deck runner."""

from backend.src.emulator.note_g_deck import load_deck, run_once


def test_note_g_deck_loads():
    deck = load_deck()
    assert len(deck) == 25
    assert deck[0]["op"] == 1


def test_note_g_deck_run_once_n1():
    state = run_once(1)
    assert state["V24"].to_decimal() == 0.5
