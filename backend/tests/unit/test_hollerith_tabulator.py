"""Unit tests for the Hollerith Tabulating Machine emulator (1890)."""

import pytest

from backend.src.emulator.hollerith_tabulator import (
    CARD_COLS,
    CARD_ROWS,
    SORT_BINS,
    CounterDial,
    HollerithTabulator,
    PunchedCard,
)

# ---------------------------------------------------------------------------
# PunchedCard
# ---------------------------------------------------------------------------


class TestPunchedCard:
    def test_default_empty(self):
        c = PunchedCard()
        assert c.total_holes() == 0

    def test_punch_and_read(self):
        c = PunchedCard()
        c.punch(0, 0)
        assert c.read(0, 0) is True
        assert c.read(0, 1) is False

    def test_punch_all_corners(self):
        c = PunchedCard()
        c.punch(0, 0)
        c.punch(0, CARD_COLS - 1)
        c.punch(CARD_ROWS - 1, 0)
        c.punch(CARD_ROWS - 1, CARD_COLS - 1)
        assert c.total_holes() == 4

    def test_punch_out_of_range_raises(self):
        c = PunchedCard()
        with pytest.raises(IndexError):
            c.punch(CARD_ROWS, 0)
        with pytest.raises(IndexError):
            c.punch(0, CARD_COLS)

    def test_holes_in_row(self):
        c = PunchedCard()
        c.punch(3, 5)
        c.punch(3, 10)
        assert c.holes_in_row(3) == [5, 10]

    def test_holes_in_col(self):
        c = PunchedCard()
        c.punch(2, 4)
        c.punch(7, 4)
        assert c.holes_in_col(4) == [2, 7]

    def test_total_holes(self):
        c = PunchedCard()
        for row in range(3):
            for col in range(4):
                c.punch(row, col)
        assert c.total_holes() == 12


# ---------------------------------------------------------------------------
# CounterDial
# ---------------------------------------------------------------------------


class TestCounterDial:
    def test_initial_zero(self):
        d = CounterDial()
        assert d.value == 0

    def test_increment_five(self):
        d = CounterDial()
        for _ in range(5):
            d.increment()
        assert d.value == 5

    def test_wrap_at_ten(self):
        d = CounterDial()
        for _ in range(10):
            d.increment()
        assert d.value == 0
        assert d.total_increments == 10

    def test_total_increments_across_wraps(self):
        d = CounterDial()
        for _ in range(25):
            d.increment()
        assert d.total_increments == 25

    def test_reset(self):
        d = CounterDial()
        for _ in range(7):
            d.increment()
        d.reset()
        assert d.value == 0


# ---------------------------------------------------------------------------
# read_card
# ---------------------------------------------------------------------------


class TestReadCard:
    def test_single_hole_increments_counter(self):
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(5, 10)
        t.read_card(c)
        assert t.counter(5, 10).value == 1

    def test_multiple_holes_increment_all(self):
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(0, 0)
        c.punch(3, 7)
        c.punch(11, 23)
        t.read_card(c)
        assert t.counter(0, 0).value == 1
        assert t.counter(3, 7).value == 1
        assert t.counter(11, 23).value == 1

    def test_cards_processed_counter(self):
        t = HollerithTabulator()
        for _ in range(5):
            t.read_card(PunchedCard())
        assert t.cards_processed == 5

    def test_same_hole_multiple_cards(self):
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(2, 3)
        for _ in range(7):
            t.read_card(c)
        assert t.counter(2, 3).total_increments == 7


# ---------------------------------------------------------------------------
# tabulate (batch)
# ---------------------------------------------------------------------------


class TestTabulate:
    def test_tabulate_empty(self):
        t = HollerithTabulator()
        result = t.tabulate([])
        assert result == {}

    def test_tabulate_counts_correctly(self):
        t = HollerithTabulator()
        cards = []
        for _ in range(10):
            c = PunchedCard()
            c.punch(0, 5)  # same hole on every card
            cards.append(c)
        result = t.tabulate(cards)
        assert result[(0, 5)] == 10

    def test_tabulate_multiple_fields(self):
        t = HollerithTabulator()
        c1 = PunchedCard()
        c1.punch(0, 0)
        c1.punch(1, 1)
        c2 = PunchedCard()
        c2.punch(0, 0)  # same
        c2.punch(2, 2)  # different
        t.tabulate([c1, c2])
        assert t.counter(0, 0).total_increments == 2
        assert t.counter(1, 1).total_increments == 1
        assert t.counter(2, 2).total_increments == 1


# ---------------------------------------------------------------------------
# sort
# ---------------------------------------------------------------------------


class TestSort:
    def test_sort_into_bins(self):
        t = HollerithTabulator()
        cards = []
        for row in range(CARD_ROWS):
            c = PunchedCard()
            c.punch(row, col=0)
            cards.append(c)
        bins = t.sort(cards, sort_column=0)
        # Each card goes to a unique bin (row 0..11 -> bin 0..11)
        for row in range(CARD_ROWS):
            assert len(bins[row]) == 1

    def test_blank_card_goes_to_reject_bin(self):
        t = HollerithTabulator()
        blank = PunchedCard()
        bins = t.sort([blank], sort_column=0)
        assert blank in bins[SORT_BINS - 1]

    def test_sort_invalid_column_raises(self):
        t = HollerithTabulator()
        with pytest.raises(IndexError):
            t.sort([], sort_column=CARD_COLS)

    def test_sort_all_to_same_bin(self):
        t = HollerithTabulator()
        cards = []
        for _ in range(5):
            c = PunchedCard()
            c.punch(3, col=0)  # all row 3 -> bin 3
            cards.append(c)
        bins = t.sort(cards, sort_column=0)
        assert len(bins[3]) == 5


# ---------------------------------------------------------------------------
# Reset and report
# ---------------------------------------------------------------------------


class TestResetAndReport:
    def test_reset_clears_all(self):
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(0, 0)
        t.read_card(c)
        t.reset_counters()
        assert t.cards_processed == 0
        assert t.counter(0, 0).value == 0

    def test_report_structure(self):
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(1, 2)
        t.read_card(c)
        report = t.report()
        assert "cards_processed" in report
        assert "counts" in report
        assert report["cards_processed"] == 1
        assert "R1C2" in report["counts"]

    def test_aux_counter_out_of_range(self):
        t = HollerithTabulator()
        with pytest.raises(IndexError):
            t.aux_counter(40)
