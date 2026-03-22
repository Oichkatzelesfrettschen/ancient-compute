"""Unit tests for the Hollerith Tabulating Machine emulator (1890)."""

import pytest

from backend.src.emulator.hollerith_tabulator import (
    CARD_COLS,
    CARD_ROWS,
    MAX_COUNTERS,
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


# ---------------------------------------------------------------------------
# PunchedCard extended
# ---------------------------------------------------------------------------


class TestPunchedCardExtended:
    """Extended PunchedCard edge cases and boundary conditions."""

    def test_punch_idempotent(self) -> None:
        c = PunchedCard()
        c.punch(0, 0)
        c.punch(0, 0)  # punching same hole twice is no-op
        assert c.total_holes() == 1

    def test_read_out_of_range_row_raises(self) -> None:
        c = PunchedCard()
        with pytest.raises(IndexError):
            c.read(CARD_ROWS, 0)

    def test_read_out_of_range_col_raises(self) -> None:
        c = PunchedCard()
        with pytest.raises(IndexError):
            c.read(0, CARD_COLS)

    def test_punch_negative_row_raises(self) -> None:
        c = PunchedCard()
        with pytest.raises(IndexError):
            c.punch(-1, 0)

    def test_punch_negative_col_raises(self) -> None:
        c = PunchedCard()
        with pytest.raises(IndexError):
            c.punch(0, -1)

    def test_holes_in_row_empty_when_no_holes(self) -> None:
        c = PunchedCard()
        assert c.holes_in_row(0) == []

    def test_holes_in_col_empty_when_no_holes(self) -> None:
        c = PunchedCard()
        assert c.holes_in_col(0) == []

    def test_holes_in_row_returns_sorted_cols(self) -> None:
        c = PunchedCard()
        c.punch(5, 10)
        c.punch(5, 3)
        c.punch(5, 7)
        assert c.holes_in_row(5) == [3, 7, 10]

    def test_holes_in_col_returns_sorted_rows(self) -> None:
        c = PunchedCard()
        c.punch(8, 2)
        c.punch(1, 2)
        c.punch(11, 2)
        assert c.holes_in_col(2) == [1, 8, 11]

    def test_punch_max_row(self) -> None:
        c = PunchedCard()
        c.punch(CARD_ROWS - 1, 0)
        assert c.read(CARD_ROWS - 1, 0) is True

    def test_punch_max_col(self) -> None:
        c = PunchedCard()
        c.punch(0, CARD_COLS - 1)
        assert c.read(0, CARD_COLS - 1) is True

    def test_total_holes_all_columns_one_row(self) -> None:
        c = PunchedCard()
        for col in range(CARD_COLS):
            c.punch(0, col)
        assert c.total_holes() == CARD_COLS

    def test_unread_position_is_false(self) -> None:
        c = PunchedCard()
        c.punch(3, 5)
        assert c.read(3, 6) is False
        assert c.read(4, 5) is False

    def test_card_rows_and_cols_constants(self) -> None:
        assert CARD_ROWS == 12
        assert CARD_COLS == 24


# ---------------------------------------------------------------------------
# CounterDial extended
# ---------------------------------------------------------------------------


class TestCounterDialExtended:
    """CounterDial: total_increments, wrap, reset preserves total."""

    def test_total_increments_not_reset_by_reset(self) -> None:
        d = CounterDial()
        for _ in range(15):
            d.increment()
        d.reset()
        assert d.value == 0
        # total_increments is a running total -- reset() does NOT clear it
        # (historical machines: total_increments tracks lifetime usage)
        # Verify this by checking total_increments still equals 15
        assert d.total_increments == 15

    def test_wrap_at_ten_value_zero(self) -> None:
        d = CounterDial()
        for _ in range(10):
            d.increment()
        assert d.value == 0

    def test_wrap_multiple_times(self) -> None:
        d = CounterDial()
        for _ in range(23):
            d.increment()
        assert d.value == 3
        assert d.total_increments == 23

    def test_initial_total_increments_zero(self) -> None:
        d = CounterDial()
        assert d.total_increments == 0

    def test_initial_value_zero(self) -> None:
        d = CounterDial()
        assert d.value == 0

    def test_increment_nine_times(self) -> None:
        d = CounterDial()
        for _ in range(9):
            d.increment()
        assert d.value == 9

    def test_value_cycles_mod_ten(self) -> None:
        d = CounterDial()
        for i in range(30):
            d.increment()
            assert d.value == (i + 1) % 10


# ---------------------------------------------------------------------------
# HollerithTabulator extended
# ---------------------------------------------------------------------------


class TestHollerithTabulatorExtended:
    """Extended HollerithTabulator coverage: counters, aux, constants."""

    def test_counter_lazy_init_on_first_access(self) -> None:
        t = HollerithTabulator()
        # Access a counter that hasn't been used: creates fresh dial
        dial = t.counter(0, 0)
        assert dial.value == 0
        assert dial.total_increments == 0

    def test_aux_counter_zero_valid(self) -> None:
        t = HollerithTabulator()
        assert t.aux_counter(0).value == 0

    def test_aux_counter_max_valid(self) -> None:
        t = HollerithTabulator()
        assert t.aux_counter(MAX_COUNTERS - 1).value == 0

    def test_aux_counter_negative_raises(self) -> None:
        t = HollerithTabulator()
        with pytest.raises(IndexError):
            t.aux_counter(-1)

    def test_max_counters_constant(self) -> None:
        assert MAX_COUNTERS == 40

    def test_sort_bins_constant(self) -> None:
        assert SORT_BINS == 26

    def test_read_card_blank_card_increments_none(self) -> None:
        t = HollerithTabulator()
        result = t.read_card(PunchedCard())
        assert result == {}
        assert t.cards_processed == 1

    def test_read_card_returns_incremented_positions(self) -> None:
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(1, 2)
        c.punch(3, 4)
        result = t.read_card(c)
        assert (1, 2) in result
        assert (3, 4) in result

    def test_counter_returns_same_object_on_repeated_access(self) -> None:
        t = HollerithTabulator()
        dial_a = t.counter(5, 5)
        dial_a.increment()
        dial_b = t.counter(5, 5)
        assert dial_b.value == 1  # same object

    def test_reset_clears_lazy_counters(self) -> None:
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(0, 0)
        t.read_card(c)
        t.reset_counters()
        # After reset, counter dict is cleared; new access creates fresh dial
        assert t.counter(0, 0).value == 0

    def test_reset_clears_aux_counters(self) -> None:
        t = HollerithTabulator()
        t.aux_counter(0).increment()
        t.reset_counters()
        assert t.aux_counter(0).value == 0

    def test_tabulate_returns_total_increments(self) -> None:
        # tabulate() returns dict with total_increments (not value after wraps)
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(0, 0)
        t.tabulate([c] * 12)
        t.tabulate([])  # flush without adding more
        # After 12 increments the dial wrapped to 2; total_increments=12
        assert t.counter(0, 0).total_increments == 12

    def test_sort_column_zero_valid(self) -> None:
        t = HollerithTabulator()
        bins = t.sort([], sort_column=0)
        assert isinstance(bins, dict)
        assert len(bins) == SORT_BINS

    def test_sort_last_valid_column(self) -> None:
        t = HollerithTabulator()
        bins = t.sort([], sort_column=CARD_COLS - 1)
        assert isinstance(bins, dict)

    def test_sort_negative_column_raises(self) -> None:
        t = HollerithTabulator()
        with pytest.raises(IndexError):
            t.sort([], sort_column=-1)

    def test_sort_multiple_holes_same_col_topmost_row_wins(self) -> None:
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(5, 0)
        c.punch(3, 0)  # row 3 < row 5 -> card goes to bin 3
        bins = t.sort([c], sort_column=0)
        assert c in bins[3]
        assert c not in bins[5]

    def test_sort_bins_structure_has_sort_bins_keys(self) -> None:
        t = HollerithTabulator()
        bins = t.sort([], sort_column=0)
        assert set(bins.keys()) == set(range(SORT_BINS))

    def test_report_empty_when_no_cards(self) -> None:
        t = HollerithTabulator()
        report = t.report()
        assert report["cards_processed"] == 0
        assert report["counts"] == {}

    def test_report_excludes_zero_count_positions(self) -> None:
        t = HollerithTabulator()
        t.counter(0, 0)  # access creates dial but never incremented
        report = t.report()
        assert "R0C0" not in report["counts"]

    def test_report_keys_format_R_C(self) -> None:
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(0, 5)
        c.punch(11, 23)
        t.read_card(c)
        report = t.report()
        assert "R0C5" in report["counts"]
        assert "R11C23" in report["counts"]


# ---------------------------------------------------------------------------
# HollerithTabulator integration
# ---------------------------------------------------------------------------


class TestHollerithIntegration:
    """Multi-card batch processing and census-style workflows."""

    def test_census_batch_with_three_fields(self) -> None:
        # Simulate 10 census records: sex (row 0), age_bracket (row 3), occupation (row 7)
        t = HollerithTabulator()
        cards = []
        for i in range(10):
            c = PunchedCard()
            c.punch(0, i % 2)  # sex: alternating col 0/1
            c.punch(3, i % 5)  # age: cycles through 0-4
            c.punch(7, i % 3)  # occupation: cycles 0-2
            cards.append(c)
        t.tabulate(cards)
        assert t.cards_processed == 10

    def test_read_then_tabulate_accumulates(self) -> None:
        t = HollerithTabulator()
        c = PunchedCard()
        c.punch(2, 7)
        t.read_card(c)  # 1 manual
        t.tabulate([c, c])  # 2 more
        assert t.counter(2, 7).total_increments == 3

    def test_sort_then_tabulate_by_bin(self) -> None:
        t = HollerithTabulator()
        cards_by_type = []
        for row in range(CARD_ROWS):
            c = PunchedCard()
            c.punch(row, 0)  # sort key column 0
            cards_by_type.append(c)
        bins = t.sort(cards_by_type, sort_column=0)
        for row in range(CARD_ROWS):
            assert len(bins[row]) == 1
