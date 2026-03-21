"""Hollerith Tabulating Machine Emulator (United States, 1890).

Herman Hollerith (1860-1929) invented the electromechanical punched-card
tabulator for the 1890 US Census, reducing tabulation time from 7 years
(1880 census, manual) to under 1 year. The technology became the basis for
IBM (International Business Machines, founded 1911 as Computing-Tabulating-
Recording Company after Hollerith sold his company in 1911).

Architecture (1890 census machine):
  - Punched card: 12 rows x 24 columns (later 12 x 80 in IBM standard).
    Each hole represents one data attribute (race, age, sex, occupation...).
  - Card reader: A plate pressed down on the card; pins drop through holes
    into mercury cups, completing electrical circuits.
  - Counters: 40 electromagnetic counter dials, each with a 10-position
    wheel. A completed circuit increments the corresponding counter.
  - Sorter: Optional bin mechanism to file cards into one of 26 sort bins
    based on a selected column (sort key).
  - Accumulator: Not present in 1890 model; Hollerith's 1895 improved
    machine added numerical summation.

Card encoding (1890 model):
  Row 0..11: Each row = one attribute field.
  Column 0..23: Position within field (up to 24 distinct values per row).
  A hole at (row, col) = True means that attribute/value is present.

This emulator models:
  - 12x24 punched card (historical 1890 census configuration).
  - Counter bank with 40 dials.
  - Card reading and counter increment logic.
  - Sort-bin assignment based on a selected column.
  - Batch processing of multiple cards.

References:
  - Hollerith, H. (1889). An Electric Tabulating System. PhD thesis,
    Columbia University School of Mines. (Hollerith's own description.)
  - Heide, L. (2009). Punched-Card Systems and the Early Information
    Explosion. Johns Hopkins University Press.
  - Austrian, G. D. (1982). Herman Hollerith: Forgotten Giant of
    Information Processing. Columbia University Press.
"""

from __future__ import annotations

from dataclasses import dataclass, field

CARD_ROWS = 12
CARD_COLS = 24
MAX_COUNTERS = 40
SORT_BINS = 26


@dataclass
class PunchedCard:
    """A single 12x24 Hollerith punched card.

    holes[row][col] = True means there is a hole at that position.
    Row 0 = top row ("12-row" in later IBM terminology).
    Row 11 = bottom row ("1-row").
    """

    holes: list[list[bool]] = field(
        default_factory=lambda: [[False] * CARD_COLS for _ in range(CARD_ROWS)]
    )

    def punch(self, row: int, col: int) -> None:
        """Punch a hole at (row, col)."""
        if not (0 <= row < CARD_ROWS and 0 <= col < CARD_COLS):
            raise IndexError(
                f"Position ({row},{col}) out of range [{CARD_ROWS}x{CARD_COLS}]"
            )
        self.holes[row][col] = True

    def read(self, row: int, col: int) -> bool:
        """Return True if a hole exists at (row, col)."""
        if not (0 <= row < CARD_ROWS and 0 <= col < CARD_COLS):
            raise IndexError(
                f"Position ({row},{col}) out of range [{CARD_ROWS}x{CARD_COLS}]"
            )
        return self.holes[row][col]

    def holes_in_row(self, row: int) -> list[int]:
        """Return list of column indices where row has holes."""
        return [c for c in range(CARD_COLS) if self.holes[row][c]]

    def holes_in_col(self, col: int) -> list[int]:
        """Return list of row indices where col has holes."""
        return [r for r in range(CARD_ROWS) if self.holes[r][col]]

    def total_holes(self) -> int:
        return sum(self.holes[r][c] for r in range(CARD_ROWS) for c in range(CARD_COLS))


class CounterDial:
    """One 10-position electromagnetic counter dial.

    Increments when energized. Resets to 0 manually.
    Overflows at 10 are not automatically carried to adjacent dials in the
    1890 model (each counter is independent).
    """

    def __init__(self) -> None:
        self._value: int = 0
        self._total_increments: int = 0  # lifetime count (no overflow reset)

    @property
    def value(self) -> int:
        return self._value

    def increment(self) -> None:
        self._value = (self._value + 1) % 10
        self._total_increments += 1

    def reset(self) -> None:
        self._value = 0

    @property
    def total_increments(self) -> int:
        return self._total_increments


class HollerithTabulator:
    """Hollerith Tabulating Machine (1890 Census model).

    Usage::

        tabulator = HollerithTabulator()

        # Build cards
        card = PunchedCard()
        card.punch(row=3, col=5)    # e.g., race field, value 5
        card.punch(row=7, col=1)    # age bracket 1

        # Read and count
        tabulator.read_card(card)

        # Inspect counters
        print(tabulator.counter(3, 5).value)   # counter for row=3, col=5
        print(tabulator.cards_processed)

        # Sort a batch by column 5
        bins = tabulator.sort([card1, card2, card3], sort_column=5)
    """

    def __init__(self) -> None:
        # Counter matrix: one counter per (row, col) hole position.
        # Stored as dict for sparse representation (most positions empty).
        self._counters: dict[tuple[int, int], CounterDial] = {}
        self.cards_processed: int = 0

        # Auxiliary counters (numbered 0..MAX_COUNTERS-1) for user assignment.
        self._aux_counters: list[CounterDial] = [
            CounterDial() for _ in range(MAX_COUNTERS)
        ]

    def counter(self, row: int, col: int) -> CounterDial:
        """Return the counter for hole position (row, col).

        Creates the counter on first access (lazy init).
        """
        key = (row, col)
        if key not in self._counters:
            self._counters[key] = CounterDial()
        return self._counters[key]

    def aux_counter(self, index: int) -> CounterDial:
        """Return auxiliary counter by index (0..MAX_COUNTERS-1)."""
        if not 0 <= index < MAX_COUNTERS:
            raise IndexError(f"Aux counter index {index} out of range [0, {MAX_COUNTERS-1}]")
        return self._aux_counters[index]

    def read_card(self, card: PunchedCard) -> dict[tuple[int, int], int]:
        """Process one card: increment a counter for each punched hole.

        Returns a dict mapping (row, col) -> counter value after increment.
        """
        incremented: dict[tuple[int, int], int] = {}
        for row in range(CARD_ROWS):
            for col in range(CARD_COLS):
                if card.holes[row][col]:
                    c = self.counter(row, col)
                    c.increment()
                    incremented[(row, col)] = c.value
        self.cards_processed += 1
        return incremented

    def tabulate(self, cards: list[PunchedCard]) -> dict[tuple[int, int], int]:
        """Process a batch of cards.

        Returns dict mapping each (row, col) that was punched at least once
        to the total count across all cards.
        """
        for card in cards:
            self.read_card(card)
        return {key: dial.total_increments for key, dial in self._counters.items()}

    def sort(self, cards: list[PunchedCard], sort_column: int) -> dict[int, list[PunchedCard]]:
        """Sort cards into bins by the highest punched row in sort_column.

        Historical sorter: the bin is determined by which row has a hole in
        the selected column. If multiple rows are punched in that column,
        the top-most (smallest row index) determines the bin. Cards with no
        hole in sort_column go to bin SORT_BINS-1 ("reject bin").

        Bins 0..SORT_BINS-2 correspond to sort values 0..SORT_BINS-2.
        Bin SORT_BINS-1 is the reject/blank bin.

        Args:
            cards: List of cards to sort.
            sort_column: Column index (0..CARD_COLS-1) used as sort key.

        Returns:
            Dict mapping bin number -> list of cards sorted into that bin.
        """
        if not 0 <= sort_column < CARD_COLS:
            raise IndexError(
                f"Sort column {sort_column} out of range [0, {CARD_COLS-1}]"
            )
        bins: dict[int, list[PunchedCard]] = {i: [] for i in range(SORT_BINS)}
        for card in cards:
            punched_rows = card.holes_in_col(sort_column)
            if punched_rows:
                bin_key = punched_rows[0] % SORT_BINS
            else:
                bin_key = SORT_BINS - 1  # reject bin
            bins[bin_key].append(card)
        return bins

    def reset_counters(self) -> None:
        """Reset all counters and the cards-processed tally."""
        for dial in self._counters.values():
            dial.reset()
        for dial in self._aux_counters:
            dial.reset()
        self._counters.clear()
        self.cards_processed = 0

    def report(self) -> dict[str, object]:
        """Return a summary report of all non-zero counters."""
        return {
            "cards_processed": self.cards_processed,
            "counts": {
                f"R{r}C{c}": d.total_increments
                for (r, c), d in sorted(self._counters.items())
                if d.total_increments > 0
            },
        }
