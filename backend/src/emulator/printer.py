"""
Printer and Stereotyper: Output Subsystems for Difference Engine No. 2

Implements the mechanical printing and stereotype plate generation mechanisms
that output computed results from the Difference Engine.

Historical Context:
  - The Difference Engine No. 2 was designed to both compute and print results
  - Babbage's printer could output 8-digit numbers in neat rows
  - The stereotyper created stereotype plates for mass printing (revolutionary!)
  - Ada Lovelace's notes describe the output mechanisms in detail
  - Printing was synchronized with mechanical cycles via the main shaft

Output Mechanisms:
  - Printer: Direct mechanical printing to paper (8 digit columns)
  - Stereotyper: Creates mold frames for mass printing (8 × 50 mold size)

References:
  - Babbage, Charles. "Passages from the Life of a Philosopher" (1864)
  - Swade, Doron. "The Cogwheel Brain" (2001), Chapter 10
  - Menabrea/Lovelace. "Notes on the Analytical Engine" (1843)
"""

from dataclasses import dataclass, field
from enum import Enum


class PrinterState(Enum):
    """State of printer mechanism."""

    IDLE = "IDLE"                      # Waiting for command
    POSITIONING = "POSITIONING"        # Moving to next print position
    INKING = "INKING"                  # Inking roller engaged
    STRIKING = "STRIKING"              # Hammer striking paper
    ADVANCING = "ADVANCING"            # Advancing to next line


@dataclass
class PrinterSnapshot:
    """Complete state of printer mechanism at a point in time."""

    state: PrinterState                 # Current mechanical state
    type_wheels: list[int]              # 8 digit positions (0-9 each)
    inking_engaged: bool                # Inking roller active
    hammer_ready: bool                  # Hammer positioned
    platen_position: int                # Current line number
    printed_lines: list[str] = field(default_factory=list)  # Lines printed so far
    total_operations: int = 0           # Total print operations


@dataclass
class StereotyperSnapshot:
    """Complete state of stereotyper mechanism."""

    x_position: int                     # 0-7 (digit column positions)
    y_position: int                     # 0-49 (line positions)
    mold_image: dict[tuple[int, int], bool] = field(default_factory=dict)  # (x,y) -> raised
    molds_completed: int = 0            # Completed and extracted molds
    current_height: int = 0             # Current y-position for mold build


class Printer:
    """
    Mechanical Printer for Difference Engine No. 2

    Outputs computed results as 8-digit numbers with proper formatting.
    Simulates mechanical printing with:
    - Type wheel positioning (8 wheels for 8 columns)
    - Inking roller engagement
    - Hammer striking mechanism
    - Platen (paper) advancement
    - Line and page management

    Specifications:
    - 8 digit columns (corresponds to 8-column results)
    - Each digit 0-9
    - Multiple lines per page (up to 50)
    - Formatted output with spacing
    """

    # Printer specifications
    NUM_COLUMNS = 8                     # 8 digit columns (matches 8 computation columns)
    LINES_PER_PAGE = 50                 # Standard page size
    DIGITS_PER_NUMBER = 8               # Formatted as 8-digit numbers

    def __init__(self):
        """Initialize printer with empty state."""
        self.state = PrinterState.IDLE
        self.type_wheels: list[int] = [0] * self.NUM_COLUMNS  # 8 wheels, each 0-9
        self.inking_engaged = False
        self.hammer_ready = False
        self.platen_position = 0        # Current line (0 = top of page)
        self.printed_lines: list[str] = []
        self.total_operations = 0

    def print_number(self, number: int) -> str:
        """
        Print a single number (8-digit result).

        Args:
            number: Integer value to print (0 to 99,999,999)

        Returns:
            Formatted string representation (8 digits, zero-padded)

        Mechanical phases:
            1. Position type wheels to digit values
            2. Engage inking roller
            3. Strike hammer
            4. Record printed output
        """
        if number < 0:
            raise ValueError(f"Cannot print negative number: {number}")
        if number > 99_999_999:
            raise ValueError(f"Number too large for 8-digit printer: {number}")

        # Extract digits (right-aligned, zero-padded)
        number_str = str(number).zfill(self.DIGITS_PER_NUMBER)

        # Update type wheels to match digits
        self.state = PrinterState.POSITIONING
        for i, digit_char in enumerate(number_str):
            self.type_wheels[i] = int(digit_char)
        self.total_operations += 1

        # Engage inking and strike
        self.state = PrinterState.INKING
        self.inking_engaged = True
        self.total_operations += 1

        self.state = PrinterState.STRIKING
        self.hammer_ready = True
        self.total_operations += 1

        # Record printed output
        formatted_line = self._format_output_line(number_str)
        self.printed_lines.append(formatted_line)
        self.platen_position += 1
        self.state = PrinterState.ADVANCING
        self.total_operations += 1

        return formatted_line

    def print_formatted(self, number: int, separator: str = " ") -> str:
        """
        Print with custom formatting and separators.

        Args:
            number: Integer value to print
            separator: Character between digit groups (default: space)

        Returns:
            Formatted string with separators
        """
        number_str = str(number).zfill(self.DIGITS_PER_NUMBER)
        return separator.join(number_str)

    def advance_line(self) -> None:
        """
        Advance platen to next line without printing.

        Used between polynomial evaluations.
        """
        if self.platen_position < self.LINES_PER_PAGE:
            self.printed_lines.append("")  # Blank line
            self.platen_position += 1
            self.total_operations += 1

    def advance_page(self) -> None:
        """
        Advance to next page (eject current page).

        Resets platen position to 0.
        """
        self.platen_position = 0
        self.total_operations += 1

    def print_multiple(self, numbers: list[int]) -> list[str]:
        """
        Print a sequence of numbers.

        Args:
            numbers: List of integers to print

        Returns:
            List of formatted output lines
        """
        lines = []
        for number in numbers:
            line = self.print_number(number)
            lines.append(line)
        return lines

    def get_printed_output(self) -> str:
        """
        Get complete printed output as multi-line string.

        Returns:
            All printed lines joined with newlines
        """
        return "\n".join(self.printed_lines)

    def get_current_page(self) -> list[str]:
        """
        Get lines printed on current page.

        Returns:
            List of lines (up to LINES_PER_PAGE)
        """
        start_idx = (len(self.printed_lines) // self.LINES_PER_PAGE) * self.LINES_PER_PAGE
        return self.printed_lines[start_idx:]

    def get_snapshot(self) -> PrinterSnapshot:
        """
        Get complete printer state snapshot.

        Returns:
            PrinterSnapshot with all mechanical state
        """
        return PrinterSnapshot(
            state=self.state,
            type_wheels=self.type_wheels.copy(),
            inking_engaged=self.inking_engaged,
            hammer_ready=self.hammer_ready,
            platen_position=self.platen_position,
            printed_lines=self.printed_lines.copy(),
            total_operations=self.total_operations,
        )

    def reset(self) -> None:
        """Reset printer to initial state."""
        self.state = PrinterState.IDLE
        self.type_wheels = [0] * self.NUM_COLUMNS
        self.inking_engaged = False
        self.hammer_ready = False
        self.platen_position = 0
        self.printed_lines = []
        self.total_operations = 0

    def _format_output_line(self, number_str: str) -> str:
        """
        Format a printed line with spacing.

        Args:
            number_str: 8-digit string

        Returns:
            Formatted output line with spaces between digits
        """
        # Format as: "d d d d d d d d" (space-separated)
        return " ".join(number_str)


class Stereotyper:
    """
    Mechanical Stereotyper for Difference Engine No. 2

    Creates stereotype plates (molds) from computed results.
    The stereotyper can create multiple molds, each containing
    an 8×50 grid of raised/flat points representing digits.

    Historical Significance:
    - Unique feature of Babbage's design
    - Allowed mass printing of computed results
    - Each mold could be used repeatedly
    - Results could be permanently preserved

    Specifications:
    - Grid size: 8 columns × 50 rows
    - 8 columns match 8-digit output
    - 50 rows allow for multiple results per mold
    - Raised (1) vs. flat (0) represents digit pattern
    """

    MOLD_WIDTH = 8                      # 8 digit columns
    MOLD_HEIGHT = 50                    # 50 rows per mold
    MAX_MOLDS = 100                     # Max molds in storage

    def __init__(self):
        """Initialize stereotyper with empty mold."""
        self.x_position = 0              # Current column (0-7)
        self.y_position = 0              # Current row (0-49)
        self.mold_image: dict[tuple[int, int], bool] = {}  # (x,y) -> raised (True/False)
        self.completed_molds: list[dict[tuple[int, int], bool]] = []
        self.total_operations = 0

    def engrave_digit(self, x: int, digit: int) -> None:
        """
        Engrave a single digit at current position.

        Args:
            x: Column (0-7)
            digit: Digit value (0-9)

        Raises:
            ValueError: If position out of range
        """
        if x < 0 or x >= self.MOLD_WIDTH:
            raise ValueError(f"X position {x} out of range [0,{self.MOLD_WIDTH-1}]")
        if self.y_position >= self.MOLD_HEIGHT:
            raise ValueError(f"Y position {self.y_position} exceeds mold height {self.MOLD_HEIGHT}")
        if digit < 0 or digit > 9:
            raise ValueError(f"Digit {digit} out of range [0,9]")

        # Engrave digit as raised points (True) in mold
        # Simple representation: any digit creates a raised point
        # In practice, digit shape would be encoded
        key = (x, self.y_position)
        self.mold_image[key] = bool(digit % 2)  # Alternating pattern for visualization
        self.total_operations += 1

    def engrave_number(self, number: int) -> None:
        """
        Engrave an 8-digit number at current mold position.

        Args:
            number: Integer to engrave (0 to 99,999,999)

        Raises:
            ValueError: If number too large or mold full
        """
        if number < 0:
            raise ValueError(f"Cannot engrave negative number: {number}")
        if number > 99_999_999:
            raise ValueError(f"Number too large: {number}")
        if self.y_position >= self.MOLD_HEIGHT:
            raise ValueError("Mold full - extract and create new mold")

        number_str = str(number).zfill(8)

        for col, digit_char in enumerate(number_str):
            self.engrave_digit(col, int(digit_char))

        # Advance to next row
        self.y_position += 1
        self.total_operations += 1

    def engrave_multiple(self, numbers: list[int]) -> None:
        """
        Engrave multiple numbers to current mold.

        Args:
            numbers: List of integers to engrave
        """
        for number in numbers:
            if self.y_position >= self.MOLD_HEIGHT:
                self.extract_mold()
            self.engrave_number(number)

    def extract_mold(self) -> dict[tuple[int, int], bool]:
        """
        Extract current mold (move to completed storage).

        Returns:
            Completed mold image dictionary
        """
        if len(self.completed_molds) >= self.MAX_MOLDS:
            raise RuntimeError(f"Mold storage full ({self.MAX_MOLDS} molds)")

        # Save current mold
        mold = self.mold_image.copy()
        self.completed_molds.append(mold)
        self.total_operations += 1

        # Reset for next mold
        self.mold_image = {}
        self.y_position = 0

        return mold

    def get_mold_image(self) -> dict[tuple[int, int], bool]:
        """
        Get current mold image.

        Returns:
            Dictionary of (x,y) -> raised/flat
        """
        return self.mold_image.copy()

    def get_mold_as_grid(self) -> list[list[bool]]:
        """
        Get current mold as 2D grid (for visualization).

        Returns:
            8×50 grid where True = raised, False = flat
        """
        grid = [[False] * self.MOLD_WIDTH for _ in range(self.MOLD_HEIGHT)]

        for (x, y), raised in self.mold_image.items():
            if 0 <= x < self.MOLD_WIDTH and 0 <= y < self.MOLD_HEIGHT:
                grid[y][x] = raised

        return grid

    def get_snapshot(self) -> StereotyperSnapshot:
        """
        Get complete stereotyper state snapshot.

        Returns:
            StereotyperSnapshot with all mechanical state
        """
        return StereotyperSnapshot(
            x_position=self.x_position,
            y_position=self.y_position,
            mold_image=self.mold_image.copy(),
            molds_completed=len(self.completed_molds),
            current_height=self.y_position,
        )

    def reset(self) -> None:
        """Reset stereotyper, clearing current mold but keeping completed molds."""
        self.x_position = 0
        self.y_position = 0
        self.mold_image = {}
        self.total_operations = 0

    def clear_all(self) -> None:
        """Clear all molds (current and completed)."""
        self.mold_image = {}
        self.completed_molds = []
        self.x_position = 0
        self.y_position = 0
        self.total_operations = 0

    def get_completed_mold_count(self) -> int:
        """Get number of completed molds in storage."""
        return len(self.completed_molds)

    def get_mold_fullness(self) -> float:
        """Get percentage of current mold filled (0.0 to 1.0)."""
        return self.y_position / self.MOLD_HEIGHT

    def mold_is_full(self) -> bool:
        """Check if current mold is full."""
        return self.y_position >= self.MOLD_HEIGHT


class PrinterStereotyperSystem:
    """
    Combined Printer and Stereotyper System

    Manages both output mechanisms in coordination.
    Allows simultaneous printing and stereotype creation.
    """

    def __init__(self):
        """Initialize combined system."""
        self.printer = Printer()
        self.stereotyper = Stereotyper()
        self.total_operations = 0

    def output_number(self, number: int, to_printer: bool = True, to_stereotyper: bool = True) -> str:
        """
        Output a number to printer and/or stereotyper.

        Args:
            number: Integer to output
            to_printer: Send to printer
            to_stereotyper: Send to stereotyper

        Returns:
            Formatted output line from printer
        """
        output = ""

        if to_printer:
            output = self.printer.print_number(number)
            self.total_operations += 1

        if to_stereotyper:
            if self.stereotyper.mold_is_full():
                self.stereotyper.extract_mold()
            self.stereotyper.engrave_number(number)
            self.total_operations += 1

        return output

    def output_sequence(self, numbers: list[int]) -> list[str]:
        """
        Output a sequence of numbers.

        Args:
            numbers: List of integers

        Returns:
            List of formatted output lines
        """
        results = []
        for number in numbers:
            output = self.output_number(number)
            results.append(output)
        return results

    def get_snapshot(self) -> dict:
        """
        Get complete system state snapshot.

        Returns:
            Dictionary with both printer and stereotyper snapshots
        """
        return {
            "printer": self.printer.get_snapshot(),
            "stereotyper": self.stereotyper.get_snapshot(),
            "total_operations": self.total_operations,
        }

    def reset(self) -> None:
        """Reset both printer and stereotyper."""
        self.printer.reset()
        self.stereotyper.reset()
        self.total_operations = 0

    def get_printed_output(self) -> str:
        """Get all printed output."""
        return self.printer.get_printed_output()

    def get_mold_count(self) -> int:
        """Get number of completed molds."""
        return self.stereotyper.get_completed_mold_count()
