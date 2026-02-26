"""
Difference Engine No. 2 Column Components

Core mechanical components for digit storage and arithmetic in DE2:
  - DigitColumn: Single column storing 31 decimal digits
  - ColumnBank: Unified state management for 8 columns

Based on SMG Technical Description and Babbage's mechanical notation.

DigitColumn Architecture:
  - 31 digit positions (0-30), each holding 0-9
  - Figure wheel storage (equivalent to Babbage's figure wheel)
  - Carry state management (from previous column)
  - Add difference operation with carry propagation
  - Latch mechanism (open/closed state)
  - Advancing mechanism (row shifting)

References:
  - SMG Technical Description: Charles Babbage's Difference Engine No. 2
  - Babbage Mechanical Notation (1826): Column specifications
  - Working hardware (1991 DE2): Physical implementation verified
"""

from .types import ColumnSnapshot


class DigitColumn:
    """
    Single column of Difference Engine No. 2.

    Stores 31 decimal digits (0-9 each) representing one number in a
    difference table. Handles arithmetic (addition with carry) and
    mechanical state (latching, advancing).

    Digit numbering: [0] is least significant (units), [30] is most significant
    Carry representation: Boolean flags (True = carry, False = no carry)

    Example:
        col = DigitColumn(column_index=0, initial_value=12345)
        col.add_difference([2, 3, 4, 5, ...])  # Add difference row
        value = col.get_value_as_int()         # Extract result
    """

    def __init__(self, column_index: int, initial_value: int = 0):
        """
        Initialize column with index and optional initial value.

        Parameters:
            column_index: Position in array (0-7 for DE2)
            initial_value: Initial numeric value (0 by default)
        """
        self.column_index = column_index
        self.digits: list[int] = [0] * 31
        self.carry_in = False
        self.carry_out = False
        self.is_latched = False
        self.is_advancing = False
        self.phase = "idle"

        # Set initial value if provided
        if initial_value != 0:
            self.set_value_from_int(initial_value)

    def set_value_from_int(self, value: int) -> None:
        """
        Load integer value into column (converts to digit array).

        Parameters:
            value: Integer value (0-9999999999...)
                  Automatically wraps if exceeds 31 digits
        """
        # Ensure value is positive (DE2 uses unsigned)
        value = abs(value)

        # Convert to 31-digit decimal representation
        for i in range(31):
            self.digits[i] = value % 10
            value //= 10

        # If value still has digits beyond 31, truncate (unsigned overflow)
        # This matches Babbage's mechanical behavior

    def get_value_as_int(self) -> int:
        """
        Extract integer value from digit array.

        Returns:
            Integer representation of all 31 digits (most significant first)
        """
        result = 0
        for i in range(30, -1, -1):  # Iterate from high to low
            result = result * 10 + self.digits[i]
        return result

    def add_difference(self, difference_digits: list[int]) -> None:
        """
        Add difference row to column with carry propagation.

        Babbage's addition algorithm:
          1. Add difference digits to current digits
          2. Propagate carries from position 0 to 30
          3. Set carry_out flag if final carry exceeds 31 digits

        Parameters:
            difference_digits: 31-element array of digit differences

        Raises:
            ValueError: If difference_digits length != 31
        """
        if len(difference_digits) != 31:
            raise ValueError(f"Expected 31 digits, got {len(difference_digits)}")

        # Add difference to each position with carry propagation
        carry = 1 if self.carry_in else 0

        for i in range(31):
            total = self.digits[i] + difference_digits[i] + carry
            self.digits[i] = total % 10
            carry = 1 if total >= 10 else 0

        # Set outgoing carry (exceeds 31 digits)
        self.carry_out = carry == 1

    def add_single(self, value: int) -> None:
        """
        Add single value to column (simpler than add_difference).

        Parameters:
            value: Integer to add (can be positive or negative)
        """
        current = self.get_value_as_int()
        new_value = (current + value) % (10**31)  # Wrap at 31 digits
        self.set_value_from_int(new_value)

    def get_digit(self, position: int) -> int:
        """
        Get single digit at position.

        Parameters:
            position: 0-30 (0 = units, 30 = most significant)

        Returns:
            Digit (0-9)
        """
        if not (0 <= position < 31):
            raise IndexError(f"Digit position out of range: {position}")
        return self.digits[position]

    def set_digit(self, position: int, digit: int) -> None:
        """
        Set single digit at position.

        Parameters:
            position: 0-30
            digit: 0-9
        """
        if not (0 <= position < 31):
            raise IndexError(f"Digit position out of range: {position}")
        if not (0 <= digit <= 9):
            raise ValueError(f"Invalid digit: {digit}")
        self.digits[position] = digit

    def reset(self) -> None:
        """Reset column to zero with carry cleared."""
        self.digits = [0] * 31
        self.carry_in = False
        self.carry_out = False

    def set_carry_in(self, carry: bool) -> None:
        """Set incoming carry flag."""
        self.carry_in = carry

    def get_carry_out(self) -> bool:
        """Get outgoing carry flag."""
        return self.carry_out

    def latch(self) -> None:
        """Close latch (mechanical lock)."""
        self.is_latched = True

    def unlatch(self) -> None:
        """Open latch (mechanical unlock)."""
        self.is_latched = False

    def is_locked(self) -> bool:
        """Check if latch is closed."""
        return self.is_latched

    def start_advancing(self) -> None:
        """Begin mechanical advance to next row."""
        self.is_advancing = True

    def stop_advancing(self) -> None:
        """Stop mechanical advance."""
        self.is_advancing = False

    def is_advancing_state(self) -> bool:
        """Check if currently advancing."""
        return self.is_advancing

    def get_snapshot(self) -> ColumnSnapshot:
        """
        Capture complete state for debugging/tracing.

        Returns:
            ColumnSnapshot with all current state
        """
        return ColumnSnapshot(
            column_index=self.column_index,
            digits=self.digits.copy(),
            carry_in=self.carry_in,
            carry_out=self.carry_out,
            is_latched=self.is_latched,
            is_advancing=self.is_advancing,
            phase=self.phase,
        )

    def set_phase(self, phase_name: str) -> None:
        """Set current mechanical phase (for tracing/debugging)."""
        self.phase = phase_name

    def __repr__(self) -> str:
        """String representation for debugging."""
        value = self.get_value_as_int()
        return f"DigitColumn(index={self.column_index}, value={value}, carry_out={self.carry_out})"


class ColumnBank:
    """
    Unified state management for 8 columns of Difference Engine No. 2.

    DE2 has 8 physical columns arranged in a row, each holding 31 digits.
    ColumnBank manages synchronized operations across all columns:
      - Synchronized addition (add_difference_row)
      - Unified state snapshots
      - Carry propagation between columns
      - Global state reset

    The "difference table" is formed by 8 columns of numbers:
      Column 0: Function values (f(x))
      Column 1: First differences (Δf)
      Column 2: Second differences (Δ²f)
      ... and so on

    Example:
        bank = ColumnBank(initial_differences=[1, 2, 2, 0, 0, 0, 0, 0])
        bank.add_difference_row([...row 1...])  # Synchronized add across columns
        values = bank.get_all_values()          # Extract all 8 values
    """

    def __init__(self, initial_differences: list[int] | None = None):
        """
        Initialize column bank with 8 columns.

        Parameters:
            initial_differences: Optional list of 8 initial values for columns
                                (used for difference table setup)
        """
        self.columns = [DigitColumn(i) for i in range(8)]

        if initial_differences:
            if len(initial_differences) != 8:
                raise ValueError(f"Expected 8 initial differences, got {len(initial_differences)}")
            for i, value in enumerate(initial_differences):
                self.columns[i].set_value_from_int(value)

    def add_difference_row(self, difference_rows: list[list[int]]) -> None:
        """
        Add synchronized difference row across all 8 columns.

        This is the core operation of DE2: each column adds its corresponding
        difference row simultaneously, with carries propagating left-to-right.

        Parameters:
            difference_rows: List of 8 lists (31 digits each)
                            difference_rows[i] is 31 digits for column i

        Raises:
            ValueError: If incorrect number/size of difference rows
        """
        if len(difference_rows) != 8:
            raise ValueError(f"Expected 8 difference rows, got {len(difference_rows)}")

        for i, col in enumerate(self.columns):
            if len(difference_rows[i]) != 31:
                raise ValueError(f"Row {i}: expected 31 digits, got {len(difference_rows[i])}")

        # Add all rows, propagating carries left-to-right (columns 0→7)
        for i, col in enumerate(self.columns):
            col.add_difference(difference_rows[i])
            # Carry propagates to next column
            if i < 7:
                if col.get_carry_out():
                    self.columns[i + 1].set_carry_in(True)

    def get_all_values(self) -> list[int]:
        """
        Extract integer values from all 8 columns.

        Returns:
            List of 8 integers (one per column)
        """
        return [col.get_value_as_int() for col in self.columns]

    def set_all_values(self, values: list[int]) -> None:
        """
        Set all 8 columns to given values.

        Parameters:
            values: List of 8 integers
        """
        if len(values) != 8:
            raise ValueError(f"Expected 8 values, got {len(values)}")
        for i, col in enumerate(self.columns):
            col.set_value_from_int(values[i])

    def get_column(self, index: int) -> DigitColumn:
        """
        Get specific column by index.

        Parameters:
            index: 0-7

        Returns:
            DigitColumn instance
        """
        if not (0 <= index < 8):
            raise IndexError(f"Column index out of range: {index}")
        return self.columns[index]

    def reset_all(self) -> None:
        """Reset all columns to zero."""
        for col in self.columns:
            col.reset()

    def state_snapshot(self) -> list[ColumnSnapshot]:
        """
        Capture state of all 8 columns for debugging.

        Returns:
            List of 8 ColumnSnapshot objects
        """
        return [col.get_snapshot() for col in self.columns]

    def set_all_phases(self, phase_name: str) -> None:
        """Set mechanical phase name for all columns (tracing)."""
        for col in self.columns:
            col.set_phase(phase_name)

    def latch_all(self) -> None:
        """Close latches on all columns."""
        for col in self.columns:
            col.latch()

    def unlatch_all(self) -> None:
        """Open latches on all columns."""
        for col in self.columns:
            col.unlatch()

    def are_all_latched(self) -> bool:
        """Check if all columns are latched."""
        return all(col.is_locked() for col in self.columns)

    def __repr__(self) -> str:
        """String representation showing all column values."""
        values = self.get_all_values()
        return f"ColumnBank(values={values})"
