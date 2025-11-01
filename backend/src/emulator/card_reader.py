"""
CardReader: Punch Card Input System for Difference Engine No. 2

Implements the punch card reading mechanism that loads polynomial coefficients,
variable ranges, and control instructions into the Analytical Engine.

Historical Context:
  - Ada Lovelace famously prepared punch cards for the Difference Engine
  - Cards specified polynomial coefficients and x-value ranges
  - Each card could hold a single operation or data value
  - Babbage's notation and card format inspired by Jacquard looms

Card Format:
  - Each card has 140 columns (Jacquard standard)
  - Each column can have holes in up to 80 positions (rows)
  - Holes represent binary data; absence means 0
  - Specific column ranges encode different data types:
    * Columns 1-20: Operation code and parameters
    * Columns 21-80: Coefficient value (50-digit BabbageNumber)
    * Columns 81-100: Variable x-value
    * Columns 101-140: Control flags and checksums

Data Encoding:
  - Coefficients: 50-digit decimal numbers (BabbageNumber fixed-point)
  - X-values: Integer range (start, end)
  - Operations: LOAD_COEFF, SET_RANGE, PRINT_RESULT, RESET, etc.

References:
  - Lovelace, Ada. "Notes on the Analytical Engine" (1843)
  - Menabrea, Luigi Federico. "Sketch of the Analytical Engine" (1842)
  - Swade, Doron. "The Cogwheel Brain" (2001)
"""

from typing import List, Optional, Dict, Tuple
from dataclasses import dataclass, field
from enum import Enum


class CardOperation(Enum):
    """Operation codes for punch cards."""

    LOAD_COEFF = "LOAD_COEFF"          # Load polynomial coefficient
    SET_X_RANGE = "SET_X_RANGE"        # Set x-value range (start, end)
    PRINT_RESULT = "PRINT_RESULT"      # Print current result
    RESET_ENGINE = "RESET_ENGINE"      # Reset all registers
    HALT = "HALT"                      # Stop execution


@dataclass
class PunchCard:
    """Represents a single punch card with data and metadata."""

    card_id: int                        # Card number in sequence
    operation: CardOperation            # Operation to perform
    coefficient: Optional[int] = None   # Polynomial coefficient (as decimal)
    x_start: Optional[int] = None       # X-range start
    x_end: Optional[int] = None         # X-range end
    holes: List[List[bool]] = field(default_factory=list)  # Physical hole matrix (140 cols × 80 rows)
    is_valid: bool = True               # Card format validity
    error_message: str = ""             # Validation error if any


@dataclass
class CardSequence:
    """A sequence of punch cards forming a complete program."""

    cards: List[PunchCard] = field(default_factory=list)
    total_cards: int = 0
    is_complete: bool = False
    checksum: Optional[str] = None


class CardFormatError(Exception):
    """Exception raised for invalid punch card format."""

    pass


class CardReader:
    """
    Punch Card Reader for Difference Engine No. 2

    Reads punch cards containing:
    - Polynomial coefficients
    - Variable ranges (x-values)
    - Control instructions
    - Data validation checksums

    Supports historical Jacquard-style format and modern variants.
    """

    # Card format constants
    CARD_WIDTH = 140                  # Jacquard standard: 140 columns
    CARD_HEIGHT = 80                  # 80 rows per column
    OP_COLUMNS = (1, 20)              # Operation code columns
    COEFF_COLUMNS = (21, 80)          # Coefficient columns (50 digits × 8 bits)
    X_START_COLUMNS = (81, 90)        # X-range start
    X_END_COLUMNS = (91, 100)         # X-range end
    CHECKSUM_COLUMNS = (101, 140)     # Checksum and control flags

    # Coefficient representation: 50 digits in decimal, each stored in 4 bits
    # So 50 digits × 4 bits = 200 bits = 25 bytes = 25 columns with 8 rows each
    COEFF_DIGITS = 50
    BITS_PER_DIGIT = 4

    def __init__(self):
        """Initialize CardReader."""
        self.current_card_id = 0
        self.cards_read: List[PunchCard] = []
        self.error_log: List[str] = []

    def read_card(self, holes: List[List[bool]]) -> PunchCard:
        """
        Read a physical punch card (hole matrix) and parse its data.

        Args:
            holes: 140 × 80 boolean matrix (True = hole, False = no hole)

        Returns:
            Parsed PunchCard with operation and data extracted

        Raises:
            CardFormatError: If card format is invalid
        """
        if len(holes) != self.CARD_WIDTH:
            raise CardFormatError(
                f"Card width must be {self.CARD_WIDTH}, got {len(holes)}"
            )

        for col_idx, col in enumerate(holes):
            if len(col) != self.CARD_HEIGHT:
                raise CardFormatError(
                    f"Column {col_idx} has {len(col)} rows, expected {self.CARD_HEIGHT}"
                )

        card_id = len(self.cards_read) + 1

        # Extract operation code
        operation = self._decode_operation(holes)

        # Extract data based on operation
        coefficient = None
        x_start = None
        x_end = None

        if operation == CardOperation.LOAD_COEFF:
            coefficient = self._decode_coefficient(holes)

        elif operation == CardOperation.SET_X_RANGE:
            x_start, x_end = self._decode_x_range(holes)

        # Validate checksum
        is_valid, error_msg = self._validate_checksum(holes)

        card = PunchCard(
            card_id=card_id,
            operation=operation,
            coefficient=coefficient,
            x_start=x_start,
            x_end=x_end,
            holes=holes,
            is_valid=is_valid,
            error_message=error_msg,
        )

        self.cards_read.append(card)
        return card

    def create_card_from_data(
        self,
        operation: CardOperation,
        coefficient: Optional[int] = None,
        x_start: Optional[int] = None,
        x_end: Optional[int] = None,
    ) -> PunchCard:
        """
        Create a punch card from operation and data (programmatic, not physical).

        Args:
            operation: CardOperation to encode
            coefficient: Coefficient value (0-50 decimal digits)
            x_start: Start of x-range
            x_end: End of x-range

        Returns:
            PunchCard with holes matrix encoded
        """
        card_id = len(self.cards_read) + 1

        # Validate data based on operation
        if operation == CardOperation.LOAD_COEFF:
            if coefficient is None:
                raise ValueError("LOAD_COEFF requires coefficient")
            if coefficient < 0:
                raise ValueError("Coefficient must be non-negative")
            # Max 50 decimal digits: 10^50 - 1
            if coefficient > 10**50 - 1:
                raise ValueError(f"Coefficient exceeds 50-digit limit: {coefficient}")

        elif operation == CardOperation.SET_X_RANGE:
            if x_start is None or x_end is None:
                raise ValueError("SET_X_RANGE requires x_start and x_end")
            if x_start > x_end:
                raise ValueError(f"Invalid range: x_start ({x_start}) > x_end ({x_end})")
            if x_start < -1000 or x_end > 1000:
                raise ValueError("X-range must be within -1000 to 1000")

        # Encode data into holes matrix
        holes = [[False] * self.CARD_HEIGHT for _ in range(self.CARD_WIDTH)]

        # Encode operation code
        self._encode_operation(holes, operation)

        # Encode data
        if operation == CardOperation.LOAD_COEFF and coefficient is not None:
            self._encode_coefficient(holes, coefficient)

        elif operation == CardOperation.SET_X_RANGE and x_start is not None and x_end is not None:
            self._encode_x_range(holes, x_start, x_end)

        # Compute and encode checksum
        self._encode_checksum(holes)

        card = PunchCard(
            card_id=card_id,
            operation=operation,
            coefficient=coefficient,
            x_start=x_start,
            x_end=x_end,
            holes=holes,
            is_valid=True,
        )

        self.cards_read.append(card)
        return card

    def _decode_operation(self, holes: List[List[bool]]) -> CardOperation:
        """
        Decode operation code from columns 1-20.

        Operation encoding (columns 1-3, binary):
          000 = LOAD_COEFF
          001 = SET_X_RANGE
          010 = PRINT_RESULT
          011 = RESET_ENGINE
          100 = HALT
        """
        col1, col2, col3 = holes[0], holes[1], holes[2]

        # Read bits 0-2 (binary encoding)
        bit0 = col1[0]  # LSB
        bit1 = col2[0]
        bit2 = col3[0]  # MSB

        code = int(bit2) * 4 + int(bit1) * 2 + int(bit0)

        operations = {
            0: CardOperation.LOAD_COEFF,
            1: CardOperation.SET_X_RANGE,
            2: CardOperation.PRINT_RESULT,
            3: CardOperation.RESET_ENGINE,
            4: CardOperation.HALT,
        }

        if code not in operations:
            raise CardFormatError(f"Invalid operation code: {code}")

        return operations[code]

    def _encode_operation(self, holes: List[List[bool]], op: CardOperation) -> None:
        """
        Encode operation code into columns 1-20.

        See _decode_operation for encoding scheme.
        """
        code_map = {
            CardOperation.LOAD_COEFF: 0,
            CardOperation.SET_X_RANGE: 1,
            CardOperation.PRINT_RESULT: 2,
            CardOperation.RESET_ENGINE: 3,
            CardOperation.HALT: 4,
        }

        code = code_map[op]

        # Extract bits (binary)
        bit0 = bool(code & 1)
        bit1 = bool(code & 2)
        bit2 = bool(code & 4)

        # Punch holes in columns 1-3, rows 0-2
        holes[0][0] = bit0  # Column 1, Row 0 (LSB)
        holes[1][0] = bit1  # Column 2, Row 0
        holes[2][0] = bit2  # Column 3, Row 0 (MSB)

    def _decode_coefficient(self, holes: List[List[bool]]) -> int:
        """
        Decode a 50-digit decimal coefficient from columns 21-80.

        Encoding: 50 decimal digits, each in 4 bits (BCD).
        Digit i stored in columns (20 + i*1) to (20 + i*1).
        Each digit: 4 bits representing 0-9.
        """
        digits = []

        for digit_idx in range(self.COEFF_DIGITS):
            col_idx = 20 + digit_idx  # Column index (0-based)
            if col_idx >= self.CARD_WIDTH:
                break

            col = holes[col_idx]

            # Read 4 bits from rows 0-3 (BCD encoding)
            bit0 = col[0]  # LSB
            bit1 = col[1]
            bit2 = col[2]
            bit3 = col[3]  # MSB

            digit_value = int(bit3) * 8 + int(bit2) * 4 + int(bit1) * 2 + int(bit0)

            if digit_value > 9:
                raise CardFormatError(
                    f"Invalid BCD digit in position {digit_idx}: {digit_value}"
                )

            digits.append(str(digit_value))

        # Convert digit array to integer (most significant digit first)
        if not digits:
            return 0

        coeff_str = "".join(digits)
        return int(coeff_str) if coeff_str else 0

    def _encode_coefficient(self, holes: List[List[bool]], coeff: int) -> None:
        """
        Encode a 50-digit decimal coefficient into columns 21-80.

        See _decode_coefficient for encoding scheme.
        """
        coeff_str = str(coeff).zfill(self.COEFF_DIGITS)[-self.COEFF_DIGITS :]

        for digit_idx, digit_char in enumerate(coeff_str):
            col_idx = 20 + digit_idx
            if col_idx >= self.CARD_WIDTH:
                break

            digit_value = int(digit_char)

            # Encode digit as 4 bits (BCD)
            bit0 = bool(digit_value & 1)
            bit1 = bool(digit_value & 2)
            bit2 = bool(digit_value & 4)
            bit3 = bool(digit_value & 8)

            col = holes[col_idx]
            col[0] = bit0  # LSB
            col[1] = bit1
            col[2] = bit2
            col[3] = bit3  # MSB

    def _decode_x_range(self, holes: List[List[bool]]) -> Tuple[int, int]:
        """
        Decode x-range (start, end) from columns 81-100.

        Format:
          Columns 81-90: x_start (10 decimal digits, signed)
          Columns 91-100: x_end (10 decimal digits, signed)

        Sign encoding: Bit 9 (row 9) = sign (0 = positive, 1 = negative)
        """
        # Decode x_start (columns 81-90)
        x_start_digits = []
        x_start_sign = False

        for digit_idx in range(10):
            col_idx = 80 + digit_idx  # Columns 81-90 (0-based indexing)
            col = holes[col_idx]

            # Read sign bit (row 9)
            if digit_idx == 0:
                x_start_sign = col[9]

            # Read 4 bits for digit (rows 0-3)
            bit0 = col[0]
            bit1 = col[1]
            bit2 = col[2]
            bit3 = col[3]

            digit_value = int(bit3) * 8 + int(bit2) * 4 + int(bit1) * 2 + int(bit0)
            x_start_digits.append(str(digit_value))

        x_start_str = "".join(x_start_digits)
        x_start = int(x_start_str) if x_start_str else 0
        if x_start_sign:
            x_start = -x_start

        # Decode x_end (columns 91-100)
        x_end_digits = []
        x_end_sign = False

        for digit_idx in range(10):
            col_idx = 90 + digit_idx  # Columns 91-100 (0-based indexing)
            col = holes[col_idx]

            # Read sign bit (row 9)
            if digit_idx == 0:
                x_end_sign = col[9]

            # Read 4 bits for digit (rows 0-3)
            bit0 = col[0]
            bit1 = col[1]
            bit2 = col[2]
            bit3 = col[3]

            digit_value = int(bit3) * 8 + int(bit2) * 4 + int(bit1) * 2 + int(bit0)
            x_end_digits.append(str(digit_value))

        x_end_str = "".join(x_end_digits)
        x_end = int(x_end_str) if x_end_str else 0
        if x_end_sign:
            x_end = -x_end

        return x_start, x_end

    def _encode_x_range(self, holes: List[List[bool]], x_start: int, x_end: int) -> None:
        """
        Encode x-range into columns 81-100.

        See _decode_x_range for encoding scheme.
        """
        # Encode x_start (columns 81-90)
        x_start_abs = abs(x_start)
        x_start_sign = x_start < 0
        x_start_str = str(x_start_abs).zfill(10)[-10:]

        for digit_idx, digit_char in enumerate(x_start_str):
            col_idx = 80 + digit_idx
            col = holes[col_idx]

            digit_value = int(digit_char)

            # Encode digit as 4 bits (BCD)
            bit0 = bool(digit_value & 1)
            bit1 = bool(digit_value & 2)
            bit2 = bool(digit_value & 4)
            bit3 = bool(digit_value & 8)

            col[0] = bit0
            col[1] = bit1
            col[2] = bit2
            col[3] = bit3

            # Set sign bit in row 9 (only for first digit)
            if digit_idx == 0:
                col[9] = x_start_sign

        # Encode x_end (columns 91-100)
        x_end_abs = abs(x_end)
        x_end_sign = x_end < 0
        x_end_str = str(x_end_abs).zfill(10)[-10:]

        for digit_idx, digit_char in enumerate(x_end_str):
            col_idx = 90 + digit_idx
            col = holes[col_idx]

            digit_value = int(digit_char)

            # Encode digit as 4 bits (BCD)
            bit0 = bool(digit_value & 1)
            bit1 = bool(digit_value & 2)
            bit2 = bool(digit_value & 4)
            bit3 = bool(digit_value & 8)

            col[0] = bit0
            col[1] = bit1
            col[2] = bit2
            col[3] = bit3

            # Set sign bit in row 9 (only for first digit)
            if digit_idx == 0:
                col[9] = x_end_sign

    def _validate_checksum(self, holes: List[List[bool]]) -> Tuple[bool, str]:
        """
        Validate card checksum (columns 101-140).

        Simple checksum: XOR of all data bits (columns 1-100).
        Result stored in columns 101-108 (8 bits).
        """
        # Compute XOR of all data bits
        xor_result = False

        for col_idx in range(100):  # Columns 1-100 (0-based: 0-99)
            col = holes[col_idx]
            for row_idx in range(8):  # First 8 rows contain data
                xor_result = xor_result != col[row_idx]  # XOR operation

        # Read stored checksum (columns 101-108, rows 0-7)
        stored_checksum = 0
        for offset in range(8):
            col_idx = 100 + offset  # Columns 101-108 (0-based)
            bit = holes[col_idx][offset]
            stored_checksum |= int(bit) << offset

        computed_checksum = int(xor_result)

        if stored_checksum != computed_checksum:
            return (
                False,
                f"Checksum mismatch: computed {computed_checksum}, got {stored_checksum}",
            )

        return True, ""

    def _encode_checksum(self, holes: List[List[bool]]) -> None:
        """
        Compute and encode checksum into columns 101-140.

        See _validate_checksum for scheme.
        """
        # Compute XOR of all data bits
        xor_result = False

        for col_idx in range(100):  # Columns 1-100
            col = holes[col_idx]
            for row_idx in range(8):  # First 8 rows contain data
                xor_result = xor_result != col[row_idx]

        # Store in columns 101-108
        checksum = int(xor_result)

        for offset in range(8):
            col_idx = 100 + offset
            bit = bool(checksum & (1 << offset))
            holes[col_idx][offset] = bit

        # Remaining columns (109-140) set to False as padding
        for col_idx in range(108, self.CARD_WIDTH):
            for row_idx in range(self.CARD_HEIGHT):
                holes[col_idx][row_idx] = False

    def read_card_sequence(self, card_list: List[List[List[bool]]]) -> CardSequence:
        """
        Read a sequence of punch cards (a complete program).

        Args:
            card_list: List of punch card hole matrices

        Returns:
            CardSequence with all cards parsed and validated
        """
        sequence = CardSequence(total_cards=len(card_list))

        for holes in card_list:
            try:
                card = self.read_card(holes)
                sequence.cards.append(card)
            except CardFormatError as e:
                self.error_log.append(str(e))

        sequence.is_complete = len(sequence.cards) == sequence.total_cards
        return sequence

    def get_error_log(self) -> List[str]:
        """Return list of all errors encountered during card reading."""
        return self.error_log.copy()

    def clear_error_log(self) -> None:
        """Clear error log."""
        self.error_log.clear()
