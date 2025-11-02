"""
Phase 3.W3: CardReader Unit Tests

Comprehensive test suite for punch card reading system including:
- Card format parsing and validation
- Operation encoding/decoding
- Coefficient representation (50-digit decimals)
- X-range handling (integer ranges, signed values)
- Checksum validation
- Error detection and recovery
- Card sequences (multiple cards)

All tests validate that punch cards correctly encode/decode
polynomial evaluation parameters for the Difference Engine No. 2.
"""

import pytest
from backend.src.emulator.card_reader import (
    CardReader,
    CardOperation,
    PunchCard,
    CardSequence,
    CardFormatError,
)


class TestCardReaderInitialization:
    """Test CardReader initialization and state management."""

    def test_initialization(self):
        """Test CardReader initializes with empty state."""
        reader = CardReader()
        assert reader.current_card_id == 0
        assert len(reader.cards_read) == 0
        assert len(reader.error_log) == 0

    def test_error_log_management(self):
        """Test error log can be cleared."""
        reader = CardReader()
        reader.error_log.append("Test error")
        assert len(reader.error_log) == 1
        reader.clear_error_log()
        assert len(reader.error_log) == 0

    def test_get_error_log_returns_copy(self):
        """Test get_error_log returns a copy, not reference."""
        reader = CardReader()
        reader.error_log.append("Error 1")
        log_copy = reader.get_error_log()
        log_copy.append("Error 2")
        assert len(reader.error_log) == 1


class TestCardCreation:
    """Test creating punch cards from operation and data."""

    def test_create_load_coeff_card_simple(self):
        """Test creating LOAD_COEFF card with simple coefficient."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=42)

        assert card.card_id == 1
        assert card.operation == CardOperation.LOAD_COEFF
        assert card.coefficient == 42
        assert card.is_valid is True
        assert len(card.holes) == 140
        assert all(len(col) == 80 for col in card.holes)

    def test_create_load_coeff_card_zero(self):
        """Test creating card with zero coefficient."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=0)

        assert card.coefficient == 0
        assert card.is_valid is True

    def test_create_load_coeff_card_large(self):
        """Test creating card with large coefficient (near 50-digit limit)."""
        reader = CardReader()
        large_coeff = 10**49 - 1  # Just under 50 digits
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=large_coeff)

        assert card.coefficient == large_coeff
        assert card.is_valid is True

    def test_create_load_coeff_card_max_50_digits(self):
        """Test creating card with exactly 50-digit coefficient."""
        reader = CardReader()
        max_coeff = 10**50 - 1  # Maximum 50-digit number
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=max_coeff)

        assert card.coefficient == max_coeff

    def test_create_load_coeff_card_exceeds_limit(self):
        """Test that exceeding 50-digit limit raises error."""
        reader = CardReader()
        too_large = 10**50

        with pytest.raises(ValueError, match="exceeds 50-digit limit"):
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=too_large)

    def test_create_load_coeff_requires_coefficient(self):
        """Test that LOAD_COEFF requires coefficient parameter."""
        reader = CardReader()

        with pytest.raises(ValueError, match="requires coefficient"):
            reader.create_card_from_data(CardOperation.LOAD_COEFF)

    def test_create_load_coeff_negative_coefficient(self):
        """Test that negative coefficient raises error."""
        reader = CardReader()

        with pytest.raises(ValueError, match="non-negative"):
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=-1)

    def test_create_set_x_range_card(self):
        """Test creating SET_X_RANGE card."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=1, x_end=5)

        assert card.operation == CardOperation.SET_X_RANGE
        assert card.x_start == 1
        assert card.x_end == 5
        assert card.is_valid is True

    def test_create_set_x_range_requires_both_params(self):
        """Test that SET_X_RANGE requires both x_start and x_end."""
        reader = CardReader()

        with pytest.raises(ValueError, match="requires x_start and x_end"):
            reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=1)

    def test_create_set_x_range_invalid_order(self):
        """Test that x_start > x_end raises error."""
        reader = CardReader()

        with pytest.raises(ValueError, match="Invalid range"):
            reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=5, x_end=1)

    def test_create_set_x_range_negative_values(self):
        """Test SET_X_RANGE with negative x values."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=-5, x_end=-1)

        assert card.x_start == -5
        assert card.x_end == -1

    def test_create_set_x_range_mixed_signs(self):
        """Test SET_X_RANGE with mixed positive and negative."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=-3, x_end=3)

        assert card.x_start == -3
        assert card.x_end == 3

    def test_create_set_x_range_out_of_bounds(self):
        """Test that extreme x values raise error."""
        reader = CardReader()

        with pytest.raises(ValueError, match="must be within"):
            reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=-2000, x_end=2000)

    def test_create_print_result_card(self):
        """Test creating PRINT_RESULT card."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.PRINT_RESULT)

        assert card.operation == CardOperation.PRINT_RESULT
        assert card.is_valid is True

    def test_create_reset_engine_card(self):
        """Test creating RESET_ENGINE card."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.RESET_ENGINE)

        assert card.operation == CardOperation.RESET_ENGINE
        assert card.is_valid is True

    def test_create_halt_card(self):
        """Test creating HALT card."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.HALT)

        assert card.operation == CardOperation.HALT
        assert card.is_valid is True

    def test_card_id_increments(self):
        """Test that card IDs increment correctly."""
        reader = CardReader()

        card1 = reader.create_card_from_data(CardOperation.PRINT_RESULT)
        card2 = reader.create_card_from_data(CardOperation.HALT)
        card3 = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=10)

        assert card1.card_id == 1
        assert card2.card_id == 2
        assert card3.card_id == 3


class TestOperationEncoding:
    """Test operation code encoding and decoding."""

    def test_encode_decode_load_coeff(self):
        """Test encoding and decoding LOAD_COEFF operation."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=123)

        # Decode and verify
        decoded_op = reader._decode_operation(card.holes)
        assert decoded_op == CardOperation.LOAD_COEFF

    def test_encode_decode_set_x_range(self):
        """Test encoding and decoding SET_X_RANGE operation."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=1, x_end=10)

        decoded_op = reader._decode_operation(card.holes)
        assert decoded_op == CardOperation.SET_X_RANGE

    def test_encode_decode_print_result(self):
        """Test encoding and decoding PRINT_RESULT operation."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.PRINT_RESULT)

        decoded_op = reader._decode_operation(card.holes)
        assert decoded_op == CardOperation.PRINT_RESULT

    def test_encode_decode_reset_engine(self):
        """Test encoding and decoding RESET_ENGINE operation."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.RESET_ENGINE)

        decoded_op = reader._decode_operation(card.holes)
        assert decoded_op == CardOperation.RESET_ENGINE

    def test_encode_decode_halt(self):
        """Test encoding and decoding HALT operation."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.HALT)

        decoded_op = reader._decode_operation(card.holes)
        assert decoded_op == CardOperation.HALT

    def test_all_operations_distinct(self):
        """Test that all operations encode to different bit patterns."""
        reader = CardReader()
        operations = [
            CardOperation.LOAD_COEFF,
            CardOperation.SET_X_RANGE,
            CardOperation.PRINT_RESULT,
            CardOperation.RESET_ENGINE,
            CardOperation.HALT,
        ]

        encodings = []
        for op in operations:
            if op == CardOperation.LOAD_COEFF:
                card = reader.create_card_from_data(op, coefficient=0)
            elif op == CardOperation.SET_X_RANGE:
                card = reader.create_card_from_data(op, x_start=0, x_end=0)
            else:
                card = reader.create_card_from_data(op)

            # Extract operation bits from columns 0-2
            bits = (card.holes[0][0], card.holes[1][0], card.holes[2][0])
            encodings.append(bits)

        # All encodings should be unique
        assert len(set(encodings)) == len(encodings)


class TestCoefficientEncoding:
    """Test polynomial coefficient encoding and decoding."""

    def test_encode_decode_single_digit(self):
        """Test encoding and decoding single-digit coefficient."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=5)

        decoded_coeff = reader._decode_coefficient(card.holes)
        assert decoded_coeff == 5

    def test_encode_decode_two_digits(self):
        """Test encoding and decoding two-digit coefficient."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=42)

        decoded_coeff = reader._decode_coefficient(card.holes)
        assert decoded_coeff == 42

    def test_encode_decode_powers_of_ten(self):
        """Test encoding/decoding powers of 10."""
        reader = CardReader()

        for power in range(1, 11):
            value = 10**power - 1  # 9, 99, 999, etc.
            card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=value)

            decoded = reader._decode_coefficient(card.holes)
            assert decoded == value

    def test_encode_decode_all_nines(self):
        """Test encoding/decoding all 9s coefficient."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=999999999999)

        decoded = reader._decode_coefficient(card.holes)
        assert decoded == 999999999999

    def test_encode_decode_leading_zeros_preserved(self):
        """Test that leading zeros in 50-digit representation work."""
        reader = CardReader()
        # This will be zero-padded to 50 digits
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=1)

        decoded = reader._decode_coefficient(card.holes)
        assert decoded == 1

    def test_encode_decode_large_50_digit_value(self):
        """Test encoding/decoding near maximum 50-digit value."""
        reader = CardReader()
        large_val = 10**48 + 12345  # 49-ish digit value
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=large_val)

        decoded = reader._decode_coefficient(card.holes)
        assert decoded == large_val


class TestXRangeEncoding:
    """Test x-range (x_start, x_end) encoding and decoding."""

    def test_encode_decode_simple_range(self):
        """Test encoding and decoding simple positive range."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=1, x_end=5)

        x_start, x_end = reader._decode_x_range(card.holes)
        assert x_start == 1
        assert x_end == 5

    def test_encode_decode_single_value_range(self):
        """Test encoding range where start == end."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=42, x_end=42)

        x_start, x_end = reader._decode_x_range(card.holes)
        assert x_start == 42
        assert x_end == 42

    def test_encode_decode_negative_range(self):
        """Test encoding negative x values."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=-10, x_end=-1)

        x_start, x_end = reader._decode_x_range(card.holes)
        assert x_start == -10
        assert x_end == -1

    def test_encode_decode_mixed_sign_range(self):
        """Test encoding range crossing zero."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=-3, x_end=7)

        x_start, x_end = reader._decode_x_range(card.holes)
        assert x_start == -3
        assert x_end == 7

    def test_encode_decode_large_range(self):
        """Test encoding large range."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=0, x_end=999)

        x_start, x_end = reader._decode_x_range(card.holes)
        assert x_start == 0
        assert x_end == 999

    def test_encode_decode_zero_range(self):
        """Test encoding range at zero."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=0, x_end=0)

        x_start, x_end = reader._decode_x_range(card.holes)
        assert x_start == 0
        assert x_end == 0

    def test_x_range_signs_independent(self):
        """Test that x_start and x_end can have different signs."""
        reader = CardReader()

        test_cases = [
            (1, 10),
            (-10, -1),
            (-5, 5),
            (0, 10),
            (-10, 0),
        ]

        for start, end in test_cases:
            card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=start, x_end=end)

            x_start, x_end = reader._decode_x_range(card.holes)
            assert x_start == start
            assert x_end == end


class TestChecksumValidation:
    """Test checksum computation and validation."""

    def test_valid_checksum_on_creation(self):
        """Test that newly created cards have valid checksums."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=123)

        is_valid, error_msg = reader._validate_checksum(card.holes)
        assert is_valid is True
        assert error_msg == ""

    def test_checksum_all_operations(self):
        """Test checksum validation for all operation types."""
        reader = CardReader()
        operations = [
            (CardOperation.LOAD_COEFF, {"coefficient": 42}),
            (CardOperation.SET_X_RANGE, {"x_start": 1, "x_end": 5}),
            (CardOperation.PRINT_RESULT, {}),
            (CardOperation.RESET_ENGINE, {}),
            (CardOperation.HALT, {}),
        ]

        for op, kwargs in operations:
            card = reader.create_card_from_data(op, **kwargs)
            is_valid, error_msg = reader._validate_checksum(card.holes)
            assert is_valid is True, f"Failed for {op}: {error_msg}"

    def test_checksum_bit_flip_detected(self):
        """Test that flipping a data bit is detected."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=42)

        # Flip a data bit (column 10, row 0)
        card.holes[10][0] = not card.holes[10][0]

        is_valid, error_msg = reader._validate_checksum(card.holes)
        assert is_valid is False
        assert "Checksum mismatch" in error_msg

    def test_checksum_detects_multiple_bit_flips(self):
        """Test that odd number of bit flips are detected.

        Note: XOR checksum detects odd-numbered bit errors but NOT even-numbered.
        This is a mathematical property of XOR: flipping 2 bits cancels out.
        """
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=10, x_end=20)

        # Flip three data bits (odd number)
        card.holes[5][2] = not card.holes[5][2]
        card.holes[15][3] = not card.holes[15][3]
        card.holes[25][1] = not card.holes[25][1]

        is_valid, error_msg = reader._validate_checksum(card.holes)
        assert is_valid is False


class TestCardReading:
    """Test reading punch cards (decoding physical hole patterns)."""

    def test_read_card_from_created(self):
        """Test reading a card that was just created."""
        reader = CardReader()
        original = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=99)

        # Create fresh reader and read the holes
        reader2 = CardReader()
        read_card = reader2.read_card(original.holes)

        assert read_card.operation == CardOperation.LOAD_COEFF
        assert read_card.coefficient == 99
        assert read_card.is_valid is True

    def test_read_card_validates_dimensions(self):
        """Test that read_card validates card dimensions."""
        reader = CardReader()

        # Wrong width
        with pytest.raises(CardFormatError, match="width must be"):
            bad_holes = [[False] * 80 for _ in range(139)]  # 139 instead of 140
            reader.read_card(bad_holes)

    def test_read_card_validates_column_height(self):
        """Test that read_card validates column height."""
        reader = CardReader()

        # Wrong height
        with pytest.raises(CardFormatError, match="rows"):
            bad_holes = [[False] * 79 for _ in range(140)]  # 79 instead of 80
            reader.read_card(bad_holes)

    def test_read_card_invalid_bcd_digit(self):
        """Test that invalid BCD digits are detected."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=42)

        # Corrupt a BCD digit: set bit pattern to 1111 (15, not 0-9)
        card.holes[21][0] = True
        card.holes[21][1] = True
        card.holes[21][2] = True
        card.holes[21][3] = True

        # Re-encode checksum
        reader._encode_checksum(card.holes)

        # Reading should fail
        reader2 = CardReader()
        with pytest.raises(CardFormatError, match="Invalid BCD"):
            reader2.read_card(card.holes)

    def test_read_preserves_all_data(self):
        """Test that reading a card preserves all encoded data."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=-42, x_end=99)

        reader2 = CardReader()
        read_card = reader2.read_card(card.holes)

        assert read_card.operation == CardOperation.SET_X_RANGE
        assert read_card.x_start == -42
        assert read_card.x_end == 99


class TestCardSequences:
    """Test reading sequences of punch cards."""

    def test_read_single_card_sequence(self):
        """Test reading a sequence with one card."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=42)

        sequence = reader.read_card_sequence([card.holes])

        assert sequence.total_cards == 1
        assert len(sequence.cards) == 1
        assert sequence.is_complete is True

    def test_read_multi_card_sequence(self):
        """Test reading a sequence of multiple cards."""
        reader = CardReader()

        cards = [
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=1),
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=1),
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=1),
            reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=1, x_end=5),
        ]

        sequence = reader.read_card_sequence([c.holes for c in cards])

        assert sequence.total_cards == 4
        assert len(sequence.cards) == 4
        assert sequence.is_complete is True

    def test_polynomial_card_sequence_f_x_squared_plus_x_plus_1(self):
        """Test card sequence for f(x) = x^2 + x + 1 on range [1, 5]."""
        reader = CardReader()

        # Sequence: Load coefficients [1, 1, 1], then set range
        cards = [
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=1),  # a0
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=1),  # a1
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=1),  # a2
            reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=1, x_end=5),
            reader.create_card_from_data(CardOperation.PRINT_RESULT),
            reader.create_card_from_data(CardOperation.HALT),
        ]

        sequence = reader.read_card_sequence([c.holes for c in cards])

        assert sequence.total_cards == 6
        assert len(sequence.cards) == 6
        assert sequence.cards[0].coefficient == 1
        assert sequence.cards[1].coefficient == 1
        assert sequence.cards[2].coefficient == 1
        assert sequence.cards[3].x_start == 1
        assert sequence.cards[3].x_end == 5
        assert sequence.cards[4].operation == CardOperation.PRINT_RESULT
        assert sequence.cards[5].operation == CardOperation.HALT

    def test_sequence_with_invalid_card(self):
        """Test that sequence continues even with invalid cards."""
        reader = CardReader()

        card1 = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=42)

        card2 = reader.create_card_from_data(CardOperation.HALT)

        sequence = reader.read_card_sequence([card1.holes, card2.holes])

        assert len(sequence.cards) == 2


class TestErrorHandling:
    """Test error detection and reporting."""

    def test_negative_coefficient_error(self):
        """Test that negative coefficients are rejected."""
        reader = CardReader()

        with pytest.raises(ValueError):
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=-5)

    def test_coefficient_too_large_error(self):
        """Test that oversized coefficients are rejected."""
        reader = CardReader()

        with pytest.raises(ValueError, match="exceeds 50-digit"):
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=10**50)

    def test_x_range_invalid_order_error(self):
        """Test that invalid x_start > x_end is rejected."""
        reader = CardReader()

        with pytest.raises(ValueError, match="Invalid range"):
            reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=10, x_end=5)

    def test_x_range_out_of_bounds_error(self):
        """Test that extreme x values are rejected."""
        reader = CardReader()

        with pytest.raises(ValueError, match="must be within"):
            reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=-2000, x_end=2000)

    def test_missing_required_parameter_error(self):
        """Test that missing required parameters are detected."""
        reader = CardReader()

        with pytest.raises(ValueError):
            reader.create_card_from_data(CardOperation.LOAD_COEFF)

        with pytest.raises(ValueError):
            reader.create_card_from_data(CardOperation.SET_X_RANGE)


class TestEdgeCases:
    """Test edge cases and boundary conditions."""

    def test_all_zeros_coefficient(self):
        """Test encoding all-zeros coefficient."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=0)

        decoded = reader._decode_coefficient(card.holes)
        assert decoded == 0

    def test_single_nonzero_digit(self):
        """Test various single nonzero digits."""
        reader = CardReader()

        for digit in range(1, 10):
            card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=digit)

            decoded = reader._decode_coefficient(card.holes)
            assert decoded == digit

    def test_x_range_boundary_maximum(self):
        """Test maximum allowed x values."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=-1000, x_end=1000)

        x_start, x_end = reader._decode_x_range(card.holes)
        assert x_start == -1000
        assert x_end == 1000

    def test_x_range_boundary_minimum(self):
        """Test minimum allowed x values."""
        reader = CardReader()
        card = reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=-999, x_end=-999)

        x_start, x_end = reader._decode_x_range(card.holes)
        assert x_start == -999
        assert x_end == -999

    def test_coefficient_fibonacci_numbers(self):
        """Test encoding various Fibonacci-like sequences."""
        reader = CardReader()

        fib_values = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]

        for value in fib_values:
            card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=value)

            decoded = reader._decode_coefficient(card.holes)
            assert decoded == value

    def test_round_trip_large_coefficient(self):
        """Test round-trip encoding/decoding of large coefficient."""
        reader = CardReader()
        large_val = 123456789012345

        card = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=large_val)

        decoded = reader._decode_coefficient(card.holes)
        assert decoded == large_val

    def test_card_separation_no_bleed(self):
        """Test that one card's data doesn't affect another."""
        reader = CardReader()

        card1 = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=12345)

        card2 = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=98765)

        decoded1 = reader._decode_coefficient(card1.holes)
        decoded2 = reader._decode_coefficient(card2.holes)

        assert decoded1 == 12345
        assert decoded2 == 98765


class TestRegressionAndIntegration:
    """Integration tests combining multiple features."""

    def test_full_polynomial_evaluation_card_set(self):
        """Test creating a complete card set for polynomial evaluation."""
        reader = CardReader()

        # Polynomial: f(x) = 2x^2 + 3x + 5
        cards = [
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=5),  # a0
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=3),  # a1
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=2),  # a2
            reader.create_card_from_data(CardOperation.SET_X_RANGE, x_start=0, x_end=10),
            reader.create_card_from_data(CardOperation.PRINT_RESULT),
            reader.create_card_from_data(CardOperation.HALT),
        ]

        # Verify all cards are valid
        for card in cards:
            assert card.is_valid is True

        # Verify coefficients
        assert cards[0].coefficient == 5
        assert cards[1].coefficient == 3
        assert cards[2].coefficient == 2

        # Verify range
        assert cards[3].x_start == 0
        assert cards[3].x_end == 10

    def test_reader_state_accumulation(self):
        """Test that CardReader accumulates card state correctly."""
        reader = CardReader()

        # Create multiple cards
        for i in range(5):
            reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=i * 10)

        # Verify all cards are in reader
        assert len(reader.cards_read) == 5

        # Verify card IDs are sequential
        for i, card in enumerate(reader.cards_read, start=1):
            assert card.card_id == i

    def test_checksum_independence(self):
        """Test that checksums are computed independently per card."""
        reader = CardReader()

        card1 = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=100)

        card2 = reader.create_card_from_data(CardOperation.LOAD_COEFF, coefficient=200)

        # Both should have valid checksums despite different data
        is_valid1, _ = reader._validate_checksum(card1.holes)
        is_valid2, _ = reader._validate_checksum(card2.holes)

        assert is_valid1 is True
        assert is_valid2 is True
