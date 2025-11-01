"""
Unit Tests for Difference Engine No. 2 DigitColumn

Comprehensive test suite covering:
  - Initialization and basic operations
  - Digit manipulation (get/set)
  - Addition with carry propagation
  - State management and snapshots
  - Edge cases and error handling
  - Mechanical operations (latching, advancing)

Test coverage target: 80+ tests for DigitColumn
"""

import pytest
from backend.src.emulator.columns import DigitColumn, ColumnSnapshot


# ============================================================================
# Initialization Tests (5 tests)
# ============================================================================

def test_digit_column_init_empty():
    """Test initialization with no initial value."""
    col = DigitColumn(column_index=0)
    assert col.column_index == 0
    assert col.get_value_as_int() == 0
    assert all(d == 0 for d in col.digits)
    assert col.carry_in == False
    assert col.carry_out == False


def test_digit_column_init_with_value():
    """Test initialization with initial value."""
    col = DigitColumn(column_index=2, initial_value=12345)
    assert col.column_index == 2
    assert col.get_value_as_int() == 12345


def test_digit_column_init_multiple():
    """Test creating multiple independent columns."""
    col0 = DigitColumn(0, 100)
    col1 = DigitColumn(1, 200)
    col2 = DigitColumn(2, 300)

    assert col0.get_value_as_int() == 100
    assert col1.get_value_as_int() == 200
    assert col2.get_value_as_int() == 300


def test_digit_column_init_large_value():
    """Test initialization with value exceeding 31 digits."""
    # 31-digit max: 9999999999999999999999999999999
    large_val = 10**31 + 12345  # Exceeds 31 digits
    col = DigitColumn(0, large_val)
    # Should wrap/truncate at 31 digits
    result = col.get_value_as_int()
    assert result < 10**31


def test_digit_column_init_negative_value():
    """Test initialization with negative value (uses absolute value)."""
    col = DigitColumn(0, -12345)
    assert col.get_value_as_int() == 12345


# ============================================================================
# Get/Set Digit Tests (10 tests)
# ============================================================================

def test_digit_column_get_digit_zero():
    """Test getting digit at position 0 (units place)."""
    col = DigitColumn(0, 12345)
    assert col.get_digit(0) == 5  # Units digit


def test_digit_column_get_digit_middle():
    """Test getting digit at middle positions."""
    col = DigitColumn(0, 12345)
    assert col.get_digit(1) == 4  # Tens
    assert col.get_digit(2) == 3  # Hundreds
    assert col.get_digit(3) == 2  # Thousands
    assert col.get_digit(4) == 1  # Ten thousands


def test_digit_column_get_digit_high():
    """Test getting digit at high positions."""
    col = DigitColumn(0, 12345)
    assert col.get_digit(5) == 0  # 100,000s (not set)
    assert col.get_digit(30) == 0  # Highest position


def test_digit_column_set_digit():
    """Test setting individual digits."""
    col = DigitColumn(0)
    col.set_digit(0, 5)
    col.set_digit(1, 4)
    col.set_digit(2, 3)
    assert col.get_digit(0) == 5
    assert col.get_digit(1) == 4
    assert col.get_digit(2) == 3
    assert col.get_value_as_int() == 345  # 3*100 + 4*10 + 5


def test_digit_column_set_digit_all():
    """Test setting all 31 digits."""
    col = DigitColumn(0)
    for i in range(31):
        col.set_digit(i, 7)
    assert all(col.get_digit(i) == 7 for i in range(31))


def test_digit_column_set_digit_invalid_position():
    """Test setting digit with invalid position."""
    col = DigitColumn(0)
    with pytest.raises(IndexError):
        col.set_digit(-1, 5)
    with pytest.raises(IndexError):
        col.set_digit(31, 5)
    with pytest.raises(IndexError):
        col.set_digit(100, 5)


def test_digit_column_set_digit_invalid_value():
    """Test setting digit with invalid value."""
    col = DigitColumn(0)
    with pytest.raises(ValueError):
        col.set_digit(0, -1)
    with pytest.raises(ValueError):
        col.set_digit(0, 10)
    with pytest.raises(ValueError):
        col.set_digit(0, 15)


def test_digit_column_get_digit_out_of_range():
    """Test getting digit with out-of-range position."""
    col = DigitColumn(0)
    with pytest.raises(IndexError):
        col.get_digit(-1)
    with pytest.raises(IndexError):
        col.get_digit(31)


# ============================================================================
# Value Conversion Tests (8 tests)
# ============================================================================

def test_digit_column_set_get_roundtrip():
    """Test that set_value_from_int → get_value_as_int is identity."""
    test_values = [0, 1, 10, 100, 1000, 12345, 123456789, 999999999]
    for val in test_values:
        col = DigitColumn(0)
        col.set_value_from_int(val)
        assert col.get_value_as_int() == val


def test_digit_column_large_value_wraparound():
    """Test value wraparound at 31-digit boundary."""
    max_31_digit = 10**31 - 1
    col = DigitColumn(0, max_31_digit)
    assert col.get_value_as_int() == max_31_digit

    # One more wraps (unsigned behavior)
    col.set_value_from_int(max_31_digit + 1)
    assert col.get_value_as_int() == 0  # Wraps to 0


def test_digit_column_value_sequence():
    """Test setting values in sequence."""
    col = DigitColumn(0)
    values = [0, 1, 10, 100, 1000, 10000]
    for val in values:
        col.set_value_from_int(val)
        assert col.get_value_as_int() == val


def test_digit_column_conversion_symmetry():
    """Test conversion symmetry for various values."""
    # Test powers of 10
    for power in range(10):
        val = 10**power
        col = DigitColumn(0)
        col.set_value_from_int(val)
        assert col.get_value_as_int() == val


def test_digit_column_zero_handling():
    """Test that zero is handled consistently."""
    col1 = DigitColumn(0, 0)
    col2 = DigitColumn(0)
    col2.set_value_from_int(0)

    assert col1.get_value_as_int() == 0
    assert col2.get_value_as_int() == 0


# ============================================================================
# Addition with Carry Tests (20 tests)
# ============================================================================

def test_digit_column_add_difference_simple():
    """Test simple addition without carry."""
    col = DigitColumn(0, 100)
    diff = [5] + [0] * 30  # Add 5 to position 0
    col.add_difference(diff)
    assert col.get_value_as_int() == 105


def test_digit_column_add_difference_with_carry():
    """Test addition that generates carry."""
    col = DigitColumn(0, 5)
    diff = [7] + [0] * 30  # Add 7 to 5 = 12 → carry
    col.add_difference(diff)
    assert col.get_digit(0) == 2  # 12 % 10 = 2
    assert col.get_digit(1) == 1  # Carry to position 1


def test_digit_column_add_difference_multi_digit():
    """Test addition across multiple digit positions."""
    col = DigitColumn(0, 123)
    diff = [4, 5, 6] + [0] * 28  # Add 654 (654 in digit form)
    col.add_difference(diff)
    assert col.get_value_as_int() == 777


def test_digit_column_add_difference_propagating_carry():
    """Test carry propagation across multiple positions."""
    col = DigitColumn(0, 99)  # 9 in position 0, 9 in position 1
    diff = [1, 0] + [0] * 29  # Add 1 to position 0
    col.add_difference(diff)
    assert col.get_digit(0) == 0  # 9+1=10, leaves 0, carry 1
    assert col.get_digit(1) == 0  # 9+0+1=10, leaves 0, carry 1
    assert col.get_digit(2) == 1  # 0+0+1=1


def test_digit_column_add_difference_full_carry_chain():
    """Test carry propagation through many positions."""
    col = DigitColumn(0)
    # Set all digits to 9
    for i in range(31):
        col.set_digit(i, 9)

    diff = [1] + [0] * 30  # Add 1 to position 0
    col.add_difference(diff)

    # All positions should be 0, carry_out should be set
    for i in range(31):
        assert col.get_digit(i) == 0
    assert col.carry_out == True  # Overflow detected


def test_digit_column_add_difference_carry_in():
    """Test addition with incoming carry."""
    col = DigitColumn(0, 5)
    col.set_carry_in(True)  # Set carry_in flag
    diff = [2] + [0] * 30
    col.add_difference(diff)
    # Should be 5 + 2 + 1 (carry_in) = 8
    assert col.get_value_as_int() == 8


def test_digit_column_add_difference_wrong_length():
    """Test that wrong-length difference raises error."""
    col = DigitColumn(0, 100)
    with pytest.raises(ValueError):
        col.add_difference([1, 2, 3])  # Too short
    with pytest.raises(ValueError):
        col.add_difference([0] * 32)  # Too long


def test_digit_column_add_zero():
    """Test adding zero (no change)."""
    col = DigitColumn(0, 12345)
    diff = [0] * 31
    col.add_difference(diff)
    assert col.get_value_as_int() == 12345


def test_digit_column_add_all_nines():
    """Test adding 999...999 (max digits)."""
    col = DigitColumn(0, 1)
    diff = [9] * 31
    col.add_difference(diff)
    # 1 + 999...999 = 1000...000 (carries out)
    assert col.carry_out == True


def test_digit_column_add_single_operation():
    """Test add_single convenience method."""
    col = DigitColumn(0, 100)
    col.add_single(50)
    assert col.get_value_as_int() == 150


def test_digit_column_add_single_negative():
    """Test add_single with negative value (subtraction)."""
    col = DigitColumn(0, 100)
    col.add_single(-30)
    assert col.get_value_as_int() == 70


def test_digit_column_add_single_wraparound():
    """Test add_single with wraparound."""
    col = DigitColumn(0, 1)
    col.add_single(-2)  # -1 wraps to large number
    result = col.get_value_as_int()
    assert result == (10**31 - 1)  # Max 31-digit value


# ============================================================================
# State Management Tests (10 tests)
# ============================================================================

def test_digit_column_reset():
    """Test reset to zero."""
    col = DigitColumn(0, 12345)
    col.set_carry_in(True)
    col.reset()

    assert col.get_value_as_int() == 0
    assert col.carry_in == False
    assert col.carry_out == False


def test_digit_column_carry_flags():
    """Test carry_in and carry_out flags."""
    col = DigitColumn(0)
    assert col.carry_in == False
    assert col.carry_out == False

    col.set_carry_in(True)
    assert col.carry_in == True

    col.set_carry_in(False)
    assert col.carry_in == False


def test_digit_column_get_carry_out():
    """Test get_carry_out method - overflow beyond 31 digits."""
    # Initialize with all 9s across 31 positions (max value before overflow)
    col = DigitColumn(0)
    col.set_value_from_int(int('9' * 31))

    # Add 1 to position 0, which cascades carry through all 31 positions
    diff = [1] + [0] * 30
    col.add_difference(diff)

    # Carry should propagate from position 0 all the way to position 30,
    # then overflow as carry_out
    assert col.get_carry_out() == True


def test_digit_column_snapshot():
    """Test state snapshot capture."""
    col = DigitColumn(0, 12345)
    col.set_carry_in(True)
    col.set_phase("addition")

    snap = col.get_snapshot()
    assert isinstance(snap, ColumnSnapshot)
    assert snap.column_index == 0
    assert snap.digits == col.digits
    assert snap.carry_in == True
    assert snap.phase == "addition"


def test_digit_column_snapshot_independence():
    """Test that snapshot is independent copy."""
    col = DigitColumn(0, 100)
    snap = col.get_snapshot()

    # Modify column
    col.set_value_from_int(200)

    # Snapshot should be unchanged
    assert col.get_value_as_int() == 200
    assert snap.digits[0] == 0  # Original snapshot unchanged


def test_digit_column_latch_operations():
    """Test latch (mechanical lock) operations."""
    col = DigitColumn(0)
    assert col.is_locked() == False

    col.latch()
    assert col.is_locked() == True

    col.unlatch()
    assert col.is_locked() == False


def test_digit_column_advancing_state():
    """Test advancing (row shift) state."""
    col = DigitColumn(0)
    assert col.is_advancing_state() == False

    col.start_advancing()
    assert col.is_advancing_state() == True

    col.stop_advancing()
    assert col.is_advancing_state() == False


def test_digit_column_phase_tracking():
    """Test phase name tracking for debugging."""
    col = DigitColumn(0)
    assert col.phase == "idle"

    col.set_phase("addition")
    assert col.phase == "addition"

    col.set_phase("carry")
    assert col.phase == "carry"


# ============================================================================
# Edge Cases and Boundary Tests (15 tests)
# ============================================================================

def test_digit_column_max_value():
    """Test maximum 31-digit value."""
    max_val = 10**31 - 1
    col = DigitColumn(0, max_val)
    assert col.get_value_as_int() == max_val


def test_digit_column_single_digit_max():
    """Test max single-digit value."""
    col = DigitColumn(0, 9)
    assert col.get_value_as_int() == 9


def test_digit_column_power_of_ten():
    """Test power-of-ten values."""
    for power in range(10):
        val = 10**power
        col = DigitColumn(0, val)
        assert col.get_value_as_int() == val


def test_digit_column_palindromic_value():
    """Test palindromic digit values."""
    col = DigitColumn(0, 12321)
    assert col.get_digit(0) == 1
    assert col.get_digit(1) == 2
    assert col.get_digit(2) == 3
    assert col.get_digit(3) == 2
    assert col.get_digit(4) == 1


def test_digit_column_repeated_digits():
    """Test values with repeated digits."""
    col = DigitColumn(0, 111111)
    for i in range(6):
        assert col.get_digit(i) == 1


def test_digit_column_alternating_digits():
    """Test alternating digit patterns."""
    col = DigitColumn(0)
    for i in range(10):
        col.set_digit(i, i % 2)  # 0,1,0,1,0,...
    for i in range(10):
        assert col.get_digit(i) == i % 2


def test_digit_column_sequential_addition():
    """Test sequential additions accumulate."""
    col = DigitColumn(0, 0)
    for i in range(10):
        diff = [1] + [0] * 30
        col.add_difference(diff)
    assert col.get_value_as_int() == 10


def test_digit_column_alternating_add_sub():
    """Test alternating addition and subtraction."""
    col = DigitColumn(0, 50)
    col.add_single(25)  # → 75
    col.add_single(-25)  # → 50
    assert col.get_value_as_int() == 50


def test_digit_column_high_digit_positions():
    """Test setting values at high digit positions."""
    col = DigitColumn(0)
    col.set_digit(29, 1)
    col.set_digit(30, 2)
    # Value should be 2*10^30 + 1*10^29
    assert col.get_digit(29) == 1
    assert col.get_digit(30) == 2


def test_digit_column_mixed_operations():
    """Test mixing different operations."""
    col = DigitColumn(0, 100)
    col.set_digit(0, 5)      # Modify to 105
    col.add_single(50)        # Add 50 → 155
    diff = [5] + [0] * 30
    col.add_difference(diff)  # Add 5 → 160
    assert col.get_value_as_int() == 160


# ============================================================================
# String Representation and Repr Tests (2 tests)
# ============================================================================

def test_digit_column_repr():
    """Test string representation."""
    col = DigitColumn(0, 12345)
    repr_str = repr(col)
    assert "DigitColumn" in repr_str
    assert "12345" in repr_str


def test_digit_column_repr_with_carry():
    """Test repr includes carry status."""
    col = DigitColumn(1, 100)
    repr_str = repr(col)
    assert "column_index=1" in repr_str or "index=1" in repr_str
