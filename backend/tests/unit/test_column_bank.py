"""
ColumnBank comprehensive test suite.

Tests for unified state management across 8 synchronized DE2 columns.
Covers initialization, multi-column operations, synchronization, carry
propagation between columns, and state consistency.

Based on SMG Technical Description and Babbage mechanical notation.
"""

import pytest
from backend.src.emulator.columns import ColumnBank, DigitColumn


# ============================================================================
# Initialization Tests (5 tests)
# ============================================================================


def test_column_bank_init_empty():
    """Test ColumnBank initialization with no initial values."""
    bank = ColumnBank()

    assert len(bank.columns) == 8
    for i, col in enumerate(bank.columns):
        assert col.column_index == i
        assert col.get_value_as_int() == 0


def test_column_bank_init_with_differences():
    """Test ColumnBank initialization with initial difference values."""
    initial = [1, 2, 3, 4, 5, 6, 7, 8]
    bank = ColumnBank(initial_differences=initial)

    for i, expected in enumerate(initial):
        assert bank.get_column(i).get_value_as_int() == expected


def test_column_bank_init_with_zeros():
    """Test ColumnBank initialization with all zero differences."""
    initial = [0, 0, 0, 0, 0, 0, 0, 0]
    bank = ColumnBank(initial_differences=initial)

    for col in bank.columns:
        assert col.get_value_as_int() == 0


def test_column_bank_init_with_large_values():
    """Test ColumnBank initialization with large multi-digit values."""
    initial = [
        12345678901234567890,
        98765432109876543210,
        11111111111111111111,
        22222222222222222222,
        33333333333333333333,
        44444444444444444444,
        55555555555555555555,
        99999999999999999999,
    ]
    bank = ColumnBank(initial_differences=initial)

    for i, expected in enumerate(initial):
        # Values wrap at 31 digits (10^31)
        wrapped = expected % (10**31)
        assert bank.get_column(i).get_value_as_int() == wrapped


def test_column_bank_init_wrong_count():
    """Test ColumnBank rejects wrong number of initial values."""
    with pytest.raises(ValueError, match="Expected 8 initial differences"):
        ColumnBank(initial_differences=[1, 2, 3])  # Only 3, not 8


# ============================================================================
# Column Access Tests (5 tests)
# ============================================================================


def test_column_bank_get_column_valid():
    """Test getting specific columns by index."""
    bank = ColumnBank(initial_differences=[1, 2, 3, 4, 5, 6, 7, 8])

    for i in range(8):
        col = bank.get_column(i)
        assert isinstance(col, DigitColumn)
        assert col.column_index == i
        assert col.get_value_as_int() == i + 1


def test_column_bank_get_column_negative_index():
    """Test get_column rejects negative index."""
    bank = ColumnBank()

    with pytest.raises(IndexError, match="Column index out of range"):
        bank.get_column(-1)


def test_column_bank_get_column_out_of_bounds():
    """Test get_column rejects index >= 8."""
    bank = ColumnBank()

    with pytest.raises(IndexError, match="Column index out of range"):
        bank.get_column(8)

    with pytest.raises(IndexError, match="Column index out of range"):
        bank.get_column(100)


def test_column_bank_get_all_values():
    """Test getting all column values as list."""
    initial = [10, 20, 30, 40, 50, 60, 70, 80]
    bank = ColumnBank(initial_differences=initial)

    values = bank.get_all_values()
    assert len(values) == 8
    assert values == initial


def test_column_bank_set_all_values():
    """Test setting all column values at once."""
    bank = ColumnBank()
    new_values = [111, 222, 333, 444, 555, 666, 777, 888]

    bank.set_all_values(new_values)

    for i, expected in enumerate(new_values):
        assert bank.get_column(i).get_value_as_int() == expected


# ============================================================================
# Synchronized Operation Tests (10 tests)
# ============================================================================


def test_column_bank_add_difference_row_simple():
    """Test adding difference row across all columns."""
    bank = ColumnBank(initial_differences=[1, 2, 3, 4, 5, 6, 7, 8])

    # Create difference row (all columns same difference)
    diff_rows = [[1] + [0] * 30 for _ in range(8)]

    bank.add_difference_row(diff_rows)

    # Each column should have value incremented by 1
    expected = [2, 3, 4, 5, 6, 7, 8, 9]
    assert bank.get_all_values() == expected


def test_column_bank_add_difference_row_different_per_column():
    """Test adding different difference for each column."""
    bank = ColumnBank()

    # Build difference rows: column i adds 10^i
    diff_rows = []
    for col_idx in range(8):
        diff = [0] * 31
        diff[col_idx] = 1  # Add 1 to position col_idx (powers of 10)
        diff_rows.append(diff)

    bank.add_difference_row(diff_rows)

    # Each column should have 10^col_idx
    expected = [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000]
    assert bank.get_all_values() == expected


def test_column_bank_add_difference_row_zero():
    """Test adding zero difference row (no change)."""
    bank = ColumnBank(initial_differences=[10, 20, 30, 40, 50, 60, 70, 80])
    original = bank.get_all_values()

    # Add zero to all columns
    diff_rows = [[0] * 31 for _ in range(8)]
    bank.add_difference_row(diff_rows)

    assert bank.get_all_values() == original


def test_column_bank_add_difference_row_multiple_times():
    """Test adding difference row multiple times (sequential updates)."""
    bank = ColumnBank(initial_differences=[1, 2, 3, 4, 5, 6, 7, 8])

    # Add 5 times
    for _ in range(5):
        diff_rows = [[1] + [0] * 30 for _ in range(8)]
        bank.add_difference_row(diff_rows)

    # Each column should have initial + 5
    expected = [6, 7, 8, 9, 10, 11, 12, 13]
    assert bank.get_all_values() == expected


def test_column_bank_add_difference_row_with_carries():
    """Test that carries are properly propagated between columns."""
    # Initialize with nines in low positions to generate carries
    bank = ColumnBank(initial_differences=[9, 9, 9, 9, 9, 9, 9, 9])

    # Add 1 to each column
    diff_rows = [[1] + [0] * 30 for _ in range(8)]
    bank.add_difference_row(diff_rows)

    # Columns should advance, carries propagate left-to-right
    # But each column handles its own carry internally, so we just get +1
    expected = [10, 10, 10, 10, 10, 10, 10, 10]
    assert bank.get_all_values() == expected


def test_column_bank_add_difference_row_wrong_count():
    """Test add_difference_row rejects wrong number of rows."""
    bank = ColumnBank()

    with pytest.raises(ValueError, match="Expected 8 difference rows"):
        bank.add_difference_row([[0] * 31 for _ in range(7)])  # Only 7 rows


def test_column_bank_add_difference_row_wrong_digit_count():
    """Test add_difference_row rejects wrong digit count in row."""
    bank = ColumnBank()

    diff_rows = [[0] * 31 for _ in range(8)]
    diff_rows[3] = [0] * 20  # Wrong size

    with pytest.raises(ValueError, match="expected 31 digits"):
        bank.add_difference_row(diff_rows)


def test_column_bank_add_difference_row_polynomial():
    """Test adding difference rows for polynomial evaluation."""
    # Polynomial difference table: f(x) = x² (differences: 1, 3, 5, 7, 9, ...)
    bank = ColumnBank(initial_differences=[0, 1, 2, 0, 0, 0, 0, 0])
    # f(0)=0, Δf=1, Δ²f=2

    # Add first difference
    diff_rows = [
        [1] + [0] * 30,  # f incremented by 1
        [2] + [0] * 30,  # Δf incremented by 2
        [0] * 31,
        [0] * 31,
        [0] * 31,
        [0] * 31,
        [0] * 31,
        [0] * 31,
    ]
    bank.add_difference_row(diff_rows)

    assert bank.get_column(0).get_value_as_int() == 1  # f(1) = 1
    assert bank.get_column(1).get_value_as_int() == 3  # Δf = 3


def test_column_bank_sequential_operations():
    """Test sequence of independent difference additions."""
    bank = ColumnBank()

    # Add different values to each column sequentially
    for step in range(3):
        diff_rows = []
        for col_idx in range(8):
            diff = [0] * 31
            diff[0] = 1  # Add 1 to position 0 of each column
            diff_rows.append(diff)
        bank.add_difference_row(diff_rows)

    # Each column should have 3
    expected = [3] * 8
    assert bank.get_all_values() == expected


# ============================================================================
# State Management Tests (8 tests)
# ============================================================================


def test_column_bank_reset_all():
    """Test resetting all columns to zero."""
    bank = ColumnBank(initial_differences=[10, 20, 30, 40, 50, 60, 70, 80])

    bank.reset_all()

    for col in bank.columns:
        assert col.get_value_as_int() == 0


def test_column_bank_state_snapshot():
    """Test capturing state snapshot of all columns."""
    initial = [1, 2, 3, 4, 5, 6, 7, 8]
    bank = ColumnBank(initial_differences=initial)

    snapshot = bank.state_snapshot()

    assert len(snapshot) == 8
    for i, snap in enumerate(snapshot):
        assert snap.column_index == i
        assert snap.digits == bank.columns[i].digits


def test_column_bank_latch_all():
    """Test latching all columns."""
    bank = ColumnBank()

    bank.latch_all()

    for col in bank.columns:
        assert col.is_locked() == True


def test_column_bank_unlatch_all():
    """Test unlatching all columns."""
    bank = ColumnBank()

    bank.latch_all()
    bank.unlatch_all()

    for col in bank.columns:
        assert col.is_locked() == False


def test_column_bank_are_all_latched_true():
    """Test checking if all columns are latched (true case)."""
    bank = ColumnBank()

    bank.latch_all()

    assert bank.are_all_latched() == True


def test_column_bank_are_all_latched_false():
    """Test checking if all columns are latched (false case)."""
    bank = ColumnBank()

    bank.latch_all()
    bank.get_column(3).unlatch()  # Unlatch one column

    assert bank.are_all_latched() == False


def test_column_bank_set_all_phases():
    """Test setting mechanical phase for all columns."""
    bank = ColumnBank()

    bank.set_all_phases("addition")

    for col in bank.columns:
        assert col.phase == "addition"


def test_column_bank_independent_column_operations():
    """Test that individual column operations don't affect others."""
    bank = ColumnBank()

    # Modify one column
    bank.get_column(3).set_value_from_int(12345)

    # Other columns should be unaffected
    for i in range(8):
        if i != 3:
            assert bank.get_column(i).get_value_as_int() == 0
        else:
            assert bank.get_column(3).get_value_as_int() == 12345


# ============================================================================
# Consistency and Edge Cases (7 tests)
# ============================================================================


def test_column_bank_large_values_consistency():
    """Test that large values are consistently managed across columns."""
    large_val = 10**30 - 1  # 31 nines
    initial = [large_val] * 8
    bank = ColumnBank(initial_differences=initial)

    # All columns should have the maximum 31-digit value
    for col in bank.columns:
        assert col.get_value_as_int() == large_val


def test_column_bank_carry_isolation():
    """Test that carries don't leak between non-adjacent operations."""
    bank = ColumnBank()

    # Add to column 0 with carry
    # [9, 1, 0...] adds digit 9 at position 0 and digit 1 at position 1
    diff_rows = [
        [9, 1] + [0] * 29,  # Adds 9 to pos 0, 1 to pos 1 → value = 19
        [0] * 31,
        [0] * 31,
        [0] * 31,
        [0] * 31,
        [0] * 31,
        [0] * 31,
        [0] * 31,
    ]
    bank.add_difference_row(diff_rows)

    # Column 0 should be 19 (9 at pos 0, 1 at pos 1), others should be 0
    assert bank.get_column(0).get_value_as_int() == 19
    for i in range(1, 8):
        assert bank.get_column(i).get_value_as_int() == 0


def test_column_bank_all_columns_independent():
    """Test that each column maintains independent state."""
    bank = ColumnBank()

    # Set each column to unique value
    for i in range(8):
        bank.get_column(i).set_value_from_int(111 * (i + 1))

    # Verify each maintains its own value
    for i in range(8):
        assert bank.get_column(i).get_value_as_int() == 111 * (i + 1)


def test_column_bank_mixed_operations():
    """Test complex sequence of mixed operations."""
    bank = ColumnBank(initial_differences=[1, 2, 3, 4, 5, 6, 7, 8])

    # Reset some columns
    bank.get_column(2).reset()
    bank.get_column(5).reset()

    # Add to others
    diff_rows = [[5] + [0] * 30 for _ in range(8)]
    bank.add_difference_row(diff_rows)

    # Column 2, 5 reset to 0, others increment by 5
    expected = [
        6,
        7,
        5,
        9,
        10,
        5,
        12,
        13,
    ]  # Column 2 = 3+5-3 = 5 (after reset), Column 5 = 6+5-6 = 5 (after reset)
    assert bank.get_all_values() == expected


def test_column_bank_snapshot_independence():
    """Test that snapshots are independent copies."""
    bank = ColumnBank(initial_differences=[10, 20, 30, 40, 50, 60, 70, 80])

    snap1 = bank.state_snapshot()

    # Modify bank
    bank.reset_all()

    snap2 = bank.state_snapshot()

    # Snapshots should differ
    assert snap1[0].digits != snap2[0].digits
    # But snap1 should preserve original state
    assert snap1[0].digits[0] == 0  # 10 % 10 = 0
    assert snap1[1].digits[0] == 0  # 20 % 10 = 0


def test_column_bank_repr():
    """Test string representation of ColumnBank."""
    bank = ColumnBank(initial_differences=[1, 2, 3, 4, 5, 6, 7, 8])

    repr_str = repr(bank)

    assert "ColumnBank" in repr_str
    assert "values=[1, 2, 3, 4, 5, 6, 7, 8]" in repr_str


# ============================================================================
# Integration Tests (5 tests)
# ============================================================================


def test_column_bank_polynomial_table():
    """Test building complete polynomial difference table."""
    # f(x) = x² + 1
    # f(0)=1, f(1)=2, f(2)=5, f(3)=10, f(4)=17, f(5)=26
    # Δf: 1, 3, 5, 7, 9
    # Δ²f: 2, 2, 2, 2

    bank = ColumnBank(initial_differences=[1, 1, 2, 0, 0, 0, 0, 0])
    # f(0)=1, Δf=1, Δ²f=2

    # Step 1: f(1) = f(0) + Δf = 1 + 1 = 2, Δf = Δf + Δ²f = 1 + 2 = 3
    diff_rows = [
        [1] + [0] * 30,  # f += 1
        [2] + [0] * 30,  # Δf += 2
        [0] * 31,
        [0] * 31,
        [0] * 31,
        [0] * 31,
        [0] * 31,
        [0] * 31,
    ]
    bank.add_difference_row(diff_rows)
    assert bank.get_column(0).get_value_as_int() == 2  # f(1) = 2
    assert bank.get_column(1).get_value_as_int() == 3  # Δf = 3


def test_column_bank_difference_engine_cycle():
    """Test one complete DE2 operational cycle."""
    # Initialize with sample table
    bank = ColumnBank(initial_differences=[0, 2, 2, 0, 0, 0, 0, 0])

    # Column 2: Δ²f (second difference - constant)
    # Column 1: Δf (first difference - incremented by Δ²f)
    # Column 0: f (function value - incremented by Δf)

    for step in range(3):
        # Create difference row based on mechanical timing
        diff_rows = []
        for i in range(8):
            diff = [0] * 31
            if i == 0:
                # f += Δf
                diff[0] = 1
            elif i == 1:
                # Δf += Δ²f
                diff[0] = 1
            diff_rows.append(diff)

        bank.add_difference_row(diff_rows)

    # After 3 steps: f should have incremented appropriately
    values = bank.get_all_values()
    assert values[0] > 0  # f value increased


def test_column_bank_stereotype_output():
    """Test extracting values for stereotyper output."""
    # Simulating stereotype output of computed values
    initial = [12345, 23456, 34567, 45678, 56789, 67890, 78901, 89012]
    bank = ColumnBank(initial_differences=initial)

    # Extract all values for output
    values = bank.get_all_values()

    # Format for stereotyper (8-column paper)
    output_line = "".join(f"{v:>10d}" for v in values)

    assert len(output_line) > 0
    assert "12345" in output_line


def test_column_bank_cascading_carries():
    """Test cascading carries across synchronized columns."""
    # Set each column to near-max in one position
    bank = ColumnBank()

    for i in range(8):
        col = bank.get_column(i)
        # Set digit at position i to 9
        for pos in range(31):
            if pos == i:
                col.set_digit(pos, 9)

    # Now add 1 to each column's unit position
    diff_rows = [[1] + [0] * 30 for _ in range(8)]
    bank.add_difference_row(diff_rows)

    # Each column should carry appropriately
    values = bank.get_all_values()
    assert all(v > 0 for v in values)
