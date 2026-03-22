"""Unit tests for the Millionaire Calculator emulator (1893)."""

import pytest

from backend.src.emulator.millionaire_calculator import _PLATE, MillionaireCalculator

# ---------------------------------------------------------------------------
# Multiplication plate
# ---------------------------------------------------------------------------


class TestMultiplicationPlate:
    def test_plate_identity(self):
        for d in range(10):
            for m in range(10):
                assert _PLATE[d][m] == d * m

    def test_plate_max(self):
        assert _PLATE[9][9] == 81


# ---------------------------------------------------------------------------
# Input and lever
# ---------------------------------------------------------------------------


class TestInput:
    def test_set_input(self):
        m = MillionaireCalculator()
        m.set_input(12345678)
        assert m._input[0] == 8  # LSB first
        assert m._input[7] == 1  # MSB last

    def test_set_input_zero(self):
        m = MillionaireCalculator()
        m.set_input(0)
        assert all(d == 0 for d in m._input)

    def test_set_input_negative_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.set_input(-1)

    def test_set_input_overflow_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.set_input(10**MillionaireCalculator.INPUT_DIGITS)

    def test_set_lever_valid(self):
        m = MillionaireCalculator()
        m.set_multiplier_lever(7)
        assert m.multiplier_lever == 7

    def test_set_lever_invalid(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.set_multiplier_lever(10)
        with pytest.raises(ValueError):
            m.set_multiplier_lever(-1)


# ---------------------------------------------------------------------------
# Direct multiplication (single crank per multiplier digit)
# ---------------------------------------------------------------------------


class TestDirectMultiplication:
    def test_multiply_basic(self):
        m = MillionaireCalculator()
        assert m.multiply(6, 7) == 42

    def test_multiply_by_zero(self):
        m = MillionaireCalculator()
        assert m.multiply(999, 0) == 0

    def test_multiply_by_one(self):
        m = MillionaireCalculator()
        assert m.multiply(12345, 1) == 12345

    def test_multiply_two_digit(self):
        m = MillionaireCalculator()
        assert m.multiply(12, 34) == 408

    def test_multiply_large(self):
        m = MillionaireCalculator()
        assert m.multiply(99999999, 9) == 899999991

    def test_multiply_matches_python(self):
        m = MillionaireCalculator()
        cases = [(3, 17), (25, 40), (7, 111), (123, 456), (999, 999)]
        for a, b in cases:
            result = m.multiply(a, b)
            assert result == a * b, f"{a}*{b}: expected {a * b}, got {result}"

    def test_multiply_crank_count_equals_digit_count(self):
        """One crank per multiplier digit -- the Millionaire's key advantage."""
        m = MillionaireCalculator()
        m.multiply(999, 99)
        # multiplier 99 has 2 digits -> 2 crank turns
        assert m.get_counter() == 2

    def test_multiply_crank_count_for_three_digits(self):
        m = MillionaireCalculator()
        m.multiply(5, 123)
        assert m.get_counter() == 3

    def test_multiply_then_add(self):
        """After multiply, add should work with lever=1."""
        m = MillionaireCalculator()
        m.multiply(10, 5)  # result = 50
        assert m.add(3) == 53


# ---------------------------------------------------------------------------
# Carriage shift
# ---------------------------------------------------------------------------


class TestCarriageShift:
    def test_shift_by_one_multiplies_by_10(self):
        m = MillionaireCalculator()
        m.set_input(5)
        m.shift_carriage(1)
        m.set_multiplier_lever(1)
        m.turn_crank()
        assert m.get_result() == 50

    def test_shift_out_of_range_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.shift_carriage(MillionaireCalculator.RESULT_DIGITS)


# ---------------------------------------------------------------------------
# Division
# ---------------------------------------------------------------------------


class TestDivision:
    def test_divide_exact(self):
        m = MillionaireCalculator()
        q, r = m.divide(100, 4)
        assert q == 25
        assert r == 0

    def test_divide_with_remainder(self):
        m = MillionaireCalculator()
        q, r = m.divide(17, 3)
        assert q == 5
        assert r == 2

    def test_divide_by_zero_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.divide(10, 0)


# ---------------------------------------------------------------------------
# State and clear
# ---------------------------------------------------------------------------


class TestState:
    def test_clear_result(self):
        m = MillionaireCalculator()
        m.multiply(99, 99)
        m.clear_result()
        assert m.get_result() == 0

    def test_clear_counter(self):
        m = MillionaireCalculator()
        m.multiply(5, 5)
        m.clear_counter()
        assert m.get_counter() == 0

    def test_state_keys(self):
        m = MillionaireCalculator()
        s = m.state()
        for key in ("result", "counter", "carriage_position", "multiplier_lever", "input"):
            assert key in s

    def test_state_reflects_current_values(self):
        m = MillionaireCalculator()
        m.multiply(7, 8)
        s = m.state()
        assert s["result"] == 56
        assert s["counter"] == 1  # one digit in multiplier 8

    def test_clear_result_independent_of_counter(self):
        m = MillionaireCalculator()
        m.multiply(3, 3)
        m.clear_result()
        assert m.get_result() == 0
        assert m.get_counter() == 1  # counter still holds prior value

    def test_clear_counter_independent_of_result(self):
        m = MillionaireCalculator()
        m.multiply(3, 3)
        m.clear_counter()
        assert m.get_counter() == 0
        assert m.get_result() == 9  # result still holds


# ---------------------------------------------------------------------------
# turn_crank edge cases
# ---------------------------------------------------------------------------


class TestTurnCrank:
    def test_lever_zero_adds_nothing(self):
        """Lever=0 means multiply by 0: result unchanged."""
        m = MillionaireCalculator()
        m.set_input(12345)
        m.set_multiplier_lever(0)
        m.turn_crank()
        assert m.get_result() == 0

    def test_lever_zero_still_increments_counter(self):
        m = MillionaireCalculator()
        m.set_input(5)
        m.set_multiplier_lever(0)
        m.turn_crank()
        assert m.get_counter() == 1

    def test_multiple_cranks_accumulate(self):
        """Three cranks with lever=2 and input=10 should give 2*10*3=60."""
        m = MillionaireCalculator()
        m.set_input(10)
        m.set_multiplier_lever(2)
        m.turn_crank()
        m.turn_crank()
        m.turn_crank()
        assert m.get_result() == 60
        assert m.get_counter() == 3

    def test_crank_at_carriage_position_1(self):
        """Crank at position 1 adds partial product shifted left by one decimal place."""
        m = MillionaireCalculator()
        m.set_input(3)
        m.shift_carriage(1)
        m.set_multiplier_lever(2)
        m.turn_crank()
        # 2 * 3 = 6, shifted by 1 -> 60
        assert m.get_result() == 60


# ---------------------------------------------------------------------------
# Additional multiply / add / divide tests
# ---------------------------------------------------------------------------


class TestAdditional:
    def test_multiply_multiplier_zero(self):
        """Multiplier of 0 produces 0."""
        m = MillionaireCalculator()
        assert m.multiply(12345, 0) == 0

    def test_multiply_single_digit_input(self):
        m = MillionaireCalculator()
        assert m.multiply(3, 7) == 21

    def test_add_cumulative(self):
        """add() accumulates: three calls with 10 each gives 30."""
        m = MillionaireCalculator()
        m.add(10)
        m.add(10)
        m.add(10)
        assert m.get_result() == 30

    def test_add_large_value(self):
        m = MillionaireCalculator()
        result = m.add(99999999)
        assert result == 99999999

    def test_set_input_max_value(self):
        m = MillionaireCalculator()
        max_val = 10**MillionaireCalculator.INPUT_DIGITS - 1
        m.set_input(max_val)
        assert m.state()["input"] == max_val

    def test_shift_carriage_zero_is_noop(self):
        m = MillionaireCalculator()
        m.shift_carriage(0)
        assert m.carriage_position == 0

    def test_divide_negative_dividend_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.divide(-1, 5)

    def test_divide_negative_divisor_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.divide(10, -1)

    def test_multiply_preserves_plate_accuracy(self):
        """Spot-check that multiply uses the plate table correctly."""
        m = MillionaireCalculator()
        assert m.multiply(9, 9) == 81
        assert m.multiply(99, 9) == 891

    def test_divide_by_one_returns_dividend(self):
        m = MillionaireCalculator()
        q, r = m.divide(12345, 1)
        assert q == 12345
        assert r == 0

    def test_divide_dividend_zero(self):
        m = MillionaireCalculator()
        q, r = m.divide(0, 7)
        assert q == 0
        assert r == 0


# ---------------------------------------------------------------------------
# Plate extended
# ---------------------------------------------------------------------------


class TestPlateExtended:
    """_PLATE lookup table structural properties."""

    def test_plate_row_zero_all_zeros(self):
        assert all(_PLATE[0][m] == 0 for m in range(10))

    def test_plate_column_zero_all_zeros(self):
        for d in range(10):
            assert _PLATE[d][0] == 0

    def test_plate_has_10_rows(self):
        assert len(_PLATE) == 10

    def test_plate_entries_match_multiplication(self):
        for d in range(10):
            for m in range(10):
                assert _PLATE[d][m] == d * m

    def test_plate_9x9_boundary(self):
        assert _PLATE[9][9] == 81
        assert _PLATE[1][9] == 9
        assert _PLATE[9][1] == 9


# ---------------------------------------------------------------------------
# Counter behavior
# ---------------------------------------------------------------------------


class TestCounterBehavior:
    """Revolution counter: increment, clear, independence."""

    def test_initial_counter_is_zero(self):
        m = MillionaireCalculator()
        assert m.get_counter() == 0

    def test_single_crank_increments_counter_by_one(self):
        m = MillionaireCalculator()
        m.set_input(1)
        m.set_multiplier_lever(1)
        m.turn_crank()
        assert m.get_counter() == 1

    def test_multiple_cranks_accumulate_counter(self):
        m = MillionaireCalculator()
        m.set_input(1)
        m.set_multiplier_lever(1)
        for _ in range(5):
            m.turn_crank()
        assert m.get_counter() == 5

    def test_clear_counter_resets_to_zero(self):
        m = MillionaireCalculator()
        m.multiply(7, 7)
        m.clear_counter()
        assert m.get_counter() == 0

    def test_counter_independent_of_result_clear(self):
        m = MillionaireCalculator()
        m.multiply(3, 4)
        m.clear_result()
        assert m.get_counter() == 1  # counter unaffected

    def test_counter_incremented_even_for_lever_zero(self):
        m = MillionaireCalculator()
        m.set_input(5)
        m.set_multiplier_lever(0)
        m.turn_crank()
        assert m.get_counter() == 1

    def test_multiply_digit_count_equals_counter(self):
        m = MillionaireCalculator()
        m.multiply(1, 12345)
        # 12345 has 5 digits -> 5 crank turns
        assert m.get_counter() == 5


# ---------------------------------------------------------------------------
# Result register
# ---------------------------------------------------------------------------


class TestResultRegister:
    """Result register initial, clear, accumulation, and length."""

    def test_initial_result_is_zero(self):
        m = MillionaireCalculator()
        assert m.get_result() == 0

    def test_clear_result_resets_to_zero(self):
        m = MillionaireCalculator()
        m.add(999)
        m.clear_result()
        assert m.get_result() == 0

    def test_add_accumulates_in_result(self):
        m = MillionaireCalculator()
        m.add(100)
        m.add(200)
        assert m.get_result() == 300

    def test_result_is_integer(self):
        m = MillionaireCalculator()
        m.multiply(7, 8)
        assert isinstance(m.get_result(), int)

    def test_clear_result_does_not_clear_counter(self):
        m = MillionaireCalculator()
        m.multiply(3, 3)
        counter_before = m.get_counter()
        m.clear_result()
        assert m.get_counter() == counter_before

    def test_result_after_multiply_matches_python(self):
        m = MillionaireCalculator()
        assert m.multiply(12345678, 2) == 12345678 * 2


# ---------------------------------------------------------------------------
# Internal state snapshot
# ---------------------------------------------------------------------------


class TestInternalState:
    """state() keys, initial values, and reflection of mutations."""

    def test_state_carriage_position_initial_zero(self):
        m = MillionaireCalculator()
        assert m.state()["carriage_position"] == 0

    def test_state_multiplier_lever_initial_zero(self):
        m = MillionaireCalculator()
        assert m.state()["multiplier_lever"] == 0

    def test_state_input_initial_zero(self):
        m = MillionaireCalculator()
        assert m.state()["input"] == 0

    def test_state_updates_after_set_input(self):
        m = MillionaireCalculator()
        m.set_input(12345678)
        assert m.state()["input"] == 12345678

    def test_state_updates_after_set_lever(self):
        m = MillionaireCalculator()
        m.set_multiplier_lever(7)
        assert m.state()["multiplier_lever"] == 7

    def test_state_result_updated_after_add(self):
        m = MillionaireCalculator()
        m.add(50)
        assert m.state()["result"] == 50

    def test_state_counter_updated_after_multiply(self):
        m = MillionaireCalculator()
        m.multiply(99, 99)
        assert m.state()["counter"] == 2  # two-digit multiplier


# ---------------------------------------------------------------------------
# Carriage extended
# ---------------------------------------------------------------------------


class TestCarriageExtended:
    """shift_carriage(): absolute not relative, product shifting, max boundary."""

    def test_shift_sets_absolute_position(self):
        m = MillionaireCalculator()
        m.shift_carriage(3)
        m.shift_carriage(2)  # Should set to 2, not 5
        assert m.carriage_position == 2

    def test_shift_to_max_valid(self):
        m = MillionaireCalculator()
        max_pos = MillionaireCalculator.RESULT_DIGITS - MillionaireCalculator.INPUT_DIGITS
        m.shift_carriage(max_pos)
        assert m.carriage_position == max_pos

    def test_shift_beyond_max_raises(self):
        m = MillionaireCalculator()
        max_pos = MillionaireCalculator.RESULT_DIGITS - MillionaireCalculator.INPUT_DIGITS
        with pytest.raises(ValueError):
            m.shift_carriage(max_pos + 1)

    def test_shift_then_turn_shifts_product(self):
        # Input=1, lever=1, carriage=2 -> result 100 (1 * 10^2)
        m = MillionaireCalculator()
        m.set_input(1)
        m.shift_carriage(2)
        m.set_multiplier_lever(1)
        m.turn_crank()
        assert m.get_result() == 100

    def test_shift_negative_raises(self):
        m = MillionaireCalculator()
        with pytest.raises(ValueError):
            m.shift_carriage(-1)
