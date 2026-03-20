"""Tests for the Ludgate Analytical Machine emulator."""

import pytest

from backend.src.emulator.ludgate import _IRISH_ANTILOG, _IRISH_LOG, LudgateMachine


class TestIrishLogTable:
    def test_table_has_all_digits(self):
        assert set(_IRISH_LOG.keys()) == set(range(10))

    def test_zero_is_special(self):
        assert _IRISH_LOG[0] is None

    def test_known_values(self):
        # From Ludgate 1909, Table I
        assert _IRISH_LOG[1] == 0
        assert _IRISH_LOG[2] == 1
        assert _IRISH_LOG[3] == 7
        assert _IRISH_LOG[4] == 2
        assert _IRISH_LOG[5] == 5
        assert _IRISH_LOG[6] == 8
        assert _IRISH_LOG[7] == 3
        assert _IRISH_LOG[8] == 6
        assert _IRISH_LOG[9] == 4

    def test_antilog_is_inverse_for_small(self):
        # For single digits where product is < 10:
        # index 0 -> digit 1, verify 1's index is 0
        assert _IRISH_ANTILOG[0] == 1
        assert _IRISH_ANTILOG[1] == 2
        assert _IRISH_ANTILOG[2] == 4
        assert _IRISH_ANTILOG[3] == 8
        assert _IRISH_ANTILOG[4] == 9
        assert _IRISH_ANTILOG[5] == 5
        assert _IRISH_ANTILOG[6] == 7
        assert _IRISH_ANTILOG[7] == 3
        assert _IRISH_ANTILOG[8] == 6


class TestIrishMultiply:
    """Verify all 10x10 multiplication pairs via Irish logarithms."""

    @pytest.mark.parametrize("a,b", [(a, b) for a in range(1, 10) for b in range(1, 10)])
    def test_single_digit_multiply(self, a, b):
        m = LudgateMachine()
        assert m.irish_multiply(a, b) == a * b

    def test_multiply_by_zero(self):
        m = LudgateMachine()
        assert m.irish_multiply(0, 7) == 0
        assert m.irish_multiply(7, 0) == 0

    def test_multiply_negatives(self):
        m = LudgateMachine()
        assert m.irish_multiply(-3, 4) == -12
        assert m.irish_multiply(3, -4) == -12
        assert m.irish_multiply(-3, -4) == 12

    def test_multiply_larger_numbers(self):
        m = LudgateMachine()
        assert m.irish_multiply(12, 11) == 132
        assert m.irish_multiply(25, 4) == 100


class TestLudgateStore:
    def test_store_and_load(self):
        m = LudgateMachine()
        m.store_value(0, 42)
        assert m.load_value(0) == 42

    def test_store_all_columns(self):
        m = LudgateMachine()
        for i in range(10):
            m.store_value(i, i * 10)
        for i in range(10):
            assert m.load_value(i) == i * 10

    def test_store_overflow_raises(self):
        m = LudgateMachine()
        with pytest.raises(OverflowError):
            m.store_value(0, 10**21)

    def test_store_out_of_range_raises(self):
        m = LudgateMachine()
        with pytest.raises(IndexError):
            m.store_value(192, 1)


class TestLudgateArithmetic:
    def test_add(self):
        m = LudgateMachine()
        assert m.add(10, 5) == 15

    def test_subtract(self):
        m = LudgateMachine()
        assert m.subtract(10, 3) == 7

    def test_multiply_method(self):
        m = LudgateMachine()
        assert m.multiply(6, 7) == 42

    def test_divide(self):
        m = LudgateMachine()
        q, r = m.divide(10, 3)
        assert q == 3
        assert r == 1

    def test_divide_by_zero(self):
        m = LudgateMachine()
        with pytest.raises(ZeroDivisionError):
            m.divide(5, 0)


class TestLudgateProgram:
    def test_run_simple_program(self):
        m = LudgateMachine()
        results = m.run(
            [
                ("add", 3, 4),
                ("mult", 2, 5),
            ]
        )
        assert results[0] == 7
        assert results[1] == 10

    def test_state_dict(self):
        m = LudgateMachine()
        m.add(1, 2)
        d = m.state_dict()
        assert d["accumulator"] == 3
        assert "cycle_count" in d

    def test_reset(self):
        m = LudgateMachine()
        m.add(5, 5)
        m.reset()
        assert m.state.accumulator == 0
        assert m.state.cycle_count == 0


class TestIrishMultiplyEdgeCases:
    """Edge cases in Irish logarithm multiplication."""

    def test_multiply_nine_by_nine(self):
        # 9 * 9 = 81: combined index = 4+4 = 8, antilog[8] = 6 -> tens carry
        m = LudgateMachine()
        assert m.irish_multiply(9, 9) == 81

    def test_multiply_nine_by_eight(self):
        # 9 * 8 = 72: combined 4+6 = 10, triggers direct product path
        m = LudgateMachine()
        assert m.irish_multiply(9, 8) == 72

    def test_multiply_all_nines_two_digit(self):
        # 99 * 9 = 891
        m = LudgateMachine()
        assert m.irish_multiply(99, 9) == 891

    def test_multiply_large_both_nines(self):
        # 99 * 99 = 9801
        m = LudgateMachine()
        assert m.irish_multiply(99, 99) == 9801

    def test_multiply_zero_with_large(self):
        # 0 * 999 = 0 (all-zero short-circuit)
        m = LudgateMachine()
        assert m.irish_multiply(0, 999) == 0
        assert m.irish_multiply(999, 0) == 0

    def test_multiply_both_negative(self):
        # (-7) * (-8) = 56 (sign-magnitude: both negative -> positive)
        m = LudgateMachine()
        assert m.irish_multiply(-7, -8) == 56

    def test_multiply_identity(self):
        # a * 1 = a for multi-digit a
        m = LudgateMachine()
        assert m.irish_multiply(123, 1) == 123

    def test_multiply_commutative(self):
        # irish_multiply must be commutative for arbitrary pairs
        m = LudgateMachine()
        for a, b in [(37, 42), (99, 12), (5, 0), (0, 5)]:
            assert m.irish_multiply(a, b) == m.irish_multiply(
                b, a
            ), f"Commutativity failed for {a} * {b}"


class TestLudgateStoreEdgeCases:
    """Store boundary and negative value handling."""

    def test_store_negative_value(self):
        # Ludgate store holds signed integers (negative allowed)
        m = LudgateMachine()
        m.store_value(0, -100)
        assert m.load_value(0) == -100

    def test_store_last_column(self):
        m = LudgateMachine()
        m.store_value(191, 42)
        assert m.load_value(191) == 42

    def test_store_first_and_last(self):
        m = LudgateMachine()
        m.store_value(0, 111)
        m.store_value(191, 222)
        assert m.load_value(0) == 111
        assert m.load_value(191) == 222

    def test_divide_negative_quotient(self):
        # 10 / -3: quotient and remainder satisfy q * divisor + r == dividend
        m = LudgateMachine()
        q, r = m.divide(10, -3)
        assert q * (-3) + r == 10

    def test_subtract_to_negative(self):
        m = LudgateMachine()
        result = m.subtract(3, 10)
        assert result == -7
