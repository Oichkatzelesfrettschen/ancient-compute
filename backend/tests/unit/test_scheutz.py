"""Tests for the Scheutz Difference Engine emulator."""

from decimal import Decimal

import pytest

from backend.src.emulator.scheutz import ScheutzDifferenceEngine


class TestScheutzInit:
    def test_initial_state_zero(self):
        engine = ScheutzDifferenceEngine()
        assert all(r == Decimal(0) for r in engine.state.registers)

    def test_reset_clears_state(self):
        engine = ScheutzDifferenceEngine()
        engine.load([1, 1])
        engine.crank()
        engine.reset()
        assert all(r == Decimal(0) for r in engine.state.registers)
        assert engine.state.cycle_count == 0

    def test_load_values(self):
        engine = ScheutzDifferenceEngine()
        engine.load([5, 2, 0])
        assert engine.state.registers[0] == Decimal(5)
        assert engine.state.registers[1] == Decimal(2)

    def test_load_too_many_values_raises(self):
        engine = ScheutzDifferenceEngine()
        with pytest.raises(ValueError):
            engine.load([1] * 8)


class TestScheutzLinear:
    def test_constant_sequence(self):
        # D0=10, D1=0: should produce [10, 10, 10, ...]
        engine = ScheutzDifferenceEngine()
        engine.load([10, 0])
        results = engine.tabulate(3)
        assert all(r == Decimal(10) for r in results)

    def test_arithmetic_sequence(self):
        # D0=0, D1=1: should produce [1, 2, 3, 4, 5]
        # (after first crank D0 becomes 0+1=1, then 1+1=2, etc.)
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        results = engine.tabulate(5)
        for i, r in enumerate(results, 1):
            assert r == Decimal(i), f"Expected {i}, got {r}"

    def test_arithmetic_sequence_nonzero_start(self):
        # D0=5, D1=3: produces 5+3=8, 8+3=11, 11+3=14
        engine = ScheutzDifferenceEngine()
        engine.load([5, 3])
        results = engine.tabulate(3)
        assert results[0] == Decimal(8)
        assert results[1] == Decimal(11)
        assert results[2] == Decimal(14)


class TestScheutzQuadratic:
    def test_squares_sequence(self):
        # Tabulating n^2: f(1)=1, f(2)=4, f(3)=9, f(4)=16, f(5)=25
        # Initial differences for n^2 starting at n=0:
        # D0=0 (f(0)=0), D1=1 (first diff at n=0), D2=2 (second diff)
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 2])
        results = engine.tabulate(5)
        # After n cranks, D0 should be n^2
        expected = [1, 4, 9, 16, 25]
        for i, (r, e) in enumerate(zip(results, expected, strict=True)):
            assert r == Decimal(e), f"Step {i+1}: expected {e}, got {r}"

    def test_triangular_numbers(self):
        # T(n) = n*(n+1)/2: 1,3,6,10,15
        # D0=0, D1=1, D2=1 (second difference of triangular numbers = 1)
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 1])
        results = engine.tabulate(5)
        expected = [1, 3, 6, 10, 15]
        for r, e in zip(results, expected, strict=True):
            assert r == Decimal(e)


class TestScheutzHigherOrder:
    def test_cubic_polynomial(self):
        # f(n) = n^3: 1, 8, 27, 64, 125
        # Third differences of n^3 = 6 (constant)
        # D0=0, D1=1, D2=6, D3=6
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 6, 6])
        results = engine.tabulate(5)
        expected = [1, 8, 27, 64, 125]
        for r, e in zip(results, expected, strict=True):
            assert r == Decimal(e), f"Expected {e}, got {r}"

    def test_cycle_count_increments(self):
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        engine.tabulate(5)
        assert engine.state.cycle_count == 5

    def test_output_table_populated(self):
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        engine.tabulate(3)
        assert len(engine.state.output_table) >= 3

    def test_state_dict(self):
        engine = ScheutzDifferenceEngine()
        engine.load([1, 2])
        engine.crank()
        d = engine.state_dict()
        assert "registers" in d
        assert "cycle_count" in d
        assert d["cycle_count"] == 1

    def test_step_alias(self):
        engine = ScheutzDifferenceEngine()
        engine.load([0, 2])
        val = engine.step()
        assert val == Decimal(2)


class TestScheutzNegativeDifferences:
    """Negative initial values and differences propagate correctly."""

    def test_negative_constant(self):
        # D0=-5, D1=0: every output is -5
        engine = ScheutzDifferenceEngine()
        engine.load([-5, 0])
        results = engine.tabulate(3)
        assert all(r == Decimal(-5) for r in results)

    def test_decreasing_sequence(self):
        # D0=10, D1=-1: produces 9, 8, 7, 6, 5
        engine = ScheutzDifferenceEngine()
        engine.load([10, -1])
        results = engine.tabulate(5)
        expected = [9, 8, 7, 6, 5]
        for r, e in zip(results, expected, strict=True):
            assert r == Decimal(e)

    def test_negative_quadratic(self):
        # f(n) = -n^2: output -1, -4, -9
        # D0=0, D1=-1, D2=-2
        engine = ScheutzDifferenceEngine()
        engine.load([0, -1, -2])
        results = engine.tabulate(3)
        assert results[0] == Decimal(-1)
        assert results[1] == Decimal(-4)
        assert results[2] == Decimal(-9)

    def test_sign_change_across_zero(self):
        # D0=3, D1=-2: produces 1, -1, -3 (crosses zero)
        engine = ScheutzDifferenceEngine()
        engine.load([3, -2])
        results = engine.tabulate(3)
        assert results[0] == Decimal(1)
        assert results[1] == Decimal(-1)
        assert results[2] == Decimal(-3)


class TestScheutzHigherOrderPolynomials:
    """5th and 6th order polynomial tabulation."""

    def test_fifth_order_polynomial(self):
        # f(n) = n^5: 1, 32, 243, 1024, 3125
        # 5th differences of n^5 = 120 (constant).
        # Initial differences at n=0:
        #   D0=0, D1=1, D2=30, D3=150, D4=240, D5=120
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 30, 150, 240, 120])
        results = engine.tabulate(5)
        expected = [1, 32, 243, 1024, 3125]
        for i, (r, e) in enumerate(zip(results, expected, strict=True)):
            assert r == Decimal(e), f"Step {i+1}: expected {e}, got {r}"

    def test_sixth_order_polynomial(self):
        # f(n) = n^6: 1, 64, 729, 4096, 15625
        # 6th differences of n^6 = 720 (constant).
        # Initial differences at n=0:
        #   D0=0, D1=1, D2=62, D3=540, D4=1560, D5=1800, D6=720
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 62, 540, 1560, 1800, 720])
        results = engine.tabulate(5)
        expected = [1, 64, 729, 4096, 15625]
        for i, (r, e) in enumerate(zip(results, expected, strict=True)):
            assert r == Decimal(e), f"Step {i+1}: expected {e}, got {r}"

    def test_reset_between_tabulations(self):
        # After reset, a fresh polynomial starts from zero
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 2])
        engine.tabulate(3)
        engine.reset()
        engine.load([0, 1])
        results = engine.tabulate(3)
        assert results == [Decimal(1), Decimal(2), Decimal(3)]
        assert engine.state.cycle_count == 3


class TestScheutzOverflow:
    """Overflow flag is set when D0 exceeds 15-digit capacity."""

    def test_overflow_flag_set(self):
        # Load a value near the 15-digit limit, then add a large difference
        limit = 10**15 - 1
        engine = ScheutzDifferenceEngine()
        engine.load([limit - 1, 2])  # next step: limit+1 -> overflow
        engine.crank()
        assert engine.state.overflow_flags[0] is True

    def test_overflow_flag_not_set_for_normal_values(self):
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 2])
        engine.tabulate(10)
        assert not any(engine.state.overflow_flags)
