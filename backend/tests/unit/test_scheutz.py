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
        for i, (r, e) in enumerate(zip(results, expected)):
            assert r == Decimal(e), f"Step {i+1}: expected {e}, got {r}"

    def test_triangular_numbers(self):
        # T(n) = n*(n+1)/2: 1,3,6,10,15
        # D0=0, D1=1, D2=1 (second difference of triangular numbers = 1)
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 1])
        results = engine.tabulate(5)
        expected = [1, 3, 6, 10, 15]
        for r, e in zip(results, expected):
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
        for r, e in zip(results, expected):
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
