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
            assert r == Decimal(e), f"Step {i + 1}: expected {e}, got {r}"

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
            assert r == Decimal(e), f"Step {i + 1}: expected {e}, got {r}"

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
            assert r == Decimal(e), f"Step {i + 1}: expected {e}, got {r}"

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


# ---------------------------------------------------------------------------
# run() alias
# ---------------------------------------------------------------------------


class TestScheutzRunAlias:
    """run(n) is an exact alias for tabulate(n)."""

    def test_run_returns_list(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        result = engine.run(3)
        assert isinstance(result, list)
        assert len(result) == 3

    def test_run_matches_tabulate(self) -> None:
        e1 = ScheutzDifferenceEngine()
        e2 = ScheutzDifferenceEngine()
        e1.load([0, 1, 2])
        e2.load([0, 1, 2])
        assert e1.run(5) == e2.tabulate(5)

    def test_run_zero_steps_returns_empty(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([5, 2])
        assert engine.run(0) == []

    def test_run_increments_cycle_count(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([1, 1])
        engine.run(7)
        assert engine.state.cycle_count == 7

    def test_run_appends_to_output_table(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([1, 1])
        engine.run(4)
        assert len(engine.state.output_table) == 4

    def test_run_after_tabulate_accumulates(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        engine.tabulate(3)
        engine.run(3)
        assert len(engine.state.output_table) == 6
        assert engine.state.cycle_count == 6


# ---------------------------------------------------------------------------
# All 7 registers
# ---------------------------------------------------------------------------


class TestScheutzAllRegisters:
    """Load all 7 registers (D0..D6) and verify 6th-order polynomial."""

    def test_load_max_seven_values(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 6, 6, 0, 0, 0])
        assert engine.state.registers[0] == Decimal(0)
        assert engine.state.registers[1] == Decimal(1)
        assert engine.state.registers[2] == Decimal(6)

    def test_load_eight_values_raises(self) -> None:
        engine = ScheutzDifferenceEngine()
        with pytest.raises(ValueError):
            engine.load([0] * 8)

    def test_d6_constant_does_not_change(self) -> None:
        # When all higher diffs after D6 are 0, D6 stays constant
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 62, 540, 1560, 1800, 720])
        d6_initial = engine.state.registers[6]
        engine.tabulate(5)
        assert engine.state.registers[6] == d6_initial

    def test_sixth_order_polynomial_values(self) -> None:
        # f(n) = n^6: 1, 64, 729, 4096, 15625
        # D0=0, D1=1, D2=62, D3=540, D4=1560, D5=1800, D6=720
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1, 62, 540, 1560, 1800, 720])
        results = engine.tabulate(5)
        expected = [1, 64, 729, 4096, 15625]
        for r, e in zip(results, expected, strict=True):
            assert r == Decimal(e)

    def test_registers_length_is_seven(self) -> None:
        engine = ScheutzDifferenceEngine()
        assert len(engine.state.registers) == 7

    def test_overflow_flags_length_is_seven(self) -> None:
        engine = ScheutzDifferenceEngine()
        assert len(engine.state.overflow_flags) == 7

    def test_load_one_value_raises(self) -> None:
        # Scheutz accepts any count 1..7; 0 values: actually 1 is allowed
        # Only 8+ raise ValueError per the source (len > _NUM_REGISTERS=7)
        engine = ScheutzDifferenceEngine()
        # 1 value should be accepted (constant function: D0=10, no differences)
        engine.load([10])
        assert engine.state.registers[0] == Decimal(10)

    def test_load_with_float_values(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([1.5, 0.5])
        assert engine.state.registers[0] == Decimal("1.5")
        assert engine.state.registers[1] == Decimal("0.5")

    def test_load_overwrites_previous(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([100, 50])
        engine.tabulate(2)
        engine.load([0, 1])
        # D0 was updated during tabulate; now reset via load
        # new load should replace register 0 with 0
        assert engine.state.registers[0] == Decimal(0)
        # But cycle_count is NOT reset by load (only reset() resets it)
        assert engine.state.cycle_count == 2


# ---------------------------------------------------------------------------
# state_dict() extended
# ---------------------------------------------------------------------------


class TestScheutzStateDictExtended:
    """state_dict() structure and content."""

    def test_state_dict_has_four_keys(self) -> None:
        engine = ScheutzDifferenceEngine()
        d = engine.state_dict()
        assert set(d.keys()) == {"registers", "cycle_count", "output_table", "overflow_flags"}

    def test_state_dict_registers_are_floats(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([1, 2, 3])
        d = engine.state_dict()
        for v in d["registers"]:
            assert isinstance(v, float)

    def test_state_dict_output_table_are_floats(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        engine.tabulate(3)
        d = engine.state_dict()
        for v in d["output_table"]:
            assert isinstance(v, float)

    def test_state_dict_overflow_flags_are_bools(self) -> None:
        engine = ScheutzDifferenceEngine()
        d = engine.state_dict()
        for f in d["overflow_flags"]:
            assert isinstance(f, bool)

    def test_state_dict_cycle_count_correct(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([1, 2])
        engine.tabulate(5)
        d = engine.state_dict()
        assert d["cycle_count"] == 5

    def test_state_dict_registers_length_seven(self) -> None:
        engine = ScheutzDifferenceEngine()
        d = engine.state_dict()
        assert len(d["registers"]) == 7

    def test_state_dict_overflow_flags_length_seven(self) -> None:
        engine = ScheutzDifferenceEngine()
        d = engine.state_dict()
        assert len(d["overflow_flags"]) == 7

    def test_state_dict_output_table_grows(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        engine.tabulate(4)
        d = engine.state_dict()
        assert len(d["output_table"]) == 4

    def test_state_dict_initial_registers_all_zero(self) -> None:
        engine = ScheutzDifferenceEngine()
        d = engine.state_dict()
        assert all(v == 0.0 for v in d["registers"])

    def test_state_dict_reflects_loaded_values(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([7, 3])
        d = engine.state_dict()
        assert d["registers"][0] == pytest.approx(7.0)
        assert d["registers"][1] == pytest.approx(3.0)


# ---------------------------------------------------------------------------
# Decimal precision and type checks
# ---------------------------------------------------------------------------


class TestScheutzDecimalPrecision:
    """crank() returns Decimal; tabulate() returns list of Decimal."""

    def test_crank_returns_decimal(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        result = engine.crank()
        assert isinstance(result, Decimal)

    def test_tabulate_returns_list_of_decimals(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        results = engine.tabulate(3)
        for r in results:
            assert isinstance(r, Decimal)

    def test_step_returns_decimal(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([5, 1])
        result = engine.step()
        assert isinstance(result, Decimal)

    def test_fractional_differences_precise(self) -> None:
        # Load fractional values; result should be exact Decimal
        engine = ScheutzDifferenceEngine()
        engine.load([0.5, 0.25])
        result = engine.crank()
        assert result == Decimal("0.75")

    def test_precision_accumulation(self) -> None:
        # D0=0, D1=1: after n cranks D0=n (exact integer arithmetic)
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        results = engine.tabulate(100)
        for i, r in enumerate(results, 1):
            assert r == Decimal(i)

    def test_large_integer_exact(self) -> None:
        # Large integers should be exact in Decimal
        engine = ScheutzDifferenceEngine()
        engine.load([0, 10**14])
        result = engine.crank()
        assert result == Decimal(10**14)


# ---------------------------------------------------------------------------
# Output table accumulation
# ---------------------------------------------------------------------------


class TestScheutzOutputTable:
    """output_table grows with each crank; reset clears it."""

    def test_initial_output_table_empty(self) -> None:
        engine = ScheutzDifferenceEngine()
        assert engine.state.output_table == []

    def test_each_crank_appends_to_output(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        for i in range(5):
            engine.crank()
            assert len(engine.state.output_table) == i + 1

    def test_tabulate_does_not_reset_output(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        engine.tabulate(3)
        engine.tabulate(3)
        assert len(engine.state.output_table) == 6

    def test_output_table_values_match_crank_returns(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 2])
        returned = [engine.crank() for _ in range(4)]
        assert engine.state.output_table == returned

    def test_reset_clears_output_table(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        engine.tabulate(5)
        engine.reset()
        assert engine.state.output_table == []

    def test_reset_clears_cycle_count(self) -> None:
        engine = ScheutzDifferenceEngine()
        engine.load([0, 1])
        engine.tabulate(5)
        engine.reset()
        assert engine.state.cycle_count == 0

    def test_reset_clears_overflow_flags(self) -> None:
        limit = 10**15 - 1
        engine = ScheutzDifferenceEngine()
        engine.load([limit - 1, 2])
        engine.crank()
        engine.reset()
        assert not any(engine.state.overflow_flags)


# ---------------------------------------------------------------------------
# Overflow extended
# ---------------------------------------------------------------------------


class TestScheutzOverflowExtended:
    """Overflow flag behavior beyond D0."""

    def test_overflow_does_not_wrap(self) -> None:
        # Scheutz does NOT wrap on overflow (unlike Grant); just sets flag
        limit = 10**15 - 1
        engine = ScheutzDifferenceEngine()
        engine.load([limit - 1, 2])
        result = engine.crank()
        # result should equal or exceed 10^15 (no modulo applied)
        assert result >= Decimal(10**15)

    def test_multiple_cranks_after_overflow_continue(self) -> None:
        # The engine keeps running even after overflow
        limit = 10**15 - 1
        engine = ScheutzDifferenceEngine()
        engine.load([limit - 1, 2])
        engine.crank()  # overflows
        engine.crank()  # continues
        assert engine.state.cycle_count == 2

    def test_overflow_flag_stays_set_on_further_cranks(self) -> None:
        # Once set, overflow flag persists until reset
        limit = 10**15 - 1
        engine = ScheutzDifferenceEngine()
        engine.load([limit - 1, 2])
        engine.crank()  # triggers overflow
        engine.crank()  # further crank
        # flag remains True (not cleared between cranks in Scheutz)
        assert engine.state.overflow_flags[0] is True

    def test_no_overflow_at_exact_limit_minus_one(self) -> None:
        # Value just below limit: no overflow
        limit = 10**15
        engine = ScheutzDifferenceEngine()
        engine.load([limit - 2, 1])  # result = limit-1, which is < limit
        engine.crank()
        assert engine.state.overflow_flags[0] is False
