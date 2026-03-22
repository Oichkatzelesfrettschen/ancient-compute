"""Hypothesis property-based tests for core arithmetic invariants.

Covers four historical arithmetic machines:
  BabbageNumber          -- 50-digit fixed-point type (types.py)
  PascalineEmulator      -- 9's-complement sautoir-carry adder
  LeibnizReckonerEmulator -- stepped-drum multiplier/divider
  ThomasArithmometer     -- pinwheel multiplier/divider

WHY property tests rather than examples: each machine's arithmetic must
satisfy algebraic laws (commutativity, identity, dividend = q*d + r) for
ALL inputs in the valid range, not just the handful checked by unit tests.
Hypothesis generates hundreds of counter-examples automatically.
"""

from __future__ import annotations

import pytest
from hypothesis import assume, given, settings
from hypothesis import strategies as st

from backend.src.emulator.leibniz_reckoner import LeibnizReckonerEmulator
from backend.src.emulator.pascaline import PascalineEmulator
from backend.src.emulator.thomas_arithmometer import ThomasArithmometer
from backend.src.emulator.types import BabbageNumber

# ---------------------------------------------------------------------------
# Shared strategies
# ---------------------------------------------------------------------------

_BABBAGE = st.integers(-999_999, 999_999)  # well within 50-digit range
_SMALL = st.integers(0, 999)
_TINY = st.integers(0, 99)
_POS = st.integers(1, 999)
_POS_TINY = st.integers(1, 99)


# ---------------------------------------------------------------------------
# BabbageNumber arithmetic invariants
# ---------------------------------------------------------------------------


class TestBabbageNumberProperties:
    """Algebraic laws that must hold for all inputs in the valid range."""

    @given(_BABBAGE)
    def test_round_trip_integer(self, n: int) -> None:
        """BabbageNumber(n).to_decimal() recovers n exactly for integers."""
        assert BabbageNumber(n).to_decimal() == pytest.approx(float(n), abs=1e-9)

    @given(_BABBAGE)
    def test_additive_identity(self, a: int) -> None:
        """a + 0 == a."""
        assert (BabbageNumber(a) + BabbageNumber(0)).value == BabbageNumber(a).value

    @given(_BABBAGE)
    def test_additive_inverse(self, a: int) -> None:
        """a - a == 0."""
        assert (BabbageNumber(a) - BabbageNumber(a)).value == 0

    @given(_BABBAGE, _BABBAGE)
    def test_addition_commutativity(self, a: int, b: int) -> None:
        """a + b == b + a."""
        assert (BabbageNumber(a) + BabbageNumber(b)).value == (
            BabbageNumber(b) + BabbageNumber(a)
        ).value

    @given(_BABBAGE, _BABBAGE, _BABBAGE)
    @settings(max_examples=50)
    def test_addition_associativity(self, a: int, b: int, c: int) -> None:
        """(a + b) + c == a + (b + c)."""
        lhs = (BabbageNumber(a) + BabbageNumber(b)) + BabbageNumber(c)
        rhs = BabbageNumber(a) + (BabbageNumber(b) + BabbageNumber(c))
        assert lhs.value == rhs.value

    @given(_BABBAGE, _BABBAGE)
    def test_subtraction_recovers_original(self, a: int, b: int) -> None:
        """(a - b) + b == a."""
        result = (BabbageNumber(a) - BabbageNumber(b)) + BabbageNumber(b)
        assert result.value == BabbageNumber(a).value

    @given(_BABBAGE)
    def test_multiplicative_identity(self, a: int) -> None:
        """a * 1 == a."""
        assert (BabbageNumber(a) * BabbageNumber(1)).value == BabbageNumber(a).value

    @given(_BABBAGE)
    def test_multiplicative_zero(self, a: int) -> None:
        """a * 0 == 0."""
        assert (BabbageNumber(a) * BabbageNumber(0)).value == 0

    @given(_BABBAGE, _BABBAGE)
    def test_multiplication_commutativity(self, a: int, b: int) -> None:
        """a * b == b * a."""
        assert (BabbageNumber(a) * BabbageNumber(b)).value == (
            BabbageNumber(b) * BabbageNumber(a)
        ).value

    @given(st.integers(1, 999), st.integers(1, 999))
    def test_multiplication_correct_for_integers(self, a: int, b: int) -> None:
        """BabbageNumber(a) * BabbageNumber(b) produces the correct product."""
        result = BabbageNumber(a) * BabbageNumber(b)
        assert result.to_decimal() == pytest.approx(float(a * b), rel=1e-9)

    @given(st.integers(1, 999), _POS)
    def test_division_undoes_multiplication(self, a: int, b: int) -> None:
        """(a * b) / b == a for exact integer inputs."""
        product = BabbageNumber(a * b)
        quotient = product / BabbageNumber(b)
        assert quotient.to_decimal() == pytest.approx(float(a), rel=1e-9)

    @given(_BABBAGE, _POS)
    def test_division_by_nonzero_never_raises(self, a: int, b: int) -> None:
        """Dividing by any nonzero value never raises an unexpected exception."""
        result = BabbageNumber(a) / BabbageNumber(b)
        assert isinstance(result, BabbageNumber)

    @given(_BABBAGE)
    def test_division_by_zero_raises(self, a: int) -> None:
        """Division by zero always raises ZeroDivisionError."""
        with pytest.raises(ZeroDivisionError):
            _ = BabbageNumber(a) / BabbageNumber(0)

    @given(_BABBAGE, _BABBAGE)
    def test_distributivity(self, a: int, b: int) -> None:
        """a * (b + 1) == a*b + a  (partial distributivity test)."""
        one = BabbageNumber(1)
        lhs = BabbageNumber(a) * (BabbageNumber(b) + one)
        rhs = BabbageNumber(a) * BabbageNumber(b) + BabbageNumber(a) * one
        assert lhs.value == rhs.value


# ---------------------------------------------------------------------------
# PascalineEmulator arithmetic invariants
# ---------------------------------------------------------------------------


class TestPascalineProperties:
    """Algebraic laws for the Pascaline 9's-complement adder.

    Machine constraints (8-digit, max 99_999_999):
    - subtract() requires set_nines_complement_mode(True) first.
    - a - a produces 99_999_999 (no end-around carry when minuend == subtrahend).
      This is a physical limitation, not a software bug.
    - subtract(b) from a gives the correct a-b only when a > b.
    """

    @given(st.integers(0, 99_999_999))
    def test_add_from_zero_returns_operand(self, a: int) -> None:
        """Fresh machine: add(a) == a."""
        p = PascalineEmulator()
        assert p.add(a) == a

    @given(st.integers(0, 9_999), st.integers(0, 9_999))
    def test_add_is_cumulative(self, a: int, b: int) -> None:
        """add(a) then add(b) returns a + b for small operands."""
        assume(a + b < 10**8)
        p = PascalineEmulator()
        p.add(a)
        assert p.add(b) == a + b

    @given(st.integers(0, 9_999), st.integers(0, 9_999))
    def test_add_commutativity(self, a: int, b: int) -> None:
        """The order of two additions does not affect the final sum."""
        assume(a + b < 10**8)
        p1 = PascalineEmulator()
        p1.add(a)
        p1.add(b)
        p2 = PascalineEmulator()
        p2.add(b)
        p2.add(a)
        assert p1.get_value() == p2.get_value()

    @given(st.integers(0, 9_999_999))
    def test_reset_clears_to_zero(self, a: int) -> None:
        """After add then reset, the accumulator is 0."""
        p = PascalineEmulator()
        p.add(a)
        p.reset()
        assert p.get_value() == 0

    @given(st.integers(2, 9_999), st.integers(1, 9_999))
    def test_subtract_strict_gives_difference(self, a: int, b: int) -> None:
        """subtract(b) from a where a > b gives a - b exactly."""
        assume(a > b)
        p = PascalineEmulator()
        p.add(a)
        p.set_nines_complement_mode(True)
        assert p.subtract(b) == a - b

    @given(st.integers(1, 9_999_999))
    def test_subtract_zero_is_noop(self, a: int) -> None:
        """subtract(0) leaves the accumulator unchanged when a >= 1."""
        p = PascalineEmulator()
        p.add(a)
        p.set_nines_complement_mode(True)
        assert p.subtract(0) == a


# ---------------------------------------------------------------------------
# LeibnizReckonerEmulator arithmetic invariants
# ---------------------------------------------------------------------------


class TestLeibnizReckonerProperties:
    """Algebraic laws for the stepped-drum multiply/divide machine.

    Operand bounds chosen to stay within the 16-digit accumulator:
    - multiply: a, b in [0, 99] -> product <= 9801 (well within bounds)
    - divide: a in [0, 999], b in [1, 999]
    """

    @given(_TINY, _TINY)
    def test_multiply_correctness(self, a: int, b: int) -> None:
        """multiply(a, b) == a * b."""
        lr = LeibnizReckonerEmulator()
        assert lr.multiply(a, b) == a * b

    @given(_TINY, _TINY)
    def test_multiply_commutativity(self, a: int, b: int) -> None:
        """multiply(a, b) == multiply(b, a)."""
        lr = LeibnizReckonerEmulator()
        assert lr.multiply(a, b) == lr.multiply(b, a)

    @given(_TINY)
    def test_multiply_by_zero(self, a: int) -> None:
        """a * 0 == 0."""
        lr = LeibnizReckonerEmulator()
        assert lr.multiply(a, 0) == 0

    @given(_TINY)
    def test_multiply_by_one(self, a: int) -> None:
        """a * 1 == a."""
        lr = LeibnizReckonerEmulator()
        assert lr.multiply(a, 1) == a

    @given(_SMALL, _POS)
    def test_divide_quotient_remainder_identity(self, a: int, b: int) -> None:
        """q * b + r == a  for all dividends and nonzero divisors."""
        lr = LeibnizReckonerEmulator()
        q, r = lr.divide(a, b)
        assert q * b + r == a

    @given(st.integers(0, 99), _POS_TINY)
    def test_divide_exact_has_zero_remainder(self, q_exp: int, b: int) -> None:
        """divide(q * b, b) == (q, 0) for exact division."""
        a = q_exp * b
        lr = LeibnizReckonerEmulator()
        q, r = lr.divide(a, b)
        assert r == 0
        assert q == q_exp

    @given(_SMALL, _POS)
    def test_divide_remainder_less_than_divisor(self, a: int, b: int) -> None:
        """0 <= remainder < divisor always."""
        lr = LeibnizReckonerEmulator()
        _, r = lr.divide(a, b)
        assert 0 <= r < b

    @given(st.integers(1, 99), _POS_TINY)
    def test_multiply_then_divide_roundtrip(self, a: int, b: int) -> None:
        """divide(a * b, b) == (a, 0): multiply and divide are inverse operations."""
        assume(a * b < 10**7)
        lr = LeibnizReckonerEmulator()
        product = lr.multiply(a, b)
        q, r = lr.divide(product, b)
        assert q == a
        assert r == 0


# ---------------------------------------------------------------------------
# ThomasArithmometer arithmetic invariants
# ---------------------------------------------------------------------------


class TestThomasArithmometerProperties:
    """Algebraic laws for the Thomas de Colmar pinwheel machine.

    Input limit: 8 digits (max 99_999_999).
    Operand bounds chosen to keep products within the 16-digit result register.
    """

    @given(_SMALL, _SMALL)
    def test_multiply_correctness(self, a: int, b: int) -> None:
        """multiply(a, b) == a * b."""
        t = ThomasArithmometer()
        assert t.multiply(a, b) == a * b

    @given(_SMALL, _SMALL)
    def test_multiply_commutativity(self, a: int, b: int) -> None:
        """multiply(a, b) == multiply(b, a)."""
        t = ThomasArithmometer()
        assert t.multiply(a, b) == t.multiply(b, a)

    @given(_SMALL)
    def test_multiply_by_zero(self, a: int) -> None:
        """a * 0 == 0."""
        t = ThomasArithmometer()
        assert t.multiply(a, 0) == 0

    @given(_SMALL)
    def test_multiply_by_one(self, a: int) -> None:
        """a * 1 == a."""
        t = ThomasArithmometer()
        assert t.multiply(a, 1) == a

    @given(st.integers(0, 9_999), _POS)
    def test_divide_quotient_remainder_identity(self, a: int, b: int) -> None:
        """q * b + r == a  for all valid inputs."""
        t = ThomasArithmometer()
        q, r = t.divide(a, b)
        assert q * b + r == a

    @given(st.integers(0, 99), _POS_TINY)
    def test_divide_exact_has_zero_remainder(self, q_exp: int, b: int) -> None:
        """divide(q * b, b) == (q, 0) for exact division."""
        a = q_exp * b
        t = ThomasArithmometer()
        q, r = t.divide(a, b)
        assert r == 0
        assert q == q_exp

    @given(st.integers(0, 9_999), _POS)
    def test_divide_remainder_less_than_divisor(self, a: int, b: int) -> None:
        """0 <= remainder < divisor always."""
        t = ThomasArithmometer()
        _, r = t.divide(a, b)
        assert 0 <= r < b

    @given(st.integers(1, 999))
    def test_add_from_zero_gives_operand(self, a: int) -> None:
        """After clear, a single add(a) returns a."""
        t = ThomasArithmometer()
        t.clear_result()
        t.clear_counter()
        assert t.add(a) == a

    @given(st.integers(1, 99), _POS_TINY)
    def test_multiply_then_divide_roundtrip(self, a: int, b: int) -> None:
        """divide(a * b, b) == (a, 0): exact round-trip."""
        assume(a * b < 10**7)
        t = ThomasArithmometer()
        product = t.multiply(a, b)
        q, r = t.divide(product, b)
        assert q == a
        assert r == 0


# ---------------------------------------------------------------------------
# Additional property tests
# ---------------------------------------------------------------------------

_POS_SMALL = st.integers(1, 1000)
_NONNEG_SMALL = st.integers(0, 1000)


class TestBabbageNumberHypothesisExtra:
    """Additional BabbageNumber property invariants."""

    @given(_NONNEG_SMALL)
    def test_zero_is_additive_identity(self, n: int) -> None:
        a = BabbageNumber(n)
        z = BabbageNumber(0)
        assert (a + z).to_decimal() == a.to_decimal()

    @given(_POS_SMALL)
    def test_value_minus_itself_is_zero(self, n: int) -> None:
        a = BabbageNumber(n)
        result = a - a
        assert result.to_decimal() == 0

    @given(_POS_SMALL, _POS_SMALL)
    def test_product_ge_larger_factor(self, a: int, b: int) -> None:
        """a * b >= max(a, b) for positive a, b >= 1."""
        ba = BabbageNumber(a)
        bb = BabbageNumber(b)
        product = ba * bb
        assert product.to_decimal() >= max(a, b)


class TestPascalineHypothesisExtra:
    """Additional Pascaline invariants."""

    @given(st.integers(2, 1000), st.integers(1, 999))
    def test_add_then_subtract_smaller_gives_difference(self, a: int, b: int) -> None:
        """add(a) then subtract(b) gives a - b when a > b."""
        assume(a > b)
        p = PascalineEmulator()
        p.add(a)
        p.set_nines_complement_mode(True)
        result = p.subtract(b)
        assert result == a - b

    @given(st.integers(1, 9999))
    def test_add_result_bounded_by_max(self, n: int) -> None:
        p = PascalineEmulator()
        p.add(n)
        max_val = 10**8 - 1  # 8-digit machine
        assert p.get_value() <= max_val


class TestLeibnizHypothesisExtra:
    """Additional Leibniz Reckoner invariants."""

    @given(_POS_SMALL, _POS_SMALL)
    def test_multiply_two_values_correct(self, a: int, b: int) -> None:
        """multiply(a, b) == a * b for small values."""
        assume(a * b < 10**6)
        m = LeibnizReckonerEmulator()
        assert m.multiply(a, b) == a * b

    @given(_POS_SMALL)
    def test_reset_gives_zero_accumulator(self, a: int) -> None:
        """After set_input + crank_turn + reset, accumulator is 0."""
        m = LeibnizReckonerEmulator()
        m.set_input(a)
        m.crank_turn()
        m.reset()
        assert m.get_accumulator_value() == 0


_NON_ZERO = st.integers(1, 50)
_INT_SMALL = st.integers(0, 50)


class TestBabbageNumberHypothesisExtra:
    """Additional BabbageNumber algebraic properties."""

    @given(_INT_SMALL)
    def test_value_roundtrip_via_decimal(self, n: int) -> None:
        b = BabbageNumber(n)
        assert float(b.to_decimal()) == float(n)

    @given(_NON_ZERO)
    def test_sub_zero_is_identity(self, n: int) -> None:
        b = BabbageNumber(n)
        zero = BabbageNumber(0)
        assert float((b - zero).to_decimal()) == float(n)

    @given(_NON_ZERO, _NON_ZERO)
    def test_subtraction_reverses_addition(self, a: int, b: int) -> None:
        ba = BabbageNumber(a)
        bb = BabbageNumber(b)
        result = (ba + bb) - bb
        assert float(result.to_decimal()) == float(a)


class TestThomasArithHypothesisExtra:
    """Additional Thomas Arithmometer property tests."""

    @given(_NON_ZERO)
    def test_multiply_by_one_is_identity(self, a: int) -> None:
        t = ThomasArithmometer()
        assert t.multiply(a, 1) == a

    @given(_NON_ZERO)
    def test_multiply_by_zero_is_zero(self, a: int) -> None:
        t = ThomasArithmometer()
        assert t.multiply(a, 0) == 0

    @given(_NON_ZERO, _NON_ZERO)
    def test_divide_quotient_satisfies_dividend_equation(self, a: int, b: int) -> None:
        t = ThomasArithmometer()
        q, r = t.divide(a, b)
        assert q * b + r == a
