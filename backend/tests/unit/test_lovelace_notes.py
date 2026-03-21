"""Tests for Lovelace Notes B, C, D executable decks."""

import pytest

from backend.src.emulator.lovelace_notes import (
    run_note_b,
    run_note_b_compound,
    run_note_b_div,
    run_note_b_mult,
    run_note_c,
    run_note_c_native,
    run_note_d,
)
from backend.src.emulator.types import BabbageNumber


def _close(bn: BabbageNumber, expected: float, tol: float = 1e-6) -> bool:
    """Check BabbageNumber is close to expected float."""
    return abs(bn.to_decimal() - expected) < tol


# ---------------------------------------------------------------------------
# Note B -- Deck 1: Multiplication
# ---------------------------------------------------------------------------


class TestNoteBMult:
    def test_simple_multiply(self):
        result = run_note_b_mult(3.0, 4.0)
        assert _close(result, 12.0)

    def test_multiply_by_zero(self):
        result = run_note_b_mult(5.0, 0.0)
        assert _close(result, 0.0)

    def test_multiply_by_one(self):
        result = run_note_b_mult(7.0, 1.0)
        assert _close(result, 7.0)

    def test_multiply_fractions(self):
        result = run_note_b_mult(2.5, 4.0)
        assert _close(result, 10.0)

    def test_multiply_negatives(self):
        result = run_note_b_mult(-3.0, 4.0)
        assert _close(result, -12.0)

    def test_matches_python_arithmetic(self):
        a, b = 6.0, 7.0
        result = run_note_b_mult(a, b)
        assert _close(result, a * b)


# ---------------------------------------------------------------------------
# Note B -- Deck 2: Division
# ---------------------------------------------------------------------------


class TestNoteBDiv:
    def test_simple_divide(self):
        result = run_note_b_div(12.0, 4.0)
        assert _close(result, 3.0)

    def test_divide_by_one(self):
        result = run_note_b_div(9.0, 1.0)
        assert _close(result, 9.0)

    def test_divide_fraction(self):
        result = run_note_b_div(1.0, 4.0)
        assert _close(result, 0.25)

    def test_matches_python_arithmetic(self):
        a, b = 15.0, 3.0
        result = run_note_b_div(a, b)
        assert _close(result, a / b)


# ---------------------------------------------------------------------------
# Note B -- Deck 3: Compound expression a*b + c*d
# ---------------------------------------------------------------------------


class TestNoteBCompound:
    def test_basic(self):
        # 2*3 + 4*5 = 6 + 20 = 26
        result = run_note_b_compound(2.0, 3.0, 4.0, 5.0)
        assert _close(result, 26.0)

    def test_zeros(self):
        result = run_note_b_compound(0.0, 5.0, 3.0, 0.0)
        assert _close(result, 0.0)

    def test_ones(self):
        result = run_note_b_compound(1.0, 1.0, 1.0, 1.0)
        assert _close(result, 2.0)

    def test_matches_python_arithmetic(self):
        a, b, c, d = 3.0, 7.0, 2.0, 5.0
        result = run_note_b_compound(a, b, c, d)
        assert _close(result, a * b + c * d)

    def test_cli_interface(self):
        # run_note_b(n) uses default b=2, c=3, d=4, a=n
        result = run_note_b(1)
        assert isinstance(result, BabbageNumber)


# ---------------------------------------------------------------------------
# Note C -- Triangular numbers
# ---------------------------------------------------------------------------


class TestNoteC:
    @pytest.mark.parametrize(
        "n,expected",
        [
            (1, 1),
            (2, 3),
            (3, 6),
            (4, 10),
            (5, 15),
            (10, 55),
        ],
    )
    def test_triangular_number(self, n, expected):
        result = run_note_c(n)
        assert _close(
            result, float(expected)
        ), f"T({n}) expected {expected}, got {result.to_decimal()}"

    def test_matches_python_sum(self):
        for n in range(1, 8):
            result = run_note_c(n)
            expected = sum(range(1, n + 1))
            assert _close(result, float(expected))

    def test_zero(self):
        result = run_note_c(0)
        assert _close(result, 0.0)

    def test_returns_babbage_number(self):
        assert isinstance(run_note_c(5), BabbageNumber)

    def test_invalid_n(self):
        with pytest.raises(ValueError):
            run_note_c(-1)


# ---------------------------------------------------------------------------
# Note C -- Native AE engine execution (combination cards + backing)
# ---------------------------------------------------------------------------


class TestNoteCNative:
    """Tests for run_note_c_native() -- Note C on the live AE engine.

    WHY: The Python-level run_note_c() simulates looping externally.
    run_note_c_native() runs triangular_number.ae on the Engine with real
    JMP-based looping, proving the engine's backing / combination-card
    model executes correctly.
    """

    @pytest.mark.parametrize(
        "n,expected",
        [
            (1, 1),
            (2, 3),
            (3, 6),
            (5, 15),
            (10, 55),
        ],
    )
    def test_triangular_number_native(self, n, expected):
        result = run_note_c_native(n)
        assert _close(result, float(expected)), (
            f"T({n}) expected {expected}, got {result.to_decimal()}"
        )

    def test_matches_python_runner(self):
        """Native AE result must agree with the Python-level runner."""
        for n in range(1, 8):
            assert _close(run_note_c_native(n), run_note_c(n).to_decimal())

    def test_zero(self):
        result = run_note_c_native(0)
        assert _close(result, 0.0)

    def test_invalid_n(self):
        with pytest.raises(ValueError):
            run_note_c_native(-1)

    def test_returns_babbage_number(self):
        from backend.src.emulator.types import BabbageNumber

        assert isinstance(run_note_c_native(5), BabbageNumber)

    def test_large_n(self):
        """T(20) = 210 -- exercises multiple loop iterations."""
        result = run_note_c_native(20)
        assert _close(result, 210.0)


# ---------------------------------------------------------------------------
# Note D -- Diagram of development: (a + b*n)(a - b*n)
# ---------------------------------------------------------------------------


class TestNoteD:
    def test_ada_example_n1(self):
        # a=5, b=2, n=1: (5+2)(5-2) = 7*3 = 21
        result = run_note_d(1, a=5.0, b=2.0)
        assert _close(result, 21.0)

    def test_ada_example_n2(self):
        # a=5, b=2, n=2: (5+4)(5-4) = 9*1 = 9
        result = run_note_d(2, a=5.0, b=2.0)
        assert _close(result, 9.0)

    def test_matches_algebraic_formula(self):
        # (a+bn)(a-bn) = a^2 - b^2*n^2
        for n in range(1, 5):
            a, b = 5.0, 2.0
            result = run_note_d(n, a=a, b=b)
            expected = a**2 - (b * n) ** 2
            assert _close(
                result, expected, tol=1e-4
            ), f"n={n}: expected {expected}, got {result.to_decimal()}"

    def test_zero_n(self):
        # n=0: (a+0)(a-0) = a*a = a^2
        result = run_note_d(0, a=3.0, b=2.0)
        assert _close(result, 9.0)

    def test_returns_babbage_number(self):
        assert isinstance(run_note_d(1), BabbageNumber)

    def test_cli_interface(self):
        result = run_note_d(1)
        assert isinstance(result, BabbageNumber)
