"""Tests for slide rule emulator."""

import math

import pytest

from backend.src.emulator.slide_rule import SlideRuleEmulator


def test_slide_rule_multiply():
    emu = SlideRuleEmulator()
    result = emu.multiply(2.0, 3.0)
    assert math.isclose(result, 6.0, rel_tol=1e-6)


def test_slide_rule_divide():
    emu = SlideRuleEmulator()
    result = emu.divide(10.0, 2.0)
    assert math.isclose(result, 5.0, rel_tol=1e-6)


def test_slide_rule_requires_positive():
    emu = SlideRuleEmulator()
    with pytest.raises(ValueError):
        emu.multiply(1.0, 0.0)


class TestSlideRuleEmulator:
    def test_multiply_identity(self) -> None:
        emu = SlideRuleEmulator()
        assert math.isclose(emu.multiply(7.0, 1.0), 7.0, rel_tol=1e-6)

    def test_multiply_fractions(self) -> None:
        emu = SlideRuleEmulator()
        assert math.isclose(emu.multiply(0.5, 4.0), 2.0, rel_tol=1e-6)

    def test_multiply_large_numbers(self) -> None:
        emu = SlideRuleEmulator()
        assert math.isclose(emu.multiply(100.0, 1000.0), 100_000.0, rel_tol=1e-6)

    def test_multiply_commutative(self) -> None:
        emu = SlideRuleEmulator()
        assert math.isclose(emu.multiply(3.7, 2.1), emu.multiply(2.1, 3.7), rel_tol=1e-9)

    def test_divide_identity(self) -> None:
        emu = SlideRuleEmulator()
        assert math.isclose(emu.divide(9.0, 1.0), 9.0, rel_tol=1e-6)

    def test_divide_self_equals_one(self) -> None:
        emu = SlideRuleEmulator()
        assert math.isclose(emu.divide(7.0, 7.0), 1.0, rel_tol=1e-9)

    def test_divide_fraction_result(self) -> None:
        emu = SlideRuleEmulator()
        assert math.isclose(emu.divide(1.0, 4.0), 0.25, rel_tol=1e-6)

    def test_divide_large_by_small(self) -> None:
        emu = SlideRuleEmulator()
        assert math.isclose(emu.divide(1000.0, 0.1), 10_000.0, rel_tol=1e-6)

    def test_multiply_then_divide_roundtrip(self) -> None:
        emu = SlideRuleEmulator()
        product = emu.multiply(6.0, 4.0)
        result = emu.divide(product, 4.0)
        assert math.isclose(result, 6.0, rel_tol=1e-9)

    def test_multiply_zero_raises(self) -> None:
        emu = SlideRuleEmulator()
        with pytest.raises(ValueError):
            emu.multiply(0.0, 5.0)

    def test_multiply_negative_raises(self) -> None:
        emu = SlideRuleEmulator()
        with pytest.raises(ValueError):
            emu.multiply(-3.0, 2.0)

    def test_divide_zero_denominator_raises(self) -> None:
        emu = SlideRuleEmulator()
        with pytest.raises(ValueError):
            emu.divide(5.0, 0.0)

    def test_divide_negative_denominator_raises(self) -> None:
        emu = SlideRuleEmulator()
        with pytest.raises(ValueError):
            emu.divide(5.0, -1.0)

    def test_multiply_small_fractions(self) -> None:
        emu = SlideRuleEmulator()
        assert math.isclose(emu.multiply(0.1, 0.1), 0.01, rel_tol=1e-6)


class TestSlideRuleEmulatorAccuracy:
    """Verify mathematical accuracy of multiply and divide operations."""

    def test_multiply_pi_e(self) -> None:
        emu = SlideRuleEmulator()
        result = emu.multiply(math.pi, math.e)
        assert math.isclose(result, math.pi * math.e, rel_tol=1e-6)

    def test_divide_pi_by_e(self) -> None:
        emu = SlideRuleEmulator()
        result = emu.divide(math.pi, math.e)
        assert math.isclose(result, math.pi / math.e, rel_tol=1e-6)

    def test_multiply_sqrt_two_squared(self) -> None:
        emu = SlideRuleEmulator()
        result = emu.multiply(math.sqrt(2), math.sqrt(2))
        assert math.isclose(result, 2.0, rel_tol=1e-6)

    def test_divide_is_inverse_of_multiply(self) -> None:
        emu = SlideRuleEmulator()
        a, b = 7.3, 2.9
        product = emu.multiply(a, b)
        assert math.isclose(emu.divide(product, b), a, rel_tol=1e-9)

    def test_chain_multiply_three_values(self) -> None:
        emu = SlideRuleEmulator()
        r1 = emu.multiply(2.0, 3.0)
        r2 = emu.multiply(r1, 4.0)
        assert math.isclose(r2, 24.0, rel_tol=1e-6)

    def test_multiply_near_unity_gives_identity(self) -> None:
        emu = SlideRuleEmulator()
        result = emu.multiply(1.0000001, 1.0000001)
        assert math.isclose(result, 1.0000002, rel_tol=1e-5)

    def test_both_operands_negative_raises(self) -> None:
        emu = SlideRuleEmulator()
        with pytest.raises(ValueError):
            emu.multiply(-1.0, -1.0)
