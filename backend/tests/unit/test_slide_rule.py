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
