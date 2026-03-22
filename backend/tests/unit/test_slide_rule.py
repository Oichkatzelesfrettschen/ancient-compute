"""Tests for slide rule emulator."""

import math

import pytest

from backend.src.emulator.adapter import SlideRuleAdapter, _parse_slide_rule_a68_output
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


class TestSlideRuleChainOps:
    """Verify chained multiply/divide operations produce consistent results."""

    def test_multiply_chain_three(self) -> None:
        emu = SlideRuleEmulator()
        r = emu.multiply(emu.multiply(2.0, 3.0), 4.0)
        assert math.isclose(r, 24.0, rel_tol=1e-9)

    def test_divide_chain_two(self) -> None:
        emu = SlideRuleEmulator()
        r = emu.divide(emu.divide(100.0, 5.0), 4.0)
        assert math.isclose(r, 5.0, rel_tol=1e-9)

    def test_multiply_then_divide_exact(self) -> None:
        emu = SlideRuleEmulator()
        for a in [2.0, 3.14, 7.0, 9.99]:
            r = emu.divide(emu.multiply(a, 5.0), 5.0)
            assert math.isclose(r, a, rel_tol=1e-9)

    def test_divide_then_multiply_exact(self) -> None:
        emu = SlideRuleEmulator()
        for b in [2.0, 4.0, 6.28]:
            r = emu.multiply(emu.divide(b, 3.0), 3.0)
            assert math.isclose(r, b, rel_tol=1e-9)

    def test_multiply_by_reciprocal_gives_one(self) -> None:
        emu = SlideRuleEmulator()
        for x in [2.0, 5.0, 7.3, 100.0]:
            r = emu.multiply(x, emu.divide(1.0, x))
            assert math.isclose(r, 1.0, rel_tol=1e-9)

    def test_long_multiply_chain_five_values(self) -> None:
        emu = SlideRuleEmulator()
        # 2 * 3 * 5 * 7 * 11 = 2310
        vals = [2.0, 3.0, 5.0, 7.0, 11.0]
        result = vals[0]
        for v in vals[1:]:
            result = emu.multiply(result, v)
        assert math.isclose(result, 2310.0, rel_tol=1e-9)

    def test_divide_chain_produces_fraction(self) -> None:
        emu = SlideRuleEmulator()
        # 1 / 2 / 4 = 1/8 = 0.125
        r = emu.divide(emu.divide(1.0, 2.0), 4.0)
        assert math.isclose(r, 0.125, rel_tol=1e-9)

    def test_multiply_small_positive_fractions(self) -> None:
        emu = SlideRuleEmulator()
        r = emu.multiply(0.01, 0.01)
        assert math.isclose(r, 0.0001, rel_tol=1e-6)


class TestSlideRuleEdgeCases:
    """Precision with extreme operand magnitudes and unit operands."""

    def test_multiply_by_one_is_identity(self) -> None:
        emu = SlideRuleEmulator()
        for x in [0.001, 1.0, 100.0, 9999.0]:
            assert math.isclose(emu.multiply(x, 1.0), x, rel_tol=1e-9)

    def test_divide_by_one_is_identity(self) -> None:
        emu = SlideRuleEmulator()
        for x in [0.001, 1.0, 100.0, 9999.0]:
            assert math.isclose(emu.divide(x, 1.0), x, rel_tol=1e-9)

    def test_multiply_very_large_numbers(self) -> None:
        emu = SlideRuleEmulator()
        r = emu.multiply(1e7, 1e7)
        assert math.isclose(r, 1e14, rel_tol=1e-9)

    def test_divide_very_large_by_itself(self) -> None:
        emu = SlideRuleEmulator()
        r = emu.divide(1e12, 1e12)
        assert math.isclose(r, 1.0, rel_tol=1e-9)

    def test_multiply_very_small_numbers(self) -> None:
        emu = SlideRuleEmulator()
        r = emu.multiply(1e-5, 1e-5)
        assert math.isclose(r, 1e-10, rel_tol=1e-6)

    def test_divide_very_small(self) -> None:
        emu = SlideRuleEmulator()
        r = emu.divide(1e-3, 1e-3)
        assert math.isclose(r, 1.0, rel_tol=1e-9)

    def test_multiply_one_times_one(self) -> None:
        emu = SlideRuleEmulator()
        assert math.isclose(emu.multiply(1.0, 1.0), 1.0, rel_tol=1e-12)

    def test_divide_large_by_small_gives_large(self) -> None:
        emu = SlideRuleEmulator()
        r = emu.divide(1e6, 1e-3)
        assert math.isclose(r, 1e9, rel_tol=1e-9)

    def test_zero_numerator_raises(self) -> None:
        emu = SlideRuleEmulator()
        with pytest.raises(ValueError):
            emu.divide(0.0, 5.0)

    def test_negative_numerator_divide_raises(self) -> None:
        emu = SlideRuleEmulator()
        with pytest.raises(ValueError):
            emu.divide(-1.0, 2.0)

    def test_multiply_one_negative_raises(self) -> None:
        emu = SlideRuleEmulator()
        with pytest.raises(ValueError):
            emu.multiply(5.0, -2.0)


class TestSlideRuleAdapterALGOL68:
    """SlideRuleAdapter.execute_algol68_ops: ALGOL68 backend round-trip tests."""

    def test_multiply_six_by_seven(self) -> None:
        adapter = SlideRuleAdapter(SlideRuleEmulator())
        result = adapter.execute_algol68_ops([{"op": "multiply", "a": 6.0, "b": 7.0}])
        assert math.isclose(result["result"], 42.0, rel_tol=1e-9)

    def test_divide_forty_two_by_six(self) -> None:
        adapter = SlideRuleAdapter(SlideRuleEmulator())
        result = adapter.execute_algol68_ops([{"op": "divide", "a": 42.0, "b": 6.0}])
        assert math.isclose(result["result"], 7.0, rel_tol=1e-9)

    def test_sqrt_144(self) -> None:
        adapter = SlideRuleAdapter(SlideRuleEmulator())
        result = adapter.execute_algol68_ops([{"op": "sqrt", "a": 144.0}])
        assert math.isclose(result["result"], 12.0, rel_tol=1e-9)

    def test_power_2_to_10(self) -> None:
        adapter = SlideRuleAdapter(SlideRuleEmulator())
        result = adapter.execute_algol68_ops([{"op": "power", "a": 2.0, "n": 10}])
        assert math.isclose(result["result"], 1024.0, rel_tol=1e-9)

    def test_ops_counter_increments(self) -> None:
        adapter = SlideRuleAdapter(SlideRuleEmulator())
        adapter.execute_algol68_ops([{"op": "multiply", "a": 2.0, "b": 3.0}])
        assert adapter._ops == 1

    def test_result_synced_to_adapter(self) -> None:
        adapter = SlideRuleAdapter(SlideRuleEmulator())
        adapter.execute_algol68_ops([{"op": "multiply", "a": 5.0, "b": 4.0}])
        assert math.isclose(adapter._result, 20.0, rel_tol=1e-9)

    def test_snapshot_reflects_result(self) -> None:
        adapter = SlideRuleAdapter(SlideRuleEmulator())
        adapter.execute_algol68_ops([{"op": "divide", "a": 100.0, "b": 10.0}])
        snap = adapter.get_snapshot()
        assert math.isclose(snap["result"], 10.0, rel_tol=1e-9)

    def test_multiple_ops_sequence(self) -> None:
        adapter = SlideRuleAdapter(SlideRuleEmulator())
        result = adapter.execute_algol68_ops([
            {"op": "multiply", "a": 3.0, "b": 4.0},
        ])
        assert math.isclose(result["result"], 12.0, rel_tol=1e-9)
        result2 = adapter.execute_algol68_ops([
            {"op": "divide", "a": result["result"], "b": 2.0},
        ])
        assert math.isclose(result2["result"], 6.0, rel_tol=1e-9)

    def test_unknown_op_is_ignored(self) -> None:
        adapter = SlideRuleAdapter(SlideRuleEmulator())
        # Should not raise; unknown op is skipped, only the quit opcode runs.
        result = adapter.execute_algol68_ops([{"op": "bogus_op"}])
        assert "result" in result

    def test_ops_returned_from_a68g(self) -> None:
        adapter = SlideRuleAdapter(SlideRuleEmulator())
        result = adapter.execute_algol68_ops([{"op": "multiply", "a": 2.0, "b": 3.0}])
        assert result["ops"] == 1


class TestParseSlideRuleA68Output:
    """Unit tests for _parse_slide_rule_a68_output."""

    def test_basic_parse(self) -> None:
        raw = "result=+4.20000000000000e  +1\nops=1\n"
        parsed = _parse_slide_rule_a68_output(raw)
        assert math.isclose(parsed["result"], 42.0, rel_tol=1e-9)
        assert parsed["ops"] == 1

    def test_empty_output_defaults(self) -> None:
        parsed = _parse_slide_rule_a68_output("")
        assert parsed["result"] == 0.0
        assert parsed["ops"] == 0

    def test_result_one(self) -> None:
        raw = "result=+1.00000000000000e  +0\nops=1\n"
        parsed = _parse_slide_rule_a68_output(raw)
        assert math.isclose(parsed["result"], 1.0, rel_tol=1e-9)

    def test_large_ops_count(self) -> None:
        raw = "result=+1.00000000000000e  +0\nops=99\n"
        parsed = _parse_slide_rule_a68_output(raw)
        assert parsed["ops"] == 99

    def test_partial_output(self) -> None:
        raw = "result=+2.00000000000000e  +0\n"
        parsed = _parse_slide_rule_a68_output(raw)
        assert math.isclose(parsed["result"], 2.0, rel_tol=1e-9)
        assert parsed["ops"] == 0
