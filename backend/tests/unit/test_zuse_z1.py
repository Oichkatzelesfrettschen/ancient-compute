"""Tests for the Zuse Z1 mechanical binary computer emulator."""

import pytest

from backend.src.emulator.zuse_z1 import ZuseFloat, ZuseZ1


def _close(a: float, b: float, tol: float = 1e-4) -> bool:
    if abs(b) < 1e-10:
        return abs(a) < tol
    return abs(a - b) / abs(b) < tol


class TestZuseFloat:
    def test_zero(self):
        zf = ZuseFloat(0)
        assert zf.to_float() == 0.0
        assert zf.is_zero()

    def test_one(self):
        zf = ZuseFloat.from_float(1.0)
        assert _close(zf.to_float(), 1.0)

    def test_negative(self):
        zf = ZuseFloat.from_float(-3.5)
        assert zf.to_float() < 0
        assert _close(abs(zf.to_float()), 3.5)

    def test_small_positive(self):
        zf = ZuseFloat.from_float(0.125)
        assert _close(zf.to_float(), 0.125)

    def test_large_positive(self):
        zf = ZuseFloat.from_float(1024.0)
        assert _close(zf.to_float(), 1024.0, tol=1e-3)

    def test_roundtrip_various(self):
        for val in [1.0, -1.0, 2.0, 0.5, 3.14, 100.0, 0.01]:
            zf = ZuseFloat.from_float(val)
            assert _close(zf.to_float(), val, tol=1e-3), f"Roundtrip failed for {val}"

    def test_equality(self):
        a = ZuseFloat.from_float(5.0)
        b = ZuseFloat.from_float(5.0)
        assert a == b

    def test_add_operator(self):
        a = ZuseFloat.from_float(3.0)
        b = ZuseFloat.from_float(2.0)
        result = a + b
        assert _close(result.to_float(), 5.0)

    def test_sub_operator(self):
        a = ZuseFloat.from_float(5.0)
        b = ZuseFloat.from_float(2.0)
        result = a - b
        assert _close(result.to_float(), 3.0)

    def test_mul_operator(self):
        a = ZuseFloat.from_float(3.0)
        b = ZuseFloat.from_float(4.0)
        result = a * b
        assert _close(result.to_float(), 12.0)

    def test_div_operator(self):
        a = ZuseFloat.from_float(6.0)
        b = ZuseFloat.from_float(2.0)
        result = a / b
        assert _close(result.to_float(), 3.0)

    def test_div_by_zero(self):
        a = ZuseFloat.from_float(1.0)
        b = ZuseFloat(0)
        with pytest.raises(ZeroDivisionError):
            _ = a / b

    def test_repr(self):
        zf = ZuseFloat.from_float(1.0)
        assert "ZuseFloat" in repr(zf)
        assert "0b" in repr(zf)

    def test_22_bit_word(self):
        zf = ZuseFloat.from_float(1.0)
        # Word must fit in 22 bits
        assert 0 <= zf.word < (1 << 22)

    def test_sign_bit_for_negative(self):
        zf = ZuseFloat.from_float(-1.0)
        # MSB should be set for negative
        assert (zf.word >> 21) & 1 == 1


class TestZuseZ1Memory:
    def test_store_and_load(self):
        z = ZuseZ1()
        val = ZuseFloat.from_float(42.0)
        z.store(0, val)
        z.load(0)
        assert _close(z.state.accumulator.to_float(), 42.0)

    def test_memory_size(self):
        z = ZuseZ1()
        assert len(z.state.memory) == 64

    def test_out_of_range_raises(self):
        z = ZuseZ1()
        with pytest.raises(IndexError):
            z.load(64)
        with pytest.raises(IndexError):
            z.store(64, ZuseFloat(0))

    def test_reset_clears_all(self):
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(5.0))
        z.reset()
        assert z.state.memory[0].is_zero()
        assert z.state.accumulator.is_zero()


class TestZuseZ1Arithmetic:
    def setup_method(self):
        self.z = ZuseZ1()
        self.z.store(0, ZuseFloat.from_float(10.0))
        self.z.store(1, ZuseFloat.from_float(3.0))

    def test_add_memory(self):
        self.z.load(0)
        self.z.add_memory(1)
        assert _close(self.z.state.accumulator.to_float(), 13.0)

    def test_sub_memory(self):
        self.z.load(0)
        self.z.sub_memory(1)
        assert _close(self.z.state.accumulator.to_float(), 7.0)

    def test_mul_memory(self):
        self.z.load(0)
        self.z.mul_memory(1)
        assert _close(self.z.state.accumulator.to_float(), 30.0)

    def test_div_memory(self):
        self.z.load(0)
        self.z.div_memory(1)
        assert _close(self.z.state.accumulator.to_float(), 10.0 / 3.0, tol=1e-3)

    def test_store_accumulator(self):
        self.z.load(0)
        self.z.add_memory(1)
        self.z.store_accumulator(5)
        assert _close(self.z.state.memory[5].to_float(), 13.0)

    def test_chain_ops(self):
        # (10 * 3) - 10 = 20
        self.z.load(0)
        self.z.mul_memory(1)
        self.z.sub_memory(0)
        assert _close(self.z.state.accumulator.to_float(), 20.0)


class TestZuseZ1IO:
    def test_tape_read(self):
        z = ZuseZ1()
        z.state.tape_input = [7.0, 13.0]
        z.read_tape()
        assert _close(z.state.accumulator.to_float(), 7.0)
        z.read_tape()
        assert _close(z.state.accumulator.to_float(), 13.0)

    def test_tape_exhausted(self):
        z = ZuseZ1()
        z.state.tape_input = []
        with pytest.raises(IndexError):
            z.read_tape()

    def test_write_output(self):
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(5.0)
        val = z.write_output()
        assert _close(val, 5.0)
        assert len(z.state.output_tape) == 1

    def test_lamp_panel_updates(self):
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(1.0))
        z.load(0)
        # Lamp panel should be non-zero when accumulator is non-zero
        assert z.state.lamp_panel != 0

    def test_state_dict(self):
        z = ZuseZ1()
        d = z.state_dict()
        assert "accumulator" in d
        assert "lamp_panel" in d
        assert "0b" in d["lamp_panel"]


class TestZuseZ1Program:
    def test_run_simple_program(self):
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(5.0))
        z.store(1, ZuseFloat.from_float(3.0))
        results = z.run(
            [
                ("load", 0),
                ("add", 1),
                ("output",),
            ]
        )
        assert len(results) == 1
        assert _close(results[0], 8.0)

    def test_run_immediate(self):
        z = ZuseZ1()
        z.run([("imm", 42.0)])
        assert _close(z.state.accumulator.to_float(), 42.0)


class TestZuseFloatEdgeCases:
    """ZuseFloat boundary values and sign-magnitude edge cases."""

    def test_from_float_half(self):
        # 0.5 is the minimum normalized mantissa value (mantissa = 0.5, exp = 0)
        zf = ZuseFloat.from_float(0.5)
        assert _close(zf.to_float(), 0.5)

    def test_from_float_minus_half(self):
        zf = ZuseFloat.from_float(-0.5)
        assert _close(abs(zf.to_float()), 0.5)
        assert zf.to_float() < 0

    def test_add_negative_and_positive(self):
        # 3.0 + (-1.0) = 2.0 via ZuseFloat arithmetic
        a = ZuseFloat.from_float(3.0)
        b = ZuseFloat.from_float(-1.0)
        result = a + b
        assert _close(result.to_float(), 2.0)

    def test_subtract_to_negative(self):
        # 2.0 - 5.0 = -3.0
        a = ZuseFloat.from_float(2.0)
        b = ZuseFloat.from_float(5.0)
        result = a - b
        assert _close(abs(result.to_float()), 3.0)
        assert result.to_float() < 0

    def test_mul_by_zero(self):
        # a * 0 = 0 for any a
        a = ZuseFloat.from_float(7.0)
        z = ZuseFloat(0)
        result = a * z
        assert result.is_zero()

    def test_zero_mul_by_value(self):
        # 0 * a = 0
        z = ZuseFloat(0)
        a = ZuseFloat.from_float(7.0)
        result = z * a
        assert result.is_zero()

    def test_negative_times_negative_is_positive(self):
        a = ZuseFloat.from_float(-2.0)
        b = ZuseFloat.from_float(-3.0)
        result = a * b
        assert _close(result.to_float(), 6.0)

    def test_word_zero_is_canonical_zero(self):
        zf = ZuseFloat(0)
        assert zf.word == 0
        assert zf.is_zero()
        assert zf.to_float() == 0.0

    def test_22_bit_limit_not_exceeded_for_boundary_values(self):
        for val in [0.5, 1.0, 2.0, 0.125, 64.0]:
            zf = ZuseFloat.from_float(val)
            assert 0 <= zf.word < (1 << 22), f"Word overflow for {val}"


class TestZuseZ1ArithmeticEdgeCases:
    """Z1 machine-level arithmetic edge cases."""

    def test_sub_to_negative_via_memory(self):
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(2.0))
        z.store(1, ZuseFloat.from_float(5.0))
        z.load(0)
        z.sub_memory(1)
        assert _close(abs(z.state.accumulator.to_float()), 3.0)
        assert z.state.accumulator.to_float() < 0

    def test_store_and_reload_negative(self):
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(-7.5))
        z.load(0)
        assert _close(abs(z.state.accumulator.to_float()), 7.5)
        assert z.state.accumulator.to_float() < 0

    def test_mul_gives_negative_result(self):
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(-3.0))
        z.store(1, ZuseFloat.from_float(4.0))
        z.load(0)
        z.mul_memory(1)
        assert _close(abs(z.state.accumulator.to_float()), 12.0)
        assert z.state.accumulator.to_float() < 0

    def test_chain_mul_add(self):
        # (2 * 3) + 4 = 10
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(2.0))
        z.store(1, ZuseFloat.from_float(3.0))
        z.store(2, ZuseFloat.from_float(4.0))
        z.load(0)
        z.mul_memory(1)       # acc = 6
        z.add_memory(2)       # acc = 10
        assert _close(z.state.accumulator.to_float(), 10.0)

    def test_program_multiply_and_output(self):
        # Program: load addr 0, mul addr 1, output
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(6.0))
        z.store(1, ZuseFloat.from_float(7.0))
        results = z.run([
            ("load", 0),
            ("mul", 1),
            ("output",),
        ])
        assert len(results) == 1
        assert _close(results[0], 42.0)

    def test_program_store_accumulator_then_reload(self):
        z = ZuseZ1()
        z.run([("imm", 99.0)])
        z.store_accumulator(10)
        z.load(10)
        assert _close(z.state.accumulator.to_float(), 99.0)

    def test_tape_multi_value_sequence(self):
        # Reading multiple values from tape into accumulator
        z = ZuseZ1()
        z.state.tape_input = [1.0, 2.0, 3.0]
        for expected in [1.0, 2.0, 3.0]:
            z.read_tape()
            assert _close(z.state.accumulator.to_float(), expected)
