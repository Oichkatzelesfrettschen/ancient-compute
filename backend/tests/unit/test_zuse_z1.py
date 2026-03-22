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
        z.mul_memory(1)  # acc = 6
        z.add_memory(2)  # acc = 10
        assert _close(z.state.accumulator.to_float(), 10.0)

    def test_program_multiply_and_output(self):
        # Program: load addr 0, mul addr 1, output
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(6.0))
        z.store(1, ZuseFloat.from_float(7.0))
        results = z.run(
            [
                ("load", 0),
                ("mul", 1),
                ("output",),
            ]
        )
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


class TestZuseFloatProperties:
    """ZuseFloat is_zero(), equality, and repr properties."""

    def test_is_zero_returns_false_for_nonzero(self) -> None:
        zf = ZuseFloat.from_float(1.0)
        assert not zf.is_zero()

    def test_is_zero_returns_false_for_negative(self) -> None:
        zf = ZuseFloat.from_float(-0.5)
        assert not zf.is_zero()

    def test_equality_with_non_zusefloat_returns_notimplemented(self) -> None:
        zf = ZuseFloat.from_float(1.0)
        result = zf.__eq__(1.0)
        assert result is NotImplemented

    def test_sign_bit_for_positive_is_zero(self) -> None:
        zf = ZuseFloat.from_float(1.0)
        # Sign bit is MSB (bit 21); should be 0 for positive
        assert (zf.word >> 21) & 1 == 0

    def test_repr_contains_float_value_substring(self) -> None:
        zf = ZuseFloat.from_float(3.14)
        r = repr(zf)
        # The float-formatted value should appear somewhere in the repr
        assert "3.1" in r

    def test_repr_binary_word_is_22_chars(self) -> None:
        zf = ZuseFloat.from_float(1.0)
        r = repr(zf)
        # Format: ZuseFloat(value, word=0b<22 binary digits>)
        binary_part = r.split("0b")[1].rstrip(")")
        assert len(binary_part) == 22

    def test_from_float_positive_has_zero_sign_bit(self) -> None:
        for val in [0.5, 1.0, 2.0, 100.0]:
            zf = ZuseFloat.from_float(val)
            assert (zf.word >> 21) & 1 == 0, f"Positive {val} should have sign bit 0"

    def test_word_fits_in_22_bits_for_negative(self) -> None:
        zf = ZuseFloat.from_float(-5.0)
        assert 0 <= zf.word < (1 << 22)


class TestZuseZ1MemoryBounds:
    """Memory address boundary and bounds-checking tests."""

    def test_store_at_address_zero(self) -> None:
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(1.0))
        assert not z.state.memory[0].is_zero()

    def test_store_at_address_63(self) -> None:
        z = ZuseZ1()
        z.store(63, ZuseFloat.from_float(2.0))
        assert not z.state.memory[63].is_zero()

    def test_load_at_address_63(self) -> None:
        z = ZuseZ1()
        z.store(63, ZuseFloat.from_float(8.0))
        z.load(63)
        assert _close(z.state.accumulator.to_float(), 8.0)

    def test_store_address_64_raises(self) -> None:
        z = ZuseZ1()
        with pytest.raises(IndexError):
            z.store(64, ZuseFloat.from_float(1.0))

    def test_load_address_negative_raises(self) -> None:
        z = ZuseZ1()
        with pytest.raises(IndexError):
            z.load(-1)

    def test_store_accumulator_address_negative_raises(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(1.0)
        with pytest.raises(IndexError):
            z.store_accumulator(-1)

    def test_store_accumulator_address_64_raises(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(1.0)
        with pytest.raises(IndexError):
            z.store_accumulator(64)

    def test_all_memory_initially_zero(self) -> None:
        z = ZuseZ1()
        assert all(m.is_zero() for m in z.state.memory)

    def test_store_accumulator_at_address_63(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(5.5)
        z.store_accumulator(63)
        assert _close(z.state.memory[63].to_float(), 5.5)

    def test_add_memory_out_of_range_raises(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(1.0)
        with pytest.raises(IndexError):
            z.add_memory(64)

    def test_sub_memory_out_of_range_raises(self) -> None:
        z = ZuseZ1()
        with pytest.raises(IndexError):
            z.sub_memory(-1)

    def test_mul_memory_out_of_range_raises(self) -> None:
        z = ZuseZ1()
        with pytest.raises(IndexError):
            z.mul_memory(64)

    def test_div_memory_out_of_range_raises(self) -> None:
        z = ZuseZ1()
        with pytest.raises(IndexError):
            z.div_memory(-1)


class TestZuseZ1IOExtended:
    """Extended I/O: output tape, lamp panel, read_tape pointer."""

    def test_write_output_appends_multiple_times(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(1.0)
        z.write_output()
        z.state.accumulator = ZuseFloat.from_float(2.0)
        z.write_output()
        assert len(z.state.output_tape) == 2
        assert _close(z.state.output_tape[0], 1.0)
        assert _close(z.state.output_tape[1], 2.0)

    def test_read_tape_consumes_first_element(self) -> None:
        z = ZuseZ1()
        z.state.tape_input = [10.0, 20.0, 30.0]
        z.read_tape()
        assert len(z.state.tape_input) == 2

    def test_read_tape_empties_single_element_tape(self) -> None:
        z = ZuseZ1()
        z.state.tape_input = [5.0]
        z.read_tape()
        assert z.state.tape_input == []

    def test_lamp_panel_matches_accumulator_word(self) -> None:
        z = ZuseZ1()
        zf = ZuseFloat.from_float(7.0)
        z.store(0, zf)
        z.load(0)
        assert z.state.lamp_panel == z.state.accumulator.word

    def test_lamp_panel_zero_when_accumulator_zero(self) -> None:
        z = ZuseZ1()
        z.load(0)  # memory[0] is zero
        assert z.state.lamp_panel == 0

    def test_write_output_returns_float(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(3.0)
        val = z.write_output()
        assert isinstance(val, float)

    def test_initial_output_tape_empty(self) -> None:
        z = ZuseZ1()
        assert z.state.output_tape == []

    def test_initial_tape_input_empty(self) -> None:
        z = ZuseZ1()
        assert z.state.tape_input == []

    def test_read_tape_increments_cycle_count(self) -> None:
        z = ZuseZ1()
        z.state.tape_input = [1.0]
        before = z.state.cycle_count
        z.read_tape()
        assert z.state.cycle_count == before + 1

    def test_write_output_appended_value_matches_accumulator(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(123.0)
        z.write_output()
        assert _close(z.state.output_tape[-1], 123.0, tol=1e-2)


class TestZuseZ1StepBehavior:
    """step() and load_program() behavior tests."""

    def test_load_program_resets_pc(self) -> None:
        z = ZuseZ1()
        z.state.program_counter = 5
        z.load_program([("imm", 1.0)])
        assert z.state.program_counter == 0

    def test_step_executes_imm_instruction(self) -> None:
        z = ZuseZ1()
        z.load_program([("imm", 42.0)])
        z.step()
        assert _close(z.state.accumulator.to_float(), 42.0)

    def test_step_increments_pc(self) -> None:
        z = ZuseZ1()
        z.load_program([("imm", 1.0), ("imm", 2.0)])
        z.step()
        assert z.state.program_counter == 1

    def test_step_increments_cycle_count(self) -> None:
        z = ZuseZ1()
        initial = z.state.cycle_count
        z.load_program([("imm", 1.0)])
        z.step()
        assert z.state.cycle_count > initial

    def test_step_past_end_does_not_raise(self) -> None:
        z = ZuseZ1()
        z.load_program([("imm", 1.0)])
        z.step()  # executes instruction
        z.step()  # past end -- should not raise
        assert z.state.program_counter == 2

    def test_step_executes_store_op(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(9.0)
        z.load_program([("store", 5)])
        z.step()
        assert _close(z.state.memory[5].to_float(), 9.0)

    def test_step_executes_load_op(self) -> None:
        z = ZuseZ1()
        z.store(2, ZuseFloat.from_float(3.5))
        z.load_program([("load", 2)])
        z.step()
        assert _close(z.state.accumulator.to_float(), 3.5)

    def test_step_executes_add_op(self) -> None:
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(4.0))
        z.state.accumulator = ZuseFloat.from_float(6.0)
        z.load_program([("add", 0)])
        z.step()
        assert _close(z.state.accumulator.to_float(), 10.0)

    def test_step_executes_sub_op(self) -> None:
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(3.0))
        z.state.accumulator = ZuseFloat.from_float(7.0)
        z.load_program([("sub", 0)])
        z.step()
        assert _close(z.state.accumulator.to_float(), 4.0)

    def test_step_executes_mul_op(self) -> None:
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(3.0))
        z.state.accumulator = ZuseFloat.from_float(5.0)
        z.load_program([("mul", 0)])
        z.step()
        assert _close(z.state.accumulator.to_float(), 15.0)

    def test_step_executes_div_op(self) -> None:
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(4.0))
        z.state.accumulator = ZuseFloat.from_float(8.0)
        z.load_program([("div", 0)])
        z.step()
        assert _close(z.state.accumulator.to_float(), 2.0)

    def test_step_executes_output_op(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(7.0)
        z.load_program([("output",)])
        z.step()
        assert len(z.state.output_tape) == 1
        assert _close(z.state.output_tape[0], 7.0)

    def test_run_returns_only_output_values(self) -> None:
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(1.0))
        z.store(1, ZuseFloat.from_float(2.0))
        results = z.run([
            ("load", 0), ("output",),
            ("load", 1), ("output",),
        ])
        assert len(results) == 2
        assert _close(results[0], 1.0)
        assert _close(results[1], 2.0)

    def test_run_no_output_instructions_returns_empty(self) -> None:
        z = ZuseZ1()
        z.store(0, ZuseFloat.from_float(5.0))
        results = z.run([("load", 0), ("imm", 3.0)])
        assert results == []


class TestZuseZ1StateDictExtended:
    """state_dict() completeness and type tests."""

    def test_state_dict_has_six_keys(self) -> None:
        z = ZuseZ1()
        d = z.state_dict()
        assert len(d) == 6

    def test_state_dict_has_expected_keys(self) -> None:
        z = ZuseZ1()
        d = z.state_dict()
        expected_keys = (
            "accumulator", "auxiliary", "program_counter",
            "cycle_count", "lamp_panel", "output_tape",
        )
        for key in expected_keys:
            assert key in d

    def test_state_dict_accumulator_is_float(self) -> None:
        z = ZuseZ1()
        d = z.state_dict()
        assert isinstance(d["accumulator"], float)

    def test_state_dict_lamp_panel_has_0b_prefix(self) -> None:
        z = ZuseZ1()
        d = z.state_dict()
        assert str(d["lamp_panel"]).startswith("0b")

    def test_state_dict_output_tape_is_list(self) -> None:
        z = ZuseZ1()
        d = z.state_dict()
        assert isinstance(d["output_tape"], list)

    def test_state_dict_initial_program_counter_zero(self) -> None:
        z = ZuseZ1()
        d = z.state_dict()
        assert d["program_counter"] == 0

    def test_state_dict_initial_cycle_count_zero(self) -> None:
        z = ZuseZ1()
        d = z.state_dict()
        assert d["cycle_count"] == 0

    def test_state_dict_reflects_current_accumulator(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(5.0)
        d = z.state_dict()
        assert _close(float(d["accumulator"]), 5.0)

    def test_state_dict_output_tape_is_copy(self) -> None:
        # Modifying the returned list should not affect internal state
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(1.0)
        z.write_output()
        d = z.state_dict()
        tape = d["output_tape"]
        tape.append(999.0)
        assert len(z.state.output_tape) == 1

    def test_state_dict_lamp_panel_is_22_bit_binary(self) -> None:
        z = ZuseZ1()
        panel = str(z.state_dict()["lamp_panel"])
        # "0b" prefix + 22 binary digits
        assert panel.startswith("0b")
        binary_digits = panel[2:]
        assert len(binary_digits) == 22
        assert all(c in "01" for c in binary_digits)


class TestZuseZ1ResetBehavior:
    """reset() comprehensive behavior tests."""

    def test_reset_clears_output_tape(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(1.0)
        z.write_output()
        z.reset()
        assert z.state.output_tape == []

    def test_reset_clears_cycle_count(self) -> None:
        z = ZuseZ1()
        z.load(0)
        z.reset()
        assert z.state.cycle_count == 0

    def test_reset_clears_program_counter(self) -> None:
        z = ZuseZ1()
        z.load_program([("imm", 1.0)])
        z.step()
        z.reset()
        assert z.state.program_counter == 0

    def test_reset_clears_all_memory(self) -> None:
        z = ZuseZ1()
        z.store(5, ZuseFloat.from_float(99.0))
        z.store(63, ZuseFloat.from_float(-7.0))
        z.reset()
        assert all(m.is_zero() for m in z.state.memory)

    def test_reset_clears_tape_input(self) -> None:
        z = ZuseZ1()
        z.state.tape_input = [1.0, 2.0, 3.0]
        z.reset()
        assert z.state.tape_input == []

    def test_reset_allows_reuse_with_new_program(self) -> None:
        z = ZuseZ1()
        z.run([("imm", 5.0), ("output",)])
        z.reset()
        z.store(0, ZuseFloat.from_float(10.0))
        results = z.run([("load", 0), ("output",)])
        assert len(results) == 1
        assert _close(results[0], 10.0)

    def test_reset_clears_accumulator(self) -> None:
        z = ZuseZ1()
        z.state.accumulator = ZuseFloat.from_float(42.0)
        z.reset()
        assert z.state.accumulator.is_zero()


class TestZuseFloatArithmetic:
    """ZuseFloat arithmetic properties: commutativity, precision, chain."""

    def test_addition_commutative(self) -> None:
        a = ZuseFloat.from_float(3.0)
        b = ZuseFloat.from_float(7.0)
        assert _close((a + b).to_float(), (b + a).to_float())

    def test_multiplication_commutative(self) -> None:
        a = ZuseFloat.from_float(4.0)
        b = ZuseFloat.from_float(2.5)
        assert _close((a * b).to_float(), (b * a).to_float())

    def test_subtraction_not_commutative(self) -> None:
        a = ZuseFloat.from_float(5.0)
        b = ZuseFloat.from_float(2.0)
        ab = (a - b).to_float()  # 3.0
        ba = (b - a).to_float()  # -3.0
        assert not _close(ab, ba)

    def test_chain_add_sub_net_zero(self) -> None:
        # 10 + 5 - 5 - 10 = 0
        a = ZuseFloat.from_float(10.0)
        b = ZuseFloat.from_float(5.0)
        result = a + b - b - a
        assert result.is_zero() or _close(result.to_float(), 0.0)

    def test_div_result_less_than_one(self) -> None:
        a = ZuseFloat.from_float(1.0)
        b = ZuseFloat.from_float(4.0)
        result = a / b
        assert _close(result.to_float(), 0.25, tol=1e-3)

    def test_mul_fractional_result(self) -> None:
        a = ZuseFloat.from_float(0.5)
        b = ZuseFloat.from_float(0.5)
        result = a * b
        assert _close(result.to_float(), 0.25, tol=1e-3)

    def test_from_float_preserves_sign_through_mul(self) -> None:
        pos = ZuseFloat.from_float(3.0)
        neg = ZuseFloat.from_float(-3.0)
        result = pos * neg
        assert result.to_float() < 0

    def test_chain_arithmetic_matches_python(self) -> None:
        # (7 * 4) / 2 + 1 = 15
        a = ZuseFloat.from_float(7.0)
        b = ZuseFloat.from_float(4.0)
        c = ZuseFloat.from_float(2.0)
        d = ZuseFloat.from_float(1.0)
        result = (a * b) / c + d
        assert _close(result.to_float(), 15.0, tol=1e-3)
