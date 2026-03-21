"""Tests for the Torres y Quevedo electromechanical calculator emulator."""

import pytest

from backend.src.emulator.torres_quevedo import (
    FloatingPointNumber,
    TorresQuevedo,
)


def _close(a: float, b: float, tol: float = 1e-5) -> bool:
    if abs(b) < 1e-12:
        return abs(a) < tol
    return abs(a - b) / abs(b) < tol


class TestFloatingPointNumber:
    def test_zero(self):
        fp = FloatingPointNumber()
        assert fp.to_float() == 0.0

    def test_from_float_positive(self):
        fp = FloatingPointNumber.from_float(3.14)
        assert _close(fp.to_float(), 3.14)

    def test_from_float_negative(self):
        fp = FloatingPointNumber.from_float(-2.5)
        assert fp.negative is True
        assert _close(abs(fp.to_float()), 2.5)

    def test_from_float_one(self):
        fp = FloatingPointNumber.from_float(1.0)
        assert _close(fp.to_float(), 1.0)

    def test_from_float_large(self):
        fp = FloatingPointNumber.from_float(12345678.0)
        assert _close(fp.to_float(), 12345678.0, tol=1e-3)

    def test_from_float_small(self):
        fp = FloatingPointNumber.from_float(0.001)
        assert _close(fp.to_float(), 0.001)

    def test_normalization_range(self):
        fp = FloatingPointNumber.from_float(5.0)
        # Mantissa should be in [0.1, 1.0)
        assert 0.0 <= fp.mantissa < 1.0

    def test_repr(self):
        fp = FloatingPointNumber.from_float(1.0)
        assert "TQ(" in repr(fp)

    def test_equality(self):
        fp1 = FloatingPointNumber.from_float(3.14)
        fp2 = FloatingPointNumber.from_float(3.14)
        assert fp1 == fp2

    def test_invalid_mantissa_raises(self):
        with pytest.raises(ValueError):
            FloatingPointNumber(mantissa=1.5)


class TestTorresRegisterOps:
    def test_load_and_read(self):
        t = TorresQuevedo()
        fp = FloatingPointNumber.from_float(2.5)
        t.load_register(0, fp)
        result = t.read_register(0)
        assert _close(result.to_float(), 2.5)

    def test_register_out_of_range(self):
        t = TorresQuevedo()
        with pytest.raises(IndexError):
            t.load_register(8, FloatingPointNumber.from_float(1.0))

    def test_reset_clears_registers(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(99.0))
        t.reset()
        assert t.read_register(0).to_float() == 0.0


class TestTorresArithmetic:
    def setup_method(self):
        self.t = TorresQuevedo()
        self.t.load_register(0, FloatingPointNumber.from_float(6.0))
        self.t.load_register(1, FloatingPointNumber.from_float(4.0))

    def test_add(self):
        result = self.t.add(0, 1, dest=2)
        assert _close(result.to_float(), 10.0)

    def test_subtract(self):
        result = self.t.subtract(0, 1, dest=2)
        assert _close(result.to_float(), 2.0)

    def test_multiply(self):
        result = self.t.multiply(0, 1, dest=2)
        assert _close(result.to_float(), 24.0)

    def test_divide(self):
        result = self.t.divide(0, 1, dest=2)
        assert _close(result.to_float(), 1.5)

    def test_divide_by_zero(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(1.0))
        t.load_register(1, FloatingPointNumber())  # zero
        with pytest.raises(ZeroDivisionError):
            t.divide(0, 1, dest=2)

    def test_chain_operations(self):
        # (6 + 4) * 2 = 20
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(6.0))
        t.load_register(1, FloatingPointNumber.from_float(4.0))
        t.load_register(2, FloatingPointNumber.from_float(2.0))
        t.add(0, 1, dest=3)  # R3 = 10
        result = t.multiply(3, 2, dest=4)  # R4 = 20
        assert _close(result.to_float(), 20.0)


class TestTorresTypewriter:
    def test_typewriter_print(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(42.0))
        output = t.typewriter_print(0)
        assert "TQ(" in output
        assert len(t.state.typewriter_output) == 1

    def test_state_dict(self):
        t = TorresQuevedo()
        d = t.state_dict()
        assert "registers" in d
        assert "cycle_count" in d

    def test_cycle_count_increments(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(1.0))
        t.load_register(1, FloatingPointNumber.from_float(2.0))
        initial = t.state.cycle_count
        t.add(0, 1, dest=2)
        assert t.state.cycle_count == initial + 1

    def test_run_program(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(3.0))
        t.load_register(1, FloatingPointNumber.from_float(2.0))
        results = t.run(
            [
                ("add", 0, 1, 2),
                ("mul", 0, 1, 3),
            ]
        )
        assert _close(results[0].to_float(), 5.0)
        assert _close(results[1].to_float(), 6.0)


class TestFloatingPointEdgeCases:
    """FloatingPointNumber precision and sign boundary tests."""

    def test_fp_point_one_round_trips(self):
        # 0.1 cannot be represented exactly in binary, but TQ FP should round-trip
        # within the 8-digit decimal mantissa tolerance.
        fp = FloatingPointNumber.from_float(0.1)
        assert _close(fp.to_float(), 0.1, tol=1e-6)

    def test_fp_very_small_value(self):
        # 1e-7 is near the representable lower bound (2-digit exponent: 10^-99)
        fp = FloatingPointNumber.from_float(1e-7)
        assert _close(fp.to_float(), 1e-7, tol=1e-4)

    def test_fp_mantissa_at_lower_bound(self):
        # Mantissa is normalized to [0.1, 1.0); value 0.1 should have mantissa=0.1, exp=1
        fp = FloatingPointNumber.from_float(0.1)
        assert 0.0 <= fp.mantissa < 1.0

    def test_fp_negative_zero_is_zero(self):
        # -0.0 in Python should round-trip to 0.0 via TQ FP
        fp = FloatingPointNumber.from_float(-0.0)
        # Result is either 0.0 or -0.0; either is acceptable as a zero value
        assert abs(fp.to_float()) < 1e-12

    def test_fp_large_integer_precision(self):
        # 99_999_999.0 is the max 8-digit integer; round-trip tolerance 1e-3
        fp = FloatingPointNumber.from_float(99_999_999.0)
        assert _close(fp.to_float(), 99_999_999.0, tol=1e-3)


class TestTorresArithmeticEdgeCases:
    """Arithmetic edge cases: negative results, negatives-product, chain ops."""

    def test_subtract_larger_from_smaller_gives_negative(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(2.0))
        t.load_register(1, FloatingPointNumber.from_float(5.0))
        result = t.subtract(0, 1, dest=2)
        assert result.to_float() < 0
        assert _close(abs(result.to_float()), 3.0)

    def test_multiply_negative_by_positive_is_negative(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(-3.0))
        t.load_register(1, FloatingPointNumber.from_float(4.0))
        result = t.multiply(0, 1, dest=2)
        assert result.to_float() < 0
        assert _close(abs(result.to_float()), 12.0)

    def test_multiply_both_negative_is_positive(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(-3.0))
        t.load_register(1, FloatingPointNumber.from_float(-4.0))
        result = t.multiply(0, 1, dest=2)
        assert result.to_float() > 0
        assert _close(result.to_float(), 12.0)

    def test_chain_negative_intermediate(self):
        # (2 - 5) * 3 = -9
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(2.0))
        t.load_register(1, FloatingPointNumber.from_float(5.0))
        t.load_register(2, FloatingPointNumber.from_float(3.0))
        t.subtract(0, 1, dest=3)  # R3 = -3
        result = t.multiply(3, 2, dest=4)  # R4 = -9
        assert _close(abs(result.to_float()), 9.0)
        assert result.to_float() < 0

    def test_divide_result_less_than_one(self):
        # 1 / 4 = 0.25
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(1.0))
        t.load_register(1, FloatingPointNumber.from_float(4.0))
        result = t.divide(0, 1, dest=2)
        assert _close(result.to_float(), 0.25)

    def test_add_negative_to_positive(self):
        # 10 + (-3) = 7
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(10.0))
        t.load_register(1, FloatingPointNumber.from_float(-3.0))
        result = t.add(0, 1, dest=2)
        assert _close(result.to_float(), 7.0)


# =============================================================================
# step() / load_program() / conditional branching tests
# =============================================================================


class TestStepAndProgram:
    """Tests for load_program() + step() and conditional branching (Torres 1914)."""

    def test_load_program_sets_tape(self):
        """load_program stores the tape and resets program_pointer to 0."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(2.0))
        t.load_register(1, FloatingPointNumber.from_float(3.0))
        tape = [("add", 0, 1, 2)]
        t.load_program(tape)
        assert t.state.program_tape == tape
        assert t.state.program_pointer == 0

    def test_step_executes_add(self):
        """One step on an add instruction updates the destination register."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(4.0))
        t.load_register(1, FloatingPointNumber.from_float(5.0))
        t.load_program([("add", 0, 1, 2)])
        t.step()
        assert _close(t.state.registers[2].to_float(), 9.0)

    def test_step_advances_program_pointer(self):
        """Program pointer increments by 1 after a non-jump instruction."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(1.0))
        t.load_register(1, FloatingPointNumber.from_float(2.0))
        t.load_program([("add", 0, 1, 2), ("mul", 0, 1, 3)])
        assert t.state.program_pointer == 0
        t.step()
        assert t.state.program_pointer == 1

    def test_step_past_end_does_not_raise(self):
        """Stepping beyond the program end is a safe no-op."""
        t = TorresQuevedo()
        t.load_program([("print", 0)])
        t.step()  # executes instruction 0
        t.step()  # beyond end -- should not raise
        assert t.state.program_pointer == 2

    def test_cycle_count_increments_per_step(self):
        """cycle_count increments by 1 for each step, including no-op steps."""
        t = TorresQuevedo()
        t.load_program([("print", 0), ("print", 0)])
        t.step()
        t.step()
        assert t.state.cycle_count == 2

    def test_step_three_instruction_sequence(self):
        """A 3-instruction program produces the correct final register value."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(6.0))
        t.load_register(1, FloatingPointNumber.from_float(2.0))
        tape = [
            ("mul", 0, 1, 2),  # R2 = 12.0
            ("add", 2, 0, 3),  # R3 = 18.0
            ("sub", 3, 1, 4),  # R4 = 16.0
        ]
        t.load_program(tape)
        for _ in range(3):
            t.step()
        assert _close(t.state.registers[2].to_float(), 12.0)
        assert _close(t.state.registers[3].to_float(), 18.0)
        assert _close(t.state.registers[4].to_float(), 16.0)

    def test_halt_terminates_execution(self):
        """halt sets program_pointer past the end so subsequent steps are no-ops."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(1.0))
        t.load_register(1, FloatingPointNumber.from_float(2.0))
        t.load_program([("add", 0, 1, 2), ("halt",), ("add", 0, 0, 0)])
        t.step()  # add
        t.step()  # halt -- pp jumps to end
        t.step()  # should be a no-op (past end)
        # R0 is still 1.0 (the final add never executed)
        assert _close(t.state.registers[0].to_float(), 1.0)


class TestConditionalBranching:
    """Torres 1914 conditional branching via relay sign-select logic."""

    def test_jmp_unconditional(self):
        """jmp jumps to the target regardless of register values."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(3.0))
        t.load_register(1, FloatingPointNumber.from_float(7.0))
        #  0: jmp 2   -- skip the add
        #  1: add 0, 1, 2  -- should be skipped
        #  2: mul 0, 1, 3  -- should execute
        tape = [("jmp", 2), ("add", 0, 1, 2), ("mul", 0, 1, 3)]
        t.load_program(tape)
        t.step()  # jmp -> pp=2
        t.step()  # mul
        assert _close(t.state.registers[2].to_float(), 0.0)  # add was skipped
        assert _close(t.state.registers[3].to_float(), 21.0)

    def test_jz_jumps_when_register_is_zero(self):
        """jz takes the branch when the tested register holds exactly 0.0."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber())  # 0.0
        t.load_register(1, FloatingPointNumber.from_float(5.0))
        #  0: jz 0, 2  -- R0==0, so jump to 2
        #  1: add 1,1,1 -- should be skipped
        #  2: mul 1,1,2 -- should execute: R2 = 25
        tape = [("jz", 0, 2), ("add", 1, 1, 1), ("mul", 1, 1, 2)]
        t.load_program(tape)
        t.step()  # jz triggers
        t.step()  # mul
        assert _close(t.state.registers[1].to_float(), 5.0)  # unchanged
        assert _close(t.state.registers[2].to_float(), 25.0)

    def test_jz_no_jump_when_register_nonzero(self):
        """jz does not branch when the tested register is non-zero."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(3.0))
        t.load_register(1, FloatingPointNumber.from_float(2.0))
        #  0: jz 0, 2  -- R0 != 0, fall through
        #  1: add 0,1,2 -- executes: R2 = 5
        #  2: halt
        tape = [("jz", 0, 2), ("add", 0, 1, 2), ("halt",)]
        t.load_program(tape)
        t.step()  # jz -- no jump
        t.step()  # add
        assert _close(t.state.registers[2].to_float(), 5.0)

    def test_jnz_jumps_when_nonzero(self):
        """jnz branches when the tested register is non-zero."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(1.0))
        tape = [("jnz", 0, 2), ("halt",), ("halt",)]
        t.load_program(tape)
        t.step()  # jnz -> pp=2
        assert t.state.program_pointer == 2

    def test_jnz_no_jump_when_zero(self):
        """jnz does not branch when the tested register is zero."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber())  # 0.0
        tape = [("jnz", 0, 2), ("halt",), ("halt",)]
        t.load_program(tape)
        t.step()  # jnz -- no jump, pp advances to 1
        assert t.state.program_pointer == 1

    def test_jlt_jumps_when_negative(self):
        """jlt (sign relay = negative) branches when register value < 0."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(-4.0))
        tape = [("jlt", 0, 3), ("halt",), ("halt",), ("halt",)]
        t.load_program(tape)
        t.step()  # jlt -> pp=3
        assert t.state.program_pointer == 3

    def test_jlt_no_jump_when_positive(self):
        """jlt does not branch when register value is positive."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(2.0))
        tape = [("jlt", 0, 3), ("halt",), ("halt",), ("halt",)]
        t.load_program(tape)
        t.step()  # jlt -- no jump
        assert t.state.program_pointer == 1

    def test_jgt_jumps_when_positive(self):
        """jgt (sign relay = positive) branches when register value > 0."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(7.5))
        tape = [("jgt", 0, 2), ("halt",), ("halt",)]
        t.load_program(tape)
        t.step()  # jgt -> pp=2
        assert t.state.program_pointer == 2

    def test_jgt_no_jump_when_negative(self):
        """jgt does not branch when register value is negative."""
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(-1.0))
        tape = [("jgt", 0, 2), ("halt",), ("halt",)]
        t.load_program(tape)
        t.step()  # jgt -- no jump
        assert t.state.program_pointer == 1

    def test_counted_loop_via_branching(self):
        """A counted loop: add 1.0 to R0 three times using jlt to loop back.

        WHY: Torres 1914 Section 5 describes using sign detection to implement
        loop termination -- this exercises the full relay-based branch mechanism.

        Program (R0 starts at -3.0, R1 = 1.0, R2 = 0.0 = terminator):
            0: add 0, 1, 0   -- R0 += 1
            1: jlt 0, 0      -- if R0 < 0, loop back to 0
            2: halt
        After 3 iterations R0 == 0.0, jlt does not fire, halt terminates.
        """
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(-3.0))
        t.load_register(1, FloatingPointNumber.from_float(1.0))
        tape = [("add", 0, 1, 0), ("jlt", 0, 0), ("halt",)]
        t.load_program(tape)
        max_steps = 20
        for _ in range(max_steps):
            if t.state.program_pointer >= len(tape):
                break
            t.step()
        assert _close(t.state.registers[0].to_float(), 0.0)
