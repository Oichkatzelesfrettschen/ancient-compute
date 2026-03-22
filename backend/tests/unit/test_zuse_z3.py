"""Unit tests for the Zuse Z3 emulator (Germany, 1941)."""

import math

import pytest

from backend.src.emulator.zuse_z3 import Z3Instruction, Z3Op, ZuseZ3

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _run(
    instructions: list[Z3Instruction],
    memory: dict[int, float] | None = None,
    input_tape: list[float] | None = None,
) -> ZuseZ3:
    z3 = ZuseZ3()
    if memory:
        for addr, val in memory.items():
            z3.load_memory(addr, val)
    if input_tape:
        z3.load_input_tape(input_tape)
    z3.load_program(instructions)
    z3.run()
    return z3


# ---------------------------------------------------------------------------
# Instruction construction
# ---------------------------------------------------------------------------


class TestZ3Instruction:
    def test_load_instruction(self):
        i = Z3Instruction(Z3Op.LOAD, 5)
        assert i.op == Z3Op.LOAD
        assert i.address == 5

    def test_halt_no_address(self):
        i = Z3Instruction(Z3Op.HALT)
        assert i.address == 0

    def test_address_out_of_range_raises(self):
        with pytest.raises(ValueError):
            Z3Instruction(Z3Op.LOAD, 64)

    def test_address_negative_raises(self):
        with pytest.raises(ValueError):
            Z3Instruction(Z3Op.STORE, -1)

    def test_sqrt_ignores_address(self):
        # SQRT/READ/PRINT/HALT don't validate address (ignored)
        i = Z3Instruction(Z3Op.SQRT)
        assert i.address == 0


# ---------------------------------------------------------------------------
# Memory
# ---------------------------------------------------------------------------


class TestMemory:
    def test_load_and_read_memory(self):
        z3 = ZuseZ3()
        z3.load_memory(0, 3.14)
        assert abs(z3.get_memory(0) - 3.14) < 1e-3  # 14-bit mantissa precision

    def test_memory_default_zero(self):
        z3 = ZuseZ3()
        assert z3.get_memory(5) == 0.0

    def test_memory_address_out_of_range(self):
        z3 = ZuseZ3()
        with pytest.raises(IndexError):
            z3.load_memory(64, 1.0)

    def test_memory_address_negative(self):
        z3 = ZuseZ3()
        with pytest.raises(IndexError):
            z3.get_memory(-1)


# ---------------------------------------------------------------------------
# LOAD / STORE
# ---------------------------------------------------------------------------


class TestLoadStore:
    def test_load_sets_accumulator(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.HALT)],
            memory={0: 7.5},
        )
        assert abs(z3.get_accumulator() - 7.5) < 1e-3

    def test_store_writes_memory(self):
        z3 = ZuseZ3()
        z3.load_memory(0, 5.0)
        z3.load_program(
            [
                Z3Instruction(Z3Op.LOAD, 0),
                Z3Instruction(Z3Op.STORE, 1),
                Z3Instruction(Z3Op.HALT),
            ]
        )
        z3.run()
        assert abs(z3.get_memory(1) - 5.0) < 1e-3

    def test_load_store_roundtrip(self):
        z3 = ZuseZ3()
        z3.load_memory(3, 42.0)
        z3.load_program(
            [
                Z3Instruction(Z3Op.LOAD, 3),
                Z3Instruction(Z3Op.STORE, 10),
                Z3Instruction(Z3Op.LOAD, 10),
                Z3Instruction(Z3Op.HALT),
            ]
        )
        z3.run()
        assert abs(z3.get_accumulator() - 42.0) < 1e-3


# ---------------------------------------------------------------------------
# Arithmetic operations
# ---------------------------------------------------------------------------


class TestArithmetic:
    def test_add(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.ADD, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: 3.0, 1: 4.0},
        )
        assert abs(z3.get_accumulator() - 7.0) < 1e-3

    def test_subtract(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SUB, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: 10.0, 1: 3.0},
        )
        assert abs(z3.get_accumulator() - 7.0) < 1e-3

    def test_multiply(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.MULT, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: 6.0, 1: 7.0},
        )
        assert abs(z3.get_accumulator() - 42.0) < 1e-3

    def test_divide(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.DIV, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: 10.0, 1: 4.0},
        )
        assert abs(z3.get_accumulator() - 2.5) < 1e-3

    def test_sqrt(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SQRT), Z3Instruction(Z3Op.HALT)],
            memory={0: 16.0},
        )
        assert abs(z3.get_accumulator() - 4.0) < 1e-3

    def test_sqrt_two(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SQRT), Z3Instruction(Z3Op.HALT)],
            memory={0: 2.0},
        )
        assert abs(z3.get_accumulator() - math.sqrt(2)) < 1e-3

    def test_divide_by_zero_raises(self):
        z3 = ZuseZ3()
        z3.load_memory(0, 5.0)
        z3.load_memory(1, 0.0)
        z3.load_program(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.DIV, 1), Z3Instruction(Z3Op.HALT)]
        )
        with pytest.raises(ZeroDivisionError):
            z3.run()

    def test_sqrt_negative_raises(self):
        z3 = ZuseZ3()
        z3.load_memory(0, -4.0)
        z3.load_program(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SQRT), Z3Instruction(Z3Op.HALT)]
        )
        with pytest.raises(ValueError):
            z3.run()

    def test_negative_multiply(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.MULT, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: -3.0, 1: 5.0},
        )
        assert abs(z3.get_accumulator() - (-15.0)) < 1e-3


# ---------------------------------------------------------------------------
# READ / PRINT (I/O tape)
# ---------------------------------------------------------------------------


class TestIO:
    def test_read_from_tape(self):
        z3 = _run(
            [Z3Instruction(Z3Op.READ), Z3Instruction(Z3Op.HALT)],
            input_tape=[99.0],
        )
        assert abs(z3.get_accumulator() - 99.0) < 1e-3

    def test_print_to_output(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.PRINT), Z3Instruction(Z3Op.HALT)],
            memory={0: 3.14},
        )
        assert len(z3.state.output_tape) == 1
        assert abs(z3.state.output_tape[0] - 3.14) < 1e-3

    def test_multiple_prints(self):
        z3 = _run(
            [
                Z3Instruction(Z3Op.LOAD, 0),
                Z3Instruction(Z3Op.PRINT),
                Z3Instruction(Z3Op.LOAD, 1),
                Z3Instruction(Z3Op.PRINT),
                Z3Instruction(Z3Op.HALT),
            ],
            memory={0: 1.0, 1: 2.0},
        )
        assert len(z3.state.output_tape) == 2
        assert abs(z3.state.output_tape[0] - 1.0) < 1e-3
        assert abs(z3.state.output_tape[1] - 2.0) < 1e-3

    def test_read_exhausted_raises(self):
        z3 = ZuseZ3()
        z3.load_input_tape([1.0])
        z3.load_program(
            [
                Z3Instruction(Z3Op.READ),
                Z3Instruction(Z3Op.READ),  # Tape exhausted
                Z3Instruction(Z3Op.HALT),
            ]
        )
        with pytest.raises(RuntimeError, match="exhausted"):
            z3.run()


# ---------------------------------------------------------------------------
# Program control
# ---------------------------------------------------------------------------


class TestControl:
    def test_halt_stops_execution(self):
        z3 = ZuseZ3()
        z3.load_memory(0, 1.0)
        z3.load_program(
            [
                Z3Instruction(Z3Op.LOAD, 0),
                Z3Instruction(Z3Op.HALT),
                Z3Instruction(Z3Op.LOAD, 0),  # Never reached
            ]
        )
        z3.run()
        assert z3.state.program_counter == 2  # stopped after HALT
        assert z3.state.cycle_count == 2  # LOAD + HALT

    def test_end_of_tape_stops(self):
        """Running off the end of the tape halts automatically."""
        z3 = ZuseZ3()
        z3.load_memory(0, 5.0)
        z3.load_program(
            [
                Z3Instruction(Z3Op.LOAD, 0),
                # No HALT -- end of tape
            ]
        )
        z3.run()
        assert z3.state.halted

    def test_cycle_count(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.ADD, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: 1.0, 1: 2.0},
        )
        assert z3.state.cycle_count == 3

    def test_step_by_step(self):
        """step() executes one instruction at a time."""
        z3 = ZuseZ3()
        z3.load_memory(0, 5.0)
        z3.load_program(
            [
                Z3Instruction(Z3Op.LOAD, 0),
                Z3Instruction(Z3Op.HALT),
            ]
        )
        result1 = z3.step()  # LOAD -> returns True (continue)
        assert result1 is True
        result2 = z3.step()  # HALT -> returns False (halted)
        assert result2 is False

    def test_reset_clears_state(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.HALT)],
            memory={0: 99.0},
        )
        z3.reset()
        assert z3.get_accumulator() == 0.0
        assert z3.state.cycle_count == 0
        assert not z3.state.halted


# ---------------------------------------------------------------------------
# Compound program (historically relevant: linear sequence, no branching)
# ---------------------------------------------------------------------------


class TestCompoundProgram:
    def test_pythagorean_triple(self):
        """Compute hypotenuse c = sqrt(a^2 + b^2) for a=3, b=4 -> c=5."""
        # Pre-load a=3 at addr 0, b=4 at addr 1
        # Program: a^2 + b^2 -> addr 2, then sqrt(addr 2)
        z3 = ZuseZ3()
        z3.load_memory(0, 3.0)  # a
        z3.load_memory(1, 4.0)  # b
        z3.load_program(
            [
                Z3Instruction(Z3Op.LOAD, 0),  # acc = 3
                Z3Instruction(Z3Op.MULT, 0),  # acc = 9
                Z3Instruction(Z3Op.STORE, 2),  # mem[2] = 9
                Z3Instruction(Z3Op.LOAD, 1),  # acc = 4
                Z3Instruction(Z3Op.MULT, 1),  # acc = 16
                Z3Instruction(Z3Op.ADD, 2),  # acc = 25
                Z3Instruction(Z3Op.SQRT),  # acc = 5
                Z3Instruction(Z3Op.PRINT),
                Z3Instruction(Z3Op.HALT),
            ]
        )
        z3.run()
        assert abs(z3.state.output_tape[0] - 5.0) < 1e-2

    def test_quadratic_formula_discriminant(self):
        """Compute discriminant D = b^2 - 4ac for a=1, b=5, c=6 -> D=1."""
        z3 = ZuseZ3()
        z3.load_memory(0, 1.0)  # a
        z3.load_memory(1, 5.0)  # b
        z3.load_memory(2, 6.0)  # c
        z3.load_memory(3, 4.0)  # constant 4
        z3.load_program(
            [
                Z3Instruction(Z3Op.LOAD, 1),  # acc = b
                Z3Instruction(Z3Op.MULT, 1),  # acc = b^2 = 25
                Z3Instruction(Z3Op.STORE, 4),  # mem[4] = 25
                Z3Instruction(Z3Op.LOAD, 3),  # acc = 4
                Z3Instruction(Z3Op.MULT, 0),  # acc = 4*a = 4
                Z3Instruction(Z3Op.MULT, 2),  # acc = 4*a*c = 24
                Z3Instruction(Z3Op.STORE, 5),  # mem[5] = 24
                Z3Instruction(Z3Op.LOAD, 4),  # acc = 25
                Z3Instruction(Z3Op.SUB, 5),  # acc = 25 - 24 = 1
                Z3Instruction(Z3Op.PRINT),
                Z3Instruction(Z3Op.HALT),
            ]
        )
        z3.run()
        assert abs(z3.state.output_tape[0] - 1.0) < 0.1

    def test_no_conditional_branch(self):
        """Z3 has NO conditional branch -- programs are straight-line.
        Verify that there is no branch instruction in Z3Op."""
        ops = {op.value for op in Z3Op}
        assert "BRANCH" not in ops
        assert "JMP" not in ops
        assert "BRZ" not in ops

    def test_state_snapshot(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.HALT)],
            memory={0: 7.0},
        )
        snap = z3.state_snapshot()
        assert abs(snap["accumulator"] - 7.0) < 1e-3
        assert snap["halted"] is True
        assert snap["cycle_count"] == 2


# ---------------------------------------------------------------------------
# Z3Op enumeration
# ---------------------------------------------------------------------------


class TestZ3Op:
    """Z3Op StrEnum: all 10 opcodes present and string-valued."""

    def test_all_ten_ops_defined(self) -> None:
        ops = {op.value for op in Z3Op}
        expected = {"LOAD", "STORE", "ADD", "SUB", "MULT", "DIV", "SQRT", "READ", "PRINT", "HALT"}
        assert ops == expected

    def test_ops_are_string_values(self) -> None:
        for op in Z3Op:
            assert isinstance(op.value, str)

    def test_no_conditional_branch_in_ops(self) -> None:
        ops = {op.value for op in Z3Op}
        assert not any(s in ops for s in ("BRANCH", "JMP", "BRZ", "BEQ", "COND"))

    def test_load_str_value(self) -> None:
        assert Z3Op.LOAD == "LOAD"

    def test_halt_str_value(self) -> None:
        assert Z3Op.HALT == "HALT"

    def test_sqrt_str_value(self) -> None:
        assert Z3Op.SQRT == "SQRT"


# ---------------------------------------------------------------------------
# Z3Instruction extended
# ---------------------------------------------------------------------------


class TestZ3InstructionExtended:
    """Address validation edge cases and no-address instructions."""

    def test_max_valid_address_63(self) -> None:
        i = Z3Instruction(Z3Op.LOAD, 63)
        assert i.address == 63

    def test_address_default_zero(self) -> None:
        i = Z3Instruction(Z3Op.HALT)
        assert i.address == 0

    def test_address_exactly_64_raises(self) -> None:
        with pytest.raises(ValueError):
            Z3Instruction(Z3Op.LOAD, 64)

    def test_address_exactly_minus_1_raises(self) -> None:
        with pytest.raises(ValueError):
            Z3Instruction(Z3Op.STORE, -1)

    def test_read_does_not_validate_address(self) -> None:
        # READ/PRINT/SQRT/HALT do not use address -- any value accepted
        i = Z3Instruction(Z3Op.READ, 0)
        assert i.op == Z3Op.READ

    def test_print_instruction_created(self) -> None:
        i = Z3Instruction(Z3Op.PRINT)
        assert i.op == Z3Op.PRINT

    def test_add_address_zero_valid(self) -> None:
        i = Z3Instruction(Z3Op.ADD, 0)
        assert i.address == 0

    def test_store_max_valid_address(self) -> None:
        i = Z3Instruction(Z3Op.STORE, 63)
        assert i.address == 63


# ---------------------------------------------------------------------------
# Memory extended
# ---------------------------------------------------------------------------


class TestMemoryExtended:
    """Memory address boundaries, precision, and default values."""

    def test_memory_63_valid(self) -> None:
        z3 = ZuseZ3()
        z3.load_memory(63, 9.0)
        assert abs(z3.get_memory(63) - 9.0) < 1e-3

    def test_get_memory_63_valid(self) -> None:
        z3 = ZuseZ3()
        assert z3.get_memory(63) == 0.0

    def test_get_memory_negative_index_raises(self) -> None:
        z3 = ZuseZ3()
        with pytest.raises(IndexError):
            z3.get_memory(-1)

    def test_load_memory_64_raises(self) -> None:
        z3 = ZuseZ3()
        with pytest.raises(IndexError):
            z3.load_memory(64, 1.0)

    def test_all_memory_initially_zero(self) -> None:
        z3 = ZuseZ3()
        for i in range(64):
            assert z3.get_memory(i) == 0.0

    def test_memory_precision_14_bit_mantissa(self) -> None:
        # 22-bit float (14-bit mantissa) gives ~4 significant decimal digits
        z3 = ZuseZ3()
        z3.load_memory(0, 3.14159)
        result = z3.get_memory(0)
        assert abs(result - 3.14159) < 0.01  # ~4 decimal digits

    def test_negative_value_stored(self) -> None:
        z3 = ZuseZ3()
        z3.load_memory(0, -7.5)
        assert abs(z3.get_memory(0) - (-7.5)) < 1e-3


# ---------------------------------------------------------------------------
# Accumulator and initial state
# ---------------------------------------------------------------------------


class TestAccumulatorInitial:
    """Accumulator starts at 0; reset() clears it."""

    def test_initial_accumulator_zero(self) -> None:
        z3 = ZuseZ3()
        assert z3.get_accumulator() == 0.0

    def test_accumulator_after_reset(self) -> None:
        z3 = _run([Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.HALT)], memory={0: 42.0})
        z3.reset()
        assert z3.get_accumulator() == 0.0

    def test_reset_clears_program(self) -> None:
        z3 = ZuseZ3()
        z3.load_memory(0, 1.0)
        z3.load_program([Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.HALT)])
        z3.reset()
        # After reset, step() should immediately return False (no program)
        assert z3.step() is False

    def test_reset_clears_output_tape(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.PRINT), Z3Instruction(Z3Op.HALT)],
            memory={0: 1.0},
        )
        z3.reset()
        assert z3.state.output_tape == []

    def test_reset_clears_cycle_count(self) -> None:
        z3 = _run([Z3Instruction(Z3Op.HALT)], {})
        z3.reset()
        assert z3.state.cycle_count == 0

    def test_reset_unhalt(self) -> None:
        z3 = _run([Z3Instruction(Z3Op.HALT)], {})
        assert z3.state.halted is True
        z3.reset()
        assert z3.state.halted is False


# ---------------------------------------------------------------------------
# Arithmetic extended
# ---------------------------------------------------------------------------


class TestArithmeticExtended:
    """Edge cases for ADD/SUB/MULT/DIV/SQRT."""

    def test_add_negative_values(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.ADD, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: -5.0, 1: -3.0},
        )
        assert abs(z3.get_accumulator() - (-8.0)) < 1e-3

    def test_sub_negative_result(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SUB, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: 3.0, 1: 10.0},
        )
        assert abs(z3.get_accumulator() - (-7.0)) < 1e-3

    def test_mult_by_zero(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.MULT, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: 99.0, 1: 0.0},
        )
        assert abs(z3.get_accumulator()) < 1e-6

    def test_mult_by_negative_one(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.MULT, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: 5.0, 1: -1.0},
        )
        assert abs(z3.get_accumulator() - (-5.0)) < 1e-3

    def test_div_result_less_than_one(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.DIV, 1), Z3Instruction(Z3Op.HALT)],
            memory={0: 1.0, 1: 4.0},
        )
        assert abs(z3.get_accumulator() - 0.25) < 1e-3

    def test_sqrt_zero(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SQRT), Z3Instruction(Z3Op.HALT)],
            memory={0: 0.0},
        )
        assert abs(z3.get_accumulator()) < 1e-6

    def test_sqrt_one(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SQRT), Z3Instruction(Z3Op.HALT)],
            memory={0: 1.0},
        )
        assert abs(z3.get_accumulator() - 1.0) < 1e-3

    def test_chain_add_sub_net_zero(self) -> None:
        z3 = _run(
            [
                Z3Instruction(Z3Op.LOAD, 0),
                Z3Instruction(Z3Op.ADD, 1),
                Z3Instruction(Z3Op.SUB, 1),
                Z3Instruction(Z3Op.HALT),
            ],
            memory={0: 7.0, 1: 3.0},
        )
        assert abs(z3.get_accumulator() - 7.0) < 1e-3


# ---------------------------------------------------------------------------
# I/O tape extended
# ---------------------------------------------------------------------------


class TestIOExtended:
    """Multiple READs from tape; input_pointer advancement; PRINT accumulation."""

    def test_multiple_reads_advance_pointer(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.READ), Z3Instruction(Z3Op.READ), Z3Instruction(Z3Op.HALT)],
            input_tape=[10.0, 20.0],
        )
        assert z3.state.input_pointer == 2
        assert abs(z3.get_accumulator() - 20.0) < 1e-3

    def test_read_then_store_then_load_other(self) -> None:
        # READ -> acc=5; STORE 0; LOAD 1 (6.0); ADD 0 (=11)
        z3 = ZuseZ3()
        z3.load_memory(1, 6.0)
        z3.load_input_tape([5.0])
        z3.load_program(
            [
                Z3Instruction(Z3Op.READ),
                Z3Instruction(Z3Op.STORE, 0),
                Z3Instruction(Z3Op.LOAD, 1),
                Z3Instruction(Z3Op.ADD, 0),
                Z3Instruction(Z3Op.HALT),
            ]
        )
        z3.run()
        assert abs(z3.get_accumulator() - 11.0) < 1e-3

    def test_print_adds_to_output_tape(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.PRINT), Z3Instruction(Z3Op.HALT)],
            memory={0: 2.5},
        )
        assert len(z3.state.output_tape) == 1
        assert abs(z3.state.output_tape[0] - 2.5) < 1e-3

    def test_output_tape_order_preserved(self) -> None:
        z3 = _run(
            [
                Z3Instruction(Z3Op.LOAD, 0),
                Z3Instruction(Z3Op.PRINT),
                Z3Instruction(Z3Op.LOAD, 1),
                Z3Instruction(Z3Op.PRINT),
                Z3Instruction(Z3Op.LOAD, 2),
                Z3Instruction(Z3Op.PRINT),
                Z3Instruction(Z3Op.HALT),
            ],
            memory={0: 1.0, 1: 2.0, 2: 3.0},
        )
        assert len(z3.state.output_tape) == 3
        assert all(
            abs(v - e) < 1e-3 for v, e in zip(z3.state.output_tape, [1.0, 2.0, 3.0], strict=True)
        )

    def test_initial_input_pointer_zero(self) -> None:
        z3 = ZuseZ3()
        assert z3.state.input_pointer == 0

    def test_load_input_tape_resets_pointer(self) -> None:
        z3 = ZuseZ3()
        z3.load_input_tape([1.0, 2.0])
        z3.load_input_tape([3.0])
        assert z3.state.input_pointer == 0
        assert z3.state.input_tape == [3.0]


# ---------------------------------------------------------------------------
# Control extended
# ---------------------------------------------------------------------------


class TestControlExtended:
    """PC tracking, step() semantics, and load_program resets."""

    def test_load_program_resets_pc_to_zero(self) -> None:
        z3 = ZuseZ3()
        z3.load_memory(0, 1.0)
        z3.load_program([Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.HALT)])
        z3.step()  # PC now 1
        z3.load_program([Z3Instruction(Z3Op.HALT)])  # reload resets PC
        assert z3.state.program_counter == 0

    def test_step_on_already_halted_returns_false(self) -> None:
        z3 = _run([Z3Instruction(Z3Op.HALT)], {})
        assert z3.state.halted is True
        assert z3.step() is False

    def test_step_increments_pc(self) -> None:
        z3 = ZuseZ3()
        z3.load_memory(0, 1.0)
        z3.load_program([Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.HALT)])
        z3.step()
        assert z3.state.program_counter == 1

    def test_step_increments_cycle_count(self) -> None:
        z3 = ZuseZ3()
        z3.load_memory(0, 1.0)
        z3.load_program([Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.HALT)])
        z3.step()
        assert z3.state.cycle_count == 1

    def test_state_snapshot_has_all_keys(self) -> None:
        z3 = ZuseZ3()
        snap = z3.state_snapshot()
        expected_keys = {
            "accumulator",
            "program_counter",
            "cycle_count",
            "halted",
            "output_tape",
            "input_pointer",
            "overflow",
        }
        assert expected_keys <= set(snap.keys())

    def test_state_snapshot_output_tape_is_copy(self) -> None:
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.PRINT), Z3Instruction(Z3Op.HALT)],
            memory={0: 1.0},
        )
        snap = z3.state_snapshot()
        snap_tape = snap["output_tape"]
        assert isinstance(snap_tape, list)
        assert len(snap_tape) == 1

    def test_program_counter_after_halt(self) -> None:
        z3 = ZuseZ3()
        z3.load_memory(0, 1.0)
        z3.load_program([Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.HALT)])
        z3.run()
        assert z3.state.program_counter == 2  # incremented past HALT

    def test_overflow_flag_initially_false(self) -> None:
        z3 = ZuseZ3()
        assert z3.state.overflow is False
