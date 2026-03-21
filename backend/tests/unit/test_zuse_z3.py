"""Unit tests for the Zuse Z3 emulator (Germany, 1941)."""

import math

import pytest

from backend.src.emulator.zuse_z3 import Z3Instruction, Z3Op, ZuseZ3


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _run(instructions: list[Z3Instruction], memory: dict[int, float] | None = None,
         input_tape: list[float] | None = None) -> ZuseZ3:
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
        z3.load_program([
            Z3Instruction(Z3Op.LOAD, 0),
            Z3Instruction(Z3Op.STORE, 1),
            Z3Instruction(Z3Op.HALT),
        ])
        z3.run()
        assert abs(z3.get_memory(1) - 5.0) < 1e-3

    def test_load_store_roundtrip(self):
        z3 = ZuseZ3()
        z3.load_memory(3, 42.0)
        z3.load_program([
            Z3Instruction(Z3Op.LOAD, 3),
            Z3Instruction(Z3Op.STORE, 10),
            Z3Instruction(Z3Op.LOAD, 10),
            Z3Instruction(Z3Op.HALT),
        ])
        z3.run()
        assert abs(z3.get_accumulator() - 42.0) < 1e-3


# ---------------------------------------------------------------------------
# Arithmetic operations
# ---------------------------------------------------------------------------


class TestArithmetic:
    def test_add(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.ADD, 1),
             Z3Instruction(Z3Op.HALT)],
            memory={0: 3.0, 1: 4.0},
        )
        assert abs(z3.get_accumulator() - 7.0) < 1e-3

    def test_subtract(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SUB, 1),
             Z3Instruction(Z3Op.HALT)],
            memory={0: 10.0, 1: 3.0},
        )
        assert abs(z3.get_accumulator() - 7.0) < 1e-3

    def test_multiply(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.MULT, 1),
             Z3Instruction(Z3Op.HALT)],
            memory={0: 6.0, 1: 7.0},
        )
        assert abs(z3.get_accumulator() - 42.0) < 1e-3

    def test_divide(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.DIV, 1),
             Z3Instruction(Z3Op.HALT)],
            memory={0: 10.0, 1: 4.0},
        )
        assert abs(z3.get_accumulator() - 2.5) < 1e-3

    def test_sqrt(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SQRT),
             Z3Instruction(Z3Op.HALT)],
            memory={0: 16.0},
        )
        assert abs(z3.get_accumulator() - 4.0) < 1e-3

    def test_sqrt_two(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SQRT),
             Z3Instruction(Z3Op.HALT)],
            memory={0: 2.0},
        )
        assert abs(z3.get_accumulator() - math.sqrt(2)) < 1e-3

    def test_divide_by_zero_raises(self):
        z3 = ZuseZ3()
        z3.load_memory(0, 5.0)
        z3.load_memory(1, 0.0)
        z3.load_program([Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.DIV, 1),
                         Z3Instruction(Z3Op.HALT)])
        with pytest.raises(ZeroDivisionError):
            z3.run()

    def test_sqrt_negative_raises(self):
        z3 = ZuseZ3()
        z3.load_memory(0, -4.0)
        z3.load_program([Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.SQRT),
                         Z3Instruction(Z3Op.HALT)])
        with pytest.raises(ValueError):
            z3.run()

    def test_negative_multiply(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.MULT, 1),
             Z3Instruction(Z3Op.HALT)],
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
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.PRINT),
             Z3Instruction(Z3Op.HALT)],
            memory={0: 3.14},
        )
        assert len(z3.state.output_tape) == 1
        assert abs(z3.state.output_tape[0] - 3.14) < 1e-3

    def test_multiple_prints(self):
        z3 = _run(
            [
                Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.PRINT),
                Z3Instruction(Z3Op.LOAD, 1), Z3Instruction(Z3Op.PRINT),
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
        z3.load_program([
            Z3Instruction(Z3Op.READ),
            Z3Instruction(Z3Op.READ),  # Tape exhausted
            Z3Instruction(Z3Op.HALT),
        ])
        with pytest.raises(RuntimeError, match="exhausted"):
            z3.run()


# ---------------------------------------------------------------------------
# Program control
# ---------------------------------------------------------------------------


class TestControl:
    def test_halt_stops_execution(self):
        z3 = ZuseZ3()
        z3.load_memory(0, 1.0)
        z3.load_program([
            Z3Instruction(Z3Op.LOAD, 0),
            Z3Instruction(Z3Op.HALT),
            Z3Instruction(Z3Op.LOAD, 0),  # Never reached
        ])
        z3.run()
        assert z3.state.program_counter == 2   # stopped after HALT
        assert z3.state.cycle_count == 2        # LOAD + HALT

    def test_end_of_tape_stops(self):
        """Running off the end of the tape halts automatically."""
        z3 = ZuseZ3()
        z3.load_memory(0, 5.0)
        z3.load_program([
            Z3Instruction(Z3Op.LOAD, 0),
            # No HALT -- end of tape
        ])
        z3.run()
        assert z3.state.halted

    def test_cycle_count(self):
        z3 = _run(
            [Z3Instruction(Z3Op.LOAD, 0), Z3Instruction(Z3Op.ADD, 1),
             Z3Instruction(Z3Op.HALT)],
            memory={0: 1.0, 1: 2.0},
        )
        assert z3.state.cycle_count == 3

    def test_step_by_step(self):
        """step() executes one instruction at a time."""
        z3 = ZuseZ3()
        z3.load_memory(0, 5.0)
        z3.load_program([
            Z3Instruction(Z3Op.LOAD, 0),
            Z3Instruction(Z3Op.HALT),
        ])
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
        z3.load_memory(0, 3.0)   # a
        z3.load_memory(1, 4.0)   # b
        z3.load_program([
            Z3Instruction(Z3Op.LOAD, 0),   # acc = 3
            Z3Instruction(Z3Op.MULT, 0),   # acc = 9
            Z3Instruction(Z3Op.STORE, 2),  # mem[2] = 9
            Z3Instruction(Z3Op.LOAD, 1),   # acc = 4
            Z3Instruction(Z3Op.MULT, 1),   # acc = 16
            Z3Instruction(Z3Op.ADD, 2),    # acc = 25
            Z3Instruction(Z3Op.SQRT),      # acc = 5
            Z3Instruction(Z3Op.PRINT),
            Z3Instruction(Z3Op.HALT),
        ])
        z3.run()
        assert abs(z3.state.output_tape[0] - 5.0) < 1e-2

    def test_quadratic_formula_discriminant(self):
        """Compute discriminant D = b^2 - 4ac for a=1, b=5, c=6 -> D=1."""
        z3 = ZuseZ3()
        z3.load_memory(0, 1.0)   # a
        z3.load_memory(1, 5.0)   # b
        z3.load_memory(2, 6.0)   # c
        z3.load_memory(3, 4.0)   # constant 4
        z3.load_program([
            Z3Instruction(Z3Op.LOAD, 1),   # acc = b
            Z3Instruction(Z3Op.MULT, 1),   # acc = b^2 = 25
            Z3Instruction(Z3Op.STORE, 4),  # mem[4] = 25
            Z3Instruction(Z3Op.LOAD, 3),   # acc = 4
            Z3Instruction(Z3Op.MULT, 0),   # acc = 4*a = 4
            Z3Instruction(Z3Op.MULT, 2),   # acc = 4*a*c = 24
            Z3Instruction(Z3Op.STORE, 5),  # mem[5] = 24
            Z3Instruction(Z3Op.LOAD, 4),   # acc = 25
            Z3Instruction(Z3Op.SUB, 5),    # acc = 25 - 24 = 1
            Z3Instruction(Z3Op.PRINT),
            Z3Instruction(Z3Op.HALT),
        ])
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
