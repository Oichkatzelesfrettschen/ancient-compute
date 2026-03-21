"""Unit tests for the ENIAC emulator (1945)."""

from decimal import Decimal

import pytest

from backend.src.emulator.eniac import _NUM_ACCUMULATORS, ENIAC, ENIACInstruction, ENIACOp


def _run(instructions, accumulators=None):
    e = ENIAC()
    if accumulators:
        for i, v in accumulators.items():
            e.load_accumulator(i, Decimal(str(v)))
    e.load_program(instructions)
    e.run()
    return e


class TestAccumulators:
    def test_load_and_read(self):
        e = ENIAC()
        e.load_accumulator(0, Decimal("42"))
        assert e.get_accumulator(0) == Decimal("42")

    def test_default_zero(self):
        e = ENIAC()
        for i in range(_NUM_ACCUMULATORS):
            assert e.get_accumulator(i) == Decimal("0")

    def test_out_of_range(self):
        e = ENIAC()
        with pytest.raises(IndexError):
            e.load_accumulator(_NUM_ACCUMULATORS, Decimal("1"))


class TestArithmetic:
    def test_add(self):
        e = _run(
            [ENIACInstruction(ENIACOp.ADD, (0, 1)), ENIACInstruction(ENIACOp.HALT)],
            accumulators={0: 10, 1: 5},
        )
        assert e.get_accumulator(0) == Decimal("15")

    def test_sub(self):
        e = _run(
            [ENIACInstruction(ENIACOp.SUB, (0, 1)), ENIACInstruction(ENIACOp.HALT)],
            accumulators={0: 10, 1: 3},
        )
        assert e.get_accumulator(0) == Decimal("7")

    def test_mult(self):
        e = _run(
            [ENIACInstruction(ENIACOp.MULT, (2, 0, 1)), ENIACInstruction(ENIACOp.HALT)],
            accumulators={0: 6, 1: 7},
        )
        assert e.get_accumulator(2) == Decimal("42")

    def test_div(self):
        e = _run(
            [ENIACInstruction(ENIACOp.DIV, (2, 0, 1)), ENIACInstruction(ENIACOp.HALT)],
            accumulators={0: 15, 1: 3},
        )
        assert e.get_accumulator(2) == Decimal("5")

    def test_sqrt(self):
        e = _run(
            [ENIACInstruction(ENIACOp.SQRT, (1, 0)), ENIACInstruction(ENIACOp.HALT)],
            accumulators={0: 144},
        )
        assert float(e.get_accumulator(1)) == pytest.approx(12.0, rel=1e-4)

    def test_clear(self):
        e = _run(
            [ENIACInstruction(ENIACOp.CLEAR, (0,)), ENIACInstruction(ENIACOp.HALT)],
            accumulators={0: 99},
        )
        assert e.get_accumulator(0) == Decimal("0")

    def test_load_immediate(self):
        e = _run([ENIACInstruction(ENIACOp.LOAD, (0, "3.14")), ENIACInstruction(ENIACOp.HALT)])
        assert float(e.get_accumulator(0)) == pytest.approx(3.14, rel=1e-4)

    def test_div_by_zero(self):
        e = ENIAC()
        e.load_accumulator(0, Decimal("5"))
        e.load_accumulator(1, Decimal("0"))
        e.load_program([ENIACInstruction(ENIACOp.DIV, (2, 0, 1)), ENIACInstruction(ENIACOp.HALT)])
        with pytest.raises(ZeroDivisionError):
            e.run()

    def test_sqrt_negative(self):
        e = ENIAC()
        e.load_accumulator(0, Decimal("-4"))
        e.load_program([ENIACInstruction(ENIACOp.SQRT, (1, 0)), ENIACInstruction(ENIACOp.HALT)])
        with pytest.raises(ValueError):
            e.run()


class TestIO:
    def test_print(self):
        e = _run(
            [
                ENIACInstruction(ENIACOp.LOAD, (0, "7")),
                ENIACInstruction(ENIACOp.PRINT, (0,)),
                ENIACInstruction(ENIACOp.HALT),
            ]
        )
        assert len(e.state.output_tape) == 1
        assert float(e.state.output_tape[0]) == pytest.approx(7.0)


class TestControl:
    def test_halt(self):
        e = ENIAC()
        e.load_program([ENIACInstruction(ENIACOp.HALT)])
        e.run()
        assert e.state.halted

    def test_cycle_count(self):
        e = _run(
            [
                ENIACInstruction(ENIACOp.LOAD, (0, "1")),
                ENIACInstruction(ENIACOp.LOAD, (1, "2")),
                ENIACInstruction(ENIACOp.HALT),
            ]
        )
        assert e.state.cycle_count == 3

    def test_pythagorean_triple(self):
        """ENIAC's first real task was ballistics -- compute c = sqrt(a^2+b^2)."""
        e = ENIAC()
        e.load_accumulator(0, Decimal("3"))
        e.load_accumulator(1, Decimal("4"))
        e.load_program(
            [
                ENIACInstruction(ENIACOp.MULT, (2, 0, 0)),  # acc[2] = 9
                ENIACInstruction(ENIACOp.MULT, (3, 1, 1)),  # acc[3] = 16
                ENIACInstruction(ENIACOp.ADD, (2, 3)),  # acc[2] = 25
                ENIACInstruction(ENIACOp.SQRT, (4, 2)),  # acc[4] = 5
                ENIACInstruction(ENIACOp.PRINT, (4,)),
                ENIACInstruction(ENIACOp.HALT),
            ]
        )
        e.run()
        assert float(e.state.output_tape[0]) == pytest.approx(5.0, rel=1e-4)

    def test_reset(self):
        e = _run([ENIACInstruction(ENIACOp.LOAD, (0, "99")), ENIACInstruction(ENIACOp.HALT)])
        e.reset()
        assert e.get_accumulator(0) == Decimal("0")
        assert e.state.cycle_count == 0

    def test_state_snapshot(self):
        e = _run([ENIACInstruction(ENIACOp.HALT)])
        s = e.state_snapshot()
        assert s["num_accumulators"] == _NUM_ACCUMULATORS
        assert s["halted"] is True


class TestStep:
    def test_step_executes_one_instruction(self):
        e = ENIAC()
        e.load_accumulator(0, Decimal("5"))
        e.load_program([
            ENIACInstruction(ENIACOp.LOAD, (1, "3")),
            ENIACInstruction(ENIACOp.HALT),
        ])
        e.step()
        assert e.get_accumulator(1) == Decimal("3")
        assert e.state.program_counter == 1

    def test_step_returns_true_while_running(self):
        e = ENIAC()
        e.load_program([
            ENIACInstruction(ENIACOp.LOAD, (0, "1")),
            ENIACInstruction(ENIACOp.HALT),
        ])
        assert e.step() is True   # LOAD
        assert e.step() is False  # HALT

    def test_step_on_halted_machine_returns_false(self):
        e = ENIAC()
        e.load_program([ENIACInstruction(ENIACOp.HALT)])
        e.run()
        assert e.step() is False

    def test_step_advances_cycle_count(self):
        e = ENIAC()
        e.load_program([
            ENIACInstruction(ENIACOp.LOAD, (0, "1")),
            ENIACInstruction(ENIACOp.LOAD, (1, "2")),
            ENIACInstruction(ENIACOp.HALT),
        ])
        e.step()
        assert e.state.cycle_count == 1
        e.step()
        assert e.state.cycle_count == 2


class TestEdgeCases:
    def test_sub_result_negative(self):
        # SUB when src > dst: result goes negative
        e = _run(
            [ENIACInstruction(ENIACOp.SUB, (0, 1)), ENIACInstruction(ENIACOp.HALT)],
            accumulators={0: 3, 1: 10},
        )
        assert e.get_accumulator(0) == Decimal("-7")

    def test_add_accumulator_to_itself(self):
        # ADD acc[0], acc[0] doubles the value
        e = _run(
            [ENIACInstruction(ENIACOp.ADD, (0, 0)), ENIACInstruction(ENIACOp.HALT)],
            accumulators={0: 6},
        )
        assert e.get_accumulator(0) == Decimal("12")

    def test_multiple_print_outputs(self):
        e = _run([
            ENIACInstruction(ENIACOp.LOAD, (0, "1")),
            ENIACInstruction(ENIACOp.LOAD, (1, "2")),
            ENIACInstruction(ENIACOp.LOAD, (2, "3")),
            ENIACInstruction(ENIACOp.PRINT, (0,)),
            ENIACInstruction(ENIACOp.PRINT, (1,)),
            ENIACInstruction(ENIACOp.PRINT, (2,)),
            ENIACInstruction(ENIACOp.HALT),
        ])
        assert len(e.state.output_tape) == 3
        assert [float(v) for v in e.state.output_tape] == pytest.approx([1.0, 2.0, 3.0])

    def test_unknown_opcode_raises(self):
        e = ENIAC()
        e.load_program([ENIACInstruction("UNKNOWN_OP", ())])
        with pytest.raises(ValueError):
            e.run()

    def test_clear_then_add(self):
        e = _run([
            ENIACInstruction(ENIACOp.LOAD, (0, "99")),
            ENIACInstruction(ENIACOp.CLEAR, (0,)),
            ENIACInstruction(ENIACOp.ADD, (0, 1)),
            ENIACInstruction(ENIACOp.HALT),
        ], accumulators={1: 42})
        assert e.get_accumulator(0) == Decimal("42")

    def test_div_fractional_result(self):
        e = _run(
            [ENIACInstruction(ENIACOp.DIV, (2, 0, 1)), ENIACInstruction(ENIACOp.HALT)],
            accumulators={0: 1, 1: 3},
        )
        # 1/3 as Decimal with 10-digit precision
        assert float(e.get_accumulator(2)) == pytest.approx(1 / 3, rel=1e-6)

    def test_reset_clears_output_tape(self):
        e = _run([
            ENIACInstruction(ENIACOp.LOAD, (0, "7")),
            ENIACInstruction(ENIACOp.PRINT, (0,)),
            ENIACInstruction(ENIACOp.HALT),
        ])
        e.reset()
        assert e.state.output_tape == []

    def test_state_snapshot_output_tape(self):
        e = _run([
            ENIACInstruction(ENIACOp.LOAD, (0, "5")),
            ENIACInstruction(ENIACOp.PRINT, (0,)),
            ENIACInstruction(ENIACOp.HALT),
        ])
        s = e.state_snapshot()
        assert s["output_tape"] == pytest.approx([5.0])
