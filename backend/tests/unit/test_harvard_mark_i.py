"""Unit tests for the Harvard Mark I (IBM ASCC) emulator (1944)."""

from __future__ import annotations

from decimal import Decimal

import pytest

from backend.src.emulator.harvard_mark_i import (
    _NUM_CONSTANTS,
    _NUM_COUNTERS,
    _TOTAL_STORAGE,
    HarvardMarkI,
    MarkIInstruction,
    MarkIOp,
)

# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------


def _run(
    instructions: list[MarkIInstruction],
    counters: dict[int, object] | None = None,
    constants: dict[int, object] | None = None,
) -> HarvardMarkI:
    m = HarvardMarkI()
    if counters:
        for i, v in counters.items():
            m.set_counter(i, Decimal(str(v)))
    if constants:
        for i, v in constants.items():
            m.set_constant(i, Decimal(str(v)))
    m.load_program(instructions)
    m.run()
    return m


# ---------------------------------------------------------------------------
# Storage: counters
# ---------------------------------------------------------------------------


class TestCounters:
    def test_set_and_get_counter(self):
        m = HarvardMarkI()
        m.set_counter(0, Decimal("42"))
        assert m.get_counter(0) == Decimal("42")

    def test_counter_default_zero(self):
        m = HarvardMarkI()
        for i in range(_NUM_COUNTERS):
            assert m.get_counter(i) == Decimal("0")

    def test_all_72_counters_settable(self):
        m = HarvardMarkI()
        for i in range(_NUM_COUNTERS):
            m.set_counter(i, Decimal(str(i)))
        for i in range(_NUM_COUNTERS):
            assert m.get_counter(i) == Decimal(str(i))

    def test_counter_out_of_range_high(self):
        m = HarvardMarkI()
        with pytest.raises(IndexError):
            m.set_counter(_NUM_COUNTERS, Decimal("1"))

    def test_counter_out_of_range_negative(self):
        m = HarvardMarkI()
        with pytest.raises(IndexError):
            m.set_counter(-1, Decimal("1"))

    def test_get_counter_out_of_range(self):
        m = HarvardMarkI()
        with pytest.raises(IndexError):
            m.get_counter(_NUM_COUNTERS)

    def test_counter_stores_decimal(self):
        m = HarvardMarkI()
        m.set_counter(10, Decimal("3.14159"))
        assert m.get_counter(10) == Decimal("3.14159")

    def test_counter_stores_negative(self):
        m = HarvardMarkI()
        m.set_counter(5, Decimal("-999"))
        assert m.get_counter(5) == Decimal("-999")


# ---------------------------------------------------------------------------
# Storage: constants
# ---------------------------------------------------------------------------


class TestConstants:
    def test_constant_default_zero(self):
        m = HarvardMarkI()
        assert m.get_constant(0) == Decimal("0")

    def test_set_constant(self):
        m = HarvardMarkI()
        m.set_constant(0, Decimal("3.14159"))
        assert m.get_constant(0) == Decimal("3.14159")

    def test_all_60_constants_settable(self):
        m = HarvardMarkI()
        for i in range(_NUM_CONSTANTS):
            m.set_constant(i, Decimal(str(i * 2)))
        for i in range(_NUM_CONSTANTS):
            assert m.get_constant(i) == Decimal(str(i * 2))

    def test_constant_out_of_range_high(self):
        m = HarvardMarkI()
        with pytest.raises(IndexError):
            m.set_constant(_NUM_CONSTANTS, Decimal("1"))

    def test_constant_out_of_range_negative(self):
        m = HarvardMarkI()
        with pytest.raises(IndexError):
            m.set_constant(-1, Decimal("1"))

    def test_get_constant_out_of_range(self):
        m = HarvardMarkI()
        with pytest.raises(IndexError):
            m.get_constant(_NUM_CONSTANTS)

    def test_constant_read_only_via_write(self):
        """Constant registers (addresses 72+) are read-only via _write."""
        m = HarvardMarkI()
        prog = [
            MarkIInstruction(MarkIOp.SET, (_NUM_COUNTERS, "1.0")),
            MarkIInstruction(MarkIOp.HALT),
        ]
        m.load_program(prog)
        with pytest.raises(PermissionError):
            m.run()

    def test_total_storage_size(self):
        assert _TOTAL_STORAGE == _NUM_COUNTERS + _NUM_CONSTANTS


# ---------------------------------------------------------------------------
# Arithmetic
# ---------------------------------------------------------------------------


class TestArithmetic:
    def test_add(self):
        m = _run(
            [MarkIInstruction(MarkIOp.ADD, (0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 3, 1: 4},
        )
        assert m.get_counter(0) == Decimal("7")

    def test_add_to_negative(self):
        m = _run(
            [MarkIInstruction(MarkIOp.ADD, (0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: -5, 1: 3},
        )
        assert m.get_counter(0) == Decimal("-2")

    def test_add_zero(self):
        m = _run(
            [MarkIInstruction(MarkIOp.ADD, (0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 42, 1: 0},
        )
        assert m.get_counter(0) == Decimal("42")

    def test_subtract(self):
        m = _run(
            [MarkIInstruction(MarkIOp.SUB, (0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 10, 1: 3},
        )
        assert m.get_counter(0) == Decimal("7")

    def test_subtract_to_negative(self):
        m = _run(
            [MarkIInstruction(MarkIOp.SUB, (0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 3, 1: 10},
        )
        assert m.get_counter(0) == Decimal("-7")

    def test_multiply(self):
        m = _run(
            [MarkIInstruction(MarkIOp.MULT, (2, 0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 6, 1: 7},
        )
        assert m.get_counter(2) == Decimal("42")

    def test_multiply_decimal(self):
        m = _run(
            [MarkIInstruction(MarkIOp.MULT, (2, 0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: "1.5", 1: "2"},
        )
        assert m.get_counter(2) == Decimal("3.0")

    def test_multiply_by_zero(self):
        m = _run(
            [MarkIInstruction(MarkIOp.MULT, (2, 0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 99, 1: 0},
        )
        assert m.get_counter(2) == Decimal("0")

    def test_multiply_with_constant(self):
        """Multiply counter by constant register value."""
        m = HarvardMarkI()
        m.set_counter(0, Decimal("7"))
        m.set_constant(0, Decimal("3"))
        m.load_program(
            [
                # counter[1] = counter[0] * const[0]
                MarkIInstruction(MarkIOp.MULT, (1, 0, _NUM_COUNTERS)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        m.run()
        assert m.get_counter(1) == Decimal("21")

    def test_divide(self):
        m = _run(
            [MarkIInstruction(MarkIOp.DIV, (2, 0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 10, 1: 4},
        )
        assert m.get_counter(2) == Decimal("2.5")

    def test_divide_exact(self):
        m = _run(
            [MarkIInstruction(MarkIOp.DIV, (2, 0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 100, 1: 5},
        )
        assert m.get_counter(2) == Decimal("20")

    def test_divide_by_zero(self):
        m = HarvardMarkI()
        m.set_counter(0, Decimal("5"))
        m.set_counter(1, Decimal("0"))
        m.load_program(
            [
                MarkIInstruction(MarkIOp.DIV, (2, 0, 1)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        with pytest.raises(ZeroDivisionError):
            m.run()

    def test_set_instruction(self):
        m = _run([MarkIInstruction(MarkIOp.SET, (0, "99")), MarkIInstruction(MarkIOp.HALT)])
        assert m.get_counter(0) == Decimal("99")

    def test_set_negative_value(self):
        m = _run([MarkIInstruction(MarkIOp.SET, (0, "-42")), MarkIInstruction(MarkIOp.HALT)])
        assert m.get_counter(0) == Decimal("-42")

    def test_set_decimal_value(self):
        m = _run([MarkIInstruction(MarkIOp.SET, (0, "2.71828")), MarkIInstruction(MarkIOp.HALT)])
        assert m.get_counter(0) == Decimal("2.71828")

    def test_bessel_function_step(self):
        """Compute one step of J0: historical first computation on Mark I."""
        m = HarvardMarkI()
        m.set_counter(0, Decimal("1"))
        m.set_counter(1, Decimal("0"))
        m.set_counter(2, Decimal("2"))
        m.load_program(
            [
                MarkIInstruction(MarkIOp.MULT, (4, 2, 1)),
                MarkIInstruction(MarkIOp.SUB, (4, 0)),
                MarkIInstruction(MarkIOp.PRINT, (4,)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        m.run()
        assert m.state.output_tape[0] == Decimal("-1")


# ---------------------------------------------------------------------------
# LOAD / STORE / SET
# ---------------------------------------------------------------------------


class TestLoadStore:
    def test_load_copies_to_counter(self):
        m = HarvardMarkI()
        m.set_counter(5, Decimal("77"))
        m.load_program(
            [
                MarkIInstruction(MarkIOp.LOAD, (0, 5)),  # counter[0] = storage[5] = 77
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        m.run()
        assert m.get_counter(0) == Decimal("77")

    def test_store_copies_between_counters(self):
        m = HarvardMarkI()
        m.set_counter(0, Decimal("77"))
        m.load_program(
            [
                MarkIInstruction(MarkIOp.STORE, (1, 0)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        m.run()
        assert m.get_counter(1) == Decimal("77")

    def test_load_from_constant(self):
        m = HarvardMarkI()
        m.set_constant(0, Decimal("2.71828"))
        m.load_program(
            [
                MarkIInstruction(MarkIOp.LOAD, (0, _NUM_COUNTERS)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        m.run()
        assert m.get_counter(0) == Decimal("2.71828")

    def test_chain_set_add_store(self):
        m = _run(
            [
                MarkIInstruction(MarkIOp.SET, (0, "10")),
                MarkIInstruction(MarkIOp.SET, (1, "5")),
                MarkIInstruction(MarkIOp.ADD, (0, 1)),
                MarkIInstruction(MarkIOp.STORE, (2, 0)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        assert m.get_counter(2) == Decimal("15")


# ---------------------------------------------------------------------------
# PRINT and OUTPUT
# ---------------------------------------------------------------------------


class TestPrint:
    def test_print_single_value(self):
        m = _run(
            [
                MarkIInstruction(MarkIOp.SET, (0, "3.14")),
                MarkIInstruction(MarkIOp.PRINT, (0,)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        assert len(m.state.output_tape) == 1
        assert float(m.state.output_tape[0]) == pytest.approx(3.14)

    def test_print_multiple_values(self):
        m = _run(
            [
                MarkIInstruction(MarkIOp.SET, (0, "1")),
                MarkIInstruction(MarkIOp.SET, (1, "2")),
                MarkIInstruction(MarkIOp.SET, (2, "3")),
                MarkIInstruction(MarkIOp.PRINT, (0,)),
                MarkIInstruction(MarkIOp.PRINT, (1,)),
                MarkIInstruction(MarkIOp.PRINT, (2,)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        assert len(m.state.output_tape) == 3
        assert [float(v) for v in m.state.output_tape] == pytest.approx([1.0, 2.0, 3.0])

    def test_print_zero(self):
        m = _run(
            [
                MarkIInstruction(MarkIOp.PRINT, (0,)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        assert m.state.output_tape[0] == Decimal("0")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


class TestControl:
    def test_halt_stops_program(self):
        m = HarvardMarkI()
        m.load_program(
            [
                MarkIInstruction(MarkIOp.HALT),
                MarkIInstruction(MarkIOp.SET, (0, "99")),  # never reached
            ]
        )
        m.run()
        assert m.get_counter(0) == Decimal("0")

    def test_halt_sets_halted_flag(self):
        m = HarvardMarkI()
        m.load_program([MarkIInstruction(MarkIOp.HALT)])
        m.run()
        assert m.state.halted

    def test_step_returns_false_when_halted(self):
        m = HarvardMarkI()
        m.load_program([MarkIInstruction(MarkIOp.HALT)])
        m.run()
        result = m.step()
        assert result is False

    def test_step_returns_false_at_end_of_program(self):
        """step() returns False if PC is past end of program."""
        m = HarvardMarkI()
        m.load_program([MarkIInstruction(MarkIOp.SET, (0, "1"))])
        m.step()  # runs SET
        result = m.step()  # PC now past end
        assert result is False

    def test_step_returns_true_normally(self):
        m = HarvardMarkI()
        m.load_program(
            [
                MarkIInstruction(MarkIOp.SET, (0, "1")),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        result = m.step()
        assert result is True

    def test_cycle_count(self):
        m = _run(
            [
                MarkIInstruction(MarkIOp.SET, (0, "1")),
                MarkIInstruction(MarkIOp.SET, (1, "2")),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        assert m.state.cycle_count == 3

    def test_last_result_updated(self):
        m = _run(
            [
                MarkIInstruction(MarkIOp.SET, (0, "77")),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        assert m.state.last_result == Decimal("77")

    def test_branch_taken(self):
        m = HarvardMarkI()
        m.load_program(
            [
                MarkIInstruction(MarkIOp.SET, (0, "5"), label="top"),
                MarkIInstruction(MarkIOp.BRANCH, ("end",)),
                MarkIInstruction(MarkIOp.SET, (0, "99")),  # not reached
                MarkIInstruction(MarkIOp.HALT, label="end"),
            ]
        )
        m.run()
        assert m.get_counter(0) == Decimal("5")

    def test_branch_not_taken_when_zero(self):
        m = HarvardMarkI()
        m.load_program(
            [
                MarkIInstruction(MarkIOp.SET, (0, "0"), label="top"),
                MarkIInstruction(MarkIOp.BRANCH, ("top",)),  # last_result=0 -> no jump
                MarkIInstruction(MarkIOp.SET, (0, "99")),  # reached
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        m.run()
        assert m.get_counter(0) == Decimal("99")

    def test_branch_undefined_label_raises(self):
        m = HarvardMarkI()
        m.load_program(
            [
                MarkIInstruction(MarkIOp.SET, (0, "1")),  # last_result = 1 != 0
                MarkIInstruction(MarkIOp.BRANCH, ("no_such_label",)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        with pytest.raises(KeyError):
            m.run()

    def test_unknown_op_raises(self):
        m = HarvardMarkI()
        m.load_program([MarkIInstruction("INVALID_OP", ())])
        with pytest.raises(ValueError, match="Unknown Mark I operation"):
            m.run()

    def test_reset_clears_state(self):
        m = _run([MarkIInstruction(MarkIOp.SET, (0, "42")), MarkIInstruction(MarkIOp.HALT)])
        m.reset()
        assert m.get_counter(0) == Decimal("0")
        assert m.state.cycle_count == 0
        assert not m.state.halted

    def test_reset_clears_output_tape(self):
        m = _run(
            [
                MarkIInstruction(MarkIOp.SET, (0, "5")),
                MarkIInstruction(MarkIOp.PRINT, (0,)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        assert len(m.state.output_tape) == 1
        m.reset()
        assert m.state.output_tape == []

    def test_load_program_resets_pc(self):
        m = HarvardMarkI()
        m.load_program([MarkIInstruction(MarkIOp.SET, (0, "1")), MarkIInstruction(MarkIOp.HALT)])
        m.run()
        assert m.state.program_counter == 2
        m.load_program([MarkIInstruction(MarkIOp.HALT)])
        assert m.state.program_counter == 0

    def test_state_snapshot_keys(self):
        m = _run([MarkIInstruction(MarkIOp.SET, (0, "7")), MarkIInstruction(MarkIOp.HALT)])
        s = m.state_snapshot()
        for key in (
            "program_counter",
            "cycle_count",
            "halted",
            "last_result",
            "output_tape",
            "num_counters",
            "num_constants",
        ):
            assert key in s

    def test_state_snapshot_values(self):
        m = _run([MarkIInstruction(MarkIOp.HALT)])
        s = m.state_snapshot()
        assert s["halted"] is True
        assert s["num_counters"] == _NUM_COUNTERS
        assert s["num_constants"] == _NUM_CONSTANTS

    def test_state_snapshot_output_tape(self):
        m = _run(
            [
                MarkIInstruction(MarkIOp.SET, (0, "42")),
                MarkIInstruction(MarkIOp.PRINT, (0,)),
                MarkIInstruction(MarkIOp.HALT),
            ]
        )
        s = m.state_snapshot()
        assert s["output_tape"] == [42.0]
