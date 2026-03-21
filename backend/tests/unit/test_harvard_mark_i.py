"""Unit tests for the Harvard Mark I (IBM ASCC) emulator (1944)."""

from decimal import Decimal

import pytest

from backend.src.emulator.harvard_mark_i import (
    HarvardMarkI,
    MarkIInstruction,
    MarkIOp,
    _NUM_CONSTANTS,
    _NUM_COUNTERS,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _run(instructions, counters=None, constants=None):
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
# Storage
# ---------------------------------------------------------------------------


class TestStorage:
    def test_set_and_get_counter(self):
        m = HarvardMarkI()
        m.set_counter(0, Decimal("42"))
        assert m.get_counter(0) == Decimal("42")

    def test_counter_default_zero(self):
        m = HarvardMarkI()
        assert m.get_counter(5) == Decimal("0")

    def test_constant_default_zero(self):
        m = HarvardMarkI()
        assert m.get_constant(0) == Decimal("0")

    def test_set_constant(self):
        m = HarvardMarkI()
        m.set_constant(0, Decimal("3.14159"))
        assert m.get_constant(0) == Decimal("3.14159")

    def test_counter_out_of_range(self):
        m = HarvardMarkI()
        with pytest.raises(IndexError):
            m.set_counter(_NUM_COUNTERS, Decimal("1"))

    def test_constant_out_of_range(self):
        m = HarvardMarkI()
        with pytest.raises(IndexError):
            m.set_constant(_NUM_CONSTANTS, Decimal("1"))

    def test_write_to_constant_raises(self):
        """Constant registers are read-only."""
        m = HarvardMarkI()
        prog = [
            MarkIInstruction(MarkIOp.SET, (_NUM_COUNTERS, "1.0")),  # constant area
            MarkIInstruction(MarkIOp.HALT),
        ]
        m.load_program(prog)
        with pytest.raises(PermissionError):
            m.run()


# ---------------------------------------------------------------------------
# Arithmetic operations
# ---------------------------------------------------------------------------


class TestArithmetic:
    def test_add(self):
        m = _run(
            [MarkIInstruction(MarkIOp.ADD, (0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 3, 1: 4},
        )
        assert m.get_counter(0) == Decimal("7")

    def test_subtract(self):
        m = _run(
            [MarkIInstruction(MarkIOp.SUB, (0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 10, 1: 3},
        )
        assert m.get_counter(0) == Decimal("7")

    def test_multiply(self):
        m = _run(
            [MarkIInstruction(MarkIOp.MULT, (2, 0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 6, 1: 7},
        )
        assert m.get_counter(2) == Decimal("42")

    def test_divide(self):
        m = _run(
            [MarkIInstruction(MarkIOp.DIV, (2, 0, 1)), MarkIInstruction(MarkIOp.HALT)],
            counters={0: 10, 1: 4},
        )
        assert m.get_counter(2) == Decimal("2.5")

    def test_divide_by_zero(self):
        m = HarvardMarkI()
        m.set_counter(0, Decimal("5"))
        m.set_counter(1, Decimal("0"))
        m.load_program([
            MarkIInstruction(MarkIOp.DIV, (2, 0, 1)),
            MarkIInstruction(MarkIOp.HALT),
        ])
        with pytest.raises(ZeroDivisionError):
            m.run()

    def test_set_instruction(self):
        m = _run(
            [MarkIInstruction(MarkIOp.SET, (0, "99")), MarkIInstruction(MarkIOp.HALT)]
        )
        assert m.get_counter(0) == Decimal("99")

    def test_load_from_constant(self):
        """Constants occupy storage addresses 72..131; LOAD can read them."""
        m = HarvardMarkI()
        m.set_constant(0, Decimal("2.71828"))  # storage[72] = e
        m.load_program([
            MarkIInstruction(MarkIOp.LOAD, (0, 72)),  # counter[0] = storage[72]
            MarkIInstruction(MarkIOp.HALT),
        ])
        m.run()
        assert m.get_counter(0) == Decimal("2.71828")

    def test_bessel_function_step(self):
        """Compute one step of J0 recurrence: J0(n) ≈ 2*(n-1)/x * J0(n-1) - J0(n-2).
        Historical: the Mark I computed tables of Bessel functions in 1944."""
        m = HarvardMarkI()
        # J0(0) = 1, J0(1) ≈ 0 for large x; approximate ratio
        m.set_counter(0, Decimal("1"))    # J0(n-2)
        m.set_counter(1, Decimal("0"))    # J0(n-1)
        m.set_counter(2, Decimal("2"))    # coefficient 2
        m.set_counter(3, Decimal("5"))    # x = 5
        m.load_program([
            # Compute temp = 2 * J0(n-1)
            MarkIInstruction(MarkIOp.MULT, (4, 2, 1)),  # counter[4] = 2 * J0(n-1)
            # Compute result = temp - J0(n-2)
            MarkIInstruction(MarkIOp.SUB, (4, 0)),       # counter[4] -= J0(n-2)
            MarkIInstruction(MarkIOp.PRINT, (4,)),
            MarkIInstruction(MarkIOp.HALT),
        ])
        m.run()
        # 2 * 0 - 1 = -1
        assert m.state.output_tape[0] == Decimal("-1")


# ---------------------------------------------------------------------------
# LOAD / STORE / SET
# ---------------------------------------------------------------------------


class TestLoadStore:
    def test_store_and_load(self):
        m = HarvardMarkI()
        m.set_counter(0, Decimal("77"))
        m.load_program([
            MarkIInstruction(MarkIOp.STORE, (1, 0)),  # counter[1] = counter[0]
            MarkIInstruction(MarkIOp.HALT),
        ])
        m.run()
        assert m.get_counter(1) == Decimal("77")


# ---------------------------------------------------------------------------
# PRINT / control
# ---------------------------------------------------------------------------


class TestControl:
    def test_print_outputs_value(self):
        m = _run(
            [MarkIInstruction(MarkIOp.SET, (0, "3.14")),
             MarkIInstruction(MarkIOp.PRINT, (0,)),
             MarkIInstruction(MarkIOp.HALT)],
        )
        assert len(m.state.output_tape) == 1
        assert float(m.state.output_tape[0]) == pytest.approx(3.14)

    def test_halt_stops_program(self):
        m = HarvardMarkI()
        m.load_program([
            MarkIInstruction(MarkIOp.HALT),
            MarkIInstruction(MarkIOp.SET, (0, "99")),  # never reached
        ])
        m.run()
        assert m.get_counter(0) == Decimal("0")

    def test_cycle_count(self):
        m = _run(
            [MarkIInstruction(MarkIOp.SET, (0, "1")),
             MarkIInstruction(MarkIOp.SET, (1, "2")),
             MarkIInstruction(MarkIOp.HALT)],
        )
        assert m.state.cycle_count == 3

    def test_branch_taken(self):
        """BRANCH jumps when last_result != 0."""
        m = HarvardMarkI()
        m.load_program([
            MarkIInstruction(MarkIOp.SET, (0, "5"), label="top"),
            MarkIInstruction(MarkIOp.BRANCH, ("end",)),   # last_result=5 != 0 -> jump
            MarkIInstruction(MarkIOp.SET, (0, "99")),      # not reached
            MarkIInstruction(MarkIOp.HALT, label="end"),
        ])
        m.run()
        assert m.get_counter(0) == Decimal("5")

    def test_branch_not_taken_when_zero(self):
        m = HarvardMarkI()
        m.load_program([
            MarkIInstruction(MarkIOp.SET, (0, "0"), label="top"),
            MarkIInstruction(MarkIOp.BRANCH, ("top",)),   # last_result=0 -> no jump
            MarkIInstruction(MarkIOp.SET, (0, "99")),      # reached
            MarkIInstruction(MarkIOp.HALT),
        ])
        m.run()
        assert m.get_counter(0) == Decimal("99")

    def test_reset_clears_state(self):
        m = _run(
            [MarkIInstruction(MarkIOp.SET, (0, "42")), MarkIInstruction(MarkIOp.HALT)]
        )
        m.reset()
        assert m.get_counter(0) == Decimal("0")
        assert m.state.cycle_count == 0

    def test_state_snapshot(self):
        m = _run(
            [MarkIInstruction(MarkIOp.SET, (0, "7")), MarkIInstruction(MarkIOp.HALT)]
        )
        s = m.state_snapshot()
        assert s["halted"] is True
        assert s["num_counters"] == _NUM_COUNTERS
        assert s["num_constants"] == _NUM_CONSTANTS
