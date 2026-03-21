"""Unit tests for the EDSAC emulator (Cambridge, 1949)."""

import pytest

from backend.src.emulator.edsac import (
    EDSAC,
    EDSACInstruction,
    _FC,
    _STORE_SIZE,
    _to_signed,
    _to_unsigned,
)


class TestTwosComplement:
    def test_positive(self):
        assert _to_signed(7) == 7

    def test_negative(self):
        # 17-bit two's complement: 0x1FFFF = 131071 = -1
        assert _to_signed(0x1FFFF) == -1

    def test_to_unsigned_negative(self):
        assert _to_unsigned(-1) == 0x1FFFF


class TestInstructionEncoding:
    def test_encode_add(self):
        instr = EDSACInstruction("A", 10)
        word = instr.encode()
        # FC=0 (A), long=0, address=10 -> 0 | 0 | (10 << 6) = 640
        assert (word >> 6) & 0x1FF == 10
        assert (word & 0x1F) == _FC["A"]

    def test_encode_sub(self):
        instr = EDSACInstruction("S", 5)
        word = instr.encode()
        assert (word & 0x1F) == _FC["S"]
        assert (word >> 6) & 0x1FF == 5

    def test_encode_long_mode(self):
        instr = EDSACInstruction("A", 0, long=True)
        word = instr.encode()
        assert (word >> 5) & 1 == 1

    def test_unknown_function_raises(self):
        with pytest.raises(ValueError):
            EDSACInstruction("Q", 0)

    def test_address_out_of_range(self):
        with pytest.raises(ValueError):
            EDSACInstruction("A", _STORE_SIZE)


class TestMemory:
    def test_store_and_read(self):
        e = EDSAC()
        e.store_value(10, 42)
        assert e.get_value(10) == 42

    def test_store_negative(self):
        e = EDSAC()
        e.store_value(3, -5)
        assert e.get_value(3) == -5

    def test_out_of_range(self):
        e = EDSAC()
        with pytest.raises(IndexError):
            e.store_value(_STORE_SIZE, 0)


class TestArithmetic:
    def _make(self, instructions, data=None):
        e = EDSAC()
        if data:
            for addr, val in data.items():
                e.store_value(addr, val)
        e.store_instructions(0, instructions)
        e.run()
        return e

    def test_add(self):
        """A += store[N]."""
        e2 = EDSAC()
        e2.store_value(50, 7)
        e2.store_instructions(0, [EDSACInstruction("A", 50), EDSACInstruction("Z", 0)])
        e2.state.accumulator = 3  # set AFTER store_instructions
        e2.run()
        assert e2.state.accumulator == 10

    def test_sub(self):
        """A -= store[N]."""
        e = EDSAC()
        e.store_value(50, 3)
        e.store_instructions(0, [EDSACInstruction("S", 50), EDSACInstruction("Z", 0)])
        e.state.accumulator = 10  # set AFTER store_instructions
        e.run()
        assert e.state.accumulator == 7

    def test_transfer_clears_acc(self):
        """T: store[N] = A, A = 0."""
        e = EDSAC()
        e.store_instructions(0, [EDSACInstruction("T", 50), EDSACInstruction("Z", 0)])
        e.state.accumulator = 99  # set AFTER store_instructions
        e.run()
        assert e.get_value(50) == 99
        assert e.state.accumulator == 0

    def test_u_store_no_clear(self):
        """U: store[N] = A, A unchanged."""
        e = EDSAC()
        e.store_instructions(0, [EDSACInstruction("U", 50), EDSACInstruction("Z", 0)])
        e.state.accumulator = 55  # set AFTER store_instructions
        e.run()
        assert e.get_value(50) == 55
        assert e.state.accumulator == 55

    def test_h_and_v_multiply(self):
        """H sets R; V: A += store[N] * R."""
        e = EDSAC()
        e.store_value(50, 6)   # multiplicand
        e.store_value(51, 7)   # multiplier
        e.store_instructions(0, [
            EDSACInstruction("H", 51),   # R = store[51] = 7
            EDSACInstruction("V", 50),   # A += store[50] * R = 6 * 7 = 42
            EDSACInstruction("Z", 0),
        ])
        e.run()
        assert e.state.accumulator == 42

    def test_right_shift(self):
        """R: A >>= 1."""
        e = EDSAC()
        e.store_instructions(0, [EDSACInstruction("R", 0), EDSACInstruction("Z", 0)])
        e.state.accumulator = 8  # set AFTER store_instructions
        e.run()
        assert e.state.accumulator == 4

    def test_left_shift(self):
        """L: A <<= 1."""
        e = EDSAC()
        e.store_instructions(0, [EDSACInstruction("L", 0), EDSACInstruction("Z", 0)])
        e.state.accumulator = 3  # set AFTER store_instructions
        e.run()
        assert e.state.accumulator == 6

    def test_collate_and(self):
        """C: A &= store[N]."""
        e = EDSAC()
        e.store_value(50, 0b1100)
        e.store_instructions(0, [EDSACInstruction("C", 50), EDSACInstruction("Z", 0)])
        e.state.accumulator = 0b1010  # set AFTER store_instructions
        e.run()
        assert e.state.accumulator == 0b1000

    def test_load_f(self):
        """F: A = store[N]."""
        e = EDSAC()
        e.store_value(50, 77)
        e.store_instructions(0, [EDSACInstruction("F", 50), EDSACInstruction("Z", 0)])
        e.run()
        assert e.state.accumulator == 77


class TestBranching:
    def test_e_branch_if_non_negative(self):
        """E addr: branch to addr if A >= 0."""
        e = EDSAC()
        # Program at 0: E 10 (branch to 10), then at 10: Z (halt)
        # (CI starts at -1, step() increments to 0 before first fetch)
        e.store_instructions(0, [
            EDSACInstruction("E", 10),  # branch to addr 10
            EDSACInstruction("T", 50),  # skipped
        ])
        e.store_value(11, 0)  # ensure Z at addr 11
        e.state.store[11] = EDSACInstruction("Z", 0).encode()
        e.state.accumulator = 5   # positive -> branch taken (set AFTER store_instructions)
        e.run()
        assert e.get_value(50) == 0  # T was skipped

    def test_g_branch_if_negative(self):
        """G addr: branch to addr if A < 0."""
        e = EDSAC()
        e.store_instructions(0, [
            EDSACInstruction("G", 10),  # branch to addr 10
            EDSACInstruction("T", 50),  # skipped
        ])
        e.state.store[11] = EDSACInstruction("Z", 0).encode()
        e.state.accumulator = -1   # negative -> branch taken (set AFTER store_instructions)
        e.run()
        assert e.get_value(50) == 0  # T was skipped

    def test_no_branch_when_condition_false(self):
        """G: no branch if A >= 0."""
        e = EDSAC()
        e.store_instructions(0, [
            EDSACInstruction("G", 20),   # no branch (A >= 0)
            EDSACInstruction("T", 50),   # executed
            EDSACInstruction("Z", 0),
        ])
        e.state.accumulator = 1   # positive -> G does NOT branch (set AFTER store_instructions)
        e.run()
        assert e.get_value(50) == 1  # T was executed


class TestIO:
    def test_input_tape(self):
        e = EDSAC()
        e.load_input_tape([42])
        e.store_instructions(0, [
            EDSACInstruction("I", 50),   # store input at addr 50
            EDSACInstruction("Z", 0),
        ])
        e.run()
        assert e.get_value(50) == 42

    def test_input_exhausted_raises(self):
        e = EDSAC()
        e.load_input_tape([1])
        e.store_instructions(0, [
            EDSACInstruction("I", 50),
            EDSACInstruction("I", 51),   # tape exhausted
            EDSACInstruction("Z", 0),
        ])
        with pytest.raises(RuntimeError, match="exhausted"):
            e.run()

    def test_output(self):
        e = EDSAC()
        e.store_value(50, 1)   # ITA2 value 1 = 'A' + 0 = letter
        e.store_instructions(0, [
            EDSACInstruction("O", 50),
            EDSACInstruction("Z", 0),
        ])
        e.run()
        assert len(e.state.output_tape) == 1


class TestControl:
    def test_halt(self):
        e = EDSAC()
        e.store_instructions(0, [EDSACInstruction("Z", 0)])
        e.run()
        assert e.state.halted

    def test_add_and_output(self):
        """Classic EDSAC first computation: sum of integers."""
        e = EDSAC()
        e.store_value(50, 100)
        e.store_value(51, 200)
        e.store_instructions(0, [
            EDSACInstruction("T", 52),   # store[52] = 0 (clear acc)
            EDSACInstruction("A", 50),   # acc += 100
            EDSACInstruction("A", 51),   # acc += 200
            EDSACInstruction("T", 52),   # store[52] = 300, acc = 0
            EDSACInstruction("Z", 0),
        ])
        e.run()
        assert e.get_value(52) == 300

    def test_reset(self):
        e = EDSAC()
        e.state.accumulator = 42
        e.reset()
        assert e.state.accumulator == 0
        assert not e.state.halted

    def test_state_snapshot(self):
        e = EDSAC()
        e.store_instructions(0, [EDSACInstruction("Z", 0)])
        e.run()
        s = e.state_snapshot()
        assert "accumulator" in s
        assert "multiplier_register" in s
        assert s["halted"] is True
