"""Unit tests for the EDSAC emulator (Cambridge, 1949)."""

from __future__ import annotations

import pytest

from backend.src.emulator.edsac import (
    _FC,
    _STORE_SIZE,
    _WORD_BITS,
    EDSAC,
    EDSACInstruction,
    _to_signed,
    _to_unsigned,
)

# ---------------------------------------------------------------------------
# Two's complement helpers (17-bit)
# ---------------------------------------------------------------------------


class TestTwosComplement:
    def test_positive(self):
        assert _to_signed(7) == 7

    def test_zero(self):
        assert _to_signed(0) == 0

    def test_negative_minus_one(self):
        assert _to_signed(0x1FFFF) == -1

    def test_negative_minus_two(self):
        assert _to_signed(0x1FFFE) == -2

    def test_max_positive(self):
        # 0x0FFFF = 65535 -- max positive for 17 bits
        assert _to_signed(0x0FFFF) == 65535

    def test_min_negative(self):
        # 0x10000 = -65536 in 17-bit two's complement
        assert _to_signed(0x10000) == -65536

    def test_to_unsigned_zero(self):
        assert _to_unsigned(0) == 0

    def test_to_unsigned_positive(self):
        assert _to_unsigned(5) == 5

    def test_to_unsigned_negative(self):
        assert _to_unsigned(-1) == 0x1FFFF

    def test_to_unsigned_wraps_at_17_bits(self):
        assert _to_unsigned(1 << _WORD_BITS) == 0

    def test_round_trip(self):
        for v in (-100, -1, 0, 1, 100):
            assert _to_signed(_to_unsigned(v)) == v


# ---------------------------------------------------------------------------
# EDSACInstruction encoding
# ---------------------------------------------------------------------------


class TestInstructionEncoding:
    def test_encode_add(self):
        instr = EDSACInstruction("A", 10)
        word = instr.encode()
        assert (word >> 6) & 0x1FF == 10
        assert (word & 0x1F) == _FC["A"]

    def test_encode_sub(self):
        instr = EDSACInstruction("S", 5)
        word = instr.encode()
        assert (word & 0x1F) == _FC["S"]
        assert (word >> 6) & 0x1FF == 5

    def test_encode_halt(self):
        instr = EDSACInstruction("Z", 0)
        word = instr.encode()
        assert (word & 0x1F) == _FC["Z"]

    def test_encode_long_mode(self):
        instr = EDSACInstruction("A", 0, long=True)
        word = instr.encode()
        assert (word >> 5) & 1 == 1

    def test_encode_short_mode(self):
        instr = EDSACInstruction("A", 0, long=False)
        word = instr.encode()
        assert (word >> 5) & 1 == 0

    def test_encode_h_set_multiplier(self):
        instr = EDSACInstruction("H", 100)
        word = instr.encode()
        assert (word & 0x1F) == _FC["H"]
        assert (word >> 6) & 0x1FF == 100

    def test_encode_v_multiply_accumulate(self):
        instr = EDSACInstruction("V", 50)
        word = instr.encode()
        assert (word & 0x1F) == _FC["V"]

    def test_encode_e_branch(self):
        instr = EDSACInstruction("E", 200)
        word = instr.encode()
        assert (word & 0x1F) == _FC["E"]
        assert (word >> 6) & 0x1FF == 200

    def test_function_code_map(self):
        """Spot-check several function code values."""
        assert _FC["Z"] == 15
        assert _FC["T"] == 3
        assert _FC["U"] == 4
        assert _FC["C"] == 1
        assert _FC["R"] == 7
        assert _FC["L"] == 8

    def test_unknown_function_raises(self):
        with pytest.raises(ValueError):
            EDSACInstruction("Q", 0)

    def test_lowercase_accepted(self):
        """Constructor normalizes to uppercase."""
        instr = EDSACInstruction("a", 10)
        assert instr.function == "A"

    def test_address_zero_valid(self):
        instr = EDSACInstruction("A", 0)
        assert instr.address == 0

    def test_address_max_valid(self):
        instr = EDSACInstruction("A", _STORE_SIZE - 1)
        assert instr.address == _STORE_SIZE - 1

    def test_address_out_of_range_high(self):
        with pytest.raises(ValueError):
            EDSACInstruction("A", _STORE_SIZE)

    def test_address_out_of_range_negative(self):
        with pytest.raises(ValueError):
            EDSACInstruction("A", -1)


# ---------------------------------------------------------------------------
# Memory
# ---------------------------------------------------------------------------


class TestMemory:
    def test_store_and_read(self):
        e = EDSAC()
        e.store_value(10, 42)
        assert e.get_value(10) == 42

    def test_store_negative(self):
        e = EDSAC()
        e.store_value(3, -5)
        assert e.get_value(3) == -5

    def test_store_zero(self):
        e = EDSAC()
        e.store_value(0, 0)
        assert e.get_value(0) == 0

    def test_fresh_store_all_zero(self):
        e = EDSAC()
        for addr in range(10):
            assert e.get_value(addr) == 0

    def test_store_max_valid_address(self):
        e = EDSAC()
        e.store_value(_STORE_SIZE - 1, 7)
        assert e.get_value(_STORE_SIZE - 1) == 7

    def test_out_of_range_high(self):
        e = EDSAC()
        with pytest.raises(IndexError):
            e.store_value(_STORE_SIZE, 0)

    def test_out_of_range_negative(self):
        e = EDSAC()
        with pytest.raises(IndexError):
            e.store_value(-1, 0)

    def test_store_mask_to_17_bits(self):
        e = EDSAC()
        e.store_value(5, 0x1FFFF + 1)  # 0x20000 -> masked to 0
        assert e.get_value(5) == 0


# ---------------------------------------------------------------------------
# Arithmetic instructions
# ---------------------------------------------------------------------------


class TestArithmetic:
    def _make(
        self,
        instructions: list[EDSACInstruction],
        data: dict[int, int] | None = None,
        acc: int = 0,
    ) -> EDSAC:
        e = EDSAC()
        if data:
            for addr, val in data.items():
                e.store_value(addr, val)
        e.store_instructions(0, instructions)
        e.state.accumulator = acc
        e.run()
        return e

    def test_add(self):
        e = self._make([EDSACInstruction("A", 50), EDSACInstruction("Z", 0)], data={50: 7}, acc=3)
        assert e.state.accumulator == 10

    def test_add_negative_value(self):
        e = self._make([EDSACInstruction("A", 50), EDSACInstruction("Z", 0)], data={50: -5}, acc=3)
        assert e.state.accumulator == -2

    def test_sub(self):
        e = self._make([EDSACInstruction("S", 50), EDSACInstruction("Z", 0)], data={50: 3}, acc=10)
        assert e.state.accumulator == 7

    def test_sub_goes_negative(self):
        e = self._make([EDSACInstruction("S", 50), EDSACInstruction("Z", 0)], data={50: 10}, acc=3)
        assert e.state.accumulator == -7

    def test_transfer_clears_acc(self):
        e = self._make([EDSACInstruction("T", 50), EDSACInstruction("Z", 0)], acc=99)
        assert e.get_value(50) == 99
        assert e.state.accumulator == 0

    def test_u_store_no_clear(self):
        e = self._make([EDSACInstruction("U", 50), EDSACInstruction("Z", 0)], acc=55)
        assert e.get_value(50) == 55
        assert e.state.accumulator == 55

    def test_h_and_v_multiply(self):
        e = EDSAC()
        e.store_value(50, 6)
        e.store_value(51, 7)
        e.store_instructions(
            0,
            [
                EDSACInstruction("H", 51),  # R = 7
                EDSACInstruction("V", 50),  # A += 6 * 7 = 42
                EDSACInstruction("Z", 0),
            ],
        )
        e.run()
        assert e.state.accumulator == 42

    def test_v_multiply_from_zero_acc(self):
        """V: A += store[N] * R starting from acc=0."""
        e = EDSAC()
        e.store_value(50, 5)
        e.store_value(51, 4)
        e.store_instructions(
            0,
            [
                EDSACInstruction("H", 51),  # R = 4
                EDSACInstruction("V", 50),  # A = 0 + 5*4 = 20
                EDSACInstruction("Z", 0),
            ],
        )
        e.run()
        assert e.state.accumulator == 20

    def test_h_sets_multiplier_register(self):
        e = EDSAC()
        e.store_value(50, 13)
        e.store_instructions(
            0,
            [EDSACInstruction("H", 50), EDSACInstruction("Z", 0)],
        )
        e.run()
        assert e.state.multiplier_register == 13

    def test_right_shift(self):
        e = self._make([EDSACInstruction("R", 0), EDSACInstruction("Z", 0)], acc=8)
        assert e.state.accumulator == 4

    def test_right_shift_odd(self):
        e = self._make([EDSACInstruction("R", 0), EDSACInstruction("Z", 0)], acc=7)
        assert e.state.accumulator == 3

    def test_left_shift(self):
        e = self._make([EDSACInstruction("L", 0), EDSACInstruction("Z", 0)], acc=3)
        assert e.state.accumulator == 6

    def test_left_shift_overflow_masked(self):
        """Left shift of max 17-bit positive value overflows to negative."""
        e = self._make([EDSACInstruction("L", 0), EDSACInstruction("Z", 0)], acc=65535)
        # 65535 << 1 = 131070 -> masked to 17 bits: 131070 & 0x1FFFF = 131070 -> signed = -2
        expected = _to_signed(_to_unsigned(65535 * 2))
        assert e.state.accumulator == expected

    def test_collate_and(self):
        e = EDSAC()
        e.store_value(50, 0b1100)
        e.store_instructions(0, [EDSACInstruction("C", 50), EDSACInstruction("Z", 0)])
        e.state.accumulator = 0b1010
        e.run()
        assert e.state.accumulator == 0b1000

    def test_collate_with_zero(self):
        e = EDSAC()
        e.store_value(50, 0)
        e.store_instructions(0, [EDSACInstruction("C", 50), EDSACInstruction("Z", 0)])
        e.state.accumulator = 0xFFFF
        e.run()
        assert e.state.accumulator == 0

    def test_load_f(self):
        e = EDSAC()
        e.store_value(50, 77)
        e.store_instructions(0, [EDSACInstruction("F", 50), EDSACInstruction("Z", 0)])
        e.run()
        assert e.state.accumulator == 77

    def test_load_f_negative(self):
        e = EDSAC()
        e.store_value(50, -10)
        e.store_instructions(0, [EDSACInstruction("F", 50), EDSACInstruction("Z", 0)])
        e.run()
        assert e.state.accumulator == -10

    def test_add_and_store_chain(self):
        """Classic EDSAC: sum 100 + 200, store result."""
        e = EDSAC()
        e.store_value(50, 100)
        e.store_value(51, 200)
        e.store_instructions(
            0,
            [
                EDSACInstruction("T", 52),  # store 0 -> clear acc
                EDSACInstruction("A", 50),  # acc += 100
                EDSACInstruction("A", 51),  # acc += 200
                EDSACInstruction("T", 52),  # store 300 -> acc = 0
                EDSACInstruction("Z", 0),
            ],
        )
        e.run()
        assert e.get_value(52) == 300


# ---------------------------------------------------------------------------
# Branching
# ---------------------------------------------------------------------------


class TestBranching:
    def test_e_branch_if_non_negative(self):
        """E addr: branch to addr if A >= 0."""
        e = EDSAC()
        e.store_instructions(
            0,
            [
                EDSACInstruction("E", 10),  # branch to 10
                EDSACInstruction("T", 50),  # skipped
            ],
        )
        e.state.store[11] = EDSACInstruction("Z", 0).encode()
        e.state.accumulator = 5  # positive -> branch taken
        e.run()
        assert e.get_value(50) == 0  # T was skipped

    def test_e_no_branch_if_negative(self):
        """E: no branch if A < 0."""
        e = EDSAC()
        e.store_instructions(
            0,
            [
                EDSACInstruction("E", 20),  # no branch (A < 0)
                EDSACInstruction("T", 50),  # executed
                EDSACInstruction("Z", 0),
            ],
        )
        e.state.accumulator = -1  # negative -> no branch
        e.run()
        assert e.get_value(50) == -1  # T stores accumulator (-1 round-trips correctly)

    def test_g_branch_if_negative(self):
        """G addr: branch to addr if A < 0."""
        e = EDSAC()
        e.store_instructions(
            0,
            [
                EDSACInstruction("G", 10),
                EDSACInstruction("T", 50),  # skipped
            ],
        )
        e.state.store[11] = EDSACInstruction("Z", 0).encode()
        e.state.accumulator = -1  # negative -> branch taken
        e.run()
        assert e.get_value(50) == 0  # T was skipped

    def test_g_no_branch_if_non_negative(self):
        """G: no branch if A >= 0."""
        e = EDSAC()
        e.store_instructions(
            0,
            [
                EDSACInstruction("G", 20),  # no branch (A >= 0)
                EDSACInstruction("T", 50),  # executed
                EDSACInstruction("Z", 0),
            ],
        )
        e.state.accumulator = 1
        e.run()
        assert e.get_value(50) == 1

    def test_e_branch_if_zero(self):
        """E: zero is >= 0, so branch is taken."""
        e = EDSAC()
        e.store_instructions(
            0,
            [
                EDSACInstruction("E", 10),  # A=0 >= 0, branch
                EDSACInstruction("T", 50),  # skipped
            ],
        )
        e.state.store[11] = EDSACInstruction("Z", 0).encode()
        e.state.accumulator = 0  # zero -> branch taken
        e.run()
        assert e.get_value(50) == 0  # T skipped


# ---------------------------------------------------------------------------
# Input / Output
# ---------------------------------------------------------------------------


class TestIO:
    def test_input_tape_single(self):
        e = EDSAC()
        e.load_input_tape([42])
        e.store_instructions(
            0,
            [EDSACInstruction("I", 50), EDSACInstruction("Z", 0)],
        )
        e.run()
        assert e.get_value(50) == 42

    def test_input_tape_multiple(self):
        e = EDSAC()
        e.load_input_tape([10, 20, 30])
        e.store_instructions(
            0,
            [
                EDSACInstruction("I", 50),
                EDSACInstruction("I", 51),
                EDSACInstruction("I", 52),
                EDSACInstruction("Z", 0),
            ],
        )
        e.run()
        assert e.get_value(50) == 10
        assert e.get_value(51) == 20
        assert e.get_value(52) == 30

    def test_input_exhausted_raises(self):
        e = EDSAC()
        e.load_input_tape([1])
        e.store_instructions(
            0,
            [
                EDSACInstruction("I", 50),
                EDSACInstruction("I", 51),  # tape exhausted
                EDSACInstruction("Z", 0),
            ],
        )
        with pytest.raises(RuntimeError, match="exhausted"):
            e.run()

    def test_output_produces_entry(self):
        e = EDSAC()
        e.store_value(50, 1)  # char value 1
        e.store_instructions(
            0,
            [EDSACInstruction("O", 50), EDSACInstruction("Z", 0)],
        )
        e.run()
        assert len(e.state.output_tape) == 1

    def test_output_char_value_zero_as_string(self):
        """Output value 0 -> not a letter, so str(0) = '0'."""
        e = EDSAC()
        e.store_value(50, 0)
        e.store_instructions(
            0,
            [EDSACInstruction("O", 50), EDSACInstruction("Z", 0)],
        )
        e.run()
        assert e.state.output_tape[0] == "0"


# ---------------------------------------------------------------------------
# Control
# ---------------------------------------------------------------------------


class TestControl:
    def test_halt(self):
        e = EDSAC()
        e.store_instructions(0, [EDSACInstruction("Z", 0)])
        e.run()
        assert e.state.halted

    def test_step_returns_false_when_halted(self):
        e = EDSAC()
        e.state.halted = True
        assert e.step() is False

    def test_step_returns_false_on_z(self):
        e = EDSAC()
        e.store_instructions(0, [EDSACInstruction("Z", 0)])
        result = e.step()
        assert result is False

    def test_step_returns_true_normally(self):
        e = EDSAC()
        e.store_instructions(
            0,
            [EDSACInstruction("A", 50), EDSACInstruction("Z", 0)],
        )
        result = e.step()
        assert result is True

    def test_cycle_count_increments(self):
        e = EDSAC()
        e.store_value(50, 0)
        e.store_instructions(
            0,
            [
                EDSACInstruction("A", 50),
                EDSACInstruction("A", 50),
                EDSACInstruction("Z", 0),
            ],
        )
        e.run()
        assert e.state.cycle_count == 3

    def test_max_cycles_respected(self):
        e = EDSAC()
        # No halt instruction -> max_cycles applies
        e.store_instructions(0, [EDSACInstruction("A", 50)])  # loops over nothing
        count = e.run(max_cycles=5)
        assert count <= 5

    def test_reset_clears_state(self):
        e = EDSAC()
        e.state.accumulator = 42
        e.state.multiplier_register = 7
        e.reset()
        assert e.state.accumulator == 0
        assert e.state.multiplier_register == 0
        assert not e.state.halted

    def test_reset_clears_output_tape(self):
        e = EDSAC()
        e.state.output_tape.append("A")
        e.reset()
        assert e.state.output_tape == []

    def test_state_snapshot_keys(self):
        e = EDSAC()
        e.store_instructions(0, [EDSACInstruction("Z", 0)])
        e.run()
        s = e.state_snapshot()
        for key in (
            "accumulator",
            "multiplier_register",
            "ci",
            "halted",
            "cycle_count",
            "output_tape",
        ):
            assert key in s

    def test_state_snapshot_halted(self):
        e = EDSAC()
        e.store_instructions(0, [EDSACInstruction("Z", 0)])
        e.run()
        assert e.state_snapshot()["halted"] is True

    def test_store_instructions_resets_accumulator(self):
        e = EDSAC()
        e.state.accumulator = 999
        e.store_instructions(0, [EDSACInstruction("Z", 0)])
        assert e.state.accumulator == 0
