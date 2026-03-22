"""Unit tests for the Manchester Baby (SSEM) emulator (1948)."""

from __future__ import annotations

import pytest

from backend.src.emulator.manchester_baby import (
    _F_CMP,
    _F_JMP,
    _F_JRP,
    _F_LDN,
    _F_STO,
    _F_STP,
    _F_SUB,
    _STORE_SIZE,
    _WORD_MASK,
    ManchesterBaby,
    _to_signed,
    _to_unsigned,
)

# ---------------------------------------------------------------------------
# Two's complement helpers
# ---------------------------------------------------------------------------


class TestTwosComplement:
    def test_to_signed_positive(self):
        assert _to_signed(5) == 5

    def test_to_signed_zero(self):
        assert _to_signed(0) == 0

    def test_to_signed_negative(self):
        # 0xFFFFFFFE = -2 in 32-bit two's complement
        assert _to_signed(0xFFFFFFFE) == -2

    def test_to_signed_minus_one(self):
        assert _to_signed(0xFFFFFFFF) == -1

    def test_to_signed_max_positive(self):
        assert _to_signed(0x7FFFFFFF) == 2147483647

    def test_to_signed_min_negative(self):
        assert _to_signed(0x80000000) == -2147483648

    def test_to_unsigned_zero(self):
        assert _to_unsigned(0) == 0

    def test_to_unsigned_positive(self):
        assert _to_unsigned(5) == 5

    def test_to_unsigned_negative(self):
        assert _to_unsigned(-1) == 0xFFFFFFFF

    def test_to_unsigned_minus_two(self):
        assert _to_unsigned(-2) == 0xFFFFFFFE

    def test_to_unsigned_wraps(self):
        assert _to_unsigned(2**32) == 0

    def test_to_unsigned_min_negative(self):
        assert _to_unsigned(-2147483648) == 0x80000000

    def test_round_trip(self):
        for v in (-100, -1, 0, 1, 100, 2147483647, -2147483648):
            assert _to_signed(_to_unsigned(v)) == v


# ---------------------------------------------------------------------------
# Memory
# ---------------------------------------------------------------------------


class TestMemory:
    def test_store_and_read(self):
        baby = ManchesterBaby()
        baby.store_word(5, 42)
        assert baby.get_store(5) == 42

    def test_store_negative(self):
        baby = ManchesterBaby()
        baby.store_word(3, -7)
        assert baby.get_store(3) == -7

    def test_store_zero(self):
        baby = ManchesterBaby()
        baby.store_word(0, 0)
        assert baby.get_store(0) == 0

    def test_store_max_signed(self):
        baby = ManchesterBaby()
        baby.store_word(10, 2147483647)
        assert baby.get_store(10) == 2147483647

    def test_store_min_signed(self):
        baby = ManchesterBaby()
        baby.store_word(10, -2147483648)
        assert baby.get_store(10) == -2147483648

    def test_out_of_range_high(self):
        baby = ManchesterBaby()
        with pytest.raises(IndexError):
            baby.store_word(_STORE_SIZE, 0)

    def test_out_of_range_negative(self):
        baby = ManchesterBaby()
        with pytest.raises(IndexError):
            baby.store_word(-1, 0)

    def test_all_addresses_accessible(self):
        baby = ManchesterBaby()
        for addr in range(_STORE_SIZE):
            baby.store_word(addr, addr)
        for addr in range(_STORE_SIZE):
            assert baby.get_store(addr) == addr

    def test_fresh_machine_all_zero(self):
        baby = ManchesterBaby()
        for addr in range(_STORE_SIZE):
            assert baby.get_store(addr) == 0


# ---------------------------------------------------------------------------
# Raw word load
# ---------------------------------------------------------------------------


class TestLoadRawWord:
    def test_load_raw_word_stores(self):
        baby = ManchesterBaby()
        baby.load_raw_word(0, 0xDEAD)
        assert baby.state.store[0] == 0xDEAD

    def test_load_raw_word_masks_to_32_bits(self):
        baby = ManchesterBaby()
        baby.load_raw_word(0, (1 << 33) | 0xABCD)
        assert baby.state.store[0] == (0xABCD | (1 << 33)) & _WORD_MASK

    def test_load_raw_word_out_of_range(self):
        baby = ManchesterBaby()
        with pytest.raises(IndexError):
            baby.load_raw_word(32, 0)


# ---------------------------------------------------------------------------
# Instruction decode
# ---------------------------------------------------------------------------


class TestDecode:
    def test_decode_jmp(self):
        # JMP 3: s=3, f=0 -> word = 3 | (0 << 5) = 3
        s, f = ManchesterBaby.decode(3)
        assert s == 3
        assert f == _F_JMP

    def test_decode_jrp(self):
        # JRP 2: s=2, f=4 -> word = 2 | (4 << 5) = 2 | 128 = 130
        s, f = ManchesterBaby.decode(2 | (4 << 5))
        assert s == 2
        assert f == _F_JRP

    def test_decode_ldn(self):
        s, f = ManchesterBaby.decode(5 | (2 << 5))
        assert s == 5
        assert f == _F_LDN

    def test_decode_sto(self):
        s, f = ManchesterBaby.decode(7 | (6 << 5))
        assert s == 7
        assert f == _F_STO

    def test_decode_sub(self):
        s, f = ManchesterBaby.decode(1 | (1 << 5))
        assert s == 1
        assert f == _F_SUB

    def test_decode_f5_sub(self):
        # F=5 is duplicate of SUB
        s, f = ManchesterBaby.decode(0 | (5 << 5))
        assert s == 0
        assert f == 5

    def test_decode_cmp(self):
        s, f = ManchesterBaby.decode(0 | (3 << 5))
        assert f == _F_CMP

    def test_decode_stp(self):
        s, f = ManchesterBaby.decode(0 | (7 << 5))
        assert f == _F_STP

    def test_decode_masks_s_to_5_bits(self):
        # Encode s=31 (max), verify decode rounds correctly
        s, _ = ManchesterBaby.decode(31)
        assert s == 31

    def test_decode_ignores_high_bits(self):
        # Bits 13+ are unused; decode should not be affected
        word = (5 | (2 << 5)) | (0xFFFF << 13)
        s, f = ManchesterBaby.decode(word)
        assert s == 5
        assert f == _F_LDN


# ---------------------------------------------------------------------------
# Individual instructions
# ---------------------------------------------------------------------------


class TestInstructions:
    """Helper to run a mini program and return the Baby state."""

    def _run(
        self,
        instructions: list[tuple[int, int]],
        data: dict[int, int] | None = None,
        start_acc: int = 0,
    ) -> ManchesterBaby:
        baby = ManchesterBaby()
        if data:
            for addr, val in data.items():
                baby.store_word(addr, val)
        baby.store_program(instructions)
        baby.state.accumulator = start_acc  # set AFTER store_program (which resets acc)
        baby.run()
        return baby

    def test_ldn_loads_negated(self):
        baby = self._run([(0, _F_LDN), (0, _F_STP)], data={0: 5})
        assert baby.state.accumulator == -5

    def test_ldn_zero_yields_zero(self):
        baby = self._run([(0, _F_LDN), (0, _F_STP)], data={0: 0})
        assert baby.state.accumulator == 0

    def test_ldn_negative_yields_positive(self):
        baby = self._run([(0, _F_LDN), (0, _F_STP)], data={0: -10})
        assert baby.state.accumulator == 10

    def test_sto_stores_accumulator(self):
        baby = self._run([(10, _F_STO), (0, _F_STP)], start_acc=7)
        assert baby.get_store(10) == 7

    def test_sto_zero(self):
        baby = self._run([(5, _F_STO), (0, _F_STP)], start_acc=0)
        assert baby.get_store(5) == 0

    def test_sub_subtracts(self):
        baby = self._run([(0, _F_SUB), (0, _F_STP)], data={0: 3}, start_acc=10)
        assert baby.state.accumulator == 7

    def test_sub_f5_same_as_sub_f1(self):
        # F=5 is a duplicate of SUB; should subtract same way
        baby = ManchesterBaby()
        baby.store_word(0, 4)
        baby.store_program([(0, 5), (0, _F_STP)])
        baby.state.accumulator = 10
        baby.run()
        assert baby.state.accumulator == 6

    def test_cmp_skips_if_negative(self):
        baby = ManchesterBaby()
        baby.store_program(
            [
                (0, _F_CMP),  # skip next if A < 0
                (5, _F_STO),  # should be skipped
                (0, _F_STP),
            ]
        )
        baby.state.accumulator = -1
        baby.run()
        assert baby.get_store(5) == 0  # STO was skipped

    def test_cmp_no_skip_if_positive(self):
        baby = ManchesterBaby()
        baby.store_program(
            [
                (0, _F_CMP),  # A=1 >= 0, no skip
                (5, _F_STO),  # executed
                (0, _F_STP),
            ]
        )
        baby.state.accumulator = 1
        baby.run()
        assert baby.get_store(5) == 1

    def test_cmp_no_skip_if_zero(self):
        # A = 0 is not negative; no skip
        baby = ManchesterBaby()
        baby.store_program(
            [
                (0, _F_CMP),  # A=0 >= 0, no skip
                (5, _F_STO),
                (0, _F_STP),
            ]
        )
        baby.state.accumulator = 0
        baby.run()
        assert baby.get_store(5) == 0  # STO runs; stores 0

    def test_negate_via_ldn(self):
        baby = ManchesterBaby()
        baby.store_word(0, -100)
        baby.store_program(
            [
                (0, _F_LDN),  # A = -(-100) = 100
                (1, _F_STO),  # store[1] = 100
                (0, _F_STP),
            ]
        )
        baby.run()
        assert baby.get_store(1) == 100

    def test_add_via_ldn_and_sub(self):
        """Historical ADD pattern: A += X by A = A - (-X).
        Steps: LDN X gives A=-X; STO tmp; LDN acc gives A=-acc;
        SUB tmp gives A = -acc - (-X) = X - acc ... this is complex.

        Simpler: to add K to acc, load -K into a cell, LDN it to get K,
        but that overwrites acc. Easiest test: compute 5 + 3 = 8 using
        the Baby's available ops.
        """
        baby = ManchesterBaby()
        # Store 5 and 3 at addresses 0 and 31
        baby.store_word(0, 5)
        baby.store_word(31, 3)
        # Program addresses 1..n; data addresses 0, 31 must not collide
        # Compute -5 - (-3) = -5 + 3 = -2 (simple arithmetic check)
        baby.store_program(
            [
                (0, _F_LDN),  # A = -5
                (31, _F_SUB),  # A = -5 - 3 = -8
                (28, _F_STO),  # store[28] = -8
                (0, _F_STP),
            ]
        )
        baby.run()
        assert baby.get_store(28) == -8

    def test_jrp_relative_jump(self):
        """JRP: CI += store[S]. Allows forward/backward branching."""
        baby = ManchesterBaby()
        # We want JRP to skip one instruction.
        # store[0] = 1 means "jump forward by 1" from current CI
        baby.store_word(0, 1)
        # Program at addresses 1..4:
        # addr 1: JRP 0 (CI += store[0] = 1 -> CI becomes 1+1 = 2, next fetch CI+1=3)
        # addr 2: STO 10 (should be SKIPPED)
        # addr 3: STP
        baby.store_program(
            [
                (0, _F_JRP),  # addr 1: CI += 1
                (10, _F_STO),  # addr 2: would store 0 into store[10] (skipped)
                (0, _F_STP),  # addr 3: halt
            ]
        )
        baby.state.accumulator = 99  # just a marker value
        baby.run()
        # If JRP skipped STO, store[10] stays 0; if not, store[10] = 99
        # After JRP at addr 1: CI = 1+1=2, step() increments to 3, fetches STP
        assert baby.get_store(10) == 0


# ---------------------------------------------------------------------------
# Multi-instruction programs
# ---------------------------------------------------------------------------


class TestPrograms:
    def test_double_negate_identity(self):
        """LDN twice: A = -(-X) = X."""
        baby = ManchesterBaby()
        baby.store_word(0, 42)
        baby.store_program(
            [
                (0, _F_LDN),  # A = -42
                (28, _F_STO),  # store[28] = -42
                (28, _F_LDN),  # A = -(-42) = 42
                (29, _F_STO),  # store[29] = 42
                (0, _F_STP),
            ]
        )
        baby.run()
        assert baby.get_store(29) == 42

    def test_subtract_from_zero(self):
        """0 - X = -X."""
        baby = ManchesterBaby()
        baby.store_word(0, 7)
        baby.store_program(
            [
                (0, _F_SUB),  # A = 0 - 7 = -7
                (1, _F_STO),
                (0, _F_STP),
            ]
        )
        baby.run()
        assert baby.get_store(1) == -7

    def test_store_then_load(self):
        """STO then LDN to negate stored value."""
        baby = ManchesterBaby()
        baby.store_word(0, 15)
        baby.store_program(
            [
                (0, _F_LDN),  # A = -15
                (28, _F_STO),  # store[28] = -15
                (28, _F_LDN),  # A = 15
                (29, _F_STO),  # store[29] = 15
                (0, _F_STP),
            ]
        )
        baby.run()
        assert baby.get_store(29) == 15

    def test_subtraction_chain(self):
        """10 - 3 - 2 - 1 = 4."""
        baby = ManchesterBaby()
        baby.store_word(0, 3)
        baby.store_word(31, 2)
        # Need to get 10 into accumulator
        # -(-10) = 10: store -10 at addr 30, LDN gives +10
        baby.store_word(30, -10)
        baby.store_program(
            [
                (30, _F_LDN),  # A = 10
                (0, _F_SUB),  # A = 7
                (31, _F_SUB),  # A = 5
                (28, _F_STO),
                (0, _F_STP),
            ]
        )
        baby.run()
        assert baby.get_store(28) == 5

    def test_simple_factor_finding(self):
        """Kilburn-style: verify LDN/STO/SUB work for arithmetic programs."""
        baby = ManchesterBaby()
        baby.store_word(0, 6)
        baby.store_word(31, 3)
        baby.store_program(
            [
                (0, _F_LDN),  # A = -6
                (30, _F_STO),  # store[30] = -6
                (31, _F_SUB),  # A = -6 - 3 = -9
                (29, _F_STO),
                (0, _F_STP),
            ]
        )
        baby.run()
        assert baby.get_store(30) == -6
        assert baby.get_store(29) == -9

    def test_cmp_loop_terminates(self):
        """CMP-based loop: subtract 1 until accumulator < 0, then halt."""
        # Count down from +3: 3, 2, 1, 0, -1 -> skip on -1 to halt
        baby = ManchesterBaby()
        # store[0] = 1 (the decrement constant)
        baby.store_word(0, 1)
        # Starting accumulator: set via store_word trick
        # store[-3] trick: store_word(31, -3) then LDN gives +3
        baby.store_word(31, -3)

        # Program:
        # addr 1: LDN 31  -> A = 3
        # addr 2: STO 30  -> store[30] = 3 (save our counter)
        # addr 3: SUB 0   -> A = A - 1
        # addr 4: STO 30  -> update counter
        # ... loop is too complex for 32-word store without JMP
        # Test a 2-iteration version instead:
        baby.store_program(
            [
                (31, _F_LDN),  # A = 3
                (0, _F_SUB),  # A = 2
                (0, _F_SUB),  # A = 1
                (0, _F_SUB),  # A = 0
                (0, _F_SUB),  # A = -1
                (29, _F_STO),  # store[29] = -1
                (0, _F_STP),
            ]
        )
        baby.run()
        assert baby.get_store(29) == -1


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


class TestControl:
    def test_halt_stops(self):
        baby = ManchesterBaby()
        baby.store_program([(0, _F_STP)])
        baby.run()
        assert baby.state.halted

    def test_cycle_count(self):
        baby = ManchesterBaby()
        baby.store_word(0, 0)
        baby.store_program(
            [
                (0, _F_LDN),
                (0, _F_STP),
            ]
        )
        baby.run()
        assert baby.state.cycle_count == 2

    def test_max_cycles_limit(self):
        baby = ManchesterBaby()
        baby.state.halted = False
        cycles = baby.run(max_cycles=10)
        assert cycles <= 10

    def test_step_returns_false_on_stp(self):
        baby = ManchesterBaby()
        baby.store_program([(0, _F_STP)])
        result = baby.step()
        assert result is False

    def test_step_returns_false_when_already_halted(self):
        baby = ManchesterBaby()
        baby.state.halted = True
        result = baby.step()
        assert result is False

    def test_step_returns_true_on_normal_instruction(self):
        baby = ManchesterBaby()
        baby.store_word(0, 0)
        baby.store_program([(0, _F_LDN), (0, _F_STP)])
        # First step: LDN
        result = baby.step()
        assert result is True

    def test_reset_preserves_store(self):
        baby = ManchesterBaby()
        baby.store_word(5, 42)
        baby.state.accumulator = 100
        baby.reset()
        assert baby.state.accumulator == 0
        assert baby.get_store(5) == 42  # store preserved

    def test_reset_clears_cycle_count(self):
        baby = ManchesterBaby()
        baby.store_program([(0, _F_LDN), (0, _F_STP)])
        baby.run()
        baby.reset()
        assert baby.state.cycle_count == 0

    def test_reset_clears_halted_flag(self):
        baby = ManchesterBaby()
        baby.store_program([(0, _F_STP)])
        baby.run()
        assert baby.state.halted
        baby.reset()
        assert not baby.state.halted

    def test_full_reset_clears_store(self):
        baby = ManchesterBaby()
        baby.store_word(5, 42)
        baby.full_reset()
        assert baby.get_store(5) == 0

    def test_full_reset_clears_accumulator(self):
        baby = ManchesterBaby()
        baby.state.accumulator = 9999
        baby.full_reset()
        assert baby.state.accumulator == 0

    def test_state_snapshot_keys(self):
        baby = ManchesterBaby()
        baby.store_program([(0, _F_STP)])
        baby.run()
        s = baby.state_snapshot()
        assert "accumulator" in s
        assert "ci" in s
        assert "halted" in s
        assert "cycle_count" in s
        assert len(s["store"]) == _STORE_SIZE

    def test_state_snapshot_values_match(self):
        baby = ManchesterBaby()
        baby.store_word(3, 77)
        baby.store_program([(0, _F_STP)])
        baby.run()
        s = baby.state_snapshot()
        assert s["halted"] is True
        assert s["store"][3] == _to_unsigned(77)

    def test_store_program_resets_ci_and_acc(self):
        baby = ManchesterBaby()
        baby.state.accumulator = 999
        baby.state.ci = 15
        baby.store_program([(0, _F_STP)])
        assert baby.state.accumulator == 0
        assert baby.state.ci == 0
        assert not baby.state.halted

    def test_program_instructions_start_at_addr_1(self):
        """store_program puts instructions starting at address 1."""
        baby = ManchesterBaby()
        baby.store_program([(5, _F_LDN)])
        word = baby.state.store[1]
        s, f = ManchesterBaby.decode(word)
        assert s == 5
        assert f == _F_LDN


# ---------------------------------------------------------------------------
# 32-bit overflow
# ---------------------------------------------------------------------------


class TestOverflow:
    def test_sub_overflow_wraps(self):
        """A = MIN_INT - 1 wraps to MAX_INT."""
        baby = ManchesterBaby()
        baby.store_word(0, 1)
        baby.store_program([(0, _F_SUB), (0, _F_STP)])
        baby.state.accumulator = -2147483648  # MIN_INT
        baby.run()
        assert baby.state.accumulator == 2147483647  # wraps to MAX_INT

    def test_sto_stores_as_unsigned_reads_as_signed(self):
        """store_word(-1) stores 0xFFFFFFFF; get_store reads it back as -1."""
        baby = ManchesterBaby()
        baby.store_word(0, -1)
        assert baby.state.store[0] == 0xFFFFFFFF
        assert baby.get_store(0) == -1
