"""Unit tests for the Colossus Mark 2 emulator (Bletchley Park, 1944)."""

from __future__ import annotations

import random

import pytest

from backend.src.emulator.colossus import (
    CHI_PERIODS,
    MOTOR_PERIODS,
    PSI_PERIODS,
    Colossus,
    ColossusCounts,
    LorenzSZ42,
    LorenzWheel,
)

# ---------------------------------------------------------------------------
# LorenzWheel
# ---------------------------------------------------------------------------


class TestLorenzWheel:
    def test_uniform_all_zero(self):
        w = LorenzWheel.uniform(10, 0)
        assert all(c == 0 for c in w.cams)
        assert w.period == 10

    def test_uniform_all_one(self):
        w = LorenzWheel.uniform(5, 1)
        assert w.current() == 1
        assert all(c == 1 for c in w.cams)

    def test_from_pattern(self):
        w = LorenzWheel.from_pattern([1, 0, 1, 0])
        assert w.current() == 1
        w.step()
        assert w.current() == 0

    def test_from_pattern_sets_period(self):
        w = LorenzWheel.from_pattern([1, 0, 0, 1, 1])
        assert w.period == 5

    def test_step_cycles(self):
        w = LorenzWheel.from_pattern([1, 0])
        for _ in range(6):
            w.step()
        assert w.pos == 0

    def test_step_increments_position(self):
        w = LorenzWheel.from_pattern([0, 1, 0, 0])
        w.step()
        assert w.pos == 1

    def test_step_wraps_at_period(self):
        w = LorenzWheel.uniform(3, 0)
        w.step()
        w.step()
        w.step()
        assert w.pos == 0

    def test_current_at_position(self):
        """current() always returns cams[pos]."""
        w = LorenzWheel.from_pattern([0, 1, 1, 0, 1])
        for i in range(5):
            assert w.current() == w.cams[i]
            w.step()

    def test_period_one_wraps_immediately(self):
        w = LorenzWheel.uniform(1, 1)
        assert w.current() == 1
        w.step()
        assert w.pos == 0
        assert w.current() == 1

    def test_reset_returns_to_zero(self):
        w = LorenzWheel.from_pattern([0, 1, 1])
        w.step()
        w.step()
        w.reset()
        assert w.pos == 0

    def test_cams_not_mutated_by_step(self):
        cams = [1, 0, 1, 0]
        w = LorenzWheel.from_pattern(cams)
        w.step()
        w.step()
        assert w.cams == cams

    def test_chi_periods_constant(self):
        assert CHI_PERIODS == [41, 31, 29, 26, 23]

    def test_psi_periods_constant(self):
        assert PSI_PERIODS == [43, 47, 51, 53, 59]

    def test_motor_periods_constant(self):
        assert MOTOR_PERIODS == [37, 61]


# ---------------------------------------------------------------------------
# LorenzSZ42
# ---------------------------------------------------------------------------


class TestLorenzSZ42:
    def test_with_random_key_constructs(self):
        lorenz = LorenzSZ42.with_random_key(seed=0)
        assert len(lorenz.chi_wheels) == 5
        assert len(lorenz.psi_wheels) == 5
        assert len(lorenz.mu_wheels) == 2

    def test_wheel_periods_correct(self):
        lorenz = LorenzSZ42.with_random_key(seed=0)
        for w, p in zip(lorenz.chi_wheels, CHI_PERIODS, strict=True):
            assert w.period == p
        for w, p in zip(lorenz.psi_wheels, PSI_PERIODS, strict=True):
            assert w.period == p

    def test_motor_periods_correct(self):
        lorenz = LorenzSZ42.with_random_key(seed=0)
        assert lorenz.mu_wheels[0].period == 37
        assert lorenz.mu_wheels[1].period == 61

    def test_encipher_self_reciprocal(self):
        """Lorenz SZ is a Vernam cipher: XOR with key stream twice = identity."""
        lorenz1 = LorenzSZ42.with_random_key(seed=7)
        plain = [[1, 0, 1, 0, 1]] * 20
        cipher = lorenz1.encipher(plain)
        lorenz2 = LorenzSZ42.with_random_key(seed=7)
        recovered = lorenz2.encipher(cipher)
        assert recovered == plain

    def test_encipher_longer_message(self):
        """Reciprocal property holds for 100 characters."""
        lorenz1 = LorenzSZ42.with_random_key(seed=99)
        plain = [[random.randint(0, 1) for _ in range(5)] for _ in range(100)]
        cipher = lorenz1.encipher(plain)
        lorenz2 = LorenzSZ42.with_random_key(seed=99)
        recovered = lorenz2.encipher(cipher)
        assert recovered == plain

    def test_different_seeds_different_keys(self):
        lorenz1 = LorenzSZ42.with_random_key(seed=1)
        lorenz2 = LorenzSZ42.with_random_key(seed=2)
        chi1 = [w.cams[:5] for w in lorenz1.chi_wheels]
        chi2 = [w.cams[:5] for w in lorenz2.chi_wheels]
        assert chi1 != chi2

    def test_current_key_is_chi_xor_psi(self):
        lorenz = LorenzSZ42.with_random_key(seed=0)
        chi = lorenz.current_chi()
        psi = lorenz.current_psi()
        key = lorenz.current_key()
        for c, p, k in zip(chi, psi, key, strict=True):
            assert k == c ^ p

    def test_current_chi_length_five(self):
        lorenz = LorenzSZ42.with_random_key(seed=0)
        assert len(lorenz.current_chi()) == 5

    def test_current_psi_length_five(self):
        lorenz = LorenzSZ42.with_random_key(seed=0)
        assert len(lorenz.current_psi()) == 5

    def test_current_key_length_five(self):
        lorenz = LorenzSZ42.with_random_key(seed=0)
        assert len(lorenz.current_key()) == 5

    def test_all_key_bits_are_zero_or_one(self):
        lorenz = LorenzSZ42.with_random_key(seed=3)
        for _ in range(50):
            key = lorenz.current_key()
            assert all(b in (0, 1) for b in key)
            lorenz._step()

    def test_reset_returns_to_start(self):
        lorenz = LorenzSZ42.with_random_key(seed=3)
        initial_chi = lorenz.current_chi()
        lorenz.encipher([[0, 1, 0, 1, 0]] * 10)
        lorenz.reset()
        assert lorenz.current_chi() == initial_chi

    def test_reset_preserves_cam_settings(self):
        lorenz = LorenzSZ42.with_random_key(seed=5)
        cams_before = [list(w.cams) for w in lorenz.chi_wheels]
        lorenz.encipher([[1, 0, 1, 0, 1]] * 20)
        lorenz.reset()
        cams_after = [list(w.cams) for w in lorenz.chi_wheels]
        assert cams_before == cams_after

    def test_encipher_char_advances_chi_wheels(self):
        lorenz = LorenzSZ42.with_random_key(seed=11)
        pos_before = lorenz.chi_wheels[0].pos
        lorenz.encipher_char([1, 0, 1, 0, 1])
        # Each encipher_char call advances all chi wheels
        assert lorenz.chi_wheels[0].pos != pos_before or lorenz.chi_wheels[0].period == 1

    def test_encipher_empty_list(self):
        lorenz = LorenzSZ42.with_random_key(seed=0)
        assert lorenz.encipher([]) == []

    def test_from_key_explicit(self):
        key = {
            "chi_1": [1, 0] * 20 + [1],  # 41 cams
            "chi_2": [0, 1] * 15 + [0],  # 31 cams
            "chi_3": [1] * 29,
            "chi_4": [0, 1] * 13,  # 26 cams
            "chi_5": [1, 0] * 11 + [1],  # 23 cams
            "psi_1": [1] * 43,
            "psi_2": [0] * 47,
            "psi_3": [1, 0] * 25 + [1],
            "psi_4": [0, 1] * 26 + [1],
            "psi_5": [1] * 59,
            "mu_37": [1, 0] * 18 + [1],
            "mu_61": [0, 1] * 30 + [0],
        }
        lorenz = LorenzSZ42.from_key(key)
        assert lorenz.chi_wheels[0].period == 41
        assert lorenz.psi_wheels[1].period == 47

    def test_all_zero_key_encipher_is_identity(self):
        """With all-zero cam patterns, key stream = 0 XOR 0 = 0, so cipher = plain."""
        key = {
            **{f"chi_{i + 1}": [0] * CHI_PERIODS[i] for i in range(5)},
            **{f"psi_{i + 1}": [0] * PSI_PERIODS[i] for i in range(5)},
            "mu_37": [1] * 37,
            "mu_61": [1] * 61,
        }
        lorenz = LorenzSZ42.from_key(key)
        plain = [[1, 0, 1, 1, 0]] * 10
        cipher = lorenz.encipher(plain)
        assert cipher == plain  # key = 0, so XOR is identity

    def test_all_one_psi_zero_chi_xor_is_plain(self):
        """chi=1, psi=1 -> key=0 -> cipher=plain."""
        key = {
            **{f"chi_{i + 1}": [1] * CHI_PERIODS[i] for i in range(5)},
            **{f"psi_{i + 1}": [1] * PSI_PERIODS[i] for i in range(5)},
            "mu_37": [1] * 37,
            "mu_61": [1] * 61,
        }
        lorenz = LorenzSZ42.from_key(key)
        plain = [[0, 1, 0, 1, 0]] * 5
        cipher = lorenz.encipher(plain)
        assert cipher == plain


# ---------------------------------------------------------------------------
# Colossus chi-breaking
# ---------------------------------------------------------------------------


class TestColossus:
    def _setup_zero_psi(
        self, chi_start: int = 0, tape_len: int = 200
    ) -> tuple[Colossus, list[list[int]], int]:
        """Lorenz with all-zero psi: chi-breaking has a clear signal."""
        key: dict[str, list[int]] = {
            **{
                f"chi_{i + 1}": random.Random(i).choices([0, 1], k=CHI_PERIODS[i]) for i in range(5)
            },
            "psi_1": [0] * 43,
            "psi_2": [0] * 47,
            "psi_3": [0] * 51,
            "psi_4": [0] * 53,
            "psi_5": [0] * 59,
            "mu_37": [1] * 37,
            "mu_61": [1] * 61,
        }
        lorenz = LorenzSZ42.from_key(key)
        lorenz.chi_wheels[0].pos = chi_start
        plaintext = [[1, 1, 1, 1, 1]] * tape_len
        cipher = lorenz.encipher(plaintext)
        lorenz.reset()
        colossus = Colossus(lorenz)
        return colossus, cipher, chi_start

    def _setup(
        self, seed: int = 42, tape_len: int = 100
    ) -> tuple[Colossus, list[list[int]], list[int]]:
        lorenz = LorenzSZ42.with_random_key(seed=seed)
        true_starts = [w.pos for w in lorenz.chi_wheels]
        plaintext = [[1, 0, 1, 0, 1]] * tape_len
        cipher = lorenz.encipher(plaintext)
        lorenz.reset()
        colossus = Colossus(lorenz)
        return colossus, cipher, true_starts

    def test_chi_break_single_returns_counts(self):
        colossus, tape, _ = self._setup(tape_len=50)
        counts = colossus.chi_break_single_wheel(tape, wheel_index=0)
        assert isinstance(counts, dict)
        assert len(counts) == CHI_PERIODS[0]  # 41 positions
        assert all(0 <= v <= 50 for v in counts.values())

    def test_chi_break_finds_true_start(self):
        """With all-zero psi the correct chi position gives count = tape_length."""
        tape_len = 200
        true_start = 7
        colossus, tape, _ = self._setup_zero_psi(chi_start=true_start, tape_len=tape_len)
        counts = colossus.chi_break_single_wheel(tape, wheel_index=0)
        peak_pos, peak_count = colossus.find_peak(counts)
        assert peak_pos == true_start
        assert peak_count == tape_len

    def test_chi_break_all_returns_all_wheels(self):
        colossus, tape, _ = self._setup(tape_len=50)
        all_counts = colossus.chi_break_all(tape)
        assert len(all_counts) == 5
        for i, period in enumerate(CHI_PERIODS):
            assert len(all_counts[i]) == period

    def test_chi_break_all_wheel_periods_match(self):
        """Each entry in chi_break_all has exactly period[i] positions."""
        colossus, tape, _ = self._setup(tape_len=30)
        all_counts = colossus.chi_break_all(tape)
        for i in range(5):
            assert sorted(all_counts[i].keys()) == list(range(CHI_PERIODS[i]))

    def test_chi_break_any_active_function(self):
        colossus, tape, _ = self._setup(tape_len=40)
        counts = colossus.chi_break_single_wheel(tape, wheel_index=0, function="any_active")
        assert len(counts) == CHI_PERIODS[0]
        assert all(0 <= v <= 40 for v in counts.values())

    def test_chi_break_all_active_function(self):
        colossus, tape, _ = self._setup(tape_len=40)
        counts = colossus.chi_break_single_wheel(tape, wheel_index=0, function="all_active")
        assert len(counts) == CHI_PERIODS[0]
        assert all(0 <= v <= 40 for v in counts.values())

    def test_chi_break_bit_1_function(self):
        colossus, tape, _ = self._setup(tape_len=40)
        counts = colossus.chi_break_single_wheel(tape, wheel_index=0, function="bit_1")
        assert len(counts) == CHI_PERIODS[0]
        assert all(0 <= v <= 40 for v in counts.values())

    def test_find_peak(self):
        counts = {0: 10, 1: 30, 2: 20}
        pos, val = Colossus.find_peak(counts)
        assert pos == 1
        assert val == 30

    def test_find_peak_single_entry(self):
        pos, val = Colossus.find_peak({7: 42})
        assert pos == 7
        assert val == 42

    def test_find_peak_ties_returns_one(self):
        """If counts are tied, find_peak returns one of the peaks (no error)."""
        counts = {0: 50, 1: 50, 2: 50}
        pos, val = Colossus.find_peak(counts)
        assert val == 50
        assert pos in (0, 1, 2)

    def test_run_count_increments(self):
        colossus, tape, _ = self._setup(tape_len=30)
        colossus.chi_break_single_wheel(tape, wheel_index=0)
        assert colossus.run_count == CHI_PERIODS[0]

    def test_run_count_after_chi_break_all(self):
        """chi_break_all runs 5 single-wheel scans; total runs = sum(periods)."""
        colossus, tape, _ = self._setup(tape_len=20)
        colossus.chi_break_all(tape)
        expected = sum(CHI_PERIODS)
        assert colossus.run_count == expected

    def test_state_dict_keys(self):
        lorenz = LorenzSZ42.with_random_key()
        col = Colossus(lorenz)
        s = col.state()
        assert "chi_positions" in s
        assert "psi_positions" in s
        assert "tape_length" in s
        assert "run_count" in s

    def test_state_initial_run_count_zero(self):
        lorenz = LorenzSZ42.with_random_key()
        col = Colossus(lorenz)
        assert col.state()["run_count"] == 0

    def test_state_chi_positions_count(self):
        lorenz = LorenzSZ42.with_random_key()
        col = Colossus(lorenz)
        assert len(col.state()["chi_positions"]) == 5

    def test_state_psi_positions_count(self):
        lorenz = LorenzSZ42.with_random_key()
        col = Colossus(lorenz)
        assert len(col.state()["psi_positions"]) == 5

    def test_state_run_count_increments_after_run(self):
        colossus, tape, _ = self._setup(tape_len=20)
        colossus.chi_break_single_wheel(tape, wheel_index=2)
        assert colossus.state()["run_count"] == CHI_PERIODS[2]

    def test_ita2_e(self):
        assert Colossus.ita2_bits_to_char([0, 0, 0, 0, 1]) == "E"

    def test_ita2_t(self):
        assert Colossus.ita2_bits_to_char([0, 0, 0, 1, 0]) == "T"

    def test_ita2_zero_returns_at(self):
        # ITA2 value 0 = "@" (null/blank)
        assert Colossus.ita2_bits_to_char([0, 0, 0, 0, 0]) == "@"

    def test_ita2_out_of_range_returns_question(self):
        # Value >= 27 is out of ITA2 letters range
        assert Colossus.ita2_bits_to_char([1, 1, 0, 1, 1]) == "?"

    def test_ita2_all_ones_out_of_range(self):
        result = Colossus.ita2_bits_to_char([1, 1, 1, 1, 1])
        assert result == "?"

    def test_unknown_function_raises(self):
        lorenz = LorenzSZ42.with_random_key()
        col = Colossus(lorenz)
        with pytest.raises(ValueError, match="Unknown function"):
            col._run_at_position([[1, 0, 1, 0, 1]], [0, 0, 0, 0, 0], "bad_fn")

    def test_chi_break_second_wheel(self):
        """chi_break_single_wheel works for wheel_index=1 (period=31)."""
        colossus, tape, _ = self._setup(tape_len=40)
        counts = colossus.chi_break_single_wheel(tape, wheel_index=1)
        assert len(counts) == CHI_PERIODS[1]  # 31

    def test_chi_break_fifth_wheel(self):
        """chi_break_single_wheel works for wheel_index=4 (period=23)."""
        colossus, tape, _ = self._setup(tape_len=40)
        counts = colossus.chi_break_single_wheel(tape, wheel_index=4)
        assert len(counts) == CHI_PERIODS[4]  # 23


# ---------------------------------------------------------------------------
# ColossusCounts dataclass
# ---------------------------------------------------------------------------


class TestColossusCounts:
    def test_construct(self):
        cc = ColossusCounts(tape_length=100, function_label="XOR_sum_odd")
        assert cc.tape_length == 100
        assert cc.function_label == "XOR_sum_odd"
        assert cc.counts == {}

    def test_add_counts(self):
        cc = ColossusCounts(tape_length=50, function_label="bit_1")
        cc.counts[(0, 0, 0, 0, 0)] = 25
        assert cc.counts[(0, 0, 0, 0, 0)] == 25
