"""Unit tests for the Colossus Mark 2 emulator (Bletchley Park, 1944)."""

import pytest

from backend.src.emulator.colossus import (
    CHI_PERIODS,
    PSI_PERIODS,
    Colossus,
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

    def test_from_pattern(self):
        w = LorenzWheel.from_pattern([1, 0, 1, 0])
        assert w.current() == 1
        w.step()
        assert w.current() == 0

    def test_step_cycles(self):
        w = LorenzWheel.from_pattern([1, 0])
        for _ in range(6):
            w.step()
        # 6 steps on period 2 -> back to start
        assert w.pos == 0

    def test_reset(self):
        w = LorenzWheel.from_pattern([0, 1, 1])
        w.step()
        w.step()
        w.reset()
        assert w.pos == 0

    def test_chi_periods(self):
        assert CHI_PERIODS == [41, 31, 29, 26, 23]

    def test_psi_periods(self):
        assert PSI_PERIODS == [43, 47, 51, 53, 59]


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

    def test_encipher_self_reciprocal(self):
        """Lorenz SZ is a Vernam cipher: XOR with key stream twice = identity."""
        lorenz1 = LorenzSZ42.with_random_key(seed=7)
        plain = [[1, 0, 1, 0, 1]] * 20
        cipher = lorenz1.encipher(plain)
        # Reset and decipher with same key
        lorenz2 = LorenzSZ42.with_random_key(seed=7)
        recovered = lorenz2.encipher(cipher)
        assert recovered == plain

    def test_different_seeds_different_keys(self):
        lorenz1 = LorenzSZ42.with_random_key(seed=1)
        lorenz2 = LorenzSZ42.with_random_key(seed=2)
        chi1 = [w.cams[:5] for w in lorenz1.chi_wheels]
        chi2 = [w.cams[:5] for w in lorenz2.chi_wheels]
        assert chi1 != chi2

    def test_current_key_is_xor(self):
        lorenz = LorenzSZ42.with_random_key(seed=0)
        chi = lorenz.current_chi()
        psi = lorenz.current_psi()
        key = lorenz.current_key()
        for c, p, k in zip(chi, psi, key, strict=True):
            assert k == c ^ p

    def test_reset_returns_to_start(self):
        lorenz = LorenzSZ42.with_random_key(seed=3)
        initial_chi = lorenz.current_chi()
        lorenz.encipher([[0, 1, 0, 1, 0]] * 10)
        lorenz.reset()
        assert lorenz.current_chi() == initial_chi

    def test_from_key_explicit(self):
        """Construct Lorenz from explicit cam patterns."""
        key = {
            "chi_1": [1, 0] * 20 + [1],  # 41 cams
            "chi_2": [0, 1] * 15 + [0],  # 31 cams
            "chi_3": [1] * 29,  # 29 cams
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


# ---------------------------------------------------------------------------
# Colossus chi-breaking
# ---------------------------------------------------------------------------


class TestColossus:
    def _setup_zero_psi(
        self, chi_start: int = 0, tape_len: int = 200
    ) -> tuple[Colossus, list[list[int]], int]:
        """Build a Lorenz with all-zero psi so chi-breaking has a clear signal.

        WHY all-zero psi: with psi=0, the key stream equals chi stream, so
        cipher = plain XOR chi.  At the correct chi starting position the
        Colossus computes tape XOR chi_test = plain, which has deterministic
        parity.  Any wrong position produces a different XOR pattern.

        Returns (colossus, ciphertext_tape, true_chi_1_start).
        """
        key: dict[str, list[int]] = {
            **{
                f"chi_{i+1}": __import__("random", fromlist=[])
                .Random(i)
                .choices([0, 1], k=CHI_PERIODS[i])
                for i in range(5)
            },
            "psi_1": [0] * 43,
            "psi_2": [0] * 47,
            "psi_3": [0] * 51,
            "psi_4": [0] * 53,
            "psi_5": [0] * 59,
            "mu_37": [1] * 37,  # always-on: psi always steps (doesn't matter, psi=0)
            "mu_61": [1] * 61,
        }
        # Set chi wheel 1 to a specific starting position
        lorenz = LorenzSZ42.from_key(key)
        lorenz.chi_wheels[0].pos = chi_start
        # All-one plaintext: [1,1,1,1,1] -> parity always 1
        plaintext = [[1, 1, 1, 1, 1]] * tape_len
        cipher = lorenz.encipher(plaintext)
        lorenz.reset()
        # Restore chi wheel 1 to position 0 (as it was BEFORE we set the start)
        # The Colossus scans all positions of chi wheel 1
        colossus = Colossus(lorenz)
        return colossus, cipher, chi_start

    def _setup(
        self, seed: int = 42, tape_len: int = 100
    ) -> tuple[Colossus, list[list[int]], list[int]]:
        """Build a Lorenz, encrypt some plaintext, return (colossus, tape, true_chi_starts)."""
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
        """With all-zero psi, the correct chi position gives count = tape_length
        (all bits sum to odd parity since plain = [1,1,1,1,1]).
        All other positions give lower counts due to chi XOR mismatch."""
        tape_len = 200
        true_start = 7  # arbitrary chi_1 start
        colossus, tape, _ = self._setup_zero_psi(chi_start=true_start, tape_len=tape_len)
        counts = colossus.chi_break_single_wheel(tape, wheel_index=0)
        peak_pos, peak_count = colossus.find_peak(counts)
        assert peak_pos == true_start, (
            f"Expected peak at {true_start}, got {peak_pos} "
            f"(top 3: {sorted(counts.items(), key=lambda x: -x[1])[:3]})"
        )
        assert peak_count == tape_len

    def test_chi_break_all_returns_all_wheels(self):
        colossus, tape, _ = self._setup(tape_len=50)
        all_counts = colossus.chi_break_all(tape)
        assert len(all_counts) == 5
        for i, period in enumerate(CHI_PERIODS):
            assert len(all_counts[i]) == period

    def test_find_peak(self):
        counts = {0: 10, 1: 30, 2: 20}
        pos, val = Colossus.find_peak(counts)
        assert pos == 1
        assert val == 30

    def test_run_count_increments(self):
        colossus, tape, _ = self._setup(tape_len=30)
        colossus.chi_break_single_wheel(tape, wheel_index=0)
        assert colossus.run_count == CHI_PERIODS[0]

    def test_state_dict(self):
        lorenz = LorenzSZ42.with_random_key()
        col = Colossus(lorenz)
        s = col.state()
        assert "chi_positions" in s
        assert "psi_positions" in s
        assert len(s["chi_positions"]) == 5

    def test_ita2_conversion(self):
        # ITA2 value 1 = E, value 2 = T (per the table)
        assert Colossus.ita2_bits_to_char([0, 0, 0, 0, 1]) == "E"
        assert Colossus.ita2_bits_to_char([0, 0, 0, 1, 0]) == "T"

    def test_unknown_function_raises(self):
        lorenz = LorenzSZ42.with_random_key()
        col = Colossus(lorenz)
        with pytest.raises(ValueError, match="Unknown function"):
            col._run_at_position([[1, 0, 1, 0, 1]], [0, 0, 0, 0, 0], "bad_fn")
