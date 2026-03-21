"""Colossus Mark 2 Emulator (Bletchley Park, 1944).

Colossus was designed by Tommy Flowers (1905-1998), a Post Office engineer,
and programmed by Max Newman and Bill Tutte's team at Bletchley Park.
Mark 1 became operational in February 1943; the improved Mark 2 in June 1944,
just in time for D-Day. Ten Colossi were in operation by the end of WW2.

Colossus was built to break the German Lorenz SZ 40/42 cipher machine
(codenamed "Tunny" at Bletchley), used for Hitler's high-command traffic.
Unlike the Enigma (broken by the Bombe), Tunny used a 5-bit teleprinter code
(ITA2 / Baudot code) with 12 wheels of different periods.

Architecture:
  - Optical tape reader: reads a looped paper tape at 5,000 characters/second
    (25 mph). The tape carries ciphertext in 5-bit ITA2 teleprinter code.
  - Thyratron ring counters: simulate the 12 Lorenz SZ wheels (chi, psi, motor).
  - Electronic logic: counts and compares patterns of XORs between tape and
    wheel outputs using Boolean logic circuits.
  - Plugboard and switches: configure which Boolean operations to count.
  - Output: electric typewriter printing counts.

Lorenz SZ 42 wheel configuration:
  - 5 chi (chi_1..chi_5) wheels with periods 41, 31, 29, 26, 23.
  - 5 psi (psi_1..psi_5) wheels with periods 43, 47, 51, 53, 59.
  - 2 motor wheels (mu_37, mu_61) controlling psi stepping.

Colossus operation (simplified):
  For each tape position, Colossus computes:
    T_i XOR chi_i (for each bit i = 1..5)
  and counts positions where a selected Boolean function of these 5 bits
  equals 1. The count peak identifies the correct chi wheel starting positions.
  (This is called the "chi-breaking" run; psi-breaking is a separate step.)

This emulator models:
  - LorenzWheel: A single Lorenz SZ wheel as a cyclic bit pattern.
  - LorenzSZ42: The full 12-wheel Lorenz machine (chi + psi + motor).
  - Colossus: The tape reader, wheel counters, and counting logic.

References:
  - Flowers, T. H. (1983). The design of Colossus. Annals of the History of
    Computing, 5(3), 239-252.
  - Good, I. J., Michie, D., & Timms, G. (1945). General Report on Tunny.
    (Declassified 2000, National Archives HW 25/4 & 25/5.)
  - Copeland, B. J. (Ed.). (2006). Colossus: The Secrets of Bletchley Park's
    Codebreaking Computers. Oxford University Press.
  - National Museum of Computing, Bletchley Park.
    https://www.tnmoc.org/colossus (Colossus rebuild documentation)
"""

from __future__ import annotations

from dataclasses import dataclass, field

# ---------------------------------------------------------------------------
# ITA2 (Baudot-Murray) teleprinter code constants
# ---------------------------------------------------------------------------

# Lorenz wheel periods (number of cams per wheel)
CHI_PERIODS = [41, 31, 29, 26, 23]  # 5 chi wheels
PSI_PERIODS = [43, 47, 51, 53, 59]  # 5 psi wheels
MOTOR_PERIODS = [37, 61]  # 2 motor wheels (mu_37, mu_61)


# ---------------------------------------------------------------------------
# Lorenz wheel
# ---------------------------------------------------------------------------


@dataclass
class LorenzWheel:
    """One Lorenz SZ wheel: a cyclic sequence of 0/1 cam settings.

    Each cam is either active (1) or inactive (0). The wheel advances by one
    position on each tape step (for chi/motor wheels) or conditionally for
    psi wheels.

    period: Number of cams (length of the cycle).
    cams:   Bit pattern of length `period` (0 or 1 per cam).
    pos:    Current position (0..period-1).
    """

    period: int
    cams: list[int]
    pos: int = 0

    @classmethod
    def from_pattern(cls, cams: list[int]) -> LorenzWheel:
        """Create a wheel from a list of 0/1 cam settings."""
        return cls(period=len(cams), cams=list(cams), pos=0)

    @classmethod
    def uniform(cls, period: int, value: int = 0) -> LorenzWheel:
        """Create a wheel with all cams set to `value` (0 or 1)."""
        return cls(period=period, cams=[value] * period, pos=0)

    def current(self) -> int:
        """Return the current cam value (0 or 1)."""
        return self.cams[self.pos % self.period]

    def step(self) -> None:
        """Advance the wheel one position."""
        self.pos = (self.pos + 1) % self.period

    def reset(self) -> None:
        self.pos = 0


# ---------------------------------------------------------------------------
# Lorenz SZ 42
# ---------------------------------------------------------------------------


@dataclass
class LorenzSZ42:
    """Lorenz SZ 40/42 cipher machine (Germany, 1940-1945).

    A 12-wheel Vernam-type stream cipher attached to a teleprinter.
    Each character is XOR-ed with a 5-bit key stream derived from the 12 wheels.

    Key stream for bit i (1..5):
      key_i = chi_i XOR psi_i
    Cipher bit i:
      cipher_i = plain_i XOR key_i

    Psi stepping rule (active psi stepping):
      Psi wheels step only when mu_61 = 1.
      mu_61 steps when mu_37 = 1 (basic motor mode).

    References:
      Good et al. (1945), General Report on Tunny, Ch. 2.
    """

    chi_wheels: list[LorenzWheel]  # 5 chi wheels (always step)
    psi_wheels: list[LorenzWheel]  # 5 psi wheels (conditional step)
    mu_wheels: list[LorenzWheel]  # 2 motor wheels (mu_37, mu_61)

    @classmethod
    def from_key(cls, key: dict[str, list[int]]) -> LorenzSZ42:
        """Create a Lorenz SZ from a key dict specifying cam patterns.

        key must have entries: 'chi_1'..'chi_5', 'psi_1'..'psi_5',
        'mu_37', 'mu_61'.  Each value is a list of 0/1 cam settings.
        """
        chi = [LorenzWheel.from_pattern(key[f"chi_{i+1}"]) for i in range(5)]
        psi = [LorenzWheel.from_pattern(key[f"psi_{i+1}"]) for i in range(5)]
        mu = [
            LorenzWheel.from_pattern(key["mu_37"]),
            LorenzWheel.from_pattern(key["mu_61"]),
        ]
        return cls(chi_wheels=chi, psi_wheels=psi, mu_wheels=mu)

    @classmethod
    def with_random_key(cls, seed: int = 42) -> LorenzSZ42:
        """Create a Lorenz SZ with a pseudorandom cam pattern."""
        import random

        rng = random.Random(seed)
        chi = [LorenzWheel(p, [rng.randint(0, 1) for _ in range(p)]) for p in CHI_PERIODS]
        psi = [LorenzWheel(p, [rng.randint(0, 1) for _ in range(p)]) for p in PSI_PERIODS]
        mu = [LorenzWheel(p, [rng.randint(0, 1) for _ in range(p)]) for p in MOTOR_PERIODS]
        return cls(chi_wheels=chi, psi_wheels=psi, mu_wheels=mu)

    def reset(self) -> None:
        """Reset all wheels to position 0."""
        for w in self.chi_wheels + self.psi_wheels + self.mu_wheels:
            w.reset()

    def current_chi(self) -> list[int]:
        """Return current 5-bit chi output."""
        return [w.current() for w in self.chi_wheels]

    def current_psi(self) -> list[int]:
        """Return current 5-bit psi output."""
        return [w.current() for w in self.psi_wheels]

    def current_key(self) -> list[int]:
        """Return the 5-bit key stream: chi XOR psi."""
        return [c ^ p for c, p in zip(self.current_chi(), self.current_psi(), strict=True)]

    def encipher_char(self, plaintext_bits: list[int]) -> list[int]:
        """Encipher one 5-bit character.

        Returns ciphertext bits. Advances all wheels after encipherment.
        """
        key = self.current_key()
        cipher = [p ^ k for p, k in zip(plaintext_bits, key, strict=True)]
        self._step()
        return cipher

    def encipher(self, plaintext: list[list[int]]) -> list[list[int]]:
        """Encipher a sequence of 5-bit characters."""
        return [self.encipher_char(ch) for ch in plaintext]

    def _step(self) -> None:
        """Advance all wheels according to the stepping rules."""
        # Chi wheels always step
        for w in self.chi_wheels:
            w.step()
        # Motor: mu_37 always steps; mu_61 steps when mu_37 = 1
        if self.mu_wheels[0].current():
            self.mu_wheels[1].step()
        self.mu_wheels[0].step()
        # Psi wheels step when mu_61 = 1
        if self.mu_wheels[1].current():
            for w in self.psi_wheels:
                w.step()


# ---------------------------------------------------------------------------
# Colossus
# ---------------------------------------------------------------------------


@dataclass
class ColossusCounts:
    """Counts from one Colossus run: how many tape positions matched
    the selected Boolean function for each of the 26^5 (or tested) positions.

    WHY: The operator would scan these counts looking for peaks. A count
    significantly above the expected random level (~tape_length/2) indicates
    the correct wheel starting positions.
    """

    tape_length: int
    function_label: str
    counts: dict[tuple[int, ...], int] = field(default_factory=dict)


class Colossus:
    """Colossus Mark 2 -- chi-wheel breaking mode.

    Reads a looped paper tape and counts the number of positions where a
    selected Boolean function of (tape XOR chi) bits evaluates to 1.
    By scanning chi wheel starting positions and finding count peaks,
    the operator determines the correct chi settings.

    Usage::

        lorenz = LorenzSZ42.with_random_key()
        plaintext = [[1,0,1,0,1]] * 100    # 100 chars of 5-bit data
        cipher_tape = lorenz.encipher(plaintext)
        lorenz.reset()

        colossus = Colossus(lorenz)
        # Scan chi wheel 1 starting positions (hold chi 2-5 at 0)
        counts = colossus.chi_break_single_wheel(cipher_tape, wheel_index=0)
    """

    def __init__(self, lorenz: LorenzSZ42) -> None:
        self.lorenz = lorenz
        self.tape_length: int = 0
        self.run_count: int = 0

    def _run_at_position(
        self,
        tape: list[list[int]],
        chi_starts: list[int],
        function: str = "XOR_sum_odd",
    ) -> int:
        """Count positions where the Boolean function is 1.

        Args:
            tape:       List of 5-bit ciphertext characters.
            chi_starts: Starting positions for chi wheels 1-5.
            function:   Which Boolean operation to count. Options:
                        "XOR_sum_odd" -- count positions where XOR(bits) is odd.
                        "any_active"  -- count where at least one bit is 1.
                        "all_active"  -- count where all bits are 1.
                        "bit_1"       -- count positions where bit 1 of (tape XOR chi) = 1.

        Returns:
            Count of matching positions.
        """
        # Set chi wheel positions
        for i, start in enumerate(chi_starts):
            self.lorenz.chi_wheels[i].pos = start % self.lorenz.chi_wheels[i].period

        count = 0
        for char_bits in tape:
            chi_bits = self.lorenz.current_chi()
            xor_bits = [t ^ c for t, c in zip(char_bits, chi_bits, strict=True)]

            if function == "XOR_sum_odd":
                # Count positions where XOR of all 5 bits is 1 (odd number of 1s)
                if sum(xor_bits) % 2 == 1:
                    count += 1
            elif function == "any_active":
                if any(xor_bits):
                    count += 1
            elif function == "all_active":
                if all(xor_bits):
                    count += 1
            elif function == "bit_1":
                if xor_bits[0] == 1:
                    count += 1
            else:
                raise ValueError(f"Unknown function: {function!r}")

            # Advance only chi wheels (this is the chi-breaking mode)
            for w in self.lorenz.chi_wheels:
                w.step()

        self.run_count += 1
        return count

    def chi_break_single_wheel(
        self,
        tape: list[list[int]],
        wheel_index: int = 0,
        function: str = "XOR_sum_odd",
    ) -> dict[int, int]:
        """Scan all starting positions for one chi wheel, holding others at 0.

        Returns a dict mapping starting position -> count. The correct
        starting position produces the highest count (peak detection).

        Args:
            tape:        Ciphertext tape (list of 5-bit chars).
            wheel_index: Which chi wheel (0-4) to scan.
            function:    Boolean function to count.

        Returns:
            Dict[starting_position -> count].
        """
        period = self.lorenz.chi_wheels[wheel_index].period
        counts: dict[int, int] = {}

        for start in range(period):
            chi_starts = [0] * 5
            chi_starts[wheel_index] = start
            counts[start] = self._run_at_position(tape, chi_starts, function)

        return counts

    def chi_break_all(
        self,
        tape: list[list[int]],
        function: str = "XOR_sum_odd",
    ) -> dict[int, dict[int, int]]:
        """Scan all 5 chi wheels independently.

        Returns a dict mapping wheel_index -> {starting_pos -> count}.
        """
        return {
            i: self.chi_break_single_wheel(tape, wheel_index=i, function=function) for i in range(5)
        }

    @staticmethod
    def find_peak(counts: dict[int, int]) -> tuple[int, int]:
        """Return (position, count) of the maximum count."""
        return max(counts.items(), key=lambda kv: kv[1])

    @staticmethod
    def ita2_bits_to_char(bits: list[int]) -> str:
        """Convert 5-bit ITA2 bits to the corresponding printable character.

        ITA2 (International Telegraph Alphabet No. 2 / Baudot-Murray):
        Bit order: b5 b4 b3 b2 b1 (b5 = MSB).
        Only LETTERS shift values are returned; figures shift is not modelled.
        """
        _ITA2_LETTERS = "@ETAOINSRHDLUCMWYFPXVBGKQJZ"
        val = (bits[0] << 4) | (bits[1] << 3) | (bits[2] << 2) | (bits[3] << 1) | bits[4]
        if val < len(_ITA2_LETTERS):
            return _ITA2_LETTERS[val]
        return "?"

    def state(self) -> dict[str, object]:
        return {
            "tape_length": self.tape_length,
            "run_count": self.run_count,
            "chi_positions": [w.pos for w in self.lorenz.chi_wheels],
            "psi_positions": [w.pos for w in self.lorenz.psi_wheels],
        }
