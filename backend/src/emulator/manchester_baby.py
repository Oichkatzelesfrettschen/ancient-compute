"""Manchester Small-Scale Experimental Machine (SSEM / Baby) Emulator (1948).

The SSEM, nicknamed "Baby", was designed by Freddie Williams (1911-1977) and
Tom Kilburn (1921-2001) at the University of Manchester. It ran its first
program on 21 June 1948, making it the first computer to run a stored program
-- i.e., a program held in electronic memory.

The SSEM was not intended as a practical computer but as a testbed for
Freddie Williams's cathode-ray tube (CRT) memory (Williams tubes). The
prototype led directly to the Manchester Mark 1 (1949) and, commercially,
the Ferranti Mark 1 (1951).

Architecture:
  - 32 words of CRT (Williams tube) memory, each 32 bits wide.
  - Accumulator (A): 32-bit two's complement integer.
  - Control register (CI): 5-bit program counter (instruction address).
  - Present instruction register (PI): holds the current instruction word.
  - All arithmetic is 32-bit signed two's complement.

Instruction format (32 bits):
  Bits 0-4:  Line number S (memory address, 0..31).
  Bits 5-12: Function field F (only bits 5-7 used, 3 bits -> 8 operations).
  Bits 13-31: Unused.

  F-field encoding:
    000 (0): JMP -- jump to value in store[S] (CI <- store[S])
    100 (4): JRP / JPR -- CI += store[S] (jump relative)
    010 (2): LDN -- accumulator A <- -store[S] (load negated)
    110 (6): STO -- store[S] <- A (store)
    001 (1): SUB -- A -= store[S] (subtract; no ADD instruction!)
    101 (5): SUB (duplicate; same as 001 in Baby)
    011 (3): CMP / SKN -- skip next if A < 0 (test accumulator)
    111 (7): STOP / HLT -- halt

  WHY no ADD: The Baby had only subtraction; addition was achieved via:
    A += X  =>  A = A - (-X)  (LDN to negate, then SUB the negated value)

This emulator models:
  - 32-word CRT memory.
  - 32-bit two's complement arithmetic (Python int wrapping).
  - All 8 Baby instructions.
  - Accurate stepping: CI increments THEN fetches (CI points to next instruction
    after fetch, matching the real Baby's behavior).

References:
  - Williams, F. C., & Kilburn, T. (1948). Electronic Digital Computers.
    Nature, 162(4117), 487.
  - Kilburn, T. (1998). From cathode ray tube to Ferranti 1. IEEE Annals of
    the History of Computing, 20(3), 9-15.
  - Manchester Baby website: https://www.cs.manchester.ac.uk/digital60/
  - Napper, B. (2000). Computer 50: The University of Manchester Celebrates
    the Birth of the Modern Computer. University of Manchester.
"""

from __future__ import annotations

from dataclasses import dataclass, field

_STORE_SIZE = 32          # 32 words of CRT memory
_WORD_BITS = 32           # 32-bit word
_WORD_MASK = (1 << _WORD_BITS) - 1   # 0xFFFFFFFF
_SIGN_BIT = 1 << (_WORD_BITS - 1)    # 0x80000000

# Instruction function codes (F field, bits 5-7)
_F_JMP  = 0  # 000: Jump to store[S]
_F_JRP  = 4  # 100: Jump relative by store[S]
_F_LDN  = 2  # 010: Load Negative
_F_STO  = 6  # 110: Store
_F_SUB  = 1  # 001: Subtract (also 5)
_F_CMP  = 3  # 011: Skip next if A < 0
_F_STP  = 7  # 111: Stop


def _to_signed(value: int) -> int:
    """Convert a 32-bit unsigned integer to a signed integer."""
    value &= _WORD_MASK
    if value & _SIGN_BIT:
        return value - (1 << _WORD_BITS)
    return value


def _to_unsigned(value: int) -> int:
    """Wrap a signed integer into 32-bit unsigned representation."""
    return value & _WORD_MASK


# ---------------------------------------------------------------------------
# Machine state
# ---------------------------------------------------------------------------


@dataclass
class SSEMState:
    """Observable state of the Manchester SSEM (Baby)."""

    store: list[int] = field(default_factory=lambda: [0] * _STORE_SIZE)
    accumulator: int = 0      # 32-bit signed, stored as Python int
    ci: int = 0               # 5-bit control (program counter), 0..31
    pi: int = 0               # Present instruction register
    halted: bool = False
    cycle_count: int = 0


# ---------------------------------------------------------------------------
# Emulator
# ---------------------------------------------------------------------------


class ManchesterBaby:
    """Manchester SSEM (Baby) -- world's first stored-program computer (1948).

    Usage::

        baby = ManchesterBaby()
        # Load Tom Kilburn's first program (June 1948): find highest factor
        # of 2^18 - 1 = 262143. Here we show a simpler example.

        # Store -5 at address 0: store[0] = -5 (twos complement)
        baby.store_word(0, -5)

        # Program: negate store[0] and store at 1
        # LDN 0: A = -store[0] = 5
        # STO 1: store[1] = A = 5
        # STP:   halt
        baby.store_program([
            (0, 2),   # address=0, F=2 (LDN 0)
            (1, 6),   # address=1, F=6 (STO 1)
            (0, 7),   # address=0, F=7 (STP)
        ])
        baby.run()
        print(baby.get_store(1))   # 5
    """

    def __init__(self) -> None:
        self.state = SSEMState()

    # ------------------------------------------------------------------
    # Memory
    # ------------------------------------------------------------------

    def store_word(self, address: int, value: int) -> None:
        """Store a 32-bit value (signed or unsigned) at address."""
        self._check_addr(address)
        self.state.store[address] = _to_unsigned(value)

    def get_store(self, address: int) -> int:
        """Read store word as a signed 32-bit integer."""
        self._check_addr(address)
        return _to_signed(self.state.store[address])

    def _check_addr(self, address: int) -> None:
        if not 0 <= address < _STORE_SIZE:
            raise IndexError(
                f"Store address {address} out of range [0, {_STORE_SIZE-1}]"
            )

    # ------------------------------------------------------------------
    # Program loading
    # ------------------------------------------------------------------

    def store_program(self, instructions: list[tuple[int, int]]) -> None:
        """Load a list of (S, F) instruction pairs into store starting at address 1.

        The Baby's first stored program was loaded starting at address 1
        (the CI points to the next instruction BEFORE execution, so starts at 0
        then increments to 1 before first fetch).

        Each instruction is encoded as: bits 0-4 = S, bits 5-7 = F.

        Args:
            instructions: List of (S, F) tuples, one per instruction.
        """
        for i, (s, f) in enumerate(instructions):
            word = (s & 0x1F) | ((f & 0x7) << 5)
            self.state.store[i + 1] = word  # program starts at address 1

        # Reset execution state
        self.state.ci = 0
        self.state.accumulator = 0
        self.state.halted = False
        self.state.cycle_count = 0

    def load_raw_word(self, address: int, word: int) -> None:
        """Directly write a raw 32-bit word (instruction or data) at address."""
        self._check_addr(address)
        self.state.store[address] = word & _WORD_MASK

    # ------------------------------------------------------------------
    # Instruction decode
    # ------------------------------------------------------------------

    @staticmethod
    def decode(word: int) -> tuple[int, int]:
        """Decode an instruction word to (S, F).

        S = bits 0-4 (5-bit address).
        F = bits 5-7 (3-bit function code).
        """
        s = word & 0x1F
        f = (word >> 5) & 0x7
        return s, f

    # ------------------------------------------------------------------
    # Execution
    # ------------------------------------------------------------------

    def step(self) -> bool:
        """Execute one Baby instruction cycle.

        Baby fetch-execute cycle:
          1. CI incremented (CI <- CI + 1).
          2. PI <- store[CI]  (fetch instruction at new CI).
          3. Execute PI.

        Returns True if execution should continue, False if halted.
        """
        if self.state.halted:
            return False

        # Step 1: Increment CI (wraps at 32)
        self.state.ci = (self.state.ci + 1) % _STORE_SIZE

        # Step 2: Fetch
        self.state.pi = self.state.store[self.state.ci]

        # Step 3: Execute
        s, f = self.decode(self.state.pi)
        self.state.cycle_count += 1

        if f == _F_JMP:
            # CI <- store[S]
            self.state.ci = self.state.store[s] & 0x1F

        elif f == _F_JRP:
            # CI <- CI + store[S]
            self.state.ci = (self.state.ci + _to_signed(self.state.store[s])) % _STORE_SIZE

        elif f == _F_LDN:
            # A <- -store[S]
            self.state.accumulator = -_to_signed(self.state.store[s])

        elif f == _F_STO:
            # store[S] <- A
            self.state.store[s] = _to_unsigned(self.state.accumulator)

        elif f in (_F_SUB, 5):
            # A <- A - store[S]
            self.state.accumulator = _to_signed(
                _to_unsigned(self.state.accumulator - _to_signed(self.state.store[s]))
            )

        elif f == _F_CMP:
            # If A < 0: skip next instruction (CI += 1)
            if self.state.accumulator < 0:
                self.state.ci = (self.state.ci + 1) % _STORE_SIZE

        elif f == _F_STP:
            self.state.halted = True
            return False

        return True

    def run(self, max_cycles: int = 100_000) -> int:
        """Run until HALT or max_cycles exceeded. Returns cycle count."""
        for _ in range(max_cycles):
            if not self.step():
                break
        return self.state.cycle_count

    def reset(self) -> None:
        """Clear all state (but preserve store contents)."""
        self.state.accumulator = 0
        self.state.ci = 0
        self.state.pi = 0
        self.state.halted = False
        self.state.cycle_count = 0

    def full_reset(self) -> None:
        """Reset state and clear all store memory."""
        self.state = SSEMState()

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def state_snapshot(self) -> dict[str, object]:
        return {
            "accumulator": self.state.accumulator,
            "ci": self.state.ci,
            "halted": self.state.halted,
            "cycle_count": self.state.cycle_count,
            "store": list(self.state.store),
        }
