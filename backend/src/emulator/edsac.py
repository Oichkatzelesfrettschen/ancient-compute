"""EDSAC Emulator (Cambridge University Mathematical Laboratory, 1949).

EDSAC (Electronic Delay Storage Automatic Calculator) was designed by
Maurice Wilkes (1913-2010) at the University of Cambridge Mathematical
Laboratory, inspired by von Neumann's EDVAC report (1945). It first
ran on 6 May 1949, making it the first practical stored-program computer
to be put into regular service.

Maurice Wilkes also invented microprogramming (1951) while working on
EDSAC's successor, EDSAC 2, and was awarded the Turing Award in 1967.

Architecture:
  - 512 words of mercury delay-line memory, each 17 bits wide.
    (Mercury acoustic delay lines: a pulse train circulates in a tube of
    liquid mercury, re-read and re-written at each cycle.)
  - 35-bit accumulator (A) -- two consecutive 17-bit words form a 35-bit
    operand for multiply; most operations use the low 17 bits.
  - 17-bit multiplier register (R).
  - All arithmetic is in 17-bit two's complement (or 35-bit for multiply).
  - Clock: ~500 kHz, ~1,500 operations per second.
  - Output: teleprinter.

Instruction format (17 bits):
  Bits 0-4:   Function code F (5 bits -> 32 possible ops; ~17 used).
  Bit  5:     Short/long modifier (0 = short 17-bit, 1 = long 35-bit).
  Bits 6-15:  Address field N (10 bits, 0..1023; only 0..511 used).
  Bit  16:    Unused (0).

  EDSAC used letter codes for the function field (as teleprinter characters):
    A(0)  -- add to accumulator: A += store[N]
    S(2)  -- subtract from accumulator: A -= store[N]
    H(6)  -- copy to multiplier register: R = store[N]
    V(5)  -- multiply and add: A += store[N] * R
    T(3)  -- transfer: store[N] = A, A = 0  (also called CLA+STO)
    U(4)  -- copy to store without clearing: store[N] = A
    C(1)  -- collate (bitwise AND): A &= store[N]
    R(7)  -- right shift: A >>= 1
    L(8)  -- left shift: A <<= 1
    E(9)  -- conditional branch if A >= 0: CI = N
    G(10) -- conditional branch if A < 0: CI = N
    I(11) -- input (from tape reader): store[N] = next input character
    O(12) -- output (to teleprinter): print store[N] as teleprinter character
    F(13) -- read last value from store[N] into accumulator: A = store[N]
    X(0)  -- no-op (placeholder / dummy)
    P(14) -- add store[N] to current address (indexed addressing)
    Z(15) -- stop

This emulator models:
  - 512-word memory.
  - 17-bit two's complement arithmetic.
  - All major EDSAC instructions.
  - Input tape and output teleprinter.
  - Wilkes' initial orders (bootstrap loader) is not separately modelled
    but the machine is pre-loaded in a ready-to-run state.

References:
  - Wilkes, M. V. (1951). The EDSAC -- An Electronic Calculating Machine.
    Journal of Scientific Instruments, 26(12), 385-391.
  - Wilkes, M. V., Wheeler, D. J., & Gill, S. (1951). The Preparation of
    Programs for an Electronic Digital Computer. Addison-Wesley. (The first
    programming textbook.)
  - Wilkes, M. V. (1985). Memoirs of a Computer Pioneer. MIT Press.
  - EDSAC Replica Project, National Museum of Computing, Bletchley Park:
    https://www.tnmoc.org/edsac
"""

from __future__ import annotations

from dataclasses import dataclass, field

_STORE_SIZE = 512  # 512 17-bit words
_WORD_BITS = 17  # 17-bit word
_WORD_MASK = (1 << _WORD_BITS) - 1  # 0x1FFFF
_SIGN_BIT = 1 << (_WORD_BITS - 1)  # 0x10000

# Function codes (letter mnemonic -> integer)
_FC = {
    "X": 0,  # no-op
    "A": 0,  # add  (overlaps with X; distinguished by context)
    "S": 2,  # subtract
    "H": 6,  # set multiplier register
    "V": 5,  # multiply and accumulate
    "T": 3,  # transfer (store and clear)
    "U": 4,  # store without clearing
    "C": 1,  # collate (AND)
    "R": 7,  # shift right
    "L": 8,  # shift left
    "E": 9,  # branch if >= 0
    "G": 10,  # branch if < 0
    "I": 11,  # input from tape
    "O": 12,  # output to printer
    "F": 13,  # load from store
    "P": 14,  # add to address (indexed)
    "Z": 15,  # stop
}
_FC_NAMES = {v: k for k, v in _FC.items()}

# The canonical function codes for each instruction
_FC_ADD = 0  # A
_FC_SUB = 2  # S
_FC_H = 6  # H (set R)
_FC_V = 5  # V (multiply + accumulate)
_FC_T = 3  # T (transfer = store + clear acc)
_FC_U = 4  # U (store, keep acc)
_FC_C = 1  # C (collate/AND)
_FC_R = 7  # R (right shift)
_FC_L = 8  # L (left shift)
_FC_E = 9  # E (branch >= 0)
_FC_G = 10  # G (branch < 0)
_FC_I = 11  # I (input)
_FC_O = 12  # O (output)
_FC_F = 13  # F (load)
_FC_P = 14  # P (indexed)
_FC_Z = 15  # Z (stop)


def _to_signed(value: int) -> int:
    value &= _WORD_MASK
    if value & _SIGN_BIT:
        return value - (1 << _WORD_BITS)
    return value


def _to_unsigned(value: int) -> int:
    return value & _WORD_MASK


# ---------------------------------------------------------------------------
# Instruction
# ---------------------------------------------------------------------------


@dataclass
class EDSACInstruction:
    """One EDSAC instruction.

    function: Letter mnemonic (A, S, H, V, T, U, C, R, L, E, G, I, O, F, Z).
    address:  N field (0..511).
    long:     Long (35-bit) modifier (default False = short/17-bit).
    """

    function: str
    address: int = 0
    long: bool = False

    def __post_init__(self) -> None:
        f = self.function.upper()
        if f not in _FC and f != "X":
            raise ValueError(f"Unknown EDSAC function: {self.function!r}")
        self.function = f
        if not 0 <= self.address < _STORE_SIZE:
            raise ValueError(f"Address {self.address} out of range [0, {_STORE_SIZE-1}]")

    def encode(self) -> int:
        """Encode this instruction to a 17-bit word."""
        fc = _FC.get(self.function, 0)
        return fc | (int(self.long) << 5) | (self.address << 6)


# ---------------------------------------------------------------------------
# Machine state
# ---------------------------------------------------------------------------


@dataclass
class EDSACState:
    """Observable state of the EDSAC."""

    store: list[int] = field(default_factory=lambda: [0] * _STORE_SIZE)
    accumulator: int = 0  # 17-bit signed (or 35-bit for long ops)
    multiplier_register: int = 0  # 17-bit
    ci: int = 0  # 10-bit control (program counter)
    halted: bool = False
    cycle_count: int = 0
    output_tape: list[str] = field(default_factory=list)
    input_tape: list[int] = field(default_factory=list)
    input_pointer: int = 0


# ---------------------------------------------------------------------------
# Emulator
# ---------------------------------------------------------------------------


class EDSAC:
    """EDSAC -- first practical stored-program computer in regular service (1949).

    Usage::

        edsac = EDSAC()
        # Load a simple program: compute 3 + 4
        edsac.store_instructions(0, [
            EDSACInstruction("T", 31),   # store 0 -> store[31] (clear acc)
            EDSACInstruction("A", 32),   # acc += store[32]
            EDSACInstruction("A", 33),   # acc += store[33]
            EDSACInstruction("T", 34),   # store acc -> store[34], acc = 0
            EDSACInstruction("Z", 0),    # halt
        ])
        edsac.store_value(32, 3)
        edsac.store_value(33, 4)
        edsac.run()
        print(edsac.get_value(34))   # 7
    """

    def __init__(self) -> None:
        self.state = EDSACState()

    # ------------------------------------------------------------------
    # Memory
    # ------------------------------------------------------------------

    def store_value(self, address: int, value: int) -> None:
        """Write a 17-bit value (signed or unsigned) to store."""
        self._check_addr(address)
        self.state.store[address] = _to_unsigned(value)

    def get_value(self, address: int) -> int:
        """Read store word as signed 17-bit integer."""
        self._check_addr(address)
        return _to_signed(self.state.store[address])

    def _check_addr(self, address: int) -> None:
        if not 0 <= address < _STORE_SIZE:
            raise IndexError(f"Store address {address} out of range [0, {_STORE_SIZE-1}]")

    def store_instructions(self, start: int, instructions: list[EDSACInstruction]) -> None:
        """Encode and store a list of EDSAC instructions starting at address ``start``."""
        for i, instr in enumerate(instructions):
            addr = start + i
            self._check_addr(addr)
            self.state.store[addr] = instr.encode()
        # Initialize CI to point one before first instruction (step() increments before fetch)
        self.state.ci = start - 1 if start > 0 else _STORE_SIZE - 1
        self.state.halted = False
        self.state.cycle_count = 0
        self.state.accumulator = 0
        self.state.multiplier_register = 0

    def load_input_tape(self, chars: list[int]) -> None:
        """Load values onto the input tape for I instructions."""
        self.state.input_tape = list(chars)
        self.state.input_pointer = 0

    def reset(self) -> None:
        self.state = EDSACState()

    # ------------------------------------------------------------------
    # Execution
    # ------------------------------------------------------------------

    def step(self) -> bool:
        """Execute one EDSAC instruction.

        EDSAC fetch-execute:
          1. CI increments.
          2. Fetch instruction from store[CI].
          3. Execute.

        Returns True if should continue, False if halted.
        """
        if self.state.halted:
            return False

        # 1. Increment CI
        self.state.ci = (self.state.ci + 1) % _STORE_SIZE

        # 2. Fetch
        word = self.state.store[self.state.ci]
        fc = word & 0x1F
        long_mode = bool((word >> 5) & 1)
        n = (word >> 6) & 0x1FF  # 9-bit address in early EDSAC; use 10 bits

        self.state.cycle_count += 1
        self._execute(fc, n, long_mode)
        return not self.state.halted

    def run(self, max_cycles: int = 100_000) -> int:
        """Run until halt or max_cycles. Returns cycle count."""
        for _ in range(max_cycles):
            if not self.step():
                break
        return self.state.cycle_count

    def _execute(self, fc: int, n: int, long_mode: bool) -> None:
        st = self.state
        acc = st.accumulator
        store = st.store

        if fc == _FC_ADD:
            st.accumulator = _to_signed(_to_unsigned(acc + _to_signed(store[n])))

        elif fc == _FC_SUB:
            st.accumulator = _to_signed(_to_unsigned(acc - _to_signed(store[n])))

        elif fc == _FC_H:
            # Set multiplier register
            st.multiplier_register = _to_signed(store[n])

        elif fc == _FC_V:
            # Multiply and accumulate: A += store[N] * R
            product = _to_signed(store[n]) * st.multiplier_register
            st.accumulator = _to_signed(_to_unsigned(acc + product))

        elif fc == _FC_T:
            # Transfer: store[N] = A, A = 0
            store[n] = _to_unsigned(acc)
            st.accumulator = 0

        elif fc == _FC_U:
            # Store without clearing accumulator
            store[n] = _to_unsigned(acc)

        elif fc == _FC_C:
            # Collate (AND)
            st.accumulator = _to_signed(_to_unsigned(acc) & _to_unsigned(store[n]))

        elif fc == _FC_R:
            # Right shift (arithmetic: preserves sign bit)
            st.accumulator = acc >> 1

        elif fc == _FC_L:
            # Left shift
            st.accumulator = _to_signed(_to_unsigned(acc << 1))

        elif fc == _FC_E:
            # Branch if accumulator >= 0
            if acc >= 0:
                st.ci = n - 1  # step() will increment before fetch

        elif fc == _FC_G:
            # Branch if accumulator < 0
            if acc < 0:
                st.ci = n - 1

        elif fc == _FC_I:
            # Input from tape
            if st.input_pointer >= len(st.input_tape):
                raise RuntimeError("EDSAC input tape exhausted")
            store[n] = _to_unsigned(st.input_tape[st.input_pointer])
            st.input_pointer += 1

        elif fc == _FC_O:
            # Output to teleprinter (as ASCII character)
            char_val = _to_signed(store[n]) & 0x1F
            st.output_tape.append(chr(64 + char_val) if 1 <= char_val <= 26 else str(char_val))

        elif fc == _FC_F:
            # Load from store into accumulator
            st.accumulator = _to_signed(store[n])

        elif fc == _FC_Z:
            # Stop
            st.halted = True

        # else: unknown function code -> treat as no-op

    # ------------------------------------------------------------------
    # Introspection
    # ------------------------------------------------------------------

    def state_snapshot(self) -> dict[str, object]:
        return {
            "accumulator": self.state.accumulator,
            "multiplier_register": self.state.multiplier_register,
            "ci": self.state.ci,
            "halted": self.state.halted,
            "cycle_count": self.state.cycle_count,
            "output_tape": list(self.state.output_tape),
        }
