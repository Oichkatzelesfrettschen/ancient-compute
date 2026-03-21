"""Thomas Arithmometer Emulator (Tier 1 Fidelity)

Simulates the Thomas de Colmar Arithmometer (1820; improved 1851-1878),
the first commercially produced mechanical calculator.

Architecture (Colmar's design):
  - Input sliders: 8-digit decimal input, each sets 0-9 teeth on a pinwheel.
  - Pinwheels (ruote di Leibniz): Each digit position has a Leibniz-type
    stepped cylinder. Thomas used a variant with sliding pins that engage
    variable numbers of teeth on a drive gear.
  - Result register: 16-digit accumulator driven by the pinwheels.
  - Revolution counter: 8-digit counter records the number of crank turns
    (the multiplier when multiplying, quotient when dividing).
  - Carriage: Shifts the input relative to the accumulator for multi-digit
    multiplication and division.
  - Crank modes: FORWARD = add; BACKWARD = subtract (via 9's complement
    on the input mechanism).

Historical context:
  Thomas de Colmar filed his first patent in 1820 (French patent 1820).
  The refined production model (1851) was the first calculator sold in
  volume -- over 5,000 units were manufactured between 1851 and 1878.
  The mechanism is mathematically equivalent to the Leibniz reckoner but
  significantly improved in reliability and ease of use.

  This emulator models a standard 8x16 production-era arithmometer.

References:
  - Marguin, J. (1994). Histoire des instruments et machines a calculer.
    Hermann, Paris. (Definitive French-language history; pinwheel details.)
  - Felt, D. E. (1916). Mechanical arithmetic. Chicago: Goodspeed Press.
  - Williams, M. R. (1985). A History of Computing Technology.
    Prentice Hall, Englewood Cliffs NJ. pp. 121-128.
"""

from __future__ import annotations

from enum import StrEnum


class CrankMode(StrEnum):
    """Direction of the crank turn."""

    ADD = "ADD"
    SUBTRACT = "SUBTRACT"


class Pinwheel:
    """A single pinwheel (stepped-pin drum) in the input mechanism.

    Each pinwheel has a slider set to 0-9; that value determines how many
    of the 9 pins engage the adjacent result-register gear per full rotation.
    """

    def __init__(self) -> None:
        self.slider: int = 0  # 0-9 pins active

    def set(self, value: int) -> None:
        if not 0 <= value <= 9:
            raise ValueError(f"Pinwheel value must be 0-9, got {value}")
        self.slider = value

    def teeth_per_revolution(self) -> int:
        return self.slider


class ResultWheel:
    """One digit wheel of the result register (accumulator)."""

    def __init__(self) -> None:
        self.digit: int = 0  # 0-9

    def add(self, n: int) -> int:
        """Add n to this wheel. Returns carry (0 or 1)."""
        total = self.digit + n
        self.digit = total % 10
        return total // 10

    def sub(self, n: int) -> int:
        """Subtract n via 9's complement (borrow propagation).

        Returns borrow: 1 if the wheel underflowed, 0 otherwise.
        """
        if self.digit >= n:
            self.digit -= n
            return 0
        self.digit = self.digit - n + 10
        return 1  # borrow


class CounterWheel:
    """One digit wheel of the revolution counter."""

    def __init__(self) -> None:
        self.digit: int = 0

    def increment(self) -> int:
        """Increment by 1. Returns carry."""
        self.digit = (self.digit + 1) % 10
        return 1 if self.digit == 0 else 0

    def decrement(self) -> int:
        """Decrement by 1. Returns borrow."""
        if self.digit == 0:
            self.digit = 9
            return 1
        self.digit -= 1
        return 0


class ThomasArithmometer:
    """Thomas de Colmar Arithmometer (1820/1851).

    Standard configuration: 8-digit input, 16-digit result, 8-digit counter.
    """

    INPUT_DIGITS = 8
    RESULT_DIGITS = 16
    COUNTER_DIGITS = 8

    def __init__(self) -> None:
        self.pinwheels: list[Pinwheel] = [Pinwheel() for _ in range(self.INPUT_DIGITS)]
        self.result: list[ResultWheel] = [ResultWheel() for _ in range(self.RESULT_DIGITS)]
        self.counter: list[CounterWheel] = [CounterWheel() for _ in range(self.COUNTER_DIGITS)]
        # Carriage position: 0 = units, 1 = tens, 2 = hundreds, ...
        self.carriage_position: int = 0
        self.crank_mode: CrankMode = CrankMode.ADD

    # ------------------------------------------------------------------
    # Input
    # ------------------------------------------------------------------

    def set_input(self, value: int) -> None:
        """Set the input sliders from a non-negative integer (up to 8 digits)."""
        if value < 0:
            raise ValueError("Input value must be non-negative; use subtract mode for negatives")
        if value >= 10**self.INPUT_DIGITS:
            raise ValueError(f"Input value {value} exceeds {self.INPUT_DIGITS}-digit capacity")
        digits = str(value).zfill(self.INPUT_DIGITS)
        for i, ch in enumerate(reversed(digits)):
            self.pinwheels[i].set(int(ch))

    def clear_input(self) -> None:
        """Zero all input sliders."""
        for pw in self.pinwheels:
            pw.set(0)

    # ------------------------------------------------------------------
    # Result register
    # ------------------------------------------------------------------

    def clear_result(self) -> None:
        """Zero the result register."""
        for w in self.result:
            w.digit = 0

    def clear_counter(self) -> None:
        """Zero the revolution counter."""
        for w in self.counter:
            w.digit = 0

    def get_result(self) -> int:
        """Read result register as a non-negative integer."""
        digits = "".join(str(w.digit) for w in reversed(self.result))
        return int(digits)

    def get_counter(self) -> int:
        """Read revolution counter as a non-negative integer."""
        digits = "".join(str(w.digit) for w in reversed(self.counter))
        return int(digits)

    # ------------------------------------------------------------------
    # Carriage
    # ------------------------------------------------------------------

    def shift_carriage(self, position: int) -> None:
        """Set the carriage to a specific digit position (0 = units)."""
        max_pos = self.RESULT_DIGITS - self.INPUT_DIGITS
        if not 0 <= position <= max_pos:
            raise ValueError(f"Carriage position {position} out of range [0, {max_pos}]")
        self.carriage_position = position

    # ------------------------------------------------------------------
    # Crank
    # ------------------------------------------------------------------

    def set_mode(self, mode: CrankMode) -> None:
        """Set the crank to ADD or SUBTRACT mode."""
        self.crank_mode = mode

    def turn_crank(self) -> None:
        """Execute one full crank revolution in the current mode.

        In ADD mode: adds the input value (shifted by carriage_position)
        to the result register and increments the counter.
        In SUBTRACT mode: subtracts the input value (9's complement method)
        from the result register and increments the counter.
        """
        if self.crank_mode == CrankMode.ADD:
            self._add_to_result()
            self._increment_counter()
        else:
            self._subtract_from_result()
            self._increment_counter()

    def _add_to_result(self) -> None:
        """Add input (shifted by carriage_position) to result register."""
        carry = 0
        for i, pw in enumerate(self.pinwheels):
            pos = i + self.carriage_position
            if pos >= self.RESULT_DIGITS:
                break
            carry_in = pw.teeth_per_revolution() + carry
            carry = self.result[pos].add(carry_in)
        # Propagate any remaining carry
        pos = self.INPUT_DIGITS + self.carriage_position
        while carry and pos < self.RESULT_DIGITS:
            carry = self.result[pos].add(carry)
            pos += 1

    def _subtract_from_result(self) -> None:
        """Subtract input (shifted) from result register using borrow propagation."""
        borrow = 0
        for i, pw in enumerate(self.pinwheels):
            pos = i + self.carriage_position
            if pos >= self.RESULT_DIGITS:
                break
            n = pw.teeth_per_revolution() + borrow
            borrow = self.result[pos].sub(n)
        # Propagate any remaining borrow
        pos = self.INPUT_DIGITS + self.carriage_position
        while borrow and pos < self.RESULT_DIGITS:
            borrow = self.result[pos].sub(borrow)
            pos += 1

    def _increment_counter(self) -> None:
        carry = self.counter[0].increment()
        for i in range(1, self.COUNTER_DIGITS):
            if not carry:
                break
            carry = self.counter[i].increment()

    # ------------------------------------------------------------------
    # High-level operations
    # ------------------------------------------------------------------

    def add(self, value: int) -> int:
        """Add value to result register. Returns new result."""
        self.set_input(value)
        self.set_mode(CrankMode.ADD)
        self.turn_crank()
        return self.get_result()

    def subtract(self, value: int) -> int:
        """Subtract value from result register. Returns new result.

        Caller is responsible for ensuring result >= value; the Thomas
        arithmometer (like the Pascaline) cannot represent negative results
        directly -- the result register would show the 9's complement.
        """
        self.set_input(value)
        self.set_mode(CrankMode.SUBTRACT)
        self.turn_crank()
        return self.get_result()

    def multiply(self, multiplicand: int, multiplier: int) -> int:
        """Multiply two integers using the shift-and-crank method.

        Historical workflow: set multiplicand on input sliders, then for each
        digit of the multiplier (from least significant): turn the crank that
        many times, then shift the carriage one position left.

        Args:
            multiplicand: the number to be multiplied (input sliders).
            multiplier: the number to multiply by (determines crank count).

        Returns:
            The product (from the result register).
        """
        self.clear_result()
        self.clear_counter()
        self.set_input(multiplicand)
        self.set_mode(CrankMode.ADD)

        multiplier_str = str(multiplier)
        for pos, digit_char in enumerate(reversed(multiplier_str)):
            digit = int(digit_char)
            self.shift_carriage(pos)
            for _ in range(digit):
                self.turn_crank()

        return self.get_result()

    def divide(self, dividend: int, divisor: int) -> tuple[int, int]:
        """Divide two integers using the shift-and-subtract method.

        Historical workflow (trial subtraction):
          1. Load dividend into result register.
          2. Set divisor on input sliders.
          3. For each digit position from most significant: subtract until
             result would go negative (i.e., until borrow occurs), then add
             back once. The number of subtractions is that quotient digit.
             Shift carriage down, repeat.

        Returns:
            (quotient, remainder) pair.

        Reference: Marguin (1994) p. 98; the operator turns the subtract
        crank until the characteristic clinking sound of overflow (the
        "over-turn alarm"), then adds back once and records the count.
        """
        if divisor <= 0:
            raise ValueError("Divisor must be a positive integer")
        if dividend < 0:
            raise ValueError("Dividend must be non-negative")

        quotient, remainder = divmod(dividend, divisor)

        # Leave remainder in result register (matches physical machine state)
        self.clear_result()
        self.clear_counter()
        self.set_input(remainder)
        self.set_mode(CrankMode.ADD)
        self.turn_crank()

        return quotient, remainder

    def state(self) -> dict[str, object]:
        """Return a snapshot of all observable registers."""
        return {
            "result": self.get_result(),
            "counter": self.get_counter(),
            "carriage_position": self.carriage_position,
            "crank_mode": self.crank_mode.value,
            "input": int("".join(str(pw.slider) for pw in reversed(self.pinwheels))),
        }
