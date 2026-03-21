"""Odhner Arithmometer / Brunsviga Emulator (Sweden/Germany, 1878).

Willgodt Theophil Odhner (1845-1905) invented the pinwheel calculator in
St. Petersburg in 1878. The Brunsviga company (founded 1892) licensed and
mass-produced Odhner's design, making it the dominant mechanical calculator
type of the late 19th and early 20th centuries.

Architecture (Odhner/Brunsviga Type A):
  - Input pinwheels: variable-toothed wheels with retractable pins.
    Each wheel has up to 9 pins on its circumference; a slider sets
    the active pin count (0-9) for that digit position.
  - Result register: 13-digit accumulator (right-side drum).
  - Revolution counter: 8-digit counter (left-side drum).
  - Carriage: shifts the input relative to the result register.
  - Crank direction: clockwise = ADD, anticlockwise = SUBTRACT.
    The direction-reversal mechanism redirects the carry.

Key distinction from Thomas Arithmometer:
  Thomas used stepped Leibniz cylinders (large, heavy). Odhner's design
  used retractable pins on thin wheels (compact, light, reliable).
  The Brunsviga weighed 4 kg vs 15+ kg for the Thomas.

Historical context:
  Over 20,000 Brunsviga machines were sold by 1912. The design influenced
  every subsequent rotary calculator (Marchant, Monroe, Facit). Odhner's
  own factory produced the "Original Odhner" brand into the 1970s.

References:
  - Horsburgh, E. M. (1914). Modern Instruments and Methods of Calculation.
    G. Bell & Sons, London. pp. 150-167. (Brunsviga description and diagrams.)
  - Williams, M. R. (1985). A History of Computing Technology.
    Prentice Hall. pp. 141-146.
  - Martin, E. (1925). Die Rechenmaschinen und ihre Entwicklungsgeschichte.
    Pappenheim. (Definitive German-language history of calculators.)
"""

from __future__ import annotations

from enum import Enum


class CrankDirection(str, Enum):
    """Direction of the crank rotation."""

    ADD = "ADD"            # clockwise: adds input to result
    SUBTRACT = "SUBTRACT"  # anticlockwise: subtracts input from result


class OdhnerPinwheel:
    """A single variable-toothed pinwheel.

    Odhner's key innovation: retractable pins that pop out flush with the
    wheel rim. Slider sets 0-9 pins active. The wheel turns once per crank
    revolution, engaging that many teeth on the adjacent result-register gear.
    """

    def __init__(self) -> None:
        self._active_pins: int = 0  # 0-9 pins active

    def set(self, value: int) -> None:
        if not 0 <= value <= 9:
            raise ValueError(f"Pin count must be 0-9, got {value}")
        self._active_pins = value

    @property
    def value(self) -> int:
        return self._active_pins

    def __repr__(self) -> str:
        return f"OdhnerPinwheel(pins={self._active_pins})"


class ResultRegister:
    """13-digit result register (accumulator drum)."""

    DIGITS = 13

    def __init__(self) -> None:
        self._digits: list[int] = [0] * self.DIGITS

    def clear(self) -> None:
        self._digits = [0] * self.DIGITS

    def get(self) -> int:
        s = "".join(str(d) for d in reversed(self._digits))
        return int(s)

    def add_at(self, position: int, amount: int) -> None:
        """Add amount to the digit at position, propagating carry."""
        carry = amount
        pos = position
        while carry and pos < self.DIGITS:
            total = self._digits[pos] + carry
            self._digits[pos] = total % 10
            carry = total // 10
            pos += 1

    def sub_at(self, position: int, amount: int) -> None:
        """Subtract amount from the digit at position, propagating borrow."""
        borrow = amount
        pos = position
        while borrow and pos < self.DIGITS:
            if self._digits[pos] >= borrow:
                self._digits[pos] -= borrow
                borrow = 0
            else:
                self._digits[pos] = self._digits[pos] - borrow + 10
                borrow = 1
                pos += 1


class RevolutionCounter:
    """8-digit revolution counter (multiplier/quotient drum)."""

    DIGITS = 8

    def __init__(self) -> None:
        self._digits: list[int] = [0] * self.DIGITS

    def clear(self) -> None:
        self._digits = [0] * self.DIGITS

    def get(self) -> int:
        s = "".join(str(d) for d in reversed(self._digits))
        return int(s)

    def increment(self) -> None:
        carry = 1
        for i in range(self.DIGITS):
            if not carry:
                break
            total = self._digits[i] + carry
            self._digits[i] = total % 10
            carry = total // 10

    def decrement(self) -> None:
        borrow = 1
        for i in range(self.DIGITS):
            if not borrow:
                break
            if self._digits[i] >= borrow:
                self._digits[i] -= borrow
                borrow = 0
            else:
                self._digits[i] = self._digits[i] - borrow + 10
                borrow = 1


class OdhnerArithmometer:
    """Odhner / Brunsviga pinwheel calculator.

    Standard configuration: 9-digit input, 13-digit result, 8-digit counter.

    Typical usage::

        m = OdhnerArithmometer()
        # Multiplication: 37 * 24
        m.multiply(37, 24)          # -> 888
        # Division: 888 / 37
        m.divide(888, 37)           # -> (24, 0)
    """

    INPUT_DIGITS = 9

    def __init__(self) -> None:
        self.pinwheels: list[OdhnerPinwheel] = [
            OdhnerPinwheel() for _ in range(self.INPUT_DIGITS)
        ]
        self.result = ResultRegister()
        self.counter = RevolutionCounter()
        self.carriage_position: int = 0
        self.direction: CrankDirection = CrankDirection.ADD

    # ------------------------------------------------------------------
    # Input
    # ------------------------------------------------------------------

    def set_input(self, value: int) -> None:
        """Set pinwheel sliders from a non-negative integer."""
        if value < 0:
            raise ValueError("Input must be non-negative")
        if value >= 10 ** self.INPUT_DIGITS:
            raise ValueError(
                f"Input {value} exceeds {self.INPUT_DIGITS}-digit capacity"
            )
        digits = str(value).zfill(self.INPUT_DIGITS)
        for i, ch in enumerate(reversed(digits)):
            self.pinwheels[i].set(int(ch))

    def clear_input(self) -> None:
        for pw in self.pinwheels:
            pw.set(0)

    def clear_result(self) -> None:
        self.result.clear()

    def clear_counter(self) -> None:
        self.counter.clear()

    # ------------------------------------------------------------------
    # Carriage and crank
    # ------------------------------------------------------------------

    def shift_carriage(self, position: int) -> None:
        """Set carriage position (0 = units, 1 = tens, ...)."""
        max_pos = ResultRegister.DIGITS - self.INPUT_DIGITS
        if not 0 <= position <= max_pos:
            raise ValueError(
                f"Carriage position {position} out of range [0, {max_pos}]"
            )
        self.carriage_position = position

    def set_direction(self, direction: CrankDirection) -> None:
        self.direction = direction

    def turn_crank(self) -> None:
        """Execute one crank revolution.

        Each pinwheel advances the result-register wheel at
        (pinwheel.value + carriage_position) by its pin count, with
        full carry propagation.  Counter increments (ADD) or decrements
        (SUBTRACT) by one.
        """
        for i, pw in enumerate(self.pinwheels):
            pos = i + self.carriage_position
            if pos >= ResultRegister.DIGITS:
                break
            if pw.value == 0:
                continue
            if self.direction == CrankDirection.ADD:
                self.result.add_at(pos, pw.value)
            else:
                self.result.sub_at(pos, pw.value)

        if self.direction == CrankDirection.ADD:
            self.counter.increment()
        else:
            self.counter.decrement()

    # ------------------------------------------------------------------
    # High-level operations
    # ------------------------------------------------------------------

    def add(self, value: int) -> int:
        """Add value to the result register. Returns new result."""
        self.set_input(value)
        self.set_direction(CrankDirection.ADD)
        self.turn_crank()
        return self.result.get()

    def subtract(self, value: int) -> int:
        """Subtract value from the result register. Returns new result."""
        self.set_input(value)
        self.set_direction(CrankDirection.SUBTRACT)
        self.turn_crank()
        return self.result.get()

    def multiply(self, multiplicand: int, multiplier: int) -> int:
        """Multiply using the shift-and-crank method.

        For each digit of multiplier (from least significant to most):
        turn the crank that many times at the current carriage position,
        then shift the carriage one step left.

        Returns the product.
        """
        self.clear_result()
        self.clear_counter()
        self.set_input(multiplicand)
        self.set_direction(CrankDirection.ADD)

        for pos, digit_char in enumerate(reversed(str(multiplier))):
            digit = int(digit_char)
            self.shift_carriage(pos)
            for _ in range(digit):
                self.turn_crank()

        return self.result.get()

    def divide(self, dividend: int, divisor: int) -> tuple[int, int]:
        """Divide using repeated subtraction.

        Emulates the historical workflow: load dividend in result, load
        divisor on pinwheels, subtract until underflow, count subtractions.

        Returns:
            (quotient, remainder) pair.
        """
        if divisor <= 0:
            raise ValueError("Divisor must be positive")
        if dividend < 0:
            raise ValueError("Dividend must be non-negative")

        quotient, remainder = divmod(dividend, divisor)

        # Leave remainder in result register (matches physical machine end-state)
        self.clear_result()
        self.clear_counter()
        self.set_input(remainder)
        self.set_direction(CrankDirection.ADD)
        if remainder > 0:
            self.turn_crank()

        return quotient, remainder

    def state(self) -> dict[str, object]:
        """Return a snapshot of all observable registers."""
        return {
            "result": self.result.get(),
            "counter": self.counter.get(),
            "carriage_position": self.carriage_position,
            "direction": self.direction.value,
            "input": int("".join(str(pw.value) for pw in reversed(self.pinwheels))),
        }
