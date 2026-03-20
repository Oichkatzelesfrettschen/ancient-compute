"""
Pascaline Emulator (Tier 1 Fidelity)

Simulates the mechanical behavior of the Pascaline, focusing on the Sautoir mechanism
for carry propagation and the method of nines complements for subtraction.

Architecture:
  - Digit Wheels: 0-9, with pins for carry.
  - Sautoir Mechanism: Lever/pawl that is lifted by wheel pins and "drops" to transmit carry.
  - Accumulator: Series of interconnected wheels.
  - Nines Complement: For subtraction.

References:
  - Blaise Pascal's original designs.
  - Historical analyses of mechanical calculators.
"""


class Wheel:
    """Represents a single digit wheel in the Pascaline."""

    def __init__(self, position: int):
        self.position = position  # Position in the machine (0 = units)
        self.value = 0  # Current digit displayed (0-9)
        self.pins_active = False  # True when pins are lifting sautoir
        self.sautoir_lifted = False  # True when sautoir is lifted

    def __repr__(self) -> str:
        return f"Wheel({self.position}, val={self.value})"


class PascalineEmulator:
    def __init__(self, digits: int = 8):
        self.num_digits = digits
        self.wheels: list[Wheel] = [Wheel(i) for i in range(digits)]
        self.nines_complement_mode = False  # True for subtraction

    def reset(self) -> None:
        for wheel in self.wheels:
            wheel.value = 0
            wheel.pins_active = False
            wheel.sautoir_lifted = False
        self.nines_complement_mode = False

    def state(self) -> dict[str, object]:
        return {
            "value": self.get_value(),
            "digits": [w.value for w in self.wheels],
            "sautoirs_lifted": [w.sautoir_lifted for w in self.wheels],
            "nines_complement_mode": self.nines_complement_mode,
        }

    def get_value(self) -> int:
        val = 0
        for i, wheel in enumerate(self.wheels):
            val += wheel.value * (10**i)
        return val

    def set_value(self, value: int) -> None:
        s_val = str(value).zfill(self.num_digits)
        if len(s_val) > self.num_digits:
            raise OverflowError(f"Value {value} exceeds {self.num_digits} digits")
        for i, char in enumerate(reversed(s_val)):
            self.wheels[i].value = int(char)

    def set_nines_complement_mode(self, active: bool) -> None:
        """Activates nines complement mode for subtraction."""
        self.nines_complement_mode = active

    def rotate_input_wheel(self, input_value: int) -> None:
        """
        Simulates rotating the input section of the Pascaline.
        This is a high-level abstraction of setting input.
        """
        if not 0 <= input_value < 10:
            raise ValueError("Input value must be 0-9")

        # In real Pascaline, input is set for each wheel.
        # This function directly adds to the units wheel, and carries propagate.
        self._add_to_wheel(0, input_value)

    def _add_to_wheel(self, position: int, amount: int) -> bool:
        """Adds amount to wheel at position, handling sautoir carries.

        Returns True if a carry propagated out of the most-significant wheel
        (i.e., position >= num_digits -- the physical overflow/end-around carry).
        WHY: The return value is needed by subtract() to detect the end-around
        carry that Pascaline used to finalize 9's-complement subtraction.
        """
        if position >= self.num_digits:
            return True  # Carry propagated past the MSB

        current_value = self.wheels[position].value
        new_value = current_value + amount

        self.wheels[position].value = new_value % 10

        carry = new_value // 10
        if carry > 0:
            # Sautoir action: lift and then drop, kicking the next wheel.
            self.wheels[position].sautoir_lifted = True
            carry_out = self._add_to_wheel(position + 1, carry)
            self.wheels[position].sautoir_lifted = False
            return carry_out
        return False

    def add(self, operand: int) -> int:
        """Adds operand to the current value of the accumulator."""
        s_op = str(operand)[::-1]  # Reverse to process units first
        for i, char in enumerate(s_op):
            if i < self.num_digits:
                self._add_to_wheel(i, int(char))
        return self.get_value()

    def subtract(self, operand: int) -> int:
        """Subtracts operand using the method of nines complements.

        WHY: Pascaline cannot subtract directly -- the stepped drum only moves
        in one direction. Subtraction is performed via 9's complement addition
        followed by end-around carry. If the carry propagates out of the MSB
        the result is positive and the carry-out (the '1') is added back to
        the units wheel (end-around carry). If no carry-out, the subtrahend
        exceeded the minuend and the result remains in 9's-complement form.

        Algorithm (Pascal 1642 / Babbage-era description):
          1. Compute 9's complement of operand.
          2. Add complement to accumulator via sautoir carry propagation.
          3. If carry propagates out of MSB: add 1 to units (end-around carry).
          4. If no carry-out: result is negative (stored as 9's complement).
        """
        if not self.nines_complement_mode:
            raise RuntimeError("Nines complement mode not active for subtraction")

        complement_operand = self._nines_complement(operand)

        # Add complement digit-by-digit, tracking carry-out from MSB
        s_op = str(complement_operand)[::-1]
        carry_out = False
        for i, char in enumerate(s_op):
            if i < self.num_digits:
                co = self._add_to_wheel(i, int(char))
                if co:
                    carry_out = True

        if carry_out:
            # End-around carry: the result is positive, add 1 to units wheel
            self._add_to_wheel(0, 1)

        return self.get_value()

    def _nines_complement(self, number: int) -> int:
        """Calculates the nines complement of a number for subtraction."""
        if not 0 <= number < (10**self.num_digits):
            raise ValueError("Number out of range for nines complement")

        complement_str = ""
        s_num = str(number).zfill(self.num_digits)
        for digit_char in s_num:
            complement_str += str(9 - int(digit_char))
        return int(complement_str)
