"""
Leibniz Reckoner Emulator (Tier 1 Fidelity)

Simulates the mechanical behavior of the Leibniz stepped reckoner, focusing on
the Staffelwalze (stepped drum) and its interaction with the counting wheels.

Architecture:
  - Stepped Drums (Staffelwalze): One per input digit, with 0-9 teeth lengths.
  - Input Levers: Set the number to be added.
  - Carriage: Moves the input unit relative to the accumulator for shifting.
  - Accumulator: Series of counting wheels.
  - Result Register: Displays the accumulated value.
  - Revolution Counter: Counts additions/subtractions.
"""


class SteppedDrum:
    """Represents a single Staffelwalze (stepped drum)."""
    def __init__(self, value: int):
        if not 0 <= value <= 9:
            raise ValueError("Stepped drum value must be 0-9")
        self.value = value # Number of active teeth (0-9)

    def get_teeth_count(self) -> int:
        return self.value

    def __repr__(self) -> str:
        return f"SteppedDrum({self.value})"

class CountingWheel:
    """A single digit counting wheel in the accumulator."""
    def __init__(self, position: int):
        self.position = position
        self.value = 0 # 0-9

    def advance(self, steps: int) -> int:
        """Advance wheel by steps, returns carry."""
        old_value = self.value
        self.value = (self.value + steps) % 10
        return 1 if (old_value + steps) >= 10 else 0

    def __repr__(self) -> str:
        return f"CountingWheel({self.position}, val={self.value})"

class LeibnizReckonerEmulator:
    def __init__(self, num_input_digits: int = 8, num_accumulator_digits: int = 16):
        self.num_input_digits = num_input_digits
        self.num_accumulator_digits = num_accumulator_digits

        self.input_drums: list[SteppedDrum] = [SteppedDrum(0) for _ in range(num_input_digits)]
        self.accumulator_wheels: list[CountingWheel] = [CountingWheel(i) for i in range(num_accumulator_digits)]
        self.carriage_position = 0 # Offset for multiplication/division
        self.turn_counter = 0 # Revolution counter for multiplication steps

    def reset(self) -> None:
        for drum in self.input_drums:
            drum.value = 0
        for wheel in self.accumulator_wheels:
            wheel.value = 0
        self.carriage_position = 0
        self.turn_counter = 0

    def state(self) -> dict:
        return {
            "accumulator": self.get_accumulator_value(),
            "carriage_position": self.carriage_position,
            "input_drums": [d.value for d in self.input_drums],
            "turn_counter": self.turn_counter
        }

    def set_input(self, value: int) -> None:
        """Sets the input drums from an integer."""
        s_val = str(value).zfill(self.num_input_digits) # Pad to num_input_digits, e.g., "00000123"
        if len(s_val) > self.num_input_digits:
            raise OverflowError(f"Input {value} exceeds {self.num_input_digits} digits")
        for i, char in enumerate(reversed(s_val)): # Store units at input_drums[0]
            self.input_drums[i].value = int(char)

    def shift_carriage(self, offset: int) -> None:
        """Shifts the carriage (relative position of input drums to accumulator)."""
        new_pos = self.carriage_position + offset
        if 0 <= new_pos < self.num_accumulator_digits - self.num_input_digits + 1:
            self.carriage_position = new_pos
        else:
            raise ValueError(f"Carriage position {new_pos} out of bounds")

    def crank_turn(self, num_turns: int = 1) -> None:
        """
        Simulates one turn of the main crank.
        This performs an addition (or subtraction).
        """
        if num_turns < 1:
            raise ValueError("Crank must be turned at least once")

        for _ in range(num_turns):
            self.turn_counter += 1
            temp_accumulator_update = [0] * self.num_accumulator_digits

            # 1. Stepped Drums engage and add to temporary accumulator update
            for i_drum, drum in enumerate(self.input_drums):
                effective_wheel_pos = i_drum + self.carriage_position
                if effective_wheel_pos < self.num_accumulator_digits:
                    temp_accumulator_update[effective_wheel_pos] += drum.get_teeth_count()

            # 2. Add temporary accumulator update to main accumulator with carries
            carry = 0
            for i in range(self.num_accumulator_digits):
                total = self.accumulator_wheels[i].value + temp_accumulator_update[i] + carry
                self.accumulator_wheels[i].value = total % 10
                carry = total // 10

            # This is where the 'delayed carry' would be modeled.
            # Leibniz had issues with simultaneous carries; they had to be propagated.
            # For Tier 1 fidelity, we model it as an immediate ripple for now.

    def get_accumulator_value(self) -> int:
        """Reads the value from the accumulator wheels."""
        val = 0
        for i, wheel in enumerate(self.accumulator_wheels):
            val += wheel.value * (10**i)
        return val

    def multiply(self, multiplicand: int, multiplier: int) -> int:
        """
        Performs multiplication using repeated addition and carriage shifts.
        """
        self.reset()
        self.set_input(multiplicand)

        s_multiplier = str(multiplier)[::-1] # Process multiplier digits from right to left

        for i, char_digit in enumerate(s_multiplier):
            digit = int(char_digit)
            if digit > 0:
                self.shift_carriage(i - self.carriage_position)
                self.crank_turn(digit)

        return self.get_accumulator_value()

    def add_value(self, value: int) -> int:
        self.set_input(value)
        self.crank_turn(1)
        return self.get_accumulator_value()
