"""
Curta Calculator Emulator

The "Math Grenade" - A hand-held mechanical calculator designed by Curt Herzstark.
Simulates the Curta Type I.

Architecture:
  - Stepped Drum (Staffelwalze): Central cylinder with varying tooth lengths.
  - Transmission Gears: Transfer values from sliders to result dials.
  - Carriage: Rotates to shift decimal position (powers of 10).
  - Tens Bell: Signals carry overflow.

Operation:
  - Input: 8 setting sliders.
  - Processing: Turning the crank.
    - Down (Normal): Addition.
    - Up (Pulled): Subtraction (via reversing gear).
  - Output:
    - Result Dial (11 digits, black).
    - Revolution Counter (6 digits, white).
"""

from enum import Enum


class CrankMode(Enum):
    ADD = "ADD"         # Normal position
    SUBTRACT = "SUB"    # Pulled position

class CurtaTypeI:
    """
    Curta Type I Emulator.
    Capacity: 8 input, 6 counter, 11 result.
    """
    def __init__(self) -> None:
        self.sliders = [0] * 8          # 8 input sliders
        self.result_dial = 0            # 11 digits
        self.counter_dial = 0           # 6 digits
        self.carriage_position = 0      # 0 to 5 (6 positions)
        self.crank_mode = CrankMode.ADD

        # Physical limits
        self.MAX_RESULT = 10**11 - 1
        self.MAX_COUNTER = 10**6 - 1

    def set_slider(self, index: int, value: int) -> None:
        """Set a specific input slider (0-7)."""
        if not 0 <= index < 8:
            raise ValueError("Slider index must be 0-7")
        if not 0 <= value <= 9:
            raise ValueError("Slider value must be 0-9")
        self.sliders[index] = value

    def set_input(self, number: int) -> None:
        """Set all sliders from an integer."""
        s = str(number).zfill(8)
        if len(s) > 8:
            raise OverflowError("Input exceeds 8 digits")
        for i, char in enumerate(reversed(s)):
            self.sliders[i] = int(char)

    def get_input_value(self) -> int:
        """Get current value represented by sliders."""
        val = 0
        for i, digit in enumerate(self.sliders):
            val += digit * (10 ** i)
        return val

    def shift_carriage(self, steps: int) -> None:
        """Rotate carriage. Positive = increase multiplier."""
        new_pos = self.carriage_position + steps
        if 0 <= new_pos <= 5: # Type I has 6 positions typically
            self.carriage_position = new_pos
        else:
            # Mechanical stop
            pass

    def set_crank_mode(self, mode: CrankMode) -> None:
        """Pull crank up (SUB) or push down (ADD)."""
        self.crank_mode = mode

    def turn_crank(self) -> None:
        """
        Execute one turn of the crank.
        Adds/Subtracts input * 10^carriage to result.
        Adds/Subtracts 1 * 10^carriage to counter.
        """
        multiplier = 10 ** self.carriage_position
        input_val = self.get_input_value() * multiplier
        count_val = 1 * multiplier

        if self.crank_mode == CrankMode.ADD:
            self._add_result(input_val)
            self._add_counter(count_val)
        else:
            self._sub_result(input_val)
            # Counter behavior depends on switch on the back.
            # Standard: Subtracting mode adds to counter (counting subtractions),
            # or reverses counter. Let's model "Reversing" logic:
            # If in SUB mode, Curta usually subtracts from result.
            # The counter usually counts the *operations*.
            # However, sophisticated division requires counter to track 'quotient'.
            # Curta Type I has a reversing lever for the counter.
            # We will assume standard mode: Crank SUB -> Counter SUB (for correcting overshoots).
            self._sub_counter(count_val)

    def _add_result(self, value: int) -> None:
        self.result_dial += value
        # Handle wrap-around (tens bell)
        if self.result_dial > self.MAX_RESULT:
            self.result_dial %= (self.MAX_RESULT + 1)

    def _sub_result(self, value: int) -> None:
        self.result_dial -= value
        if self.result_dial < 0:
            # Ten's complement wrap around
            self.result_dial += (self.MAX_RESULT + 1)

    def _add_counter(self, value: int) -> None:
        self.counter_dial += value
        if self.counter_dial > self.MAX_COUNTER:
            self.counter_dial %= (self.MAX_COUNTER + 1)

    def _sub_counter(self, value: int) -> None:
        self.counter_dial -= value
        if self.counter_dial < 0:
            self.counter_dial += (self.MAX_COUNTER + 1)

    def clear_result(self) -> None:
        """Clear result dial (clearing lever)."""
        self.result_dial = 0

    def clear_counter(self) -> None:
        """Clear revolution counter."""
        self.counter_dial = 0
