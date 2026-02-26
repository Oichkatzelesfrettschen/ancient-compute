"""
Napier's Bones Emulator

Mechanical calculation using John Napier's numbering rods (1617).
A direct precursor to logarithmic scales (Slide Rule).

Architecture:
  - Rods: Set of 10 rods (0-9) with multiplication tables
  - Index Rod: Numbered 1-9 for multiplier selection
  - Board: Frame to hold rods for computation
  - Lattice Multiplication: Diagonal addition method

Operations:
  - Multiplication: By single digit, then shift-and-add for multi-digit
  - Division: By subtraction and estimation using rods
  - Square Root: Special square root rod

References:
  - Napier, J. (1617). "Rhabdologia"
  - Williams, M. R. (1997). "A History of Computing Technology"
"""

from dataclasses import dataclass


@dataclass
class Bone:
    """A single Napier's Bone representing a digit 0-9."""
    digit: int
    squares: list[tuple[int, int]]  # (tens, units) for 1x to 9x

    def __repr__(self) -> str:
        return f"Bone({self.digit})"

    def get_row(self, index: int) -> tuple[int, int]:
        """Get the (tens, units) for digit * index."""
        if 1 <= index <= 9:
            return self.squares[index - 1]
        return (0, 0)

class NapiersBones:
    """
    Napier's Bones Calculator.

    Simulates the physical manipulation of bones for multiplication.
    """

    def __init__(self) -> None:
        self.bones: dict[int, Bone] = self._create_bones()
        self.active_bones: list[Bone] = [] # Bones currently on the board
        self.index_rod = list(range(1, 10))

    def _create_bones(self) -> dict[int, Bone]:
        """Generate the standard set of 10 bones."""
        bones = {}
        for digit in range(10):
            squares = []
            for i in range(1, 10):
                val = digit * i
                squares.append((val // 10, val % 10))
            bones[digit] = Bone(digit, squares)
        return bones

    def load_number(self, number: int) -> None:
        """Place bones on the board to represent a number."""
        self.active_bones = []
        for digit_char in str(number):
            digit = int(digit_char)
            self.active_bones.append(self.bones[digit])

    def get_lattice_row(self, multiplier: int) -> list[tuple[int, int]]:
        """
        Get the lattice row for a specific multiplier digit (1-9).
        Returns list of (tens, units) pairs for the active number.
        """
        if not 1 <= multiplier <= 9:
            raise ValueError("Multiplier must be 1-9")

        return [bone.get_row(multiplier) for bone in self.active_bones]

    def multiply_single_digit(self, multiplier: int) -> int:
        """
        Multiply currently loaded number by a single digit using diagonal addition.

        Algorithm:
          1. Get lattice row for multiplier
          2. Sum diagonals (units of Right + tens of Left) + carry
        """
        if not self.active_bones:
            return 0

        row = self.get_lattice_row(multiplier)
        result_digits = []
        carry = 0

        # Process from right to left
        # Rightmost bone: just units + carry
        # Then: tens of current + units of next-left + carry

        # Last bone (rightmost)
        last_pair = row[-1]
        val = last_pair[1] + carry
        result_digits.append(val % 10)
        carry = val // 10 + last_pair[0] # Add tens to carry for next diagonal

        # Middle bones
        for i in range(len(row) - 2, -1, -1):
            pair = row[i]
            # Diagonal: tens of current (already in carry) + units of this pair
            # Wait, standard diagonal is: units of current + tens of right
            # Let's trace carefully:
            #   [t u] [t u]
            #      \ /   \ /
            #       +     +

            # Correct logic:
            # We are iterating i from Right-1 down to 0.
            # "Right" neighbor is i+1 (which we just processed).
            # Current bone is i.
            # Diagonal sum for position i: (units of i) + (tens of i+1) + carry
            # Wait, logic above in 'Last bone' handled units of last.
            # Carry became: tens of last.

            # Now at i:
            # digit = units of i + carry
            # new carry = tens of i

            val = pair[1] + carry
            result_digits.append(val % 10)
            carry = val // 10 + pair[0]

        # Final carry (leftmost tens)
        if carry > 0:
            result_digits.append(carry)

        return int("".join(map(str, reversed(result_digits))))

    def multiply(self, multiplier: int) -> int:
        """
        Full multiplication by any integer (shift-and-add).
        """
        total = 0
        for i, digit_char in enumerate(reversed(str(multiplier))):
            digit = int(digit_char)
            if digit == 0:
                continue
            partial = self.multiply_single_digit(digit)
            total += partial * (10 ** i)
        return total
