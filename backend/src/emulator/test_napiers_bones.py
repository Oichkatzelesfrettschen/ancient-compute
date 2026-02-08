"""
Napier's Bones - Unit Tests
"""

import pytest
from backend.src.emulator.napiers_bones import NapiersBones

def test_bone_creation():
    bones = NapiersBones()
    assert len(bones.bones) == 10
    
    # Check 7 bone: 7, 14, 21...
    bone7 = bones.bones[7]
    assert bone7.get_row(1) == (0, 7)
    assert bone7.get_row(2) == (1, 4)
    assert bone7.get_row(9) == (6, 3)

def test_single_digit_multiplication():
    nb = NapiersBones()
    
    # 425 * 6
    # 425 = [4, 2, 5]
    # 6th row:
    # 4: 2/4
    # 2: 1/2
    # 5: 3/0
    # Lattice:
    #   2 / 4
    #   1 / 2
    #   3 / 0
    # Diagonals:
    #   0 (units)
    #   2+3 = 5 (tens)
    #   4+1 = 5 (hundreds)
    #   2 (thousands)
    # Result: 2550
    
    nb.load_number(425)
    assert nb.multiply_single_digit(6) == 2550

def test_multi_digit_multiplication():
    nb = NapiersBones()
    nb.load_number(123)
    assert nb.multiply(456) == 123 * 456
    
    nb.load_number(999)
    assert nb.multiply(999) == 999 * 999

def test_zero_handling():
    nb = NapiersBones()
    nb.load_number(102)
    assert nb.multiply(5) == 510
    
    nb.load_number(0)
    assert nb.multiply(55) == 0
