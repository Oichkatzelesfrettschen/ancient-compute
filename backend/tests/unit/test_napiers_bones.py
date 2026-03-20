"""
Napier's Bones - Unit Tests
"""

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


# Division tests
def test_napier_divide_exact():
    nb = NapiersBones()
    q, r = nb.divide(100, 5)
    assert q == 20
    assert r == 0


def test_napier_divide_with_remainder():
    nb = NapiersBones()
    q, r = nb.divide(17, 3)
    assert q == 5
    assert r == 2


def test_napier_divide_by_one():
    nb = NapiersBones()
    q, r = nb.divide(999, 1)
    assert q == 999
    assert r == 0


def test_napier_divide_self():
    nb = NapiersBones()
    q, r = nb.divide(42, 42)
    assert q == 1
    assert r == 0


def test_napier_divide_large():
    nb = NapiersBones()
    q, r = nb.divide(123456, 789)
    expected_q, expected_r = divmod(123456, 789)
    assert q == expected_q
    assert r == expected_r


def test_napier_divide_by_zero():
    nb = NapiersBones()
    import pytest

    with pytest.raises(ZeroDivisionError):
        nb.divide(10, 0)
