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
    with pytest.raises(ZeroDivisionError):
        nb.divide(10, 0)


class TestNapiersBones:
    def test_all_ten_bones_created(self) -> None:
        nb = NapiersBones()
        assert set(nb.bones.keys()) == set(range(10))

    def test_bone_digit_zero(self) -> None:
        nb = NapiersBones()
        for i in range(1, 10):
            assert nb.bones[0].get_row(i) == (0, 0)

    def test_bone_digit_one_identity(self) -> None:
        nb = NapiersBones()
        # 1 * n = n; tens = 0, units = n for n <= 9
        for n in range(1, 10):
            assert nb.bones[1].get_row(n) == (0, n)

    def test_bone_out_of_range_row(self) -> None:
        nb = NapiersBones()
        assert nb.bones[5].get_row(0) == (0, 0)
        assert nb.bones[5].get_row(10) == (0, 0)

    def test_load_number_sets_bones(self) -> None:
        nb = NapiersBones()
        nb.load_number(321)
        assert len(nb.active_bones) == 3
        assert nb.active_bones[0].digit == 3
        assert nb.active_bones[1].digit == 2
        assert nb.active_bones[2].digit == 1

    def test_load_single_digit(self) -> None:
        nb = NapiersBones()
        nb.load_number(7)
        assert len(nb.active_bones) == 1
        assert nb.active_bones[0].digit == 7

    def test_get_lattice_row_returns_per_bone(self) -> None:
        nb = NapiersBones()
        nb.load_number(12)
        row = nb.get_lattice_row(3)
        # 1*3=3 -> (0,3); 2*3=6 -> (0,6)
        assert row == [(0, 3), (0, 6)]

    def test_get_lattice_row_invalid_multiplier(self) -> None:
        nb = NapiersBones()
        nb.load_number(5)
        with pytest.raises(ValueError):
            nb.get_lattice_row(0)

    def test_get_lattice_row_invalid_multiplier_ten(self) -> None:
        nb = NapiersBones()
        nb.load_number(5)
        with pytest.raises(ValueError):
            nb.get_lattice_row(10)

    def test_multiply_single_digit_all_rows(self) -> None:
        nb = NapiersBones()
        nb.load_number(7)
        for m in range(1, 10):
            assert nb.multiply_single_digit(m) == 7 * m

    def test_multiply_single_digit_two_digit_number(self) -> None:
        nb = NapiersBones()
        nb.load_number(13)
        for m in range(1, 10):
            assert nb.multiply_single_digit(m) == 13 * m

    def test_multiply_by_1(self) -> None:
        nb = NapiersBones()
        nb.load_number(9876)
        assert nb.multiply(1) == 9876

    def test_multiply_by_10(self) -> None:
        nb = NapiersBones()
        nb.load_number(123)
        assert nb.multiply(10) == 1230

    def test_multiply_result_equals_python_mul(self) -> None:
        nb = NapiersBones()
        for a in [7, 42, 123, 9999]:
            for b in [1, 3, 7, 12, 99]:
                nb.load_number(a)
                assert nb.multiply(b) == a * b

    def test_divide_zero_dividend(self) -> None:
        nb = NapiersBones()
        q, r = nb.divide(0, 7)
        assert q == 0
        assert r == 0

    def test_divide_negative_raises(self) -> None:
        nb = NapiersBones()
        with pytest.raises(ValueError):
            nb.divide(-5, 2)

    def test_divide_remainder_less_than_divisor(self) -> None:
        nb = NapiersBones()
        for dividend in range(1, 50):
            for divisor in range(1, 10):
                q, r = nb.divide(dividend, divisor)
                assert r < divisor
                assert q * divisor + r == dividend
