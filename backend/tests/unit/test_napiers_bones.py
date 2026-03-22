"""
Napier's Bones - Unit Tests
"""

import pytest

from backend.src.emulator.adapter import NapierAdapter, _parse_napier_a68_output
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


# ---------------------------------------------------------------------------
# Bone dataclass coverage
# ---------------------------------------------------------------------------


class TestBoneDataclass:
    def test_bone_repr(self) -> None:
        nb = NapiersBones()
        assert "Bone(7)" in repr(nb.bones[7])

    def test_bone_digit_attribute(self) -> None:
        nb = NapiersBones()
        for d in range(10):
            assert nb.bones[d].digit == d

    def test_bone_squares_length(self) -> None:
        nb = NapiersBones()
        for d in range(10):
            assert len(nb.bones[d].squares) == 9

    def test_bone_squares_correct_values(self) -> None:
        nb = NapiersBones()
        bone5 = nb.bones[5]
        for i in range(1, 10):
            val = 5 * i
            assert bone5.squares[i - 1] == (val // 10, val % 10)

    def test_bone_get_row_all_digits_all_rows(self) -> None:
        nb = NapiersBones()
        for d in range(10):
            for row in range(1, 10):
                tens, units = nb.bones[d].get_row(row)
                expected = d * row
                assert tens * 10 + units == expected

    def test_all_bones_get_row_9_is_nine_times(self) -> None:
        nb = NapiersBones()
        for d in range(10):
            tens, units = nb.bones[d].get_row(9)
            assert tens * 10 + units == d * 9


# ---------------------------------------------------------------------------
# Board loading extended
# ---------------------------------------------------------------------------


class TestLoadNumberExtended:
    def test_load_zero(self) -> None:
        nb = NapiersBones()
        nb.load_number(0)
        assert len(nb.active_bones) == 1
        assert nb.active_bones[0].digit == 0

    def test_load_two_digit(self) -> None:
        nb = NapiersBones()
        nb.load_number(42)
        assert len(nb.active_bones) == 2
        assert nb.active_bones[0].digit == 4
        assert nb.active_bones[1].digit == 2

    def test_load_five_digit(self) -> None:
        nb = NapiersBones()
        nb.load_number(12345)
        assert len(nb.active_bones) == 5
        assert nb.active_bones[0].digit == 1

    def test_load_replaces_previous(self) -> None:
        nb = NapiersBones()
        nb.load_number(999)
        nb.load_number(1)
        assert len(nb.active_bones) == 1
        assert nb.active_bones[0].digit == 1

    def test_index_rod_is_1_to_9(self) -> None:
        nb = NapiersBones()
        assert nb.index_rod == list(range(1, 10))

    def test_load_repeated_digit(self) -> None:
        nb = NapiersBones()
        nb.load_number(333)
        for bone in nb.active_bones:
            assert bone.digit == 3


# ---------------------------------------------------------------------------
# Lattice row extended
# ---------------------------------------------------------------------------


class TestLatticeRowExtended:
    def test_lattice_row_length_matches_active_bones(self) -> None:
        nb = NapiersBones()
        nb.load_number(123)
        row = nb.get_lattice_row(5)
        assert len(row) == 3

    def test_lattice_row_single_digit_bone(self) -> None:
        nb = NapiersBones()
        nb.load_number(7)
        for m in range(1, 10):
            row = nb.get_lattice_row(m)
            assert len(row) == 1
            tens, units = row[0]
            assert tens * 10 + units == 7 * m

    def test_lattice_row_two_digit_correct(self) -> None:
        nb = NapiersBones()
        nb.load_number(37)
        row = nb.get_lattice_row(4)
        # 3*4=12, 7*4=28
        assert row[0] == (1, 2)
        assert row[1] == (2, 8)

    def test_lattice_row_multiplier_1_is_identity(self) -> None:
        nb = NapiersBones()
        nb.load_number(59)
        row = nb.get_lattice_row(1)
        # 5*1=5 -> (0,5); 9*1=9 -> (0,9)
        assert row[0] == (0, 5)
        assert row[1] == (0, 9)

    def test_lattice_row_multiplier_9_max_digits(self) -> None:
        nb = NapiersBones()
        nb.load_number(99)
        row = nb.get_lattice_row(9)
        # 9*9=81 -> (8,1)
        assert row[0] == (8, 1)
        assert row[1] == (8, 1)


# ---------------------------------------------------------------------------
# multiply_single_digit extended
# ---------------------------------------------------------------------------


class TestMultiplySingleDigitExtended:
    def test_no_active_bones_returns_zero(self) -> None:
        nb = NapiersBones()
        # No bones loaded: multiply_single_digit should return 0
        result = nb.multiply_single_digit(5)
        assert result == 0

    def test_nine_times_nine(self) -> None:
        nb = NapiersBones()
        nb.load_number(9)
        assert nb.multiply_single_digit(9) == 81

    def test_large_number_single_digit(self) -> None:
        nb = NapiersBones()
        nb.load_number(9999)
        for m in range(1, 10):
            assert nb.multiply_single_digit(m) == 9999 * m

    def test_zeros_in_number(self) -> None:
        nb = NapiersBones()
        nb.load_number(1001)
        for m in range(1, 10):
            assert nb.multiply_single_digit(m) == 1001 * m

    def test_multiplier_9_is_largest(self) -> None:
        nb = NapiersBones()
        nb.load_number(123)
        results = [nb.multiply_single_digit(m) for m in range(1, 10)]
        assert results[-1] == 123 * 9
        assert results == sorted(results)


# ---------------------------------------------------------------------------
# multiply extended
# ---------------------------------------------------------------------------


class TestMultiplyExtended:
    def test_multiply_by_100(self) -> None:
        nb = NapiersBones()
        nb.load_number(42)
        assert nb.multiply(100) == 4200

    def test_multiply_by_11(self) -> None:
        nb = NapiersBones()
        nb.load_number(9)
        assert nb.multiply(11) == 99

    def test_multiply_single_and_multi_consistent(self) -> None:
        nb = NapiersBones()
        nb.load_number(123)
        for m in range(1, 10):
            assert nb.multiply(m) == nb.multiply_single_digit(m)

    def test_multiply_zero_by_large(self) -> None:
        nb = NapiersBones()
        nb.load_number(0)
        assert nb.multiply(99999) == 0

    def test_multiply_matches_python(self) -> None:
        nb = NapiersBones()
        for a in [5, 37, 142, 9999]:
            for b in [7, 13, 99, 1000]:
                nb.load_number(a)
                assert nb.multiply(b) == a * b


# ---------------------------------------------------------------------------
# divide extended
# ---------------------------------------------------------------------------


class TestDivideExtended:
    def test_divide_exactly(self) -> None:
        nb = NapiersBones()
        for dividend, divisor in [(100, 4), (81, 9), (1000, 8)]:
            q, r = nb.divide(dividend, divisor)
            assert r == 0
            assert q == dividend // divisor

    def test_divide_small_by_large(self) -> None:
        nb = NapiersBones()
        q, r = nb.divide(3, 7)
        assert q == 0
        assert r == 3

    def test_divide_negative_divisor_raises(self) -> None:
        nb = NapiersBones()
        with pytest.raises(ValueError):
            nb.divide(10, -1)

    def test_divide_large_numbers(self) -> None:
        nb = NapiersBones()
        q, r = nb.divide(9999999, 9999)
        expected_q, expected_r = divmod(9999999, 9999)
        assert q == expected_q
        assert r == expected_r

    def test_divide_1_by_1(self) -> None:
        nb = NapiersBones()
        q, r = nb.divide(1, 1)
        assert q == 1
        assert r == 0


class TestNapierAdapterALGOL68:
    """NapierAdapter.execute_algol68_ops: ALGOL68 backend round-trip tests."""

    def test_set_multiplicand_and_multiply(self) -> None:
        adapter = NapierAdapter(NapiersBones())
        result = adapter.execute_algol68_ops([
            {"op": "set_multiplicand", "value": 123},
            {"op": "multiply", "digit": 7},
        ])
        assert result["result"] == 861
        assert result["multiplicand"] == 123
        assert result["last_multiplier"] == 7

    def test_multiply_9999_by_9(self) -> None:
        adapter = NapierAdapter(NapiersBones())
        result = adapter.execute_algol68_ops([
            {"op": "set_multiplicand", "value": 9999},
            {"op": "multiply", "digit": 9},
        ])
        assert result["result"] == 89991

    def test_multiply_by_1_is_identity(self) -> None:
        adapter = NapierAdapter(NapiersBones())
        result = adapter.execute_algol68_ops([
            {"op": "set_multiplicand", "value": 12345},
            {"op": "multiply", "digit": 1},
        ])
        assert result["result"] == 12345

    def test_reset_clears_state(self) -> None:
        adapter = NapierAdapter(NapiersBones())
        adapter.execute_algol68_ops([
            {"op": "set_multiplicand", "value": 999},
            {"op": "multiply", "digit": 9},
        ])
        result = adapter.execute_algol68_ops([{"op": "reset"}])
        assert result["result"] == 0
        assert result["multiplicand"] == 0

    def test_syncs_python_emulator(self) -> None:
        adapter = NapierAdapter(NapiersBones())
        adapter.execute_algol68_ops([
            {"op": "set_multiplicand", "value": 321},
            {"op": "multiply", "digit": 3},
        ])
        # After sync, active_bones should reflect multiplicand 321.
        assert len(adapter.machine.active_bones) == 3
        assert adapter.machine.active_bones[0].digit == 3

    def test_last_result_updated(self) -> None:
        adapter = NapierAdapter(NapiersBones())
        adapter.execute_algol68_ops([
            {"op": "set_multiplicand", "value": 7},
            {"op": "multiply", "digit": 8},
        ])
        assert adapter._last_result == 56

    def test_ops_counter_increments(self) -> None:
        adapter = NapierAdapter(NapiersBones())
        before = adapter._operations
        adapter.execute_algol68_ops([
            {"op": "set_multiplicand", "value": 12},
            {"op": "multiply", "digit": 5},
        ])
        assert adapter._operations == before + 2

    def test_snapshot_includes_result_and_ops(self) -> None:
        adapter = NapierAdapter(NapiersBones())
        adapter.execute_algol68_ops([
            {"op": "set_multiplicand", "value": 6},
            {"op": "multiply", "digit": 7},
        ])
        snap = adapter.get_snapshot()
        assert snap["last_result"] == 42
        assert "operations" in snap

    def test_unknown_op_is_ignored(self) -> None:
        adapter = NapierAdapter(NapiersBones())
        # Should not raise; unknown ops silently skipped.
        result = adapter.execute_algol68_ops([
            {"op": "set_multiplicand", "value": 5},
            {"op": "unknown_op"},
            {"op": "multiply", "digit": 3},
        ])
        assert result["result"] == 15


class TestParseNapierA68Output:
    """Unit tests for _parse_napier_a68_output."""

    def test_basic_parse(self) -> None:
        raw = "result=861\nmultiplicand=123\nlast_multiplier=7\n"
        parsed = _parse_napier_a68_output(raw)
        assert parsed["result"] == 861
        assert parsed["multiplicand"] == 123
        assert parsed["last_multiplier"] == 7

    def test_empty_output_returns_zeros(self) -> None:
        parsed = _parse_napier_a68_output("")
        assert parsed == {"result": 0, "multiplicand": 0, "last_multiplier": 0}

    def test_zero_values(self) -> None:
        raw = "result=0\nmultiplicand=0\nlast_multiplier=0\n"
        parsed = _parse_napier_a68_output(raw)
        assert parsed["result"] == 0

    def test_large_values(self) -> None:
        raw = "result=99999999\nmultiplicand=11111111\nlast_multiplier=9\n"
        parsed = _parse_napier_a68_output(raw)
        assert parsed["result"] == 99999999

    def test_partial_output_defaults_to_zero(self) -> None:
        raw = "result=42\n"
        parsed = _parse_napier_a68_output(raw)
        assert parsed["result"] == 42
        assert parsed["multiplicand"] == 0
        assert parsed["last_multiplier"] == 0
