"""Tests for abacus emulator."""

from backend.src.emulator.abacus import AbacusEmulator


def test_abacus_add_carry():
    emu = AbacusEmulator()
    emu.set_value(789)
    assert emu.add(456) == 1245
    assert emu.state()["digits"] == [1, 2, 4, 5]


def test_abacus_sub_borrow():
    emu = AbacusEmulator()
    emu.set_value(1000)
    assert emu.sub(1) == 999
    assert emu.state()["digits"] == [9, 9, 9]


class TestAbacusEmulator:
    def test_initial_value_zero(self) -> None:
        emu = AbacusEmulator()
        assert emu.state()["value"] == 0

    def test_initial_digits_zero(self) -> None:
        emu = AbacusEmulator()
        assert emu.state()["digits"] == [0]

    def test_set_value_basic(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(42)
        assert emu.state()["value"] == 42

    def test_set_value_clamps_negative(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(-99)
        assert emu.state()["value"] == 0

    def test_add_from_zero(self) -> None:
        emu = AbacusEmulator()
        result = emu.add(7)
        assert result == 7

    def test_add_accumulates(self) -> None:
        emu = AbacusEmulator()
        emu.add(5)
        assert emu.add(3) == 8

    def test_sub_basic(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(10)
        assert emu.sub(3) == 7

    def test_sub_underflow_clamps_zero(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(5)
        assert emu.sub(100) == 0

    def test_sub_to_exactly_zero(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(5)
        assert emu.sub(5) == 0

    def test_digits_single_digit(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(7)
        assert emu.state()["digits"] == [7]

    def test_digits_two_digits(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(45)
        assert emu.state()["digits"] == [4, 5]

    def test_digits_three_digits(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(123)
        assert emu.state()["digits"] == [1, 2, 3]

    def test_digits_leading_no_zero(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(100)
        assert emu.state()["digits"] == [1, 0, 0]

    def test_reset_zeroes_value(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(9999)
        emu.reset()
        assert emu.state()["value"] == 0

    def test_reset_digits_back_to_zero(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(9999)
        emu.reset()
        assert emu.state()["digits"] == [0]

    def test_state_value_consistent_with_add(self) -> None:
        emu = AbacusEmulator()
        emu.add(37)
        assert emu.state()["value"] == 37

    def test_state_keys_present(self) -> None:
        emu = AbacusEmulator()
        s = emu.state()
        assert "value" in s and "digits" in s
