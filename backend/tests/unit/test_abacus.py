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


class TestAbacusOperationsSequence:
    """Multi-step operation sequences."""

    def test_add_then_sub_roundtrip(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(50)
        emu.add(25)
        emu.sub(25)
        assert emu.state()["value"] == 50

    def test_multiple_adds_accumulate(self) -> None:
        emu = AbacusEmulator()
        for _ in range(10):
            emu.add(1)
        assert emu.state()["value"] == 10

    def test_multiple_subs_from_large(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(100)
        for _ in range(10):
            emu.sub(10)
        assert emu.state()["value"] == 0

    def test_add_then_reset_then_add(self) -> None:
        emu = AbacusEmulator()
        emu.add(42)
        emu.reset()
        emu.add(7)
        assert emu.state()["value"] == 7

    def test_sub_to_zero_and_further_stays_zero(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(3)
        emu.sub(3)
        emu.sub(1)  # should clamp at 0
        assert emu.state()["value"] == 0


class TestAbacusDigitRepresentation:
    """Digit decomposition for various values."""

    def test_zero_is_single_digit_zero(self) -> None:
        emu = AbacusEmulator()
        assert emu.state()["digits"] == [0]

    def test_nine_is_single_digit(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(9)
        assert emu.state()["digits"] == [9]

    def test_ten_is_two_digits(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(10)
        assert emu.state()["digits"] == [1, 0]

    def test_999_is_three_nines(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(999)
        assert emu.state()["digits"] == [9, 9, 9]

    def test_1000_is_four_digits(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(1000)
        assert emu.state()["digits"] == [1, 0, 0, 0]

    def test_digits_after_carry_from_add(self) -> None:
        """9 + 1 = 10 -> digits [1, 0]."""
        emu = AbacusEmulator()
        emu.set_value(9)
        emu.add(1)
        assert emu.state()["digits"] == [1, 0]

    def test_digits_match_value_always(self) -> None:
        emu = AbacusEmulator()
        for v in [0, 1, 9, 10, 99, 100, 999, 1234]:
            emu.set_value(v)
            digits = emu.state()["digits"]
            reconstructed = int("".join(str(d) for d in digits))
            assert reconstructed == v, f"Digit mismatch for value {v}"


class TestAbacusStateReturn:
    """Return value consistency and state independence."""

    def test_add_returns_numeric_value(self) -> None:
        emu = AbacusEmulator()
        result = emu.add(42)
        assert isinstance(result, int)

    def test_sub_returns_numeric_value(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(100)
        result = emu.sub(10)
        assert isinstance(result, int)

    def test_set_value_zero_gives_single_zero_digit(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(999)
        emu.set_value(0)
        assert emu.state()["digits"] == [0]

    def test_add_zero_leaves_value_unchanged(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(77)
        emu.add(0)
        assert emu.state()["value"] == 77


class TestAbacusArithmetic:
    """Multi-step arithmetic and boundary checks."""

    def test_chain_add_three_values(self) -> None:
        emu = AbacusEmulator()
        emu.add(10)
        emu.add(20)
        emu.add(30)
        assert emu.state()["value"] == 60

    def test_add_then_sub_net_zero(self) -> None:
        emu = AbacusEmulator()
        emu.add(50)
        emu.sub(50)
        assert emu.state()["value"] == 0

    def test_multiple_sets_final_value_wins(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(100)
        emu.set_value(200)
        emu.set_value(300)
        assert emu.state()["value"] == 300

    def test_reset_after_large_value(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(9999999)
        emu.reset()
        assert emu.state()["value"] == 0

    def test_add_one_hundred_times(self) -> None:
        emu = AbacusEmulator()
        for _ in range(100):
            emu.add(1)
        assert emu.state()["value"] == 100

    def test_sub_from_large_value(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(1000)
        emu.sub(999)
        assert emu.state()["value"] == 1

    def test_state_returns_dict(self) -> None:
        emu = AbacusEmulator()
        s = emu.state()
        assert isinstance(s, dict)

    def test_state_has_value_key(self) -> None:
        emu = AbacusEmulator()
        assert "value" in emu.state()

    def test_state_has_digits_key(self) -> None:
        emu = AbacusEmulator()
        assert "digits" in emu.state()


class TestAbacusEdgeCases:
    """Boundary and edge case behaviors."""

    def test_default_value_is_zero(self) -> None:
        emu = AbacusEmulator()
        assert emu.state()["value"] == 0

    def test_set_single_digit_value(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(7)
        assert emu.state()["value"] == 7

    def test_add_returns_updated_value(self) -> None:
        emu = AbacusEmulator()
        result = emu.add(5)
        assert result == 5

    def test_sub_returns_updated_value(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(10)
        result = emu.sub(3)
        assert result == 7

    def test_reset_returns_zero_state(self) -> None:
        emu = AbacusEmulator()
        emu.set_value(42)
        emu.reset()
        assert emu.state()["value"] == 0
