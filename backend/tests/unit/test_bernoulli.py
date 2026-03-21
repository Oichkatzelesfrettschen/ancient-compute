from fractions import Fraction

from backend.src.emulator.bernoulli import (
    ada_lovelace_bernoulli_series,
    bernoulli_numbers,
    bernoulli_odd_by_index,
    bernoulli_odd_series,
)


def test_bernoulli_b1_convention_is_negative_half():
    assert bernoulli_numbers(1)[1] == Fraction(-1, 2)


def test_ada_lovelace_series_first_four():
    """Ada's B1,B3,B5,B7 = modern B2,B4,B6,B8."""
    assert ada_lovelace_bernoulli_series(4) == [
        Fraction(1, 6),
        Fraction(-1, 30),
        Fraction(1, 42),
        Fraction(-1, 30),
    ]


class TestBernoulliNumbers:
    def test_b0_is_one(self) -> None:
        assert bernoulli_numbers(0)[0] == Fraction(1)

    def test_b2_is_one_sixth(self) -> None:
        assert bernoulli_numbers(2)[2] == Fraction(1, 6)

    def test_list_length_is_n_plus_one(self) -> None:
        for n in [0, 1, 5, 10]:
            assert len(bernoulli_numbers(n)) == n + 1

    def test_all_elements_are_fractions(self) -> None:
        for b in bernoulli_numbers(8):
            assert isinstance(b, Fraction)

    def test_odd_indices_above_one_are_zero(self) -> None:
        bn = bernoulli_numbers(9)
        for k in [3, 5, 7, 9]:
            assert bn[k] == Fraction(0), f"B({k}) should be 0"

    def test_b4_is_minus_one_thirtieth(self) -> None:
        assert bernoulli_numbers(4)[4] == Fraction(-1, 30)

    def test_b6_is_one_fortysecond(self) -> None:
        assert bernoulli_numbers(6)[6] == Fraction(1, 42)

    def test_b8_is_minus_one_thirtieth(self) -> None:
        assert bernoulli_numbers(8)[8] == Fraction(-1, 30)

    def test_deterministic(self) -> None:
        assert bernoulli_numbers(6) == bernoulli_numbers(6)


class TestBernoulliOddByIndex:
    def test_k1_returns_b1(self) -> None:
        assert bernoulli_odd_by_index(1) == Fraction(-1, 2)

    def test_k2_returns_b3_which_is_zero(self) -> None:
        # B_3 = 0 for all odd indices > 1
        assert bernoulli_odd_by_index(2) == Fraction(0)

    def test_returns_fraction(self) -> None:
        assert isinstance(bernoulli_odd_by_index(1), Fraction)


class TestBernoulliOddSeries:
    def test_length_equals_n(self) -> None:
        for n in [1, 3, 6]:
            assert len(bernoulli_odd_series(n)) == n

    def test_first_element_is_b1(self) -> None:
        assert bernoulli_odd_series(3)[0] == Fraction(-1, 2)

    def test_all_elements_are_fractions(self) -> None:
        for b in bernoulli_odd_series(5):
            assert isinstance(b, Fraction)


class TestAdaLovelaceSeries:
    def test_length_equals_n(self) -> None:
        for n in [1, 4, 6]:
            assert len(ada_lovelace_bernoulli_series(n)) == n

    def test_first_is_one_sixth(self) -> None:
        assert ada_lovelace_bernoulli_series(1)[0] == Fraction(1, 6)

    def test_sixth_value_known(self) -> None:
        series = ada_lovelace_bernoulli_series(6)
        assert series[5] == Fraction(-691, 2730)

    def test_all_elements_are_fractions(self) -> None:
        for b in ada_lovelace_bernoulli_series(6):
            assert isinstance(b, Fraction)

    def test_alternating_signs_from_second(self) -> None:
        # B2>0, B4<0, B6>0, B8<0 ...
        series = ada_lovelace_bernoulli_series(4)
        assert series[0] > 0
        assert series[1] < 0
        assert series[2] > 0
        assert series[3] < 0

    def test_deterministic(self) -> None:
        assert ada_lovelace_bernoulli_series(4) == ada_lovelace_bernoulli_series(4)
