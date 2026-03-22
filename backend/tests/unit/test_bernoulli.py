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


class TestBernoulliNumbersExtended:
    """Extended checks on exact values and edge cases for bernoulli_numbers()."""

    def test_b0_is_one_at_any_n_max(self) -> None:
        for n in [0, 5, 10]:
            assert bernoulli_numbers(n)[0] == Fraction(1)

    def test_b1_is_negative_half(self) -> None:
        assert bernoulli_numbers(1)[1] == Fraction(-1, 2)

    def test_b2_equals_one_sixth(self) -> None:
        assert bernoulli_numbers(2)[2] == Fraction(1, 6)

    def test_b4_equals_negative_one_thirtieth(self) -> None:
        assert bernoulli_numbers(4)[4] == Fraction(-1, 30)

    def test_b10_equals_five_sixty_sixths(self) -> None:
        assert bernoulli_numbers(10)[10] == Fraction(5, 66)

    def test_b12_equals_minus_691_over_2730(self) -> None:
        assert bernoulli_numbers(12)[12] == Fraction(-691, 2730)

    def test_negative_n_max_raises(self) -> None:
        import pytest
        with pytest.raises(ValueError):
            bernoulli_numbers(-1)

    def test_n_max_zero_returns_single_element(self) -> None:
        result = bernoulli_numbers(0)
        assert len(result) == 1
        assert result[0] == Fraction(1)

    def test_all_odd_indices_above_1_are_zero(self) -> None:
        bn = bernoulli_numbers(15)
        for k in range(3, 16, 2):  # k = 3, 5, 7, 9, 11, 13, 15
            assert bn[k] == Fraction(0), f"B({k}) should be 0"

    def test_sign_alternates_for_even_indices(self) -> None:
        # B_2>0, B_4<0, B_6>0, B_8<0 ...
        bn = bernoulli_numbers(8)
        assert bn[2] > 0
        assert bn[4] < 0
        assert bn[6] > 0
        assert bn[8] < 0


class TestBernoulliOddSeriesExtended:
    """Extended tests for bernoulli_odd_series and bernoulli_odd_by_index."""

    def test_odd_series_first_is_b1(self) -> None:
        # B_1 = -1/2
        assert bernoulli_odd_series(1)[0] == Fraction(-1, 2)

    def test_odd_series_second_is_b3_zero(self) -> None:
        # B_3 = 0 for n=2
        assert bernoulli_odd_series(2)[1] == Fraction(0)

    def test_odd_series_raises_for_zero(self) -> None:
        import pytest
        with pytest.raises(ValueError):
            bernoulli_odd_series(0)

    def test_odd_by_index_k1_equals_b1(self) -> None:
        assert bernoulli_odd_by_index(1) == Fraction(-1, 2)

    def test_odd_by_index_k3_zero(self) -> None:
        # B_5 = 0
        assert bernoulli_odd_by_index(3) == Fraction(0)

    def test_odd_by_index_raises_for_zero(self) -> None:
        import pytest
        with pytest.raises(ValueError):
            bernoulli_odd_by_index(0)

    def test_odd_series_length_five(self) -> None:
        assert len(bernoulli_odd_series(5)) == 5

    def test_odd_by_index_matches_bernoulli_numbers(self) -> None:
        for k in range(1, 6):
            idx_val = bernoulli_odd_by_index(k)
            full_val = bernoulli_numbers(2 * k - 1)[2 * k - 1]
            assert idx_val == full_val


class TestAdaLovelaceSeriesExtended:
    """Extended tests for ada_lovelace_bernoulli_series."""

    def test_raises_for_n_max_zero(self) -> None:
        import pytest
        with pytest.raises(ValueError):
            ada_lovelace_bernoulli_series(0)

    def test_raises_for_negative_n_max(self) -> None:
        import pytest
        with pytest.raises(ValueError):
            ada_lovelace_bernoulli_series(-1)

    def test_single_value_is_one_sixth(self) -> None:
        series = ada_lovelace_bernoulli_series(1)
        assert len(series) == 1
        assert series[0] == Fraction(1, 6)

    def test_seventh_value_b13(self) -> None:
        # Ada B_{13} = modern B_{14} = 7/6
        series = ada_lovelace_bernoulli_series(7)
        assert series[6] == Fraction(7, 6)

    def test_values_match_modern_even_bernoulli(self) -> None:
        modern = bernoulli_numbers(12)
        ada = ada_lovelace_bernoulli_series(6)
        for k in range(1, 7):
            assert ada[k - 1] == modern[2 * k]

    def test_series_is_deterministic(self) -> None:
        s1 = ada_lovelace_bernoulli_series(5)
        s2 = ada_lovelace_bernoulli_series(5)
        assert s1 == s2
