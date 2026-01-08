from fractions import Fraction

from backend.src.emulator.bernoulli import ada_lovelace_bernoulli_series, bernoulli_numbers


def test_bernoulli_b1_convention_is_negative_half():
    assert bernoulli_numbers(1)[1] == Fraction(-1, 2)


def test_ada_lovelace_series_first_four():
    assert ada_lovelace_bernoulli_series(4) == [
        Fraction(-1, 2),
        Fraction(1, 6),
        Fraction(-1, 30),
        Fraction(1, 42),
    ]

