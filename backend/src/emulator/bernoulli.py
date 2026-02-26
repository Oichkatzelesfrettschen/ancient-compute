"""Exact Bernoulli number utilities used for validating Note G outputs.

This module intentionally uses exact rational arithmetic (fractions.Fraction)
so tests can assert historical values without floating-point drift.
"""

from __future__ import annotations

from fractions import Fraction


def bernoulli_numbers(n_max: int) -> list[Fraction]:
    """Compute Bernoulli numbers B_0..B_n_max (Akiyama–Tanigawa).

    Convention:
    - B_1 = -1/2 (the modern convention commonly used in analysis).
    """
    if n_max < 0:
        raise ValueError("n_max must be >= 0")

    a: list[Fraction] = [Fraction(0) for _ in range(n_max + 1)]
    b: list[Fraction] = []
    for m in range(n_max + 1):
        a[m] = Fraction(1, m + 1)
        for j in range(m, 0, -1):
            a[j - 1] = j * (a[j - 1] - a[j])
        b.append(a[0])
    # Many implementations yield the alternate B1 convention (+1/2). Flip to -1/2.
    if n_max >= 1:
        b[1] = -b[1]
    return b


def bernoulli_odd_by_index(n: int) -> Fraction:
    """Return B_(2n-1) for n>=1."""
    if n < 1:
        raise ValueError("n must be >= 1")
    return bernoulli_numbers(2 * n - 1)[2 * n - 1]


def bernoulli_odd_series(n_max: int) -> list[Fraction]:
    """Return [B1, B3, ..., B_(2n_max-1)] for n_max>=1."""
    if n_max < 1:
        raise ValueError("n_max must be >= 1")
    bs = bernoulli_numbers(2 * n_max - 1)
    return [bs[2 * n - 1] for n in range(1, n_max + 1)]


def ada_lovelace_bernoulli_series(n_max: int) -> list[Fraction]:
    """Return [B1, B3, B5, ...] as used in Ada's Note G diagram.

    Ada's odd-index labelling maps to modern even-index Bernoulli numbers:
        Ada B_{2k-1} = modern B_{2k}
    So: Ada B1 = B_2 = 1/6, Ada B3 = B_4 = -1/30, etc.

    The recurrence from equation (9) in Menabrea/Lovelace:
        0 = A0 + A1*B1 + A3*B3 + ... + B_{2n-1}
    where A0 = -(1/2)*(2n-1)/(2n+1), solved for B_{2n-1}.
    """
    if n_max < 1:
        raise ValueError("n_max must be >= 1")

    # Ada B_{2k-1} = modern B_{2k}, so we need B_2, B_4, ..., B_{2*n_max}
    bs = bernoulli_numbers(2 * n_max)
    return [bs[2 * k] for k in range(1, n_max + 1)]
