"""Exact Bernoulli number utilities used for validating Note G outputs.

This module intentionally uses exact rational arithmetic (fractions.Fraction)
so tests can assert historical values without floating-point drift.
"""

from __future__ import annotations

from fractions import Fraction
from typing import List


def bernoulli_numbers(n_max: int) -> List[Fraction]:
    """Compute Bernoulli numbers B_0..B_n_max (Akiyama–Tanigawa).

    Convention:
    - B_1 = -1/2 (the modern convention commonly used in analysis).
    """
    if n_max < 0:
        raise ValueError("n_max must be >= 0")

    a: List[Fraction] = [Fraction(0) for _ in range(n_max + 1)]
    b: List[Fraction] = []
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


def bernoulli_odd_series(n_max: int) -> List[Fraction]:
    """Return [B1, B3, ..., B_(2n_max-1)] for n_max>=1."""
    if n_max < 1:
        raise ValueError("n_max must be >= 1")
    bs = bernoulli_numbers(2 * n_max - 1)
    return [bs[2 * n - 1] for n in range(1, n_max + 1)]


def ada_lovelace_bernoulli_series(n_max: int) -> List[Fraction]:
    """Return [B1, B3, B5, ...] as used in Ada's Note G diagram.

    The diagram labels outputs as B1, B3, B5, ... but the computed sequence
    corresponds to modern Bernoulli numbers [B1, B2, B4, B6, ...] (i.e., the
    non-zero terms after B1), using the B1=-1/2 convention.
    """
    if n_max < 1:
        raise ValueError("n_max must be >= 1")

    # Need up to B_{2n_max-2} to serve B_{2}, B_{4}, ...
    bs = bernoulli_numbers(max(1, 2 * n_max - 2))
    out: List[Fraction] = [bs[1]]
    for k in range(2, n_max + 1):
        out.append(bs[2 * k - 2])
    return out
