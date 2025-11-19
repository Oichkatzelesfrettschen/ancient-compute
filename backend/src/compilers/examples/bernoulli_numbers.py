"""
Bernoulli Numbers Algorithm - Ada Lovelace's Note G (1843)

This module implements the first computer program ever published, from Ada Lovelace's
Note G in her translation of Luigi Menabrea's memoir on the Analytical Engine.

Historical Context:
- Published: September 1843 in Taylor's Scientific Memoirs
- Author: Augusta Ada Byron King, Countess of Lovelace
- Significance: First algorithm intended for machine execution
- Original: Designed for Babbage's Analytical Engine (never built)
- Formula: Recursive definition using binomial coefficients

The Bernoulli numbers B_n are defined recursively:
    B_0 = 1
    B_1 = -1/2
    B_n = 0 for odd n > 1
    B_n = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × B_k] for even n > 1

Where C(n+1, k) is the binomial coefficient "n+1 choose k".

First few values:
    B_0 = 1
    B_1 = -1/2
    B_2 = 1/6
    B_4 = -1/30
    B_6 = 1/42
    B_8 = -1/30

Ancient-Compute Integration:
This implementation is part of the historical accuracy enhancement (Phase 4),
demonstrating how ancient algorithms can be expressed in modern languages while
preserving their mathematical essence.

References:
- Lovelace, A. (1843). "Notes by the Translator" in Taylor's Scientific Memoirs, Vol. 3
- Babbage, C. (1864). "Passages from the Life of a Philosopher"
- Menabrea, L.F. (1842). "Notions sur la machine analytique de M. Charles Babbage"
"""

from fractions import Fraction
from math import comb
from typing import Union


def bernoulli_number(n: int) -> Fraction:
    """
    Calculate the nth Bernoulli number using Ada Lovelace's recursive formula.

    This is the algorithm from Note G (September 1843) - the first computer program
    ever published. Ada designed this to run on Babbage's Analytical Engine.

    Args:
        n: The index of the Bernoulli number to calculate (n >= 0)

    Returns:
        The nth Bernoulli number as a Fraction (exact rational arithmetic)

    Raises:
        ValueError: If n < 0

    Mathematical Formula:
        B_0 = 1
        B_1 = -1/2
        B_n = 0 for odd n > 1
        B_n = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × B_k] for even n > 1

    Complexity:
        Time: O(n²) due to recursive summation
        Space: O(n) for recursion depth

    Examples:
        >>> bernoulli_number(0)
        Fraction(1, 1)

        >>> bernoulli_number(1)
        Fraction(-1, 2)

        >>> bernoulli_number(2)
        Fraction(1, 6)

        >>> bernoulli_number(4)
        Fraction(-1, 30)

        >>> bernoulli_number(6)
        Fraction(1, 42)

    Historical Note:
        Ada's original algorithm used 25 operations on the Analytical Engine:
        - 9 columns (V1-V9) for variables
        - Operations: Load, Add, Subtract, Multiply, Divide
        - Result stored in V11

        Modern implementation uses memoization and Python's arbitrary precision
        arithmetic, but follows the same mathematical logic.
    """
    if n < 0:
        raise ValueError(f"Bernoulli number index must be non-negative, got {n}")

    # Base case: B_0 = 1
    if n == 0:
        return Fraction(1)

    # Base case: B_1 = -1/2
    if n == 1:
        return Fraction(-1, 2)

    # All odd Bernoulli numbers (except B_1) are zero
    if n > 1 and n % 2 == 1:
        return Fraction(0)

    # Recursive formula: B_n = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × B_k]
    summation = Fraction(0)
    for k in range(n):
        binomial_coeff = comb(n + 1, k)  # C(n+1, k)
        B_k = bernoulli_number(k)         # Recursive call
        summation += binomial_coeff * B_k

    B_n = -summation / (n + 1)
    return B_n


def bernoulli_number_memoized(n: int, memo: dict = None) -> Fraction:
    """
    Calculate the nth Bernoulli number with memoization for efficiency.

    This optimized version caches previously computed values to avoid redundant
    recursive calculations. More efficient for computing multiple Bernoulli numbers.

    Args:
        n: The index of the Bernoulli number to calculate (n >= 0)
        memo: Optional memoization dictionary (used internally for recursion)

    Returns:
        The nth Bernoulli number as a Fraction

    Complexity:
        Time: O(n²) with memoization (vs O(2^n) without)
        Space: O(n) for memoization cache

    Examples:
        >>> bernoulli_number_memoized(8)
        Fraction(-1, 30)

        >>> bernoulli_number_memoized(10)
        Fraction(5, 66)
    """
    if memo is None:
        memo = {}

    if n in memo:
        return memo[n]

    if n < 0:
        raise ValueError(f"Bernoulli number index must be non-negative, got {n}")

    if n == 0:
        result = Fraction(1)
    elif n == 1:
        result = Fraction(-1, 2)
    elif n > 1 and n % 2 == 1:
        result = Fraction(0)
    else:
        summation = Fraction(0)
        for k in range(n):
            binomial_coeff = comb(n + 1, k)
            B_k = bernoulli_number_memoized(k, memo)
            summation += binomial_coeff * B_k
        result = -summation / (n + 1)

    memo[n] = result
    return result


def bernoulli_sequence(max_n: int) -> list[Fraction]:
    """
    Generate a sequence of Bernoulli numbers from B_0 to B_max_n.

    Args:
        max_n: The highest index to calculate (inclusive)

    Returns:
        List of Bernoulli numbers [B_0, B_1, ..., B_max_n]

    Examples:
        >>> bernoulli_sequence(6)
        [Fraction(1, 1), Fraction(-1, 2), Fraction(1, 6), Fraction(0, 1),
         Fraction(-1, 30), Fraction(0, 1), Fraction(1, 42)]
    """
    if max_n < 0:
        raise ValueError(f"max_n must be non-negative, got {max_n}")

    memo = {}
    return [bernoulli_number_memoized(i, memo) for i in range(max_n + 1)]


def bernoulli_as_float(n: int, precision: int = 10) -> float:
    """
    Calculate the nth Bernoulli number and return as float.

    Useful for numerical applications where exact rational arithmetic is not needed.

    Args:
        n: The index of the Bernoulli number
        precision: Number of decimal places (for display only)

    Returns:
        Bernoulli number as floating point

    Examples:
        >>> round(bernoulli_as_float(2), 6)
        0.166667

        >>> round(bernoulli_as_float(4), 6)
        -0.033333
    """
    b_n = bernoulli_number(n)
    return float(b_n)


def print_bernoulli_table(max_n: int = 10) -> None:
    """
    Print a formatted table of Bernoulli numbers.

    Args:
        max_n: Maximum index to display

    Example Output:
        n    B_n (exact)              B_n (decimal)
        ================================================
        0    1                        1.0
        1    -1/2                     -0.5
        2    1/6                      0.16666666666666666
        4    -1/30                    -0.03333333333333333
        6    1/42                     0.023809523809523808
        8    -1/30                    -0.03333333333333333
        10   5/66                     0.07575757575757576
    """
    print("n    B_n (exact)              B_n (decimal)")
    print("=" * 70)

    for i in range(max_n + 1):
        b_n = bernoulli_number(i)
        if b_n != 0:  # Only print non-zero values
            exact = str(b_n)
            decimal = float(b_n)
            print(f"{i:<4} {exact:<24} {decimal}")


# Historical demonstration: Ada's original example from Note G
def ada_lovelace_note_g_example() -> None:
    """
    Demonstrate the calculation as described in Ada Lovelace's Note G.

    Ada's original table shows the operations needed to compute B_8 on the
    Analytical Engine using 25 operations across 9 variable columns.

    This function replicates the logic (though not the exact operational sequence)
    to compute the same result.
    """
    print("Ada Lovelace's Note G - Bernoulli Numbers Calculation")
    print("=" * 70)
    print("\nOriginal Context:")
    print("  Published: September 1843")
    print("  Machine: Babbage's Analytical Engine (never built)")
    print("  Variables: 9 columns (V1-V9)")
    print("  Operations: 25 steps (Load, Add, Subtract, Multiply, Divide)")
    print()

    # Compute B_8 using Ada's formula
    n = 8
    print(f"Computing B_{n} using Ada's recursive formula...")
    print()

    b_8 = bernoulli_number(n)

    print(f"Result: B_{n} = {b_8}")
    print(f"Decimal: {float(b_8):.15f}")
    print()

    print("Verification - First 10 Bernoulli numbers:")
    print_bernoulli_table(10)


if __name__ == "__main__":
    # Run Ada's historical example
    ada_lovelace_note_g_example()
