#!/usr/bin/env python3
"""
Egyptian Multiplication Algorithm (c. 2000 BCE)

Historical Context:
This algorithm appears in the Rhind Mathematical Papyrus (c. 1650 BCE), also known
as the Ahmes Papyrus, and the Moscow Mathematical Papyrus (c. 1890 BCE). The ancient
Egyptians developed this method because their number system made doubling and halving
particularly easy - they had special hieroglyphs for powers of 2.

The algorithm is also known as:
- Russian Peasant Multiplication (rediscovered in Russia)
- Ethiopian Multiplication (still taught in Ethiopian schools)
- Binary Multiplication (the principle behind modern CPU multipliers)

Mathematical Insight:
The algorithm works because any number can be expressed as a sum of powers of 2
(binary representation). For example: 13 = 8 + 4 + 1 = 2³ + 2² + 2⁰
So: 13 × 17 = (8 + 4 + 1) × 17 = 8×17 + 4×17 + 1×17

Time Complexity: O(log n) where n is the smaller multiplicand
Space Complexity: O(log n) for storing intermediate values
"""

import time
from typing import List, Tuple, Optional
from dataclasses import dataclass


@dataclass
class CalculationStep:
    """Represents one step in the Egyptian multiplication process"""
    left: int
    right: int
    included: bool
    running_sum: int


def egyptian_multiply(a: int, b: int) -> int:
    """
    Basic Egyptian multiplication for positive integers.

    Args:
        a: First multiplicand
        b: Second multiplicand

    Returns:
        Product of a and b

    Examples:
        >>> egyptian_multiply(13, 17)
        221
        >>> egyptian_multiply(7, 8)
        56
    """
    # Handle edge cases
    if a == 0 or b == 0:
        return 0
    if a == 1:
        return b
    if b == 1:
        return a

    result = 0
    left, right = a, b

    # Ancient algorithm: double and halve
    while right > 0:
        # If right column is odd, add left column to result
        if right & 1:  # Check if odd using bitwise AND
            result += left

        # Double the left column
        left <<= 1  # Equivalent to left * 2

        # Halve the right column (integer division)
        right >>= 1  # Equivalent to right // 2

    return result


def egyptian_multiply_extended(a: int, b: int) -> int:
    """
    Extended version handling negative numbers.
    Ancient Egyptians didn't have negative numbers, but we extend the algorithm.

    Args:
        a: First multiplicand (can be negative)
        b: Second multiplicand (can be negative)

    Returns:
        Product of a and b
    """
    # Track sign
    sign = 1
    if a < 0:
        sign = -sign
        a = -a
    if b < 0:
        sign = -sign
        b = -b

    return sign * egyptian_multiply(a, b)


def egyptian_multiply_verbose(a: int, b: int) -> Tuple[int, List[CalculationStep]]:
    """
    Verbose version that returns all calculation steps.

    Args:
        a: First multiplicand
        b: Second multiplicand

    Returns:
        Tuple of (result, list of calculation steps)
    """
    steps = []
    result = 0
    left, right = a, b

    while right > 0:
        included = bool(right & 1)
        if included:
            result += left

        steps.append(CalculationStep(
            left=left,
            right=right,
            included=included,
            running_sum=result
        ))

        left <<= 1
        right >>= 1

    return result, steps


def display_calculation(a: int, b: int) -> None:
    """
    Display the Egyptian multiplication process in a table format.
    """
    print(f"\nEgyptian Multiplication of {a} × {b}:")
    print(f"{'Left':<10} {'Right':<10} {'Include?':<10} {'Sum':<10}")
    print("-" * 40)

    result, steps = egyptian_multiply_verbose(a, b)

    for step in steps:
        include_str = "Yes" if step.included else "No"
        sum_str = str(step.running_sum) if step.included else ""
        print(f"{step.left:<10} {step.right:<10} {include_str:<10} {sum_str:<10}")

    print(f"\nResult: {result}")
    print(f"Verification: {a} × {b} = {a * b} (modern)")


def egyptian_multiply_recursive(a: int, b: int) -> int:
    """
    Recursive implementation of Egyptian multiplication.
    This shows the algorithm's inherent recursive structure.

    Args:
        a: First multiplicand
        b: Second multiplicand

    Returns:
        Product of a and b
    """
    # Base cases
    if b == 0:
        return 0
    if b == 1:
        return a

    # Recursive case
    if b & 1:  # If b is odd
        return a + egyptian_multiply_recursive(a << 1, b >> 1)
    else:  # If b is even
        return egyptian_multiply_recursive(a << 1, b >> 1)


def benchmark_multiplication() -> None:
    """
    Compare performance of Egyptian multiplication with modern multiplication.
    """
    iterations = 100000
    test_cases = [
        (13, 17, "Small numbers"),
        (127, 42, "Medium numbers"),
        (999, 888, "Large numbers"),
        (12345, 6789, "Very large numbers"),
    ]

    print("\n=== Performance Comparison ===")
    print(f"Iterations per test: {iterations:,}\n")

    for a, b, description in test_cases:
        # Benchmark Egyptian multiplication
        start = time.perf_counter()
        for _ in range(iterations):
            _ = egyptian_multiply(a, b)
        egyptian_time = time.perf_counter() - start

        # Benchmark modern multiplication
        start = time.perf_counter()
        for _ in range(iterations):
            _ = a * b
        modern_time = time.perf_counter() - start

        print(f"{description} ({a} × {b}):")
        print(f"  Egyptian: {egyptian_time:.6f} seconds")
        print(f"  Modern:   {modern_time:.6f} seconds")
        print(f"  Ratio:    {egyptian_time / modern_time:.2f}x slower\n")


def analyze_binary_connection(n: int) -> None:
    """
    Show the connection between Egyptian multiplication and binary representation.
    """
    print(f"\n=== Binary Analysis of {n} ===")
    print(f"Decimal: {n}")
    print(f"Binary:  {bin(n)}")

    powers = []
    power = 0
    temp = n

    while temp > 0:
        if temp & 1:
            powers.append(2 ** power)
        power += 1
        temp >>= 1

    print(f"As sum of powers of 2: {' + '.join(map(str, reversed(powers)))}")
    print(f"This is why Egyptian multiplication works - it decomposes")
    print(f"numbers into powers of 2, just like binary representation!")


def historical_examples() -> None:
    """
    Demonstrate historical examples from ancient papyri.
    """
    print("\n=== Historical Examples ===\n")

    print("Example from Rhind Papyrus (Problem 32):")
    print("'A quantity, its third, and its quarter, added together become 2.'")
    print("This involves multiplication: x + x/3 + x/4 = 2")
    display_calculation(12, 7)  # Related calculation

    print("\nExample showing doubling sequence (common in papyri):")
    display_calculation(1, 64)  # Shows pure doubling


def main():
    """
    Main demonstration of Egyptian multiplication algorithm.
    """
    print("=" * 50)
    print("EGYPTIAN MULTIPLICATION ALGORITHM")
    print("From the Rhind Papyrus (c. 1650 BCE)")
    print("=" * 50)

    # Basic demonstration
    print("\n=== Basic Algorithm ===")
    display_calculation(13, 17)

    # Edge cases
    print("\n=== Edge Cases ===")
    print(f"0 × 5 = {egyptian_multiply(0, 5)}")
    print(f"1 × 42 = {egyptian_multiply(1, 42)}")
    print(f"8 × 8 = {egyptian_multiply(8, 8)} (power of 2)")
    print(f"16 × 7 = {egyptian_multiply(16, 7)} (power of 2)")

    # Negative numbers (modern extension)
    print("\n=== Extended Algorithm (with negatives) ===")
    print(f"-13 × 17 = {egyptian_multiply_extended(-13, 17)}")
    print(f"13 × -17 = {egyptian_multiply_extended(13, -17)}")
    print(f"-13 × -17 = {egyptian_multiply_extended(-13, -17)}")

    # Recursive version
    print("\n=== Recursive Implementation ===")
    print(f"7 × 9 = {egyptian_multiply_recursive(7, 9)}")
    print(f"25 × 11 = {egyptian_multiply_recursive(25, 11)}")

    # Show binary connection
    analyze_binary_connection(13)

    # Historical examples
    historical_examples()

    # Performance comparison
    benchmark_multiplication()

    print("\n=== Historical and Mathematical Significance ===")
    print("""
The Egyptian multiplication algorithm is remarkable for several reasons:

1. BINARY DECOMPOSITION: Ancient Egyptians discovered that any multiplication
   can be reduced to doubling and addition, essentially using binary
   representation 3,500 years before Leibniz formalized binary numbers.

2. ALGORITHMIC THINKING: This is one of the earliest documented algorithms,
   showing that ancient mathematicians thought procedurally about computation.

3. CULTURAL TRANSMISSION: The same algorithm appears independently in Russian,
   Ethiopian, and Indian mathematics, suggesting either cultural exchange or
   convergent discovery of fundamental mathematical truths.

4. MODERN RELEVANCE: This algorithm is essentially how modern CPUs perform
   multiplication at the hardware level using shift and add operations.

5. PEDAGOGICAL VALUE: It demonstrates that complex operations can always be
   decomposed into simpler ones - a fundamental principle of computer science.
    """)


if __name__ == "__main__":
    main()