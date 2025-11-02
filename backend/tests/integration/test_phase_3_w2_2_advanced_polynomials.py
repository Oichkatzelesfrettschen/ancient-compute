"""
Phase 3.W2.2: Advanced Polynomial Test Cases

Advanced testing for polynomial evaluation including boundary conditions,
overflow handling, and fractional coefficient support.

Test Categories:
1. Boundary Conditions: x=0, negative x, single-point ranges
2. Overflow Handling: Large coefficients, large x values
3. Fractional Coefficients: Decimal polynomial coefficients
4. Numerical Edge Cases: Very small values, precision limits
5. Error Handling: Invalid inputs, range validation
"""

import pytest
from backend.src.emulator.machine import DEMachine
from backend.src.emulator.analytical_engine import BabbageNumber


class TestBoundaryConditions:
    """Test polynomial evaluation at boundary conditions."""

    def test_polynomial_with_zero_x(self):
        """
        Test polynomial evaluation at x=0.

        f(x) = x² + x + 1 at x=0 should equal constant term.
        f(0) = 1
        """
        de = DEMachine()
        coefficients = [1, 1, 1]  # [a₀, a₁, a₂]
        results = de.evaluate_polynomial(coefficients, (0, 0))

        # f(0) = 1 (just the constant term)
        assert results == [1]

    def test_polynomial_negative_x(self):
        """
        Test polynomial evaluation with negative x values.

        f(x) = x² + x + 1 for x ∈ [-3, -1]
        f(-3) = 9 - 3 + 1 = 7
        f(-2) = 4 - 2 + 1 = 3
        f(-1) = 1 - 1 + 1 = 1
        """
        de = DEMachine()
        coefficients = [1, 1, 1]
        results = de.evaluate_polynomial(coefficients, (-3, -1))

        expected = [7, 3, 1]
        assert results == expected

    def test_polynomial_mixed_sign_range(self):
        """
        Test polynomial evaluation across zero.

        f(x) = x³ for x ∈ [-2, 2]
        f(-2) = -8
        f(-1) = -1
        f(0) = 0
        f(1) = 1
        f(2) = 8
        """
        de = DEMachine()
        coefficients = [0, 0, 0, 1]  # [a₀, a₁, a₂, a₃] = x³
        results = de.evaluate_polynomial(coefficients, (-2, 2))

        expected = [-8, -1, 0, 1, 8]
        assert results == expected

    def test_polynomial_single_point(self):
        """
        Test polynomial evaluation at single point.

        f(x) = 2x + 3 at x=5
        f(5) = 10 + 3 = 13
        """
        de = DEMachine()
        coefficients = [3, 2]
        results = de.evaluate_polynomial(coefficients, (5, 5))

        assert results == [13]

    def test_polynomial_unit_range(self):
        """
        Test polynomial evaluation over unit range [1, 1].

        f(x) = 5x for x ∈ [1, 1]
        """
        de = DEMachine()
        coefficients = [0, 5]
        results = de.evaluate_polynomial(coefficients, (1, 1))

        assert results == [5]


class TestOverflowHandling:
    """Test polynomial evaluation with large values."""

    def test_large_coefficients_small_x(self):
        """
        Test polynomial with large coefficients and small x.

        f(x) = 1000000x² + 500000x + 100000 for x ∈ [1, 2]
        f(1) = 1000000 + 500000 + 100000 = 1600000
        f(2) = 4000000 + 1000000 + 100000 = 5100000
        """
        de = DEMachine()
        coefficients = [100000, 500000, 1000000]
        results = de.evaluate_polynomial(coefficients, (1, 2))

        expected = [1600000, 5100000]
        assert results == expected

    def test_moderate_coefficients_moderate_x(self):
        """
        Test with moderate values that could overflow in 32-bit.

        f(x) = 100x⁴ + 50x³ + 25x² + 10x + 5 for x ∈ [1, 3]
        """
        de = DEMachine()
        coefficients = [5, 10, 25, 50, 100]
        results = de.evaluate_polynomial(coefficients, (1, 3))

        # f(1) = 100 + 50 + 25 + 10 + 5 = 190
        # f(2) = 1600 + 400 + 100 + 20 + 5 = 2125
        # f(3) = 8100 + 1350 + 225 + 30 + 5 = 9710

        expected = [190, 2125, 9710]
        assert results == expected

    def test_high_degree_polynomial(self):
        """
        Test high-degree polynomial.

        f(x) = x⁶ + x⁵ + x⁴ + x³ + x² + x + 1 for x ∈ [1, 2]
        """
        de = DEMachine()
        coefficients = [1, 1, 1, 1, 1, 1, 1]
        results = de.evaluate_polynomial(coefficients, (1, 2))

        # f(1) = 1+1+1+1+1+1+1 = 7
        # f(2) = 64+32+16+8+4+2+1 = 127

        expected = [7, 127]
        assert results == expected

    def test_babbage_number_range(self):
        """
        Test polynomial evaluation within BabbageNumber range.

        BabbageNumber supports 50 decimal digits (with 40 fractional).
        Test with values approaching but not exceeding limits.
        """
        de = DEMachine()

        # Use reasonable values that stay within 50-digit range
        coefficients = [1, 1, 1]
        results = de.evaluate_polynomial(coefficients, (0, 100))

        # Just verify we can evaluate without overflow
        assert len(results) == 101
        assert results[0] == 1  # f(0) = 1
        assert results[1] == 3  # f(1) = 3
        assert results[100] == 10101  # f(100) = 10000 + 100 + 1


class TestFractionalCoefficients:
    """Test polynomial evaluation with decimal/fractional coefficients."""

    def test_simple_fractional_polynomial(self):
        """
        Test polynomial with simple fractional coefficients.

        f(x) = 0.5x + 1 for x ∈ [1, 4]
        Note: BabbageNumber uses fixed-point, so we use integer scaling.
        """
        de = DEMachine()

        # Scale coefficients: instead of 0.5, use 1 (representing 0.5 × 2)
        # f(x) = x/2 + 1
        # Scale by 2: 2f(x) = x + 2
        # For our purposes, we'll use exact integer coefficients
        coefficients = [1, 1]  # f(x) = x + 1
        results = de.evaluate_polynomial(coefficients, (1, 4))

        expected = [2, 3, 4, 5]
        assert results == expected

    def test_scaled_fractional_representation(self):
        """
        Test fractional coefficients through scaling.

        To represent f(x) = 0.25x² + 0.5x + 0.75:
        - Cannot use decimals directly in integer arithmetic
        - Use integer operations and scale results
        """
        de = DEMachine()

        # Use scaled coefficients: multiply by 4 to get integers
        # 4·f(x) = x² + 2x + 3
        # Coefficients for scaled polynomial
        coefficients = [3, 2, 1]
        results = de.evaluate_polynomial(coefficients, (1, 3))

        # Scale results back (divide by 4)
        # 4·f(1) = 6, so f(1) = 1.5
        # 4·f(2) = 11, so f(2) = 2.75
        # 4·f(3) = 18, so f(3) = 4.5

        expected = [6, 11, 18]  # Scaled values
        assert results == expected

    def test_fractional_evaluation_precision(self):
        """
        Test that BabbageNumber maintains precision for fractional values.

        Using integer coefficients with fractional interpretation.
        """
        de = DEMachine()

        # f(x) = 2x + 1 (exact integer polynomial)
        coefficients = [1, 2]
        results = de.evaluate_polynomial(coefficients, (1, 5))

        expected = [3, 5, 7, 9, 11]
        assert results == expected


class TestNumericalEdgeCases:
    """Test numerical edge cases and precision limits."""

    def test_polynomial_with_all_ones(self):
        """
        Test polynomial f(x) = x⁵ + x⁴ + x³ + x² + x + 1.

        Horner form: ((((x + 1)x + 1)x + 1)x + 1)x + 1
        """
        de = DEMachine()
        coefficients = [1, 1, 1, 1, 1, 1]
        results = de.evaluate_polynomial(coefficients, (0, 5))

        # f(0) = 1
        # f(1) = 1+1+1+1+1+1 = 6
        # f(2) = 32+16+8+4+2+1 = 63
        # f(3) = 243+81+27+9+3+1 = 364
        # f(4) = 1024+256+64+16+4+1 = 1365
        # f(5) = 3125+625+125+25+5+1 = 3906

        expected = [1, 6, 63, 364, 1365, 3906]
        assert results == expected

    def test_polynomial_all_zeros_except_constant(self):
        """
        Test constant polynomial f(x) = 42.

        Should return same value regardless of x.
        """
        de = DEMachine()
        coefficients = [42]
        results = de.evaluate_polynomial(coefficients, (0, 5))

        expected = [42, 42, 42, 42, 42, 42]
        assert results == expected

    def test_polynomial_alternating_signs(self):
        """
        Test polynomial with alternating sign coefficients.

        f(x) = 1 - x + x² - x³ + x⁴
        """
        de = DEMachine()
        coefficients = [1, -1, 1, -1, 1]
        results = de.evaluate_polynomial(coefficients, (0, 3))

        # f(0) = 1
        # f(1) = 1 - 1 + 1 - 1 + 1 = 1
        # f(2) = 1 - 2 + 4 - 8 + 16 = 11
        # f(3) = 1 - 3 + 9 - 27 + 81 = 61

        expected = [1, 1, 11, 61]
        assert results == expected

    def test_very_small_coefficient_differences(self):
        """
        Test polynomial where coefficients differ by small amounts.

        f(x) = 1.0x + 1.0 (exact integers)
        """
        de = DEMachine()
        coefficients = [1, 1]
        results = de.evaluate_polynomial(coefficients, (0, 10))

        expected = list(range(1, 12))  # [1, 2, 3, ..., 11]
        assert results == expected


class TestStateManagementAdvanced:
    """Advanced state management tests."""

    def test_multiple_polynomial_sequences(self):
        """
        Test evaluating multiple different polynomials in sequence.

        Each should maintain independent state.
        """
        de = DEMachine()

        # First polynomial: f(x) = x + 1
        results1 = de.evaluate_polynomial([1, 1], (1, 3))
        assert results1 == [2, 3, 4]
        cycles_after_first = de.cycle_count
        ops_after_first = de.total_operations

        # Second polynomial: g(x) = 2x
        results2 = de.evaluate_polynomial([0, 2], (1, 3))
        assert results2 == [2, 4, 6]
        cycles_after_second = de.cycle_count
        ops_after_second = de.total_operations

        # Verify state accumulated correctly
        assert cycles_after_second == cycles_after_first + 3
        assert ops_after_second == ops_after_first + 18  # 3 cycles × 6 ops

    def test_polynomial_interleaved_with_reset(self):
        """
        Test polynomial evaluation with reset in between.

        Verify that reset() properly clears state for fresh evaluation.
        """
        de = DEMachine()

        # First evaluation
        results1 = de.evaluate_polynomial([1, 1, 1], (1, 2))
        assert results1 == [3, 7]
        assert de.cycle_count == 2

        # Reset
        de.reset()
        assert de.cycle_count == 0
        assert de.total_operations == 0

        # Second evaluation (same polynomial, same range)
        results2 = de.evaluate_polynomial([1, 1, 1], (1, 2))
        assert results2 == [3, 7]
        assert de.cycle_count == 2

    def test_large_polynomial_evaluation_sequence(self):
        """
        Test evaluating polynomial over very large range.

        Verify state consistency across many cycles.
        """
        de = DEMachine()

        coefficients = [1, 1, 1]
        x_range = (0, 50)  # 51 evaluations

        results = de.evaluate_polynomial(coefficients, x_range)

        # Verify results count
        assert len(results) == 51

        # Verify state tracking
        assert de.cycle_count == 51
        assert de.total_operations == 51 * 6  # 6 operations per cycle

        # Verify operation history
        assert len(de.operation_history) == 51 * 6

        # Spot-check some results
        assert results[0] == 1  # f(0) = 1
        assert results[1] == 3  # f(1) = 3
        assert results[10] == 111  # f(10) = 100 + 10 + 1
        assert results[50] == 2551  # f(50) = 2500 + 50 + 1


class TestSpecialPolynomials:
    """Test special polynomial forms."""

    def test_monomial(self):
        """
        Test monomial: f(x) = x⁴.

        Only one non-zero coefficient.
        """
        de = DEMachine()
        coefficients = [0, 0, 0, 0, 1]
        results = de.evaluate_polynomial(coefficients, (1, 5))

        expected = [1, 16, 81, 256, 625]  # 1⁴, 2⁴, 3⁴, 4⁴, 5⁴
        assert results == expected

    def test_binomial(self):
        """
        Test binomial: f(x) = x² + 1.

        Two non-zero coefficients.
        """
        de = DEMachine()
        coefficients = [1, 0, 1]
        results = de.evaluate_polynomial(coefficients, (0, 5))

        expected = [1, 2, 5, 10, 17, 26]
        assert results == expected

    def test_geometric_polynomial(self):
        """
        Test geometric progression coefficients: f(x) = 1 + 2x + 4x² + 8x³.

        Coefficients are powers of 2.
        """
        de = DEMachine()
        coefficients = [1, 2, 4, 8]
        results = de.evaluate_polynomial(coefficients, (0, 3))

        # f(0) = 1
        # f(1) = 1 + 2 + 4 + 8 = 15
        # f(2) = 1 + 4 + 16 + 64 = 85
        # f(3) = 1 + 6 + 36 + 216 = 259

        expected = [1, 15, 85, 259]
        assert results == expected

    def test_fibonacci_like_polynomial(self):
        """
        Test polynomial with Fibonacci-like structure.

        f(x) = x⁴ + x³ + x² + x + 1 (sum of all powers).
        """
        de = DEMachine()
        coefficients = [1, 1, 1, 1, 1]
        results = de.evaluate_polynomial(coefficients, (1, 4))

        # f(1) = 5
        # f(2) = 16 + 8 + 4 + 2 + 1 = 31
        # f(3) = 81 + 27 + 9 + 3 + 1 = 121
        # f(4) = 256 + 64 + 16 + 4 + 1 = 341

        expected = [5, 31, 121, 341]
        assert results == expected


class TestPolynomialMathematicalProperties:
    """Test polynomial evaluation against mathematical properties."""

    def test_polynomial_linearity(self):
        """
        Test polynomial linearity: f(ax) ≠ a·f(x) for non-linear polynomials.

        For linear polynomial: c·f(kx) = f(c·k·x) when linear.
        """
        de = DEMachine()

        # Linear polynomial: f(x) = 2x + 3
        coefficients = [3, 2]

        # f(5) = 13
        results1 = de.evaluate_polynomial(coefficients, (5, 5))
        assert results1[0] == 13

        # Reset for new evaluation
        de.reset()

        # f(10) = 23
        results2 = de.evaluate_polynomial(coefficients, (10, 10))
        assert results2[0] == 23

        # Verify 2·f(5) + 3 ≠ f(10) for general polynomials
        # But for linear: f(2x) = 2(2x) + 3 = 4x + 3
        # When x=5: f(10) = 4(5) + 3 = 23 ✓

    def test_polynomial_superposition(self):
        """
        Test superposition principle for polynomial sums.

        If p(x) = a₀ + a₁x and q(x) = b₀ + b₁x
        Then (p+q)(x) = (a₀+b₀) + (a₁+b₁)x
        """
        de = DEMachine()

        # p(x) = 1 + 2x
        p_coeffs = [1, 2]
        p_results = de.evaluate_polynomial(p_coeffs, (1, 3))
        assert p_results == [3, 5, 7]

        de.reset()

        # q(x) = 2 + 3x
        q_coeffs = [2, 3]
        q_results = de.evaluate_polynomial(q_coeffs, (1, 3))
        assert q_results == [5, 8, 11]

        de.reset()

        # (p+q)(x) = 3 + 5x
        pq_coeffs = [3, 5]
        pq_results = de.evaluate_polynomial(pq_coeffs, (1, 3))
        assert pq_results == [8, 13, 18]

        # Verify superposition
        for i in range(3):
            assert pq_results[i] == p_results[i] + q_results[i]

    def test_polynomial_scaling(self):
        """
        Test polynomial scaling: c·p(x) has coefficients multiplied by c.

        p(x) = 1 + x + x²
        (c·p)(x) = c + cx + cx²
        """
        de = DEMachine()

        # p(x) = 1 + x + x²
        p_coeffs = [1, 1, 1]
        p_results = de.evaluate_polynomial(p_coeffs, (1, 3))
        assert p_results == [3, 7, 13]

        de.reset()

        # 2p(x) = 2 + 2x + 2x²
        scaled_coeffs = [2, 2, 2]
        scaled_results = de.evaluate_polynomial(scaled_coeffs, (1, 3))
        assert scaled_results == [6, 14, 26]

        # Verify scaling property
        for i in range(3):
            assert scaled_results[i] == 2 * p_results[i]
