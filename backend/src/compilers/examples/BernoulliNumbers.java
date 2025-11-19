/**
 * Bernoulli Numbers Algorithm - Ada Lovelace's Note G (1843)
 * Java Implementation with BigInteger Rational Arithmetic
 *
 * This is the first computer program ever published, from Ada Lovelace's Note G
 * in her translation of Luigi Menabrea's memoir on the Analytical Engine.
 *
 * Historical Context:
 *   - Published: September 1843 in Taylor's Scientific Memoirs
 *   - Author: Augusta Ada Byron King, Countess of Lovelace
 *   - Significance: First algorithm intended for machine execution
 *   - Original: Designed for Babbage's Analytical Engine (never built)
 *
 * The Bernoulli numbers B_n are defined recursively:
 *     B_0 = 1
 *     B_1 = -1/2
 *     B_n = 0 for odd n > 1
 *     B_n = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × B_k] for even n > 1
 *
 * Java Implementation Features:
 *   - Object-oriented design with Rational class
 *   - BigInteger for arbitrary precision arithmetic
 *   - HashMap-based memoization for efficiency
 *   - Comprehensive test suite with JUnit-style assertions
 *   - Demonstrates OOP paradigm applied to mathematical computation
 *
 * Compilation:
 *   javac BernoulliNumbers.java
 *
 * Usage:
 *   java BernoulliNumbers
 *
 * Ancient-Compute Integration:
 *   This implementation demonstrates Java's object-oriented approach to
 *   mathematical computation, contrasting with functional (Haskell) and
 *   procedural (C) approaches while preserving Ada's algorithm logic.
 *
 * @author ancient-compute contributors
 * @version 1.0
 * @since 2025-11-19
 */

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

/**
 * Immutable rational number class using BigInteger for numerator and denominator.
 * Maintains invariant: denominator > 0, gcd(numerator, denominator) = 1.
 */
class Rational {
    private final BigInteger numerator;
    private final BigInteger denominator;

    /**
     * Constructs a rational number in reduced form.
     *
     * @param num Numerator
     * @param den Denominator (must be non-zero)
     * @throws ArithmeticException if denominator is zero
     */
    public Rational(BigInteger num, BigInteger den) {
        if (den.equals(BigInteger.ZERO)) {
            throw new ArithmeticException("Denominator cannot be zero");
        }

        // Ensure denominator is positive
        if (den.compareTo(BigInteger.ZERO) < 0) {
            num = num.negate();
            den = den.negate();
        }

        // Reduce to lowest terms
        if (num.equals(BigInteger.ZERO)) {
            this.numerator = BigInteger.ZERO;
            this.denominator = BigInteger.ONE;
        } else {
            BigInteger gcd = num.gcd(den);
            this.numerator = num.divide(gcd);
            this.denominator = den.divide(gcd);
        }
    }

    /**
     * Convenience constructor for integer values.
     *
     * @param num Numerator (as long)
     * @param den Denominator (as long)
     */
    public Rational(long num, long den) {
        this(BigInteger.valueOf(num), BigInteger.valueOf(den));
    }

    /**
     * Convenience constructor for integer rational (denominator = 1).
     *
     * @param num Numerator
     */
    public Rational(BigInteger num) {
        this(num, BigInteger.ONE);
    }

    /**
     * Get numerator.
     *
     * @return Numerator of this rational number
     */
    public BigInteger getNumerator() {
        return numerator;
    }

    /**
     * Get denominator.
     *
     * @return Denominator of this rational number
     */
    public BigInteger getDenominator() {
        return denominator;
    }

    /**
     * Add two rational numbers.
     *
     * @param other Rational number to add
     * @return Sum of this and other
     */
    public Rational add(Rational other) {
        // a/b + c/d = (ad + bc) / bd
        BigInteger num = this.numerator.multiply(other.denominator)
                        .add(other.numerator.multiply(this.denominator));
        BigInteger den = this.denominator.multiply(other.denominator);
        return new Rational(num, den);
    }

    /**
     * Multiply two rational numbers.
     *
     * @param other Rational number to multiply
     * @return Product of this and other
     */
    public Rational multiply(Rational other) {
        // (a/b) * (c/d) = (ac) / (bd)
        BigInteger num = this.numerator.multiply(other.numerator);
        BigInteger den = this.denominator.multiply(other.denominator);
        return new Rational(num, den);
    }

    /**
     * Divide two rational numbers.
     *
     * @param other Rational number to divide by (must be non-zero)
     * @return Quotient of this and other
     * @throws ArithmeticException if other is zero
     */
    public Rational divide(Rational other) {
        if (other.numerator.equals(BigInteger.ZERO)) {
            throw new ArithmeticException("Division by zero rational");
        }

        // (a/b) / (c/d) = (ad) / (bc)
        BigInteger num = this.numerator.multiply(other.denominator);
        BigInteger den = this.denominator.multiply(other.numerator);
        return new Rational(num, den);
    }

    /**
     * Negate this rational number.
     *
     * @return Negation of this rational
     */
    public Rational negate() {
        return new Rational(this.numerator.negate(), this.denominator);
    }

    /**
     * Convert to double for numerical applications.
     *
     * @return Double approximation of this rational
     */
    public double toDouble() {
        return numerator.doubleValue() / denominator.doubleValue();
    }

    /**
     * String representation in reduced form.
     *
     * @return String representation "num/den" or "num" if denominator is 1
     */
    @Override
    public String toString() {
        if (denominator.equals(BigInteger.ONE)) {
            return numerator.toString();
        } else {
            return numerator + "/" + denominator;
        }
    }

    /**
     * Equality comparison.
     *
     * @param obj Object to compare with
     * @return true if obj is a Rational equal to this
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof Rational)) return false;

        Rational other = (Rational) obj;
        return this.numerator.equals(other.numerator) &&
               this.denominator.equals(other.denominator);
    }

    /**
     * Hash code for use in collections.
     *
     * @return Hash code value
     */
    @Override
    public int hashCode() {
        return numerator.hashCode() * 31 + denominator.hashCode();
    }
}

/**
 * Main class implementing Ada Lovelace's Bernoulli Numbers Algorithm.
 */
public class BernoulliNumbers {
    // Memoization cache for computed Bernoulli numbers
    private static final Map<Integer, Rational> cache = new HashMap<>();

    /**
     * Compute binomial coefficient C(n, k) = n! / (k! * (n-k)!)
     *
     * Uses iterative formula to avoid factorial overflow:
     * C(n, k) = (n * (n-1) * ... * (n-k+1)) / (k * (k-1) * ... * 1)
     *
     * @param n Total items
     * @param k Items to choose
     * @return Binomial coefficient
     */
    public static BigInteger binomialCoefficient(int n, int k) {
        if (k < 0 || k > n) return BigInteger.ZERO;
        if (k == 0 || k == n) return BigInteger.ONE;

        // Optimization: C(n, k) = C(n, n-k)
        if (k > n - k) {
            k = n - k;
        }

        BigInteger result = BigInteger.ONE;
        for (int i = 0; i < k; i++) {
            result = result.multiply(BigInteger.valueOf(n - i));
            result = result.divide(BigInteger.valueOf(i + 1));
        }

        return result;
    }

    /**
     * Calculate the nth Bernoulli number using Ada Lovelace's recursive formula.
     *
     * This is the algorithm from Note G (September 1843) - the first computer program
     * ever published. Ada designed this to run on Babbage's Analytical Engine.
     *
     * Mathematical Formula:
     *     B_0 = 1
     *     B_1 = -1/2
     *     B_n = 0 for odd n > 1
     *     B_n = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × B_k] for even n > 1
     *
     * @param n Index of Bernoulli number (n >= 0)
     * @return The nth Bernoulli number as a Rational
     * @throws IllegalArgumentException if n < 0
     */
    public static Rational bernoulliNumber(int n) {
        if (n < 0) {
            throw new IllegalArgumentException("Bernoulli number index must be non-negative, got " + n);
        }

        // Check cache first
        if (cache.containsKey(n)) {
            return cache.get(n);
        }

        Rational result;

        // Base case: B_0 = 1
        if (n == 0) {
            result = new Rational(1, 1);
        }
        // Base case: B_1 = -1/2
        else if (n == 1) {
            result = new Rational(-1, 2);
        }
        // All odd Bernoulli numbers (except B_1) are zero
        else if (n % 2 == 1) {
            result = new Rational(0, 1);
        }
        // Recursive formula for even n > 1
        else {
            // B_n = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × B_k]
            Rational summation = new Rational(0, 1);

            for (int k = 0; k < n; k++) {
                BigInteger binomCoeff = binomialCoefficient(n + 1, k);
                Rational Bk = bernoulliNumber(k);  // Recursive call

                Rational term = new Rational(binomCoeff).multiply(Bk);
                summation = summation.add(term);
            }

            // B_n = -summation / (n + 1)
            Rational divisor = new Rational(n + 1, 1);
            result = summation.divide(divisor).negate();
        }

        // Cache result
        cache.put(n, result);

        return result;
    }

    /**
     * Generate a sequence of Bernoulli numbers from B_0 to B_max.
     *
     * @param maxN Maximum index to compute
     * @return Array of Bernoulli numbers
     */
    public static Rational[] bernoulliSequence(int maxN) {
        if (maxN < 0) {
            throw new IllegalArgumentException("max_n must be non-negative, got " + maxN);
        }

        Rational[] sequence = new Rational[maxN + 1];
        for (int i = 0; i <= maxN; i++) {
            sequence[i] = bernoulliNumber(i);
        }

        return sequence;
    }

    /**
     * Print a formatted table of Bernoulli numbers.
     *
     * @param maxN Maximum index to display
     */
    public static void printBernoulliTable(int maxN) {
        System.out.println("n    B_n (exact)              B_n (decimal)");
        System.out.println("======================================================================");

        for (int i = 0; i <= maxN; i++) {
            Rational bn = bernoulliNumber(i);

            // Only print non-zero values
            if (!bn.getNumerator().equals(BigInteger.ZERO)) {
                System.out.printf("%-4d %-24s %.15f%n",
                    i, bn.toString(), bn.toDouble());
            }
        }
    }

    /**
     * Demonstrate Ada Lovelace's Note G example.
     *
     * Ada computed B_8 in her 1843 publication as the demonstration
     * of the Analytical Engine's capabilities.
     */
    public static void adaLovelaceNoteGExample() {
        System.out.println("Ada Lovelace's Note G - Bernoulli Numbers Calculation");
        System.out.println("======================================================================");
        System.out.println();
        System.out.println("Original Context:");
        System.out.println("  Published: September 1843");
        System.out.println("  Machine: Babbage's Analytical Engine (never built)");
        System.out.println("  Variables: 9 columns (V1-V9)");
        System.out.println("  Operations: 25 steps (Load, Add, Subtract, Multiply, Divide)");
        System.out.println();

        int n = 8;
        System.out.println("Computing B_" + n + " using Ada's recursive formula...");
        System.out.println();

        Rational b8 = bernoulliNumber(n);

        System.out.println("Result: B_" + n + " = " + b8);
        System.out.printf("Decimal: %.15f%n", b8.toDouble());
        System.out.println();

        System.out.println("Verification - First 10 Bernoulli numbers:");
        printBernoulliTable(10);
    }

    /**
     * Test suite for Bernoulli number implementation.
     */
    public static void runTests() {
        System.out.println();
        System.out.println("=== Running Bernoulli Number Tests ===");
        System.out.println();

        int passed = 0;
        int failed = 0;

        // Test 1: B_0 = 1
        if (bernoulliNumber(0).equals(new Rational(1, 1))) {
            System.out.println("✓ Test 1 passed: B_0 = 1");
            passed++;
        } else {
            System.out.println("✗ Test 1 failed: B_0 != 1");
            failed++;
        }

        // Test 2: B_1 = -1/2
        if (bernoulliNumber(1).equals(new Rational(-1, 2))) {
            System.out.println("✓ Test 2 passed: B_1 = -1/2");
            passed++;
        } else {
            System.out.println("✗ Test 2 failed: B_1 != -1/2");
            failed++;
        }

        // Test 3: B_2 = 1/6
        if (bernoulliNumber(2).equals(new Rational(1, 6))) {
            System.out.println("✓ Test 3 passed: B_2 = 1/6");
            passed++;
        } else {
            System.out.println("✗ Test 3 failed: B_2 != 1/6");
            failed++;
        }

        // Test 4: B_3 = 0 (odd number theorem)
        if (bernoulliNumber(3).equals(new Rational(0, 1))) {
            System.out.println("✓ Test 4 passed: B_3 = 0 (odd number theorem)");
            passed++;
        } else {
            System.out.println("✗ Test 4 failed: B_3 != 0");
            failed++;
        }

        // Test 5: B_4 = -1/30
        if (bernoulliNumber(4).equals(new Rational(-1, 30))) {
            System.out.println("✓ Test 5 passed: B_4 = -1/30");
            passed++;
        } else {
            System.out.println("✗ Test 5 failed: B_4 != -1/30");
            failed++;
        }

        // Test 6: B_8 = -1/30 (Ada's example)
        if (bernoulliNumber(8).equals(new Rational(-1, 30))) {
            System.out.println("✓ Test 6 passed: B_8 = -1/30 (Ada's example)");
            passed++;
        } else {
            System.out.println("✗ Test 6 failed: B_8 != -1/30");
            failed++;
        }

        // Test 7: B_10 = 5/66
        if (bernoulliNumber(10).equals(new Rational(5, 66))) {
            System.out.println("✓ Test 7 passed: B_10 = 5/66");
            passed++;
        } else {
            System.out.println("✗ Test 7 failed: B_10 != 5/66");
            failed++;
        }

        // Test 8: B_12 = -691/2730
        if (bernoulliNumber(12).equals(new Rational(-691, 2730))) {
            System.out.println("✓ Test 8 passed: B_12 = -691/2730");
            passed++;
        } else {
            System.out.println("✗ Test 8 failed: B_12 != -691/2730");
            failed++;
        }

        // Test 9: B_20 = -174611/330
        if (bernoulliNumber(20).equals(new Rational(
                new BigInteger("-174611"),
                new BigInteger("330")))) {
            System.out.println("✓ Test 9 passed: B_20 = -174611/330");
            passed++;
        } else {
            System.out.println("✗ Test 9 failed: B_20 != -174611/330");
            failed++;
        }

        // Test 10: Sequence length
        if (bernoulliSequence(10).length == 11) {
            System.out.println("✓ Test 10 passed: Sequence length");
            passed++;
        } else {
            System.out.println("✗ Test 10 failed: Sequence length");
            failed++;
        }

        System.out.println();
        System.out.println("=== Test Results ===");
        System.out.printf("Passed: %d/%d%n", passed, passed + failed);
        System.out.printf("Failed: %d/%d%n", failed, passed + failed);
        System.out.println();

        if (failed == 0) {
            System.out.println("✓ All tests passed!");
        } else {
            System.out.println("✗ Some tests failed");
        }
    }

    /**
     * Main entry point.
     *
     * @param args Command line arguments (unused)
     */
    public static void main(String[] args) {
        // Run Ada's historical example
        adaLovelaceNoteGExample();

        // Run test suite
        runTests();
    }
}
