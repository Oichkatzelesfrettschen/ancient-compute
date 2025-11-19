/**
 * Bernoulli Numbers Algorithm - Ada Lovelace's Note G (1843)
 * C Implementation with Rational Arithmetic
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
 * C Implementation Notes:
 *   - Uses rational number representation (numerator/denominator pairs)
 *   - GCD-based fraction simplification to prevent overflow
 *   - Memoization with fixed-size cache array
 *   - Limited to B_0 through B_30 to avoid overflow (can be extended with BigInt)
 *
 * Compilation:
 *   gcc -std=c11 -O2 -Wall -Wextra -o bernoulli bernoulli_numbers.c -lm
 *
 * Usage:
 *   ./bernoulli
 *
 * Ancient-Compute Integration:
 *   This implementation demonstrates C's low-level control while preserving
 *   the mathematical essence of Ada's original algorithm. Shows how rational
 *   arithmetic must be manually implemented in C vs. Python's fractions.Fraction.
 *
 * @file bernoulli_numbers.c
 * @author ancient-compute contributors
 * @date 2025-11-19
 * @version 1.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#define MAX_BERNOULLI_INDEX 30
#define CACHE_SIZE (MAX_BERNOULLI_INDEX + 1)

/**
 * Rational number representation.
 * Invariant: denominator > 0, gcd(numerator, denominator) = 1 (reduced form)
 */
typedef struct {
    int64_t numerator;
    int64_t denominator;
} Rational;

/**
 * Memoization cache for Bernoulli numbers.
 * cache[i] contains B_i, or {0, 0} if not yet computed.
 */
static Rational bernoulli_cache[CACHE_SIZE];
static bool cache_initialized = false;

/**
 * Compute greatest common divisor using Euclid's algorithm.
 *
 * @param a First integer
 * @param b Second integer
 * @return GCD(a, b)
 */
int64_t gcd(int64_t a, int64_t b) {
    if (a < 0) a = -a;
    if (b < 0) b = -b;

    while (b != 0) {
        int64_t temp = b;
        b = a % b;
        a = temp;
    }

    return a;
}

/**
 * Compute least common multiple.
 *
 * @param a First integer
 * @param b Second integer
 * @return LCM(a, b)
 */
int64_t lcm(int64_t a, int64_t b) {
    if (a == 0 || b == 0) return 0;
    return (a / gcd(a, b)) * b;
}

/**
 * Create a rational number in reduced form.
 *
 * @param num Numerator
 * @param den Denominator (must be non-zero)
 * @return Reduced rational number
 */
Rational make_rational(int64_t num, int64_t den) {
    if (den == 0) {
        fprintf(stderr, "Error: Division by zero\n");
        exit(1);
    }

    // Ensure denominator is positive
    if (den < 0) {
        num = -num;
        den = -den;
    }

    // Reduce to lowest terms
    if (num == 0) {
        return (Rational){0, 1};
    }

    int64_t g = gcd(num, den);
    return (Rational){num / g, den / g};
}

/**
 * Add two rational numbers.
 *
 * @param a First rational
 * @param b Second rational
 * @return a + b
 */
Rational rational_add(Rational a, Rational b) {
    // a/b + c/d = (ad + bc) / bd
    int64_t num = a.numerator * b.denominator + b.numerator * a.denominator;
    int64_t den = a.denominator * b.denominator;
    return make_rational(num, den);
}

/**
 * Multiply two rational numbers.
 *
 * @param a First rational
 * @param b Second rational
 * @return a * b
 */
Rational rational_multiply(Rational a, Rational b) {
    // (a/b) * (c/d) = (ac) / (bd)
    int64_t num = a.numerator * b.numerator;
    int64_t den = a.denominator * b.denominator;
    return make_rational(num, den);
}

/**
 * Divide two rational numbers.
 *
 * @param a First rational
 * @param b Second rational (must be non-zero)
 * @return a / b
 */
Rational rational_divide(Rational a, Rational b) {
    if (b.numerator == 0) {
        fprintf(stderr, "Error: Division by zero rational\n");
        exit(1);
    }

    // (a/b) / (c/d) = (ad) / (bc)
    int64_t num = a.numerator * b.denominator;
    int64_t den = a.denominator * b.numerator;
    return make_rational(num, den);
}

/**
 * Negate a rational number.
 *
 * @param r Rational number
 * @return -r
 */
Rational rational_negate(Rational r) {
    return make_rational(-r.numerator, r.denominator);
}

/**
 * Convert rational to double for display.
 *
 * @param r Rational number
 * @return Double approximation
 */
double rational_to_double(Rational r) {
    return (double)r.numerator / (double)r.denominator;
}

/**
 * Print a rational number in reduced form.
 *
 * @param r Rational number to print
 */
void print_rational(Rational r) {
    if (r.denominator == 1) {
        printf("%lld", (long long)r.numerator);
    } else {
        printf("%lld/%lld", (long long)r.numerator, (long long)r.denominator);
    }
}

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
int64_t binomial_coefficient(int n, int k) {
    if (k < 0 || k > n) return 0;
    if (k == 0 || k == n) return 1;

    // Optimize: C(n, k) = C(n, n-k)
    if (k > n - k) {
        k = n - k;
    }

    int64_t result = 1;
    for (int i = 0; i < k; i++) {
        result *= (n - i);
        result /= (i + 1);
    }

    return result;
}

/**
 * Initialize the Bernoulli number cache.
 */
void init_cache(void) {
    if (cache_initialized) return;

    for (int i = 0; i <= MAX_BERNOULLI_INDEX; i++) {
        bernoulli_cache[i] = (Rational){0, 0};  // Mark as not computed
    }

    cache_initialized = true;
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
 * @param n Index of Bernoulli number (0 <= n <= MAX_BERNOULLI_INDEX)
 * @return The nth Bernoulli number as a rational
 */
Rational bernoulli_number(int n) {
    if (n < 0 || n > MAX_BERNOULLI_INDEX) {
        fprintf(stderr, "Error: Bernoulli index %d out of range [0, %d]\n",
                n, MAX_BERNOULLI_INDEX);
        exit(1);
    }

    if (!cache_initialized) {
        init_cache();
    }

    // Check cache
    if (bernoulli_cache[n].denominator != 0) {
        return bernoulli_cache[n];
    }

    Rational result;

    // Base case: B_0 = 1
    if (n == 0) {
        result = make_rational(1, 1);
    }
    // Base case: B_1 = -1/2
    else if (n == 1) {
        result = make_rational(-1, 2);
    }
    // All odd Bernoulli numbers (except B_1) are zero
    else if (n > 1 && n % 2 == 1) {
        result = make_rational(0, 1);
    }
    // Recursive formula for even n > 1
    else {
        // B_n = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × B_k]
        Rational summation = make_rational(0, 1);

        for (int k = 0; k < n; k++) {
            int64_t binom_coeff = binomial_coefficient(n + 1, k);
            Rational B_k = bernoulli_number(k);  // Recursive call

            Rational term = rational_multiply(
                make_rational(binom_coeff, 1),
                B_k
            );

            summation = rational_add(summation, term);
        }

        // B_n = -summation / (n + 1)
        Rational divisor = make_rational(n + 1, 1);
        result = rational_negate(rational_divide(summation, divisor));
    }

    // Cache result
    bernoulli_cache[n] = result;

    return result;
}

/**
 * Print a table of Bernoulli numbers.
 *
 * @param max_n Maximum index to display
 */
void print_bernoulli_table(int max_n) {
    printf("n    B_n (exact)              B_n (decimal)\n");
    printf("======================================================================\n");

    for (int i = 0; i <= max_n; i++) {
        Rational b_n = bernoulli_number(i);

        // Only print non-zero values
        if (b_n.numerator != 0) {
            printf("%-4d ", i);
            print_rational(b_n);
            printf("%*s", (int)(24 - strlen("")), "");  // Padding
            printf("%.15f\n", rational_to_double(b_n));
        }
    }
}

/**
 * Demonstrate Ada Lovelace's Note G example.
 *
 * Ada computed B_8 in her 1843 publication as the demonstration
 * of the Analytical Engine's capabilities.
 */
void ada_lovelace_note_g_example(void) {
    printf("Ada Lovelace's Note G - Bernoulli Numbers Calculation\n");
    printf("======================================================================\n\n");

    printf("Original Context:\n");
    printf("  Published: September 1843\n");
    printf("  Machine: Babbage's Analytical Engine (never built)\n");
    printf("  Variables: 9 columns (V1-V9)\n");
    printf("  Operations: 25 steps (Load, Add, Subtract, Multiply, Divide)\n\n");

    // Compute B_8 using Ada's formula
    int n = 8;
    printf("Computing B_%d using Ada's recursive formula...\n\n", n);

    Rational b_8 = bernoulli_number(n);

    printf("Result: B_%d = ", n);
    print_rational(b_8);
    printf("\n");
    printf("Decimal: %.15f\n\n", rational_to_double(b_8));

    printf("Verification - First 10 Bernoulli numbers:\n");
    print_bernoulli_table(10);
}

/**
 * Test suite for Bernoulli number implementation.
 */
void run_tests(void) {
    printf("\n=== Running Bernoulli Number Tests ===\n\n");

    int passed = 0, failed = 0;

    // Test 1: B_0 = 1
    Rational b0 = bernoulli_number(0);
    if (b0.numerator == 1 && b0.denominator == 1) {
        printf("✓ Test 1 passed: B_0 = 1\n");
        passed++;
    } else {
        printf("✗ Test 1 failed: B_0 != 1\n");
        failed++;
    }

    // Test 2: B_1 = -1/2
    Rational b1 = bernoulli_number(1);
    if (b1.numerator == -1 && b1.denominator == 2) {
        printf("✓ Test 2 passed: B_1 = -1/2\n");
        passed++;
    } else {
        printf("✗ Test 2 failed: B_1 != -1/2\n");
        failed++;
    }

    // Test 3: B_2 = 1/6
    Rational b2 = bernoulli_number(2);
    if (b2.numerator == 1 && b2.denominator == 6) {
        printf("✓ Test 3 passed: B_2 = 1/6\n");
        passed++;
    } else {
        printf("✗ Test 3 failed: B_2 != 1/6\n");
        failed++;
    }

    // Test 4: B_3 = 0 (odd number theorem)
    Rational b3 = bernoulli_number(3);
    if (b3.numerator == 0) {
        printf("✓ Test 4 passed: B_3 = 0 (odd number theorem)\n");
        passed++;
    } else {
        printf("✗ Test 4 failed: B_3 != 0\n");
        failed++;
    }

    // Test 5: B_4 = -1/30
    Rational b4 = bernoulli_number(4);
    if (b4.numerator == -1 && b4.denominator == 30) {
        printf("✓ Test 5 passed: B_4 = -1/30\n");
        passed++;
    } else {
        printf("✗ Test 5 failed: B_4 != -1/30\n");
        failed++;
    }

    // Test 6: B_8 = -1/30 (Ada's example)
    Rational b8 = bernoulli_number(8);
    if (b8.numerator == -1 && b8.denominator == 30) {
        printf("✓ Test 6 passed: B_8 = -1/30 (Ada's example)\n");
        passed++;
    } else {
        printf("✗ Test 6 failed: B_8 != -1/30\n");
        failed++;
    }

    // Test 7: B_10 = 5/66
    Rational b10 = bernoulli_number(10);
    if (b10.numerator == 5 && b10.denominator == 66) {
        printf("✓ Test 7 passed: B_10 = 5/66\n");
        passed++;
    } else {
        printf("✗ Test 7 failed: B_10 != 5/66\n");
        failed++;
    }

    // Test 8: B_12 = -691/2730
    Rational b12 = bernoulli_number(12);
    if (b12.numerator == -691 && b12.denominator == 2730) {
        printf("✓ Test 8 passed: B_12 = -691/2730\n");
        passed++;
    } else {
        printf("✗ Test 8 failed: B_12 != -691/2730\n");
        failed++;
    }

    printf("\n=== Test Results ===\n");
    printf("Passed: %d/%d\n", passed, passed + failed);
    printf("Failed: %d/%d\n", failed, passed + failed);

    if (failed == 0) {
        printf("\n✓ All tests passed!\n");
    }
}

/**
 * Main entry point.
 */
int main(void) {
    // Run Ada's historical example
    ada_lovelace_note_g_example();

    // Run test suite
    run_tests();

    return 0;
}
