/**
 * Egyptian Multiplication Algorithm (c. 2000 BCE)
 *
 * Historical Context:
 * This algorithm appears in the Rhind Mathematical Papyrus (c. 1650 BCE) and the
 * Moscow Mathematical Papyrus (c. 1890 BCE). Ancient Egyptians used this method
 * because they primarily worked with doubling and halving operations, which could
 * be performed easily using their hieroglyphic numeral system.
 *
 * The algorithm demonstrates the ancient understanding that multiplication can be
 * decomposed into a series of doublings (powers of 2), prefiguring binary
 * representation by millennia.
 *
 * Algorithm:
 * 1. Create two columns starting with multiplicands a and b
 * 2. Double left column, halve right column (integer division)
 * 3. Continue until right column reaches 1
 * 4. Sum left column values where right column is odd
 *
 * Time Complexity: O(log n) where n is the smaller multiplicand
 * Space Complexity: O(log n) for storing intermediate values
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/**
 * Egyptian multiplication for positive integers
 * @param a First multiplicand
 * @param b Second multiplicand
 * @return Product of a and b
 */
int egyptian_multiply(int a, int b) {
    // Handle edge cases
    if (a == 0 || b == 0) return 0;
    if (a == 1) return b;
    if (b == 1) return a;

    int result = 0;
    int left = a;
    int right = b;

    // Ancient algorithm: double and halve
    while (right > 0) {
        // If right column is odd, add left column to result
        if (right & 1) {  // Ancient Egyptians checked oddness differently
            result += left;
        }

        // Double the left column (shift left in binary)
        left = left << 1;  // Equivalent to left * 2

        // Halve the right column (shift right in binary)
        right = right >> 1;  // Equivalent to right / 2
    }

    return result;
}

/**
 * Extended version handling negative numbers
 * Ancient Egyptians didn't have negative numbers, but we extend the algorithm
 */
int egyptian_multiply_extended(int a, int b) {
    // Track sign
    int sign = 1;
    if (a < 0) {
        sign = -sign;
        a = -a;
    }
    if (b < 0) {
        sign = -sign;
        b = -b;
    }

    return sign * egyptian_multiply(a, b);
}

/**
 * Verbose version showing the ancient calculation steps
 */
int egyptian_multiply_verbose(int a, int b) {
    printf("\nEgyptian Multiplication of %d × %d:\n", a, b);
    printf("%-10s %-10s %s\n", "Left", "Right", "Include?");
    printf("%-10s %-10s %s\n", "----", "-----", "--------");

    int result = 0;
    int left = a;
    int right = b;

    while (right > 0) {
        char* include = (right & 1) ? "Yes" : "No";
        printf("%-10d %-10d %s", left, right, include);

        if (right & 1) {
            result += left;
            printf(" (sum = %d)", result);
        }
        printf("\n");

        left = left << 1;
        right = right >> 1;
    }

    printf("\nResult: %d\n", result);
    return result;
}

/**
 * Performance comparison with modern multiplication
 */
void benchmark_multiplication() {
    const int iterations = 1000000;
    clock_t start, end;
    double egyptian_time, modern_time;

    // Test with various number sizes
    int test_cases[][2] = {
        {13, 17},
        {127, 42},
        {999, 888},
        {12345, 6789}
    };

    printf("\n=== Performance Comparison ===\n");
    printf("Iterations per test: %d\n\n", iterations);

    for (int t = 0; t < 4; t++) {
        int a = test_cases[t][0];
        int b = test_cases[t][1];

        // Benchmark Egyptian multiplication
        start = clock();
        for (int i = 0; i < iterations; i++) {
            volatile int result = egyptian_multiply(a, b);
            (void)result;  // Prevent optimization
        }
        end = clock();
        egyptian_time = ((double)(end - start)) / CLOCKS_PER_SEC;

        // Benchmark modern multiplication
        start = clock();
        for (int i = 0; i < iterations; i++) {
            volatile int result = a * b;
            (void)result;  // Prevent optimization
        }
        end = clock();
        modern_time = ((double)(end - start)) / CLOCKS_PER_SEC;

        printf("%d × %d:\n", a, b);
        printf("  Egyptian: %.6f seconds\n", egyptian_time);
        printf("  Modern:   %.6f seconds\n", modern_time);
        printf("  Ratio:    %.2fx slower\n\n", egyptian_time / modern_time);
    }
}

int main() {
    printf("=== Ancient Egyptian Multiplication ===\n");
    printf("From the Rhind Papyrus (c. 1650 BCE)\n\n");

    // Demonstrate with historical example
    printf("Historical Example from Rhind Papyrus:\n");
    egyptian_multiply_verbose(13, 17);

    // Test edge cases
    printf("\n=== Edge Cases ===\n");
    printf("0 × 5 = %d\n", egyptian_multiply(0, 5));
    printf("1 × 42 = %d\n", egyptian_multiply(1, 42));
    printf("8 × 8 = %d (power of 2)\n", egyptian_multiply(8, 8));

    // Test with negative numbers (modern extension)
    printf("\n=== Extended Algorithm (with negatives) ===\n");
    printf("-13 × 17 = %d\n", egyptian_multiply_extended(-13, 17));
    printf("13 × -17 = %d\n", egyptian_multiply_extended(13, -17));
    printf("-13 × -17 = %d\n", egyptian_multiply_extended(-13, -17));

    // Performance comparison
    benchmark_multiplication();

    printf("\n=== Historical Significance ===\n");
    printf("The Egyptian multiplication algorithm demonstrates that ancient\n");
    printf("mathematicians understood binary decomposition millennia before\n");
    printf("the invention of binary numbers. This method using only doubling\n");
    printf("and addition operations is remarkably similar to how modern\n");
    printf("computers perform multiplication at the hardware level.\n");

    return 0;
}