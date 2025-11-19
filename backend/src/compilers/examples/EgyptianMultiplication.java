/**
 * Egyptian Multiplication Algorithm Implementation in Java
 *
 * Historical Context:
 * This algorithm dates back to approximately 2000 BCE and appears in both the
 * Rhind Mathematical Papyrus (c. 1650 BCE) and Moscow Mathematical Papyrus
 * (c. 1890 BCE). The ancient Egyptians developed this method because their
 * hieroglyphic numeral system made doubling particularly easy - they had
 * special symbols for powers of 2.
 *
 * The algorithm is known by many names across cultures:
 * - Egyptian Multiplication (origin)
 * - Russian Peasant Multiplication (rediscovered in Russia)
 * - Ethiopian Multiplication (still taught in Ethiopian schools)
 * - Binary Multiplication (the principle behind modern CPU ALUs)
 *
 * Object-Oriented Design:
 * This Java implementation demonstrates how ancient algorithms can be expressed
 * using modern OOP principles, with classes for encapsulation, interfaces for
 * abstraction, and inheritance for code reuse.
 *
 * @author Ancient Compute Project
 * @version 1.0
 * @since 2025
 */

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.function.BiFunction;

/**
 * Interface for multiplication algorithms
 */
interface MultiplicationAlgorithm {
    int multiply(int a, int b);
    String getName();
}

/**
 * Represents a single step in the Egyptian multiplication process
 */
class MultiplicationStep {
    private final int left;
    private final int right;
    private final boolean included;
    private final int runningSum;

    public MultiplicationStep(int left, int right, boolean included, int runningSum) {
        this.left = left;
        this.right = right;
        this.included = included;
        this.runningSum = runningSum;
    }

    public int getLeft() { return left; }
    public int getRight() { return right; }
    public boolean isIncluded() { return included; }
    public int getRunningSum() { return runningSum; }

    @Override
    public String toString() {
        return String.format("%-10d %-10d %-10s %s",
            left, right,
            included ? "Yes" : "No",
            included ? String.valueOf(runningSum) : "");
    }
}

/**
 * Result class containing both the final result and calculation steps
 */
class MultiplicationResult {
    private final int result;
    private final List<MultiplicationStep> steps;
    private final long executionTimeNanos;

    public MultiplicationResult(int result, List<MultiplicationStep> steps, long executionTimeNanos) {
        this.result = result;
        this.steps = Collections.unmodifiableList(steps);
        this.executionTimeNanos = executionTimeNanos;
    }

    public int getResult() { return result; }
    public List<MultiplicationStep> getSteps() { return steps; }
    public long getExecutionTimeNanos() { return executionTimeNanos; }

    public void displayCalculation(int a, int b) {
        System.out.printf("\nEgyptian Multiplication of %d × %d:\n", a, b);
        System.out.printf("%-10s %-10s %-10s %s\n", "Left", "Right", "Include?", "Sum");
        System.out.println("-".repeat(40));

        for (MultiplicationStep step : steps) {
            System.out.println(step);
        }

        System.out.printf("\nResult: %d\n", result);
        System.out.printf("Verification: %d × %d = %d (modern)\n", a, b, a * b);
        System.out.printf("Execution time: %.6f ms\n", executionTimeNanos / 1_000_000.0);
    }
}

/**
 * Main Egyptian Multiplication implementation
 */
public class EgyptianMultiplication implements MultiplicationAlgorithm {
    private boolean verbose = false;

    /**
     * Basic Egyptian multiplication for positive integers
     */
    @Override
    public int multiply(int a, int b) {
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
            if ((right & 1) == 1) {
                result += left;
            }

            // Double the left column
            left <<= 1;  // Equivalent to left * 2

            // Halve the right column
            right >>= 1;  // Equivalent to right / 2
        }

        return result;
    }

    @Override
    public String getName() {
        return "Egyptian Multiplication";
    }

    /**
     * Extended version handling negative numbers
     */
    public int multiplyExtended(int a, int b) {
        int sign = 1;
        if (a < 0) {
            sign = -sign;
            a = -a;
        }
        if (b < 0) {
            sign = -sign;
            b = -b;
        }
        return sign * multiply(a, b);
    }

    /**
     * Verbose version that returns calculation steps
     */
    public MultiplicationResult multiplyVerbose(int a, int b) {
        long startTime = System.nanoTime();
        List<MultiplicationStep> steps = new ArrayList<>();

        int result = 0;
        int left = a;
        int right = b;

        while (right > 0) {
            boolean included = (right & 1) == 1;
            if (included) {
                result += left;
            }

            steps.add(new MultiplicationStep(left, right, included, result));

            left <<= 1;
            right >>= 1;
        }

        long executionTime = System.nanoTime() - startTime;
        return new MultiplicationResult(result, steps, executionTime);
    }

    /**
     * Recursive implementation
     */
    public int multiplyRecursive(int a, int b) {
        if (b == 0) return 0;
        if (b == 1) return a;

        if ((b & 1) == 1) {
            // If b is odd
            return a + multiplyRecursive(a << 1, b >> 1);
        } else {
            // If b is even
            return multiplyRecursive(a << 1, b >> 1);
        }
    }

    /**
     * Stream-based functional implementation (Java 8+)
     */
    public int multiplyFunctional(int a, int b) {
        return Stream.iterate(
                new int[]{a, b, 0},
                arr -> arr[1] > 0,
                arr -> new int[]{
                    arr[0] << 1,
                    arr[1] >> 1,
                    arr[2] + ((arr[1] & 1) == 1 ? arr[0] : 0)
                }
            )
            .reduce((first, second) -> second)
            .map(arr -> arr[2] + ((arr[1] & 1) == 1 ? arr[0] : 0))
            .orElse(0);
    }

    /**
     * Analyze binary representation connection
     */
    public static void analyzeBinaryConnection(int n) {
        System.out.printf("\n=== Binary Analysis of %d ===\n", n);
        System.out.printf("Decimal: %d\n", n);
        System.out.printf("Binary:  %s\n", Integer.toBinaryString(n));

        List<Integer> powers = new ArrayList<>();
        int power = 0;
        int temp = n;

        while (temp > 0) {
            if ((temp & 1) == 1) {
                powers.add((int)Math.pow(2, power));
            }
            power++;
            temp >>= 1;
        }

        Collections.reverse(powers);
        System.out.printf("As sum of powers of 2: %s\n",
            powers.stream().map(String::valueOf).collect(Collectors.joining(" + ")));
        System.out.println("This demonstrates why Egyptian multiplication works -");
        System.out.println("it decomposes numbers into powers of 2!");
    }

    /**
     * Performance benchmark comparing different implementations
     */
    public static void benchmark() {
        System.out.println("\n=== Performance Benchmark ===");
        System.out.println("Iterations per test: 1,000,000\n");

        int[][] testCases = {
            {13, 17, 1},     // Small numbers
            {127, 42, 2},    // Medium numbers
            {999, 888, 3},   // Large numbers
            {12345, 6789, 4} // Very large numbers
        };

        EgyptianMultiplication em = new EgyptianMultiplication();

        for (int[] test : testCases) {
            int a = test[0];
            int b = test[1];
            System.out.printf("Test %d: %d × %d\n", test[2], a, b);

            // Benchmark Egyptian multiplication
            long start = System.nanoTime();
            for (int i = 0; i < 1_000_000; i++) {
                em.multiply(a, b);
            }
            long egyptianTime = System.nanoTime() - start;

            // Benchmark recursive version
            start = System.nanoTime();
            for (int i = 0; i < 1_000_000; i++) {
                em.multiplyRecursive(a, b);
            }
            long recursiveTime = System.nanoTime() - start;

            // Benchmark modern multiplication
            start = System.nanoTime();
            for (int i = 0; i < 1_000_000; i++) {
                int result = a * b;
            }
            long modernTime = System.nanoTime() - start;

            System.out.printf("  Egyptian:  %.6f seconds\n", egyptianTime / 1_000_000_000.0);
            System.out.printf("  Recursive: %.6f seconds\n", recursiveTime / 1_000_000_000.0);
            System.out.printf("  Modern:    %.6f seconds\n", modernTime / 1_000_000_000.0);
            System.out.printf("  Ratio:     %.2fx slower than modern\n\n",
                (double)egyptianTime / modernTime);
        }
    }

    /**
     * Demonstrate historical examples
     */
    public static void historicalExamples() {
        System.out.println("\n=== Historical Examples ===");

        EgyptianMultiplication em = new EgyptianMultiplication();

        System.out.println("\nExample from Rhind Papyrus:");
        System.out.println("Problem 32: A quantity plus its third and quarter equals 2");
        MultiplicationResult result = em.multiplyVerbose(12, 7);
        result.displayCalculation(12, 7);

        System.out.println("\nPure doubling example (powers of 2):");
        result = em.multiplyVerbose(1, 64);
        result.displayCalculation(1, 64);
    }

    /**
     * Generic multiplication method using strategy pattern
     */
    public static class MultiplicationStrategy<T extends Number> {
        private BiFunction<T, T, T> strategy;

        public MultiplicationStrategy(BiFunction<T, T, T> strategy) {
            this.strategy = strategy;
        }

        public T multiply(T a, T b) {
            return strategy.apply(a, b);
        }
    }

    /**
     * Exception class for invalid inputs
     */
    public static class InvalidMultiplicationInputException extends Exception {
        public InvalidMultiplicationInputException(String message) {
            super(message);
        }
    }

    /**
     * Validate input and perform multiplication
     */
    public static int multiplyWithValidation(int a, int b)
            throws InvalidMultiplicationInputException {
        if (a == Integer.MIN_VALUE || b == Integer.MIN_VALUE) {
            throw new InvalidMultiplicationInputException(
                "Integer.MIN_VALUE not supported due to overflow risk");
        }

        EgyptianMultiplication em = new EgyptianMultiplication();
        return em.multiplyExtended(a, b);
    }

    /**
     * Main method demonstrating all features
     */
    public static void main(String[] args) {
        System.out.println("=".repeat(60));
        System.out.println("EGYPTIAN MULTIPLICATION ALGORITHM IN JAVA");
        System.out.println("Object-Oriented Design Meets Ancient Mathematics");
        System.out.println("=".repeat(60));

        EgyptianMultiplication em = new EgyptianMultiplication();

        // Basic demonstration
        System.out.println("\n=== Basic Algorithm ===");
        MultiplicationResult result = em.multiplyVerbose(13, 17);
        result.displayCalculation(13, 17);

        // Edge cases
        System.out.println("\n=== Edge Cases ===");
        System.out.printf("0 × 5 = %d\n", em.multiply(0, 5));
        System.out.printf("1 × 42 = %d\n", em.multiply(1, 42));
        System.out.printf("8 × 8 = %d (power of 2)\n", em.multiply(8, 8));
        System.out.printf("16 × 15 = %d\n", em.multiply(16, 15));

        // Different implementations
        System.out.println("\n=== Different Implementations ===");
        int a = 25, b = 11;
        System.out.printf("Testing %d × %d:\n", a, b);
        System.out.printf("  Iterative:  %d\n", em.multiply(a, b));
        System.out.printf("  Recursive:  %d\n", em.multiplyRecursive(a, b));
        System.out.printf("  Functional: %d\n", em.multiplyFunctional(a, b));

        // Extended with negatives
        System.out.println("\n=== Extended Algorithm (with negatives) ===");
        System.out.printf("-13 × 17 = %d\n", em.multiplyExtended(-13, 17));
        System.out.printf("13 × -17 = %d\n", em.multiplyExtended(13, -17));
        System.out.printf("-13 × -17 = %d\n", em.multiplyExtended(-13, -17));

        // Binary analysis
        analyzeBinaryConnection(13);

        // Historical examples
        historicalExamples();

        // Performance benchmark
        benchmark();

        // Exception handling example
        System.out.println("\n=== Error Handling ===");
        try {
            int result2 = multiplyWithValidation(100, 200);
            System.out.printf("100 × 200 = %d (validated)\n", result2);
        } catch (InvalidMultiplicationInputException e) {
            System.err.println("Error: " + e.getMessage());
        }

        System.out.println("\n=== Historical and OOP Design Significance ===");
        System.out.println("""
        The Egyptian multiplication algorithm in Java demonstrates:

        1. ENCAPSULATION: The algorithm's steps are encapsulated in classes,
           hiding implementation details while exposing clean interfaces.

        2. POLYMORPHISM: Multiple implementations (iterative, recursive,
           functional) showcase different approaches to the same problem.

        3. ABSTRACTION: The MultiplicationAlgorithm interface allows us to
           swap implementations transparently.

        4. INHERITANCE: We can extend the basic algorithm to handle edge
           cases and additional features.

        5. HISTORICAL CONTINUITY: From ancient papyri to modern Java, the
           mathematical insight remains constant - only our expression evolves.

        6. FUNCTIONAL FEATURES: Java 8+ streams show how ancient algorithms
           can be expressed in modern functional paradigms.
        """);
    }
}