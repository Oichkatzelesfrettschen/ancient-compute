export interface ExampleProgram {
  id: string;
  title: string;
  description: string;
  language: string;
  languageColor: string;
  code: string;
  year: number;
  era: string;
  difficulty: 'beginner' | 'intermediate' | 'advanced';
  author?: string;
  tags: string[];
  inputExample?: string;
  expectedOutput?: string;
}

export const examplePrograms: ExampleProgram[] = [
  // Egyptian Multiplication (c. 2000 BC) - All languages
  {
    id: 'egyptian-mult-c',
    title: 'Egyptian Multiplication',
    description: 'Ancient Egyptian multiplication algorithm using doubling and addition, from Rhind Papyrus (1650 BC)',
    language: 'c',
    languageColor: '#00599C',
    code: `// Egyptian Multiplication Algorithm (c. 2000 BC)
// From the Rhind Mathematical Papyrus
#include <stdio.h>

int egyptian_multiply(int a, int b) {
    int result = 0;

    while (b > 0) {
        // If b is odd, add a to result
        if (b & 1) {
            result += a;
        }
        // Double a, halve b
        a <<= 1;
        b >>= 1;
    }

    return result;
}

int main() {
    int num1 = 13, num2 = 21;
    printf("Egyptian multiplication of %d × %d = %d\\n",
           num1, num2, egyptian_multiply(num1, num2));

    // Show the process step by step
    printf("\\nProcess:\\n");
    int a = num1, b = num2, result = 0;
    while (b > 0) {
        if (b & 1) {
            printf("%d × %d (add to result)\\n", a, 1);
            result += a;
        }
        a <<= 1;
        b >>= 1;
    }
    printf("Final result: %d\\n", result);

    return 0;
}`,
    year: -2000,
    era: 'Ancient',
    difficulty: 'beginner',
    author: 'Ancient Egyptian scribes',
    tags: ['arithmetic', 'algorithm', 'binary', 'historical'],
    expectedOutput: 'Egyptian multiplication of 13 × 21 = 273',
  },

  {
    id: 'egyptian-mult-python',
    title: 'Egyptian Multiplication',
    description: 'Ancient Egyptian multiplication using binary decomposition, predating modern algorithms by 3500 years',
    language: 'python',
    languageColor: '#3776AB',
    code: `# Egyptian Multiplication Algorithm (c. 2000 BC)
# Binary decomposition discovered 3500 years before Leibniz

def egyptian_multiply(a: int, b: int) -> int:
    """Multiply using ancient Egyptian doubling method."""
    result = 0
    powers = []
    values = []

    # Build powers of 2 table (as Egyptians did with papyrus)
    power = 1
    value = a
    while power <= b:
        powers.append(power)
        values.append(value)
        power *= 2
        value *= 2

    # Sum values where power divides b (marked with /)
    for i in range(len(powers) - 1, -1, -1):
        if b >= powers[i]:
            result += values[i]
            b -= powers[i]
            print(f"  {powers[i]:3} × {a:3} = {values[i]:4} ✓")
        else:
            print(f"  {powers[i]:3} × {a:3} = {values[i]:4}")

    return result

# Example from Rhind Papyrus
num1, num2 = 13, 21
print(f"Egyptian multiplication: {num1} × {num2}")
print("\\nDoubling table (Egyptian method):")
result = egyptian_multiply(num1, num2)
print(f"\\nResult: {result}")
print(f"Verification: {num1 * num2}")`,
    year: -2000,
    era: 'Ancient',
    difficulty: 'beginner',
    author: 'Ahmose (Rhind Papyrus scribe)',
    tags: ['arithmetic', 'algorithm', 'binary', 'papyrus'],
    expectedOutput: 'Result: 273',
  },

  {
    id: 'egyptian-mult-haskell',
    title: 'Egyptian Multiplication',
    description: 'Functional implementation of ancient Egyptian multiplication showing recursion patterns',
    language: 'haskell',
    languageColor: '#5D4F85',
    code: `-- Egyptian Multiplication in Haskell
-- Showing ancient algorithms through functional lens

egyptianMultiply :: Integer -> Integer -> Integer
egyptianMultiply a b = egyptianMult' a b 0
  where
    egyptianMult' :: Integer -> Integer -> Integer -> Integer
    egyptianMult' _ 0 acc = acc
    egyptianMult' a b acc
      | odd b     = egyptianMult' (a * 2) (b \`div\` 2) (acc + a)
      | otherwise = egyptianMult' (a * 2) (b \`div\` 2) acc

-- Show the doubling process
egyptianSteps :: Integer -> Integer -> [(Integer, Integer, Bool)]
egyptianSteps a b = reverse $ steps a b []
  where
    steps _ 0 acc = acc
    steps a b acc =
      let isOdd = odd b
          step = (a, b, isOdd)
      in steps (a * 2) (b \`div\` 2) (step : acc)

-- Pretty print the process
showEgyptian :: Integer -> Integer -> IO ()
showEgyptian a b = do
  putStrLn $ "Egyptian multiplication: " ++ show a ++ " × " ++ show b
  putStrLn "\\nSteps (double left, halve right):"
  let steps = egyptianSteps a b
  mapM_ printStep steps
  putStrLn $ "\\nResult: " ++ show (egyptianMultiply a b)
  where
    printStep (x, y, used) =
      putStrLn $ "  " ++ show x ++ " × " ++ show y ++
                 if used then " ✓ (odd, add to result)" else ""

main :: IO ()
main = showEgyptian 13 21`,
    year: -2000,
    era: 'Ancient',
    difficulty: 'intermediate',
    tags: ['functional', 'recursion', 'arithmetic', 'historical'],
    expectedOutput: 'Result: 273',
  },

  // Euclidean Algorithm (c. 300 BC)
  {
    id: 'euclidean-python',
    title: 'Euclidean Algorithm',
    description: "Euclid's algorithm for finding GCD from Elements Book VII (300 BC)",
    language: 'python',
    languageColor: '#3776AB',
    code: `# Euclidean Algorithm from Elements Book VII (c. 300 BC)
# "The greatest common measure of two numbers"

def euclidean_gcd(a: int, b: int) -> int:
    """Original Euclidean algorithm using subtraction."""
    print(f"Finding GCD of {a} and {b} using Euclid's method\\n")

    steps = 0
    while a != b:
        steps += 1
        if a > b:
            print(f"Step {steps}: {a} - {b} = {a - b}")
            a = a - b
        else:
            print(f"Step {steps}: {b} - {a} = {b - a}")
            b = b - a

    return a

def modern_euclidean(a: int, b: int) -> int:
    """Modern version using modulo (same principle)."""
    while b:
        a, b = b, a % b
    return a

# Example from Euclid's Elements
num1, num2 = 1071, 462
print("=" * 40)
gcd = euclidean_gcd(num1, num2)
print(f"\\nGCD = {gcd}")

# Verify with modern method
print(f"\\nVerification (modern): GCD({num1}, {num2}) = {modern_euclidean(num1, num2)}")`,
    year: -300,
    era: 'Classical',
    difficulty: 'beginner',
    author: 'Euclid of Alexandria',
    tags: ['geometry', 'number-theory', 'algorithm', 'greek'],
    expectedOutput: 'GCD = 21',
  },

  // Sieve of Eratosthenes (c. 240 BC)
  {
    id: 'sieve-haskell',
    title: 'Sieve of Eratosthenes',
    description: 'Ancient Greek algorithm for finding all prime numbers up to a limit',
    language: 'haskell',
    languageColor: '#5D4F85',
    code: `-- Sieve of Eratosthenes (c. 240 BC)
-- Infinite lazy sieve showing Haskell's elegance

-- Classic functional sieve
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x \`mod\` p /= 0]

-- All primes (infinite list)
primes :: [Integer]
primes = sieve [2..]

-- Bounded sieve (more efficient)
primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve' [2..n]
  where
    sieve' [] = []
    sieve' (p:xs)
      | p * p > n = p : xs
      | otherwise = p : sieve' (filter (\\x -> x \`mod\` p /= 0) xs)

-- Demonstrate the algorithm
main :: IO ()
main = do
  putStrLn "Sieve of Eratosthenes (c. 240 BC)"
  putStrLn "Finding primes up to 100:\\n"

  let limit = 100
  let primeList = primesUpTo limit

  putStrLn $ "Primes: " ++ show primeList
  putStrLn $ "\\nCount: " ++ show (length primeList) ++ " primes"
  putStrLn $ "Largest prime under " ++ show limit ++ ": " ++ show (last primeList)

  -- Show first 20 primes from infinite list
  putStrLn "\\nFirst 20 primes (infinite generation):"
  print $ take 20 primes`,
    year: -240,
    era: 'Classical',
    difficulty: 'intermediate',
    author: 'Eratosthenes of Cyrene',
    tags: ['primes', 'number-theory', 'lazy-evaluation', 'greek'],
    expectedOutput: 'Count: 25 primes',
  },

  // Fibonacci Sequence - LISP
  {
    id: 'fibonacci-lisp',
    title: 'Fibonacci Sequence',
    description: 'Fibonacci sequence described by Leonardo of Pisa (1202 AD)',
    language: 'lisp',
    languageColor: '#3F3F3F',
    code: `;;; Fibonacci Sequence - Liber Abaci (1202 AD)
;;; "How many pairs of rabbits in one year?"

;; Recursive definition (inefficient but clear)
(defun fibonacci-recursive (n)
  "Calculate nth Fibonacci number recursively"
  (if (<= n 1)
      n
      (+ (fibonacci-recursive (- n 1))
         (fibonacci-recursive (- n 2)))))

;; Iterative version (as Fibonacci might have calculated)
(defun fibonacci-iterative (n)
  "Calculate nth Fibonacci number iteratively"
  (if (<= n 1)
      n
      (let ((prev 0)
            (curr 1))
        (dotimes (i (- n 1) curr)
          (let ((next (+ prev curr)))
            (setq prev curr)
            (setq curr next))))))

;; Generate Fibonacci sequence
(defun fibonacci-sequence (count)
  "Generate first n Fibonacci numbers"
  (loop for i from 0 below count
        collect (fibonacci-iterative i)))

;; Golden ratio approximation
(defun golden-ratio-approx (n)
  "Approximate golden ratio using Fibonacci"
  (let ((fib-n (fibonacci-iterative n))
        (fib-n-1 (fibonacci-iterative (- n 1))))
    (/ (float fib-n) (float fib-n-1))))

;; Demonstrate
(progn
  (princ "Fibonacci Sequence (Liber Abaci, 1202)\\n")
  (princ "First 15 Fibonacci numbers:\\n")
  (print (fibonacci-sequence 15))

  (princ "\\nGolden ratio approximations:\\n")
  (loop for n from 5 to 10
        do (format t "F(~d)/F(~d) = ~f\\n"
                   n (- n 1) (golden-ratio-approx n)))

  (princ "\\nTrue golden ratio: 1.618034..."))`,
    year: 1202,
    era: 'Medieval',
    difficulty: 'intermediate',
    author: 'Leonardo Fibonacci',
    tags: ['recursion', 'iteration', 'golden-ratio', 'mathematics'],
    expectedOutput: '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377)',
  },

  // Newton's Method - Python
  {
    id: 'newton-method-python',
    title: "Newton's Method",
    description: 'Newton-Raphson method for finding square roots (1669)',
    language: 'python',
    languageColor: '#3776AB',
    code: `# Newton's Method for Square Roots (1669)
# "Method of Fluxions" by Isaac Newton

def newton_sqrt(n: float, precision: float = 0.000001) -> float:
    """Calculate square root using Newton's method."""
    if n < 0:
        raise ValueError("Cannot compute square root of negative number")

    # Initial guess (Newton used n/2)
    x = n / 2.0
    iterations = 0

    print(f"Finding √{n} using Newton's method")
    print(f"Initial guess: {x}")
    print("\\nIterations:")

    while True:
        iterations += 1
        # Newton's formula: x_new = (x + n/x) / 2
        x_new = (x + n / x) / 2

        print(f"  {iterations}: {x_new:.10f}")

        # Check convergence
        if abs(x_new - x) < precision:
            break

        x = x_new

        if iterations > 20:  # Prevent infinite loop
            break

    return x_new

# Examples
test_numbers = [2, 10, 25, 100]

for num in test_numbers:
    print("\\n" + "=" * 40)
    result = newton_sqrt(num)
    print(f"\\n√{num} = {result}")
    print(f"Verification: {result}² = {result ** 2}")
    print(f"Built-in: {num ** 0.5}")`,
    year: 1669,
    era: 'Early Modern',
    difficulty: 'intermediate',
    author: 'Isaac Newton',
    tags: ['calculus', 'numerical-methods', 'roots', 'convergence'],
    expectedOutput: '√2 = 1.4142135624',
  },

  // Quicksort - Java
  {
    id: 'quicksort-java',
    title: 'Quicksort Algorithm',
    description: "Tony Hoare's Quicksort algorithm (1959)",
    language: 'java',
    languageColor: '#007396',
    code: `// Quicksort Algorithm by Tony Hoare (1959)
// "The fastest known sorting algorithm" at the time

public class Quicksort {

    public static void quicksort(int[] arr, int low, int high) {
        if (low < high) {
            // Partition the array
            int pivotIndex = partition(arr, low, high);

            // Recursively sort left and right subarrays
            quicksort(arr, low, pivotIndex - 1);
            quicksort(arr, pivotIndex + 1, high);
        }
    }

    private static int partition(int[] arr, int low, int high) {
        // Choose rightmost element as pivot
        int pivot = arr[high];

        // Index of smaller element
        int i = low - 1;

        for (int j = low; j < high; j++) {
            // If current element is smaller than pivot
            if (arr[j] < pivot) {
                i++;
                swap(arr, i, j);
            }
        }

        // Place pivot in correct position
        swap(arr, i + 1, high);
        return i + 1;
    }

    private static void swap(int[] arr, int i, int j) {
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }

    public static void printArray(int[] arr) {
        for (int value : arr) {
            System.out.print(value + " ");
        }
        System.out.println();
    }

    public static void main(String[] args) {
        int[] data = {64, 34, 25, 12, 22, 11, 90, 45, 33, 77};

        System.out.println("Quicksort (Tony Hoare, 1959)");
        System.out.println("\\nOriginal array:");
        printArray(data);

        quicksort(data, 0, data.length - 1);

        System.out.println("\\nSorted array:");
        printArray(data);
    }
}`,
    year: 1959,
    era: 'Modern',
    difficulty: 'intermediate',
    author: 'Tony Hoare',
    tags: ['sorting', 'divide-conquer', 'recursion', 'algorithms'],
    expectedOutput: 'Sorted array: 11 12 22 25 33 34 45 64 77 90',
  },

  // Dependent Types - IDRIS2
  {
    id: 'dependent-vector-idris2',
    title: 'Length-Indexed Vectors',
    description: 'Dependent types ensuring compile-time vector length correctness',
    language: 'idris2',
    languageColor: '#8B4513',
    code: `-- Length-Indexed Vectors in Idris2
-- Dependent types prevent runtime errors at compile time

module VectorExample

-- Vector type indexed by its length
data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect n a -> Vect (S n) a

-- Safe head function - can't be called on empty vector
safeHead : Vect (S n) a -> a
safeHead (x :: _) = x

-- Append with type-level length calculation
append : Vect n a -> Vect m a -> Vect (n + m) a
append Nil ys = ys
append (x :: xs) ys = x :: append xs ys

-- Zip vectors of same length
zipSame : Vect n a -> Vect n b -> Vect n (a, b)
zipSame Nil Nil = Nil
zipSame (x :: xs) (y :: ys) = (x, y) :: zipSame xs ys

-- Matrix type using nested vectors
Matrix : Nat -> Nat -> Type -> Type
Matrix rows cols a = Vect rows (Vect cols a)

-- Safe matrix transpose
transpose : Matrix m n a -> Matrix n m a
transpose Nil = replicate _ Nil
transpose (row :: rows) = zipWith (::) row (transpose rows)

-- Example usage
example : Vect 3 Int
example = [1, 2, 3]

-- This would not compile:
-- badExample : Vect 2 Int
-- badExample = [1, 2, 3]  -- Type error: expected 2, got 3

main : IO ()
main = do
  putStrLn "Dependent Types Demo (Idris2)"
  let v1 = [1, 2, 3]
  let v2 = [4, 5, 6]
  putStrLn $ "v1 = " ++ show v1
  putStrLn $ "v2 = " ++ show v2
  putStrLn $ "append = " ++ show (append v1 v2)
  putStrLn $ "zip = " ++ show (zipSame v1 v2)`,
    year: 2020,
    era: 'Contemporary',
    difficulty: 'advanced',
    author: 'Edwin Brady',
    tags: ['dependent-types', 'type-safety', 'compile-time', 'vectors'],
    expectedOutput: 'append = [1, 2, 3, 4, 5, 6]',
  },

  // System F - Polymorphic Lambda Calculus
  {
    id: 'systemf-polymorphism',
    title: 'System F Polymorphism',
    description: 'Parametric polymorphism in System F (Girard-Reynolds, 1974)',
    language: 'systemf',
    languageColor: '#4B0082',
    code: `-- System F: Polymorphic Lambda Calculus
-- Jean-Yves Girard (1972) & John Reynolds (1974)

-- Identity function: ∀α. α → α
let id = Λα. λx:α. x

-- Composition: ∀α β γ. (β → γ) → (α → β) → (α → γ)
let compose =
  Λα. Λβ. Λγ.
    λf:(β → γ). λg:(α → β). λx:α.
      f (g x)

-- Church Booleans
let Bool = ∀α. α → α → α
let true  = Λα. λt:α. λf:α. t
let false = Λα. λt:α. λf:α. f

-- Boolean operations
let not = λb:Bool. Λα. λt:α. λf:α. b [α] f t
let and = λa:Bool. λb:Bool. a [Bool] b false
let or  = λa:Bool. λb:Bool. a [Bool] true b

-- Church Numerals
let Nat = ∀α. (α → α) → α → α
let zero = Λα. λs:(α → α). λz:α. z
let succ = λn:Nat. Λα. λs:(α → α). λz:α. s (n [α] s z)

-- Arithmetic
let plus = λm:Nat. λn:Nat.
  Λα. λs:(α → α). λz:α. m [α] s (n [α] s z)

let mult = λm:Nat. λn:Nat.
  Λα. λs:(α → α). m [α] (n [α] s)

-- List type
let List = λα:*. ∀β. (α → β → β) → β → β
let nil  = Λα. Λβ. λc:(α → β → β). λn:β. n
let cons = Λα. λh:α. λt:List α.
  Λβ. λc:(α → β → β). λn:β. c h (t [β] c n)

-- Map function
let map = Λα. Λβ. λf:(α → β).
  let rec mapRec = λl:List α.
    l [List β] (λh:α. λt:List β. cons [β] (f h) t) (nil [β])
  in mapRec

-- Example: doubling all numbers in a list
let double = λn:Nat. plus n n
let example = map [Nat] [Nat] double
               (cons [Nat] two
                (cons [Nat] three
                 (nil [Nat])))`,
    year: 1974,
    era: 'Modern',
    difficulty: 'advanced',
    author: 'Girard & Reynolds',
    tags: ['type-theory', 'polymorphism', 'lambda-calculus', 'formal'],
    expectedOutput: 'Type checks successfully',
  },

  // Babbage Assembly - Analytical Engine
  {
    id: 'babbage-factorial',
    title: 'Factorial on Analytical Engine',
    description: "Ada Lovelace's Note G algorithm for Bernoulli numbers (1843)",
    language: 'babbage-asm',
    languageColor: '#8B4513',
    code: `; Factorial Calculation for Babbage's Analytical Engine
; Based on Ada Lovelace's notation (1843)
; "The engine can arrange and combine numerical quantities"

section .variables
    ; Store locations (Babbage's columns)
    v0: dw 0    ; Input number
    v1: dw 0    ; Counter
    v2: dw 1    ; Accumulator (result)
    v3: dw 0    ; Temporary

section .cards
    ; Operation cards (instructions)

    ; Read input number
    number_card v0, 5       ; Load 5 into v0

    ; Initialize counter
    store v0                ; Store input
    load v0                  ; Load to Mill
    storec v1               ; Copy to counter

    ; Main loop - multiply accumulator by counter
loop_start:
    ; Check if counter > 0
    load v1
    cond zero, end_loop     ; If zero, jump to end

    ; Multiply accumulator by counter
    load v2                 ; Load accumulator
    load_from_mill
    load v1                 ; Load counter
    mul                     ; Multiply in Mill
    store_to_mill
    storec v2               ; Store result back

    ; Decrement counter
    load v1
    loadc 1
    sub                     ; Subtract 1
    storec v1               ; Store back

    ; Continue loop
    cb loop_start          ; Conditional back

end_loop:
    ; Output result
    load v2
    print                   ; Print result

    ; Ring the bell (calculation complete!)
    bell

    ; Punch result to card
    punch v2

    hlt                     ; Halt the engine

; Ada's Note: "The Analytical Engine has no pretensions
; to originate anything. It can do whatever we know
; how to order it to perform."`,
    year: 1843,
    era: 'Victorian',
    difficulty: 'intermediate',
    author: 'Ada Lovelace',
    tags: ['assembly', 'mechanical', 'historical', 'factorial'],
    expectedOutput: 'Result: 120 (5!)',
  },

  // Monte Carlo Pi Estimation
  {
    id: 'monte-carlo-pi',
    title: 'Monte Carlo π Estimation',
    description: 'Buffon\'s needle problem solved with random sampling (1777/1946)',
    language: 'python',
    languageColor: '#3776AB',
    code: `# Monte Carlo Method for π Estimation
# Combining Buffon's Needle (1777) with Ulam's method (1946)

import random
import math

def monte_carlo_pi(num_samples: int) -> float:
    """Estimate π using random sampling in unit square."""
    inside_circle = 0

    print(f"Monte Carlo π Estimation ({num_samples} samples)\\n")
    print("Method: Random points in unit square")
    print("Points inside unit circle: π/4 ratio\\n")

    # Generate random points and check if inside unit circle
    for i in range(num_samples):
        x = random.uniform(-1, 1)
        y = random.uniform(-1, 1)

        # Check if point is inside unit circle
        if x*x + y*y <= 1:
            inside_circle += 1

        # Progress updates
        if (i + 1) % (num_samples // 10) == 0:
            current_pi = 4 * inside_circle / (i + 1)
            error = abs(current_pi - math.pi)
            print(f"  {i+1:7} samples: π ≈ {current_pi:.6f} (error: {error:.6f})")

    # Calculate final estimate
    pi_estimate = 4 * inside_circle / num_samples
    return pi_estimate

# Historical progression
print("Historical Development:\\n")
print("1777 - Buffon: Needle dropping for π")
print("1946 - Ulam: Monte Carlo method at Los Alamos")
print("Today - Combining both approaches\\n")
print("=" * 50)

# Run estimation with increasing samples
for n in [100, 1000, 10000]:
    estimate = monte_carlo_pi(n)
    error = abs(estimate - math.pi)
    print(f"\\nFinal: π ≈ {estimate:.6f}")
    print(f"Actual π = {math.pi:.6f}")
    print(f"Error = {error:.6f} ({100*error/math.pi:.2f}%)")
    print("=" * 50)`,
    year: 1946,
    era: 'Modern',
    difficulty: 'intermediate',
    author: 'Stanislaw Ulam',
    tags: ['probability', 'simulation', 'numerical', 'pi'],
    expectedOutput: 'π ≈ 3.14',
  },

  // Lambda Calculus in LISP
  {
    id: 'lambda-church',
    title: 'Church Numerals',
    description: "Alonzo Church's lambda calculus numerals (1936)",
    language: 'lisp',
    languageColor: '#3F3F3F',
    code: `;;; Church Numerals in LISP
;;; Alonzo Church's Lambda Calculus (1936)
;;; Numbers as functions - foundation of computation

;; Church numeral definition: n = λf.λx.f^n(x)

;; Zero applies f zero times
(defun church-zero ()
  (lambda (f)
    (lambda (x) x)))

;; Successor function
(defun church-succ (n)
  (lambda (f)
    (lambda (x)
      (funcall f (funcall (funcall n f) x)))))

;; Convert Church numeral to integer
(defun church-to-int (n)
  (funcall (funcall n (lambda (x) (+ x 1))) 0))

;; Create Church numerals
(defun church-one ()
  (church-succ (church-zero)))

(defun church-two ()
  (church-succ (church-one)))

(defun church-three ()
  (church-succ (church-two)))

;; Church addition
(defun church-plus (m n)
  (lambda (f)
    (lambda (x)
      (funcall (funcall m f)
               (funcall (funcall n f) x)))))

;; Church multiplication
(defun church-mult (m n)
  (lambda (f)
    (funcall m (funcall n f))))

;; Church exponentiation
(defun church-exp (m n)
  (funcall n m))

;; Demonstrate Church arithmetic
(progn
  (princ "Church Numerals - Lambda Calculus (1936)\\n")
  (princ "=======================================\\n\\n")

  (let* ((two (church-two))
         (three (church-three))
         (sum (church-plus two three))
         (product (church-mult two three))
         (power (church-exp two three)))

    (format t "Church Two = ~d\\n" (church-to-int two))
    (format t "Church Three = ~d\\n" (church-to-int three))
    (format t "Two + Three = ~d\\n" (church-to-int sum))
    (format t "Two × Three = ~d\\n" (church-to-int product))
    (format t "Two ^ Three = ~d\\n" (church-to-int power))

    (princ "\\nNote: Numbers are functions, not data!")
    (princ "\\nThis is the foundation of all computation.")))`,
    year: 1936,
    era: 'Modern',
    difficulty: 'advanced',
    author: 'Alonzo Church',
    tags: ['lambda-calculus', 'functional', 'theoretical', 'church'],
    expectedOutput: 'Two + Three = 5',
  },

  // Binary Search Tree - Java
  {
    id: 'bst-java',
    title: 'Binary Search Tree',
    description: 'Binary tree data structure for efficient searching',
    language: 'java',
    languageColor: '#007396',
    code: `// Binary Search Tree Implementation
// Foundational data structure for databases and indexing

public class BinarySearchTree {
    static class Node {
        int data;
        Node left, right;

        Node(int value) {
            data = value;
            left = right = null;
        }
    }

    private Node root;

    // Insert a value
    public void insert(int value) {
        root = insertRec(root, value);
    }

    private Node insertRec(Node root, int value) {
        if (root == null) {
            return new Node(value);
        }

        if (value < root.data) {
            root.left = insertRec(root.left, value);
        } else if (value > root.data) {
            root.right = insertRec(root.right, value);
        }

        return root;
    }

    // Search for a value
    public boolean search(int value) {
        return searchRec(root, value);
    }

    private boolean searchRec(Node root, int value) {
        if (root == null) {
            return false;
        }

        if (value == root.data) {
            return true;
        }

        if (value < root.data) {
            return searchRec(root.left, value);
        } else {
            return searchRec(root.right, value);
        }
    }

    // In-order traversal (sorted output)
    public void inorder() {
        System.out.print("In-order: ");
        inorderRec(root);
        System.out.println();
    }

    private void inorderRec(Node root) {
        if (root != null) {
            inorderRec(root.left);
            System.out.print(root.data + " ");
            inorderRec(root.right);
        }
    }

    // Calculate height
    public int height() {
        return heightRec(root);
    }

    private int heightRec(Node root) {
        if (root == null) {
            return 0;
        }

        int leftHeight = heightRec(root.left);
        int rightHeight = heightRec(root.right);

        return Math.max(leftHeight, rightHeight) + 1;
    }

    public static void main(String[] args) {
        BinarySearchTree bst = new BinarySearchTree();

        System.out.println("Binary Search Tree Operations");
        System.out.println("==============================\\n");

        // Insert values
        int[] values = {50, 30, 70, 20, 40, 60, 80};
        System.out.print("Inserting: ");
        for (int val : values) {
            System.out.print(val + " ");
            bst.insert(val);
        }
        System.out.println("\\n");

        // Display tree
        bst.inorder();
        System.out.println("Height: " + bst.height());

        // Search operations
        System.out.println("\\nSearch Tests:");
        System.out.println("Search 40: " + bst.search(40));
        System.out.println("Search 25: " + bst.search(25));
    }
}`,
    year: 1960,
    era: 'Modern',
    difficulty: 'intermediate',
    author: 'P.F. Windley',
    tags: ['data-structures', 'trees', 'searching', 'recursion'],
    expectedOutput: 'In-order: 20 30 40 50 60 70 80',
  },

  // Turing Machine Simulator - Python
  {
    id: 'turing-machine',
    title: 'Turing Machine Simulator',
    description: "Alan Turing's theoretical machine (1936)",
    language: 'python',
    languageColor: '#3776AB',
    code: `# Turing Machine Simulator
# Alan Turing, "On Computable Numbers" (1936)
# Universal model of computation

class TuringMachine:
    """Simulate a Turing machine with tape and state transitions."""

    def __init__(self, tape, initial_state='q0'):
        self.tape = list(tape) + ['_'] * 10  # Extend with blanks
        self.head = 0
        self.state = initial_state
        self.transitions = {}

    def add_transition(self, state, read, write, move, next_state):
        """Add a state transition rule."""
        self.transitions[(state, read)] = (write, move, next_state)

    def step(self):
        """Execute one step of the machine."""
        current_symbol = self.tape[self.head]

        if (self.state, current_symbol) not in self.transitions:
            return False  # Halt

        write, move, next_state = self.transitions[(self.state, current_symbol)]

        # Write symbol
        self.tape[self.head] = write

        # Move head
        if move == 'R':
            self.head += 1
        elif move == 'L':
            self.head = max(0, self.head - 1)

        # Update state
        self.state = next_state

        return True

    def run(self, max_steps=100):
        """Run the machine until it halts."""
        steps = 0
        print(f"Initial: {''.join(self.tape[:20]).rstrip('_')}")
        print(f"State: {self.state}, Head at position {self.head}\\n")

        while steps < max_steps:
            if not self.step():
                print(f"\\nHalted after {steps} steps")
                break
            steps += 1

            if steps % 10 == 0:
                print(f"Step {steps}: {''.join(self.tape[:20]).rstrip('_')}")

        return ''.join(self.tape).rstrip('_')

# Example: Binary increment machine
print("Turing Machine: Binary Incrementer")
print("===================================\\n")

tm = TuringMachine('1101')  # Binary 13

# Define transitions for binary increment
tm.add_transition('q0', '0', '0', 'R', 'q0')
tm.add_transition('q0', '1', '1', 'R', 'q0')
tm.add_transition('q0', '_', '_', 'L', 'q1')
tm.add_transition('q1', '0', '1', 'L', 'q2')
tm.add_transition('q1', '1', '0', 'L', 'q1')
tm.add_transition('q1', '_', '1', 'S', 'qf')
tm.add_transition('q2', '0', '0', 'L', 'q2')
tm.add_transition('q2', '1', '1', 'L', 'q2')
tm.add_transition('q2', '_', '_', 'S', 'qf')

result = tm.run()
print(f"\\nFinal tape: {result}")
print(f"Binary {result} = Decimal {int(result, 2)}")
print("\\n'We can only see a short distance ahead,")
print("but we can see plenty there that needs to be done.' - Turing")`,
    year: 1936,
    era: 'Modern',
    difficulty: 'advanced',
    author: 'Alan Turing',
    tags: ['computation', 'theoretical', 'automata', 'turing'],
    expectedOutput: 'Final tape: 1110',
  },
];

// Helper function to get examples by language
export function getExamplesByLanguage(language: string): ExampleProgram[] {
  return examplePrograms.filter(p => p.language === language);
}

// Helper function to get examples by era
export function getExamplesByEra(era: string): ExampleProgram[] {
  return examplePrograms.filter(p => p.era === era);
}

// Helper function to get examples by difficulty
export function getExamplesByDifficulty(difficulty: string): ExampleProgram[] {
  return examplePrograms.filter(p => p.difficulty === difficulty);
}