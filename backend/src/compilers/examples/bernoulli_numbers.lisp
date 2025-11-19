;;;; Bernoulli Numbers Algorithm - Ada Lovelace's Note G (1843)
;;;; Common Lisp Implementation
;;;;
;;;; This is the first computer program ever published, from Ada Lovelace's Note G
;;;; in her translation of Luigi Menabrea's memoir on the Analytical Engine.
;;;;
;;;; Historical Context:
;;;;   - Published: September 1843 in Taylor's Scientific Memoirs
;;;;   - Author: Augusta Ada Byron King, Countess of Lovelace
;;;;   - Significance: First algorithm intended for machine execution
;;;;   - Original: Designed for Babbage's Analytical Engine (never built)
;;;;
;;;; The Bernoulli numbers B_n are defined recursively:
;;;;     B₀ = 1
;;;;     B₁ = -1/2
;;;;     Bₙ = 0 for odd n > 1
;;;;     Bₙ = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × Bₖ] for even n > 1
;;;;
;;;; Common Lisp Implementation Features:
;;;;   - Native rational arithmetic (built-in rational number type)
;;;;   - Symbolic computation with S-expressions
;;;;   - Functional programming paradigm
;;;;   - Dynamic typing with optional type declarations
;;;;   - Hash table memoization
;;;;   - Demonstrates LISP's mathematical elegance
;;;;
;;;; Execution:
;;;;   sbcl --script bernoulli_numbers.lisp
;;;;   or
;;;;   (load "bernoulli_numbers.lisp")
;;;;   (ada-lovelace-note-g-example)
;;;;
;;;; Ancient-Compute Integration:
;;;;   This implementation showcases LISP's native support for symbolic mathematics
;;;;   and how Lisp's code-as-data philosophy makes mathematical algorithms
;;;;   particularly elegant. LISP predates many modern languages but remains
;;;;   uniquely powerful for mathematical and AI applications.
;;;;
;;;; References:
;;;;   - Lovelace, A. (1843). "Notes by the Translator"
;;;;   - McCarthy, J. (1960). "Recursive Functions of Symbolic Expressions"
;;;;   - Steele, G. & Gabriel, R. (1996). "Common Lisp: The Language"

;;; Memoization hash table for Bernoulli numbers
(defvar *bernoulli-cache* (make-hash-table)
  "Hash table for memoizing computed Bernoulli numbers.")

;;; Binomial coefficient calculation

(defun binomial-coefficient (n k)
  "Calculate binomial coefficient C(n, k) = n! / (k! * (n-k)!)

Uses iterative formula to avoid factorial overflow:
C(n, k) = (n * (n-1) * ... * (n-k+1)) / (k * (k-1) * ... * 1)

Args:
  n - Total items
  k - Items to choose

Returns:
  Integer binomial coefficient

Examples:
  (binomial-coefficient 5 2) => 10
  (binomial-coefficient 10 3) => 120"
  (cond
    ((or (< k 0) (> k n)) 0)
    ((or (= k 0) (= k n)) 1)
    ((> k (- n k)) (binomial-coefficient n (- n k)))  ; Optimization
    (t
     ;; Iterative calculation
     (let ((result 1))
       (dotimes (i k result)
         (setf result (/ (* result (- n i))
                        (+ i 1))))))))

;;; Main Bernoulli number calculation

(defun bernoulli-number (n)
  "Calculate the nth Bernoulli number using Ada Lovelace's recursive formula.

This is the algorithm from Note G (September 1843) - the first computer program
ever published. Ada designed this to run on Babbage's Analytical Engine.

Mathematical Formula:
    B₀ = 1
    B₁ = -1/2
    Bₙ = 0 for odd n > 1
    Bₙ = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × Bₖ] for even n > 1

Args:
  n - Index of Bernoulli number (n >= 0)

Returns:
  Rational number representing B_n

Complexity:
  Time: O(n²) due to recursive summation
  Space: O(n) for recursion depth + memoization

Examples:
  (bernoulli-number 0) => 1
  (bernoulli-number 1) => -1/2
  (bernoulli-number 2) => 1/6
  (bernoulli-number 8) => -1/30

Historical Note:
  Ada's original algorithm used 25 operations on the Analytical Engine.
  This Lisp implementation preserves the mathematical logic while using
  Lisp's native rational arithmetic and functional programming style."
  (when (< n 0)
    (error "Bernoulli number index must be non-negative, got ~D" n))

  ;; Check memoization cache first
  (multiple-value-bind (cached-value present)
      (gethash n *bernoulli-cache*)
    (if present
        cached-value
        ;; Compute and cache
        (let ((result
               (cond
                 ;; Base case: B₀ = 1
                 ((= n 0) 1)
                 ;; Base case: B₁ = -1/2
                 ((= n 1) -1/2)
                 ;; All odd Bernoulli numbers (except B₁) are zero
                 ((and (> n 1) (oddp n)) 0)
                 ;; Recursive formula for even n > 1
                 (t (recursive-formula n)))))
          ;; Cache result
          (setf (gethash n *bernoulli-cache*) result)
          result))))

(defun recursive-formula (n)
  "Helper function for recursive Bernoulli formula.

Bₙ = -1/(n+1) × Σ(k=0 to n-1) [C(n+1, k) × Bₖ]"
  (let ((summation 0))
    ;; Sum over k from 0 to n-1
    (dotimes (k n)
      (let* ((binom-coeff (binomial-coefficient (+ n 1) k))
             (b-k (bernoulli-number k))
             (term (* binom-coeff b-k)))
        (incf summation term)))
    ;; Return -summation / (n + 1)
    (- (/ summation (+ n 1)))))

;;; Sequence generation

(defun bernoulli-sequence (max-n)
  "Generate a sequence of Bernoulli numbers from B₀ to B_max-n.

Args:
  max-n - Maximum index to compute

Returns:
  List of Bernoulli numbers [B₀, B₁, ..., B_max-n]

Examples:
  (bernoulli-sequence 6)
  => (1 -1/2 1/6 0 -1/30 0 1/42)"
  (when (< max-n 0)
    (error "max-n must be non-negative, got ~D" max-n))

  (loop for i from 0 to max-n
        collect (bernoulli-number i)))

;;; Conversion and display

(defun bernoulli-as-float (n)
  "Convert the nth Bernoulli number to floating point.

Args:
  n - Index of Bernoulli number

Returns:
  Double-float approximation

Examples:
  (bernoulli-as-float 2) => 0.16666666666666666d0
  (bernoulli-as-float 4) => -0.03333333333333333d0"
  (coerce (bernoulli-number n) 'double-float))

(defun format-rational (r)
  "Format a rational number for display.

Args:
  r - Rational number

Returns:
  String representation"
  (if (integerp r)
      (format nil "~D" r)
      (format nil "~D/~D" (numerator r) (denominator r))))

(defun print-bernoulli-table (max-n)
  "Print a formatted table of Bernoulli numbers.

Args:
  max-n - Maximum index to display

Example output:
  n    B_n (exact)              B_n (decimal)
  ==================================================
  0    1                        1.0d0
  1    -1/2                     -0.5d0
  2    1/6                      0.16666666666666666d0"
  (format t "n    B_n (exact)              B_n (decimal)~%")
  (format t "~V@{~A~:*~}~%" 70 "=")

  (dotimes (i (+ max-n 1))
    (let ((b-n (bernoulli-number i)))
      ;; Only print non-zero values
      (unless (zerop b-n)
        (format t "~4D ~24A ~15,13F~%"
                i
                (format-rational b-n)
                (coerce b-n 'double-float))))))

;;; Historical demonstration

(defun ada-lovelace-note-g-example ()
  "Demonstrate Ada Lovelace's Note G example.

Ada computed B₈ in her 1843 publication as the demonstration
of the Analytical Engine's capabilities."
  (format t "Ada Lovelace's Note G - Bernoulli Numbers Calculation~%")
  (format t "~V@{~A~:*~}~%" 70 "=")
  (terpri)

  (format t "Original Context:~%")
  (format t "  Published: September 1843~%")
  (format t "  Machine: Babbage's Analytical Engine (never built)~%")
  (format t "  Variables: 9 columns (V1-V9)~%")
  (format t "  Operations: 25 steps (Load, Add, Subtract, Multiply, Divide)~%")
  (terpri)

  (let ((n 8))
    (format t "Computing B_~D using Ada's recursive formula...~%" n)
    (terpri)

    (let ((b-8 (bernoulli-number n)))
      (format t "Result: B_~D = ~A~%" n (format-rational b-8))
      (format t "Decimal: ~15,13F~%" (coerce b-8 'double-float))
      (terpri)

      (format t "Verification - First 10 Bernoulli numbers:~%")
      (print-bernoulli-table 10))))

;;; Test suite

(defun run-test (test-name expected actual)
  "Run a single test and return result.

Args:
  test-name - String describing the test
  expected - Expected value
  actual - Actual value

Returns:
  Boolean indicating pass/fail"
  (let ((passed (equal expected actual)))
    (format t "~A Test: ~A~%"
            (if passed "✓" "✗")
            test-name)
    passed))

(defun run-tests ()
  "Test suite for Bernoulli number implementation."
  (terpri)
  (format t "=== Running Bernoulli Number Tests ===~%")
  (terpri)

  (let ((passed 0)
        (failed 0))

    ;; Define tests
    (macrolet ((test (name expected actual)
                 `(if (run-test ,name ,expected ,actual)
                      (incf passed)
                      (incf failed))))

      ;; Test 1: B₀ = 1
      (test "B_0 = 1" 1 (bernoulli-number 0))

      ;; Test 2: B₁ = -1/2
      (test "B_1 = -1/2" -1/2 (bernoulli-number 1))

      ;; Test 3: B₂ = 1/6
      (test "B_2 = 1/6" 1/6 (bernoulli-number 2))

      ;; Test 4: B₃ = 0 (odd number theorem)
      (test "B_3 = 0 (odd number theorem)" 0 (bernoulli-number 3))

      ;; Test 5: B₄ = -1/30
      (test "B_4 = -1/30" -1/30 (bernoulli-number 4))

      ;; Test 6: B₅ = 0 (odd number theorem)
      (test "B_5 = 0 (odd number theorem)" 0 (bernoulli-number 5))

      ;; Test 7: B₆ = 1/42
      (test "B_6 = 1/42" 1/42 (bernoulli-number 6))

      ;; Test 8: B₈ = -1/30 (Ada's example)
      (test "B_8 = -1/30 (Ada's example)" -1/30 (bernoulli-number 8))

      ;; Test 9: B₁₀ = 5/66
      (test "B_10 = 5/66" 5/66 (bernoulli-number 10))

      ;; Test 10: B₁₂ = -691/2730
      (test "B_12 = -691/2730" -691/2730 (bernoulli-number 12))

      ;; Test 11: B₂₀ = -174611/330
      (test "B_20 = -174611/330" -174611/330 (bernoulli-number 20))

      ;; Test 12: Sequence length
      (test "Sequence length" 11 (length (bernoulli-sequence 10)))

      ;; Test 13: All odd B_n = 0 (n=3,5,7,9)
      (test "All odd B_n = 0 (n=3,5,7,9)"
            t
            (every #'zerop (mapcar #'bernoulli-number '(3 5 7 9)))))

    ;; Print results
    (terpri)
    (format t "=== Test Results ===~%")
    (format t "Passed: ~D/~D~%" passed (+ passed failed))
    (format t "Failed: ~D/~D~%" failed (+ passed failed))
    (terpri)

    (if (= failed 0)
        (format t "✓ All tests passed!~%")
        (format t "✗ Some tests failed~%"))))

;;; Main execution

(defun main ()
  "Main entry point for the program."
  ;; Clear memoization cache
  (clrhash *bernoulli-cache*)

  ;; Run Ada's historical example
  (ada-lovelace-note-g-example)

  ;; Run test suite
  (run-tests))

;;; For SBCL script execution
#+sbcl
(when (member "--script" sb-ext:*posix-argv* :test #'equal)
  (main)
  (sb-ext:quit))

;;; For interactive use
#-sbcl
(format t "~%To run the program, evaluate: (main)~%~%")
