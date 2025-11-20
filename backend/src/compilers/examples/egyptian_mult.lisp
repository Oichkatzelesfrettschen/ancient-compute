;;;; Egyptian Multiplication Algorithm in LISP
;;;; ==========================================
;;;;
;;;; Historical Context:
;;;; The Egyptian multiplication algorithm (c. 2000 BCE) from the Rhind and
;;;; Moscow Mathematical Papyri represents one of humanity's earliest algorithms.
;;;; This LISP implementation showcases how ancient mathematical insights
;;;; align beautifully with LISP's symbolic computation paradigm.
;;;;
;;;; LISP's homoiconicity (code as data) mirrors the Egyptian scribes' view of
;;;; mathematics as symbol manipulation. Just as they manipulated hieroglyphs
;;;; to perform calculations, LISP manipulates S-expressions.
;;;;
;;;; The algorithm demonstrates:
;;;; - Recursion (natural in LISP since 1958)
;;;; - List processing (accumulating partial results)
;;;; - Symbolic computation (the essence of both ancient math and LISP)
;;;; - Meta-programming (generating code that computes)
;;;;
;;;; Time Complexity: O(log n) where n is the smaller multiplicand
;;;; Space Complexity: O(log n) for the recursion stack

;;; Basic Egyptian multiplication - pure functional
(defun egyptian-multiply (a b)
  "Multiply A and B using the ancient Egyptian algorithm.
   Only uses doubling and addition, no direct multiplication."
  (cond ((zerop b) 0)                           ; Base case: b = 0
        ((= b 1) a)                              ; Base case: b = 1
        ((oddp b)                                ; If b is odd
         (+ a (egyptian-multiply (* 2 a) (floor b 2))))
        (t                                       ; If b is even
         (egyptian-multiply (* 2 a) (floor b 2)))))

;;; Tail-recursive version for efficiency
(defun egyptian-multiply-tail (a b &optional (acc 0))
  "Tail-recursive Egyptian multiplication with accumulator.
   More efficient for large numbers as it doesn't grow the stack."
  (cond ((zerop b) acc)
        ((oddp b)
         (egyptian-multiply-tail (* 2 a) (floor b 2) (+ acc a)))
        (t
         (egyptian-multiply-tail (* 2 a) (floor b 2) acc))))

;;; Iterative version using loop macro
(defun egyptian-multiply-iter (a b)
  "Iterative Egyptian multiplication using LOOP.
   Shows how the algorithm maps to iteration."
  (loop with result = 0
        with left = a
        with right = b
        while (> right 0)
        do (when (oddp right)
             (incf result left))
           (setf left (* 2 left))
           (setf right (floor right 2))
        finally (return result)))

;;; Version that collects all steps for visualization
(defun egyptian-multiply-verbose (a b)
  "Egyptian multiplication returning both result and calculation steps.
   Returns a list: (result . steps) where each step is (left right included sum)."
  (let ((steps '())
        (result 0)
        (left a)
        (right b))
    (loop while (> right 0)
          do (let ((included (oddp right)))
               (when included
                 (incf result left))
               (push (list left right included result) steps)
               (setf left (* 2 left))
               (setf right (floor right 2))))
    (cons result (reverse steps))))

;;; Display the calculation in table format
(defun display-calculation (a b)
  "Display the Egyptian multiplication process in a formatted table."
  (format t "~%Egyptian Multiplication of ~D × ~D:~%" a b)
  (format t "~15A ~15A ~10A ~10A~%" "Left" "Right" "Include?" "Sum")
  (format t "~50A~%" (make-string 50 :initial-element #\-))

  (let* ((result-and-steps (egyptian-multiply-verbose a b))
         (result (car result-and-steps))
         (steps (cdr result-and-steps)))
    (dolist (step steps)
      (destructuring-bind (left right included sum) step
        (format t "~15D ~15D ~10A ~10A~%"
                left right
                (if included "Yes" "No")
                (if included (format nil "~D" sum) ""))))
    (format t "~%Result: ~D~%" result)
    (format t "Verification: ~D × ~D = ~D (modern)~%~%" a b (* a b))))

;;; Extended version handling negative numbers
(defun egyptian-multiply-extended (a b)
  "Egyptian multiplication extended to handle negative numbers.
   Ancient Egyptians didn't have negatives, but we can extend the algorithm."
  (let ((sign (if (eq (< a 0) (< b 0)) 1 -1)))
    (* sign (egyptian-multiply (abs a) (abs b)))))

;;; Higher-order function version
(defun make-egyptian-multiplier (n)
  "Create a function that multiplies by N using Egyptian algorithm.
   Demonstrates LISP's first-class functions."
  (lambda (x) (egyptian-multiply n x)))

;;; Macro for compile-time Egyptian multiplication
(defmacro egyptian-multiply-macro (a b)
  "Macro that computes Egyptian multiplication at compile time
   when both arguments are constants."
  (if (and (numberp a) (numberp b))
      (egyptian-multiply a b)
      `(egyptian-multiply ,a ,b)))

;;; Meta-circular evaluator for Egyptian multiplication
;;; This shows how the algorithm can generate its own code
(defun generate-egyptian-code (a b)
  "Generate LISP code that computes a × b using Egyptian algorithm.
   Demonstrates LISP's homoiconicity - code as data."
  (let ((terms '()))
    (loop with left = a
          with right = b
          while (> right 0)
          do (when (oddp right)
               (push left terms))
             (setf left (* 2 left))
             (setf right (floor right 2)))
    (if terms
        `(+ ,@terms)
        0)))

;;; Function that analyzes binary decomposition
(defun analyze-binary-connection (n)
  "Show how Egyptian multiplication relates to binary representation."
  (format t "~%=== Binary Analysis of ~D ===~%" n)
  (format t "Decimal: ~D~%" n)
  (format t "Binary:  ~B~%" n)
  (format t "Powers of 2: ")

  (let ((powers '())
        (power 0)
        (temp n))
    (loop while (> temp 0)
          do (when (oddp temp)
               (push (expt 2 power) powers))
             (incf power)
             (setf temp (floor temp 2)))
    (format t "~{~D~^ + ~}~%" powers)
    (format t "This demonstrates why Egyptian multiplication works:~%")
    (format t "It decomposes numbers into powers of 2!~%")))

;;; Memoized version for repeated calculations
(let ((memo-table (make-hash-table :test 'equal)))
  (defun egyptian-multiply-memoized (a b)
    "Memoized Egyptian multiplication for efficiency with repeated calls."
    (let ((key (cons a b)))
      (or (gethash key memo-table)
          (setf (gethash key memo-table)
                (egyptian-multiply a b))))))

;;; Parallel-style implementation (conceptual)
(defun egyptian-multiply-parallel (a b)
  "Conceptual parallel Egyptian multiplication.
   Shows how the algorithm could be parallelized."
  (labels ((collect-terms (left right)
             (cond ((zerop right) nil)
                   ((oddp right)
                    (cons left (collect-terms (* 2 left) (floor right 2))))
                   (t
                    (collect-terms (* 2 left) (floor right 2))))))
    ;; In a parallel system, we could sum these terms in parallel
    (reduce #'+ (collect-terms a b) :initial-value 0)))

;;; Stream-based lazy evaluation version
(defun egyptian-multiply-lazy (a b)
  "Lazy evaluation version using delayed computation.
   Demonstrates functional programming concepts."
  (labels ((lazy-steps (left right acc)
             (if (zerop right)
                 acc
                 (lazy-steps (* 2 left)
                            (floor right 2)
                            (if (oddp right) (+ acc left) acc)))))
    (lazy-steps a b 0)))

;;; Property testing functions
(defun prop-correctness (a b)
  "Property: Egyptian multiplication equals standard multiplication."
  (= (egyptian-multiply a b) (* a b)))

(defun prop-commutative (a b)
  "Property: Egyptian multiplication is commutative."
  (= (egyptian-multiply a b) (egyptian-multiply b a)))

(defun prop-identity (a)
  "Property: Multiplying by 1 returns the original number."
  (= (egyptian-multiply a 1) a))

(defun prop-zero (a)
  "Property: Multiplying by 0 returns 0."
  (zerop (egyptian-multiply a 0)))

;;; Test all properties
(defun test-properties ()
  "Test mathematical properties of Egyptian multiplication."
  (format t "~%=== Testing Properties ===~%")
  (let ((test-cases '((5 7) (13 17) (100 200) (999 888))))
    (dolist (case test-cases)
      (destructuring-bind (a b) case
        (format t "Testing ~D × ~D:~%" a b)
        (format t "  Correctness: ~A~%"
                (if (prop-correctness a b) "PASS" "FAIL"))
        (format t "  Commutative: ~A~%"
                (if (prop-commutative a b) "PASS" "FAIL"))
        (format t "  Identity: ~A~%"
                (if (prop-identity a) "PASS" "FAIL"))
        (format t "  Zero: ~A~%"
                (if (prop-zero a) "PASS" "FAIL"))))))

;;; Benchmark different implementations
(defun benchmark ()
  "Compare performance of different Egyptian multiplication implementations."
  (format t "~%=== Performance Benchmark ===~%")
  (format t "Testing with 10000 iterations~%~%")

  (let ((test-cases '((13 17) (127 42) (999 888))))
    (dolist (case test-cases)
      (destructuring-bind (a b) case
        (format t "~D × ~D:~%" a b)

        ;; Basic recursive
        (let ((start (get-internal-real-time)))
          (dotimes (i 10000) (egyptian-multiply a b))
          (format t "  Recursive: ~,6F seconds~%"
                  (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))

        ;; Tail recursive
        (let ((start (get-internal-real-time)))
          (dotimes (i 10000) (egyptian-multiply-tail a b))
          (format t "  Tail-rec:  ~,6F seconds~%"
                  (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))

        ;; Iterative
        (let ((start (get-internal-real-time)))
          (dotimes (i 10000) (egyptian-multiply-iter a b))
          (format t "  Iterative: ~,6F seconds~%"
                  (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))

        ;; Native
        (let ((start (get-internal-real-time)))
          (dotimes (i 10000) (* a b))
          (format t "  Native:    ~,6F seconds~%~%"
                  (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)))))))

;;; Historical examples
(defun historical-examples ()
  "Demonstrate historical examples from ancient papyri."
  (format t "~%=== Historical Examples ===~%")

  (format t "~%Example from Rhind Papyrus (Problem 79):~%")
  (format t "Seven houses, seven cats per house, seven mice per cat...~%")
  (format t "This involves 7^4 = 2401:~%")
  (display-calculation 7 343)  ; 7 × 7^3

  (format t "~%Pure doubling example (powers of 2):~%")
  (display-calculation 1 64))

;;; Main demonstration function
(defun main ()
  "Main demonstration of Egyptian multiplication in LISP."
  (format t "~60A~%" (make-string 60 :initial-element #\=))
  (format t "EGYPTIAN MULTIPLICATION IN LISP~%")
  (format t "Symbolic Computation Meets Ancient Mathematics~%")
  (format t "~60A~%" (make-string 60 :initial-element #\=))

  ;; Basic demonstration
  (format t "~%=== Basic Algorithm ===~%")
  (display-calculation 13 17)

  ;; Edge cases
  (format t "~%=== Edge Cases ===~%")
  (format t "0 × 5 = ~D~%" (egyptian-multiply 0 5))
  (format t "1 × 42 = ~D~%" (egyptian-multiply 1 42))
  (format t "8 × 8 = ~D (power of 2)~%" (egyptian-multiply 8 8))
  (format t "32 × 1 = ~D~%" (egyptian-multiply 32 1))

  ;; Different implementations
  (format t "~%=== Different Implementations ===~%")
  (let ((a 25) (b 11))
    (format t "Testing ~D × ~D:~%" a b)
    (format t "  Recursive:    ~D~%" (egyptian-multiply a b))
    (format t "  Tail-rec:     ~D~%" (egyptian-multiply-tail a b))
    (format t "  Iterative:    ~D~%" (egyptian-multiply-iter a b))
    (format t "  Memoized:     ~D~%" (egyptian-multiply-memoized a b))
    (format t "  Parallel:     ~D~%" (egyptian-multiply-parallel a b))
    (format t "  Lazy:         ~D~%" (egyptian-multiply-lazy a b)))

  ;; Extended with negatives
  (format t "~%=== Extended Algorithm (with negatives) ===~%")
  (format t "-13 × 17 = ~D~%" (egyptian-multiply-extended -13 17))
  (format t "13 × -17 = ~D~%" (egyptian-multiply-extended 13 -17))
  (format t "-13 × -17 = ~D~%" (egyptian-multiply-extended -13 -17))

  ;; Higher-order functions
  (format t "~%=== Higher-Order Functions ===~%")
  (let ((times-7 (make-egyptian-multiplier 7)))
    (format t "Created multiplier for 7~%")
    (format t "7 × 6 = ~D~%" (funcall times-7 6))
    (format t "7 × 13 = ~D~%" (funcall times-7 13)))

  ;; Code generation
  (format t "~%=== Code Generation (Homoiconicity) ===~%")
  (format t "Generated code for 5 × 3:~%")
  (format t "~S~%" (generate-egyptian-code 5 3))
  (format t "Evaluates to: ~D~%" (eval (generate-egyptian-code 5 3)))

  ;; Binary analysis
  (analyze-binary-connection 13)

  ;; Property testing
  (test-properties)

  ;; Historical examples
  (historical-examples)

  ;; Performance benchmark
  (benchmark)

  (format t "~%=== LISP and Ancient Mathematics ===~%")
  (format t "
The Egyptian multiplication algorithm in LISP demonstrates:

1. RECURSION: The algorithm's natural recursive structure aligns
   perfectly with LISP's recursive paradigm.

2. SYMBOLIC COMPUTATION: Just as Egyptian scribes manipulated
   hieroglyphic symbols, LISP manipulates symbolic expressions.

3. HOMOICONICITY: We can generate code that performs the multiplication,
   showing LISP's unique property of code as data.

4. FUNCTIONAL PURITY: The algorithm can be expressed as pure functions
   without side effects, embodying functional programming principles.

5. META-PROGRAMMING: Through macros, we can compute at compile-time,
   extending the language itself.

6. HISTORICAL CONTINUITY: From ancient papyri to modern LISP, the
   algorithm demonstrates timeless computational principles.
~%"))

;;; Run the demonstration when loaded
(main)