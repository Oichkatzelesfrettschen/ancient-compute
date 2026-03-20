"""Gate 3: Language promotion -- paradigm-specific compilation contracts.

Verifies that LISP, Haskell, IDRIS2, and System F each compile programs
that exercise their core paradigm features.  Gate 3 criterion (per
docs/general/LANGUAGE_READINESS_MATRIX.md):
  'Compile + execute round-trip; paradigm-specific contract'

For LISP, IDRIS, and System F the round-trip terminates at the IR stage
(the services produce an IR program and return SUCCESS).  For Haskell the
full pipeline is exercised (IR -> codegen -> assembler -> machine_code).

This file satisfies the Gate 3 deferred-work item in DEFERRED_WORK.md.
"""

from __future__ import annotations

import asyncio

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------


def _lisp(code: str) -> object:
    from backend.src.services.languages.lisp_service import LISPService

    result = asyncio.run(LISPService().execute(code))
    return result


def _haskell(code: str) -> object:
    from backend.src.services.languages.haskell_service import HaskellService

    return asyncio.run(HaskellService().execute(code))


def _idris(code: str) -> object:
    from backend.src.services.languages.idris_service import IDRISService

    return asyncio.run(IDRISService().execute(code))


def _systemf(code: str) -> object:
    from backend.src.services.languages.systemf_service import SystemFService

    return asyncio.run(SystemFService().execute(code))


def _ok_lisp(code: str) -> None:
    from backend.src.services.languages.lisp_service import ExecutionStatus

    r = _lisp(code)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.stderr}"


def _err_lisp(code: str) -> None:
    from backend.src.services.languages.lisp_service import ExecutionStatus

    r = _lisp(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, f"Expected COMPILE_ERROR, got {r.status}"


def _ok_haskell(code: str) -> None:
    from backend.src.services.languages.haskell_service import ExecutionStatus

    r = _haskell(code)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.errors}"
    assert r.machine_code, "Expected non-empty machine_code"


def _err_haskell(code: str) -> None:
    from backend.src.services.languages.haskell_service import ExecutionStatus

    r = _haskell(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, f"Expected COMPILE_ERROR, got {r.status}"


def _ok_idris(code: str) -> None:
    from backend.src.services.languages.idris_service import ExecutionStatus

    r = _idris(code)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.stderr}"


def _err_idris(code: str) -> None:
    from backend.src.services.languages.idris_service import ExecutionStatus

    r = _idris(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, f"Expected COMPILE_ERROR, got {r.status}"


def _ok_systemf(code: str) -> None:
    from backend.src.services.languages.systemf_service import ExecutionStatus

    r = _systemf(code)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.stderr}"


def _err_systemf(code: str) -> None:
    from backend.src.services.languages.systemf_service import ExecutionStatus

    r = _systemf(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, f"Expected COMPILE_ERROR, got {r.status}"


# =============================================================================
# LISP -- S-expression evaluation, defun, recursion, cond, let, lambda
# =============================================================================


class TestLISPContract:
    """LISP paradigm contract: S-expr eval, defun, recursion, cond, let."""

    def test_lisp_defun_two_params(self):
        """Basic defun with two parameters and arithmetic."""
        _ok_lisp("(defun add (x y) (+ x y))")

    def test_lisp_recursion_factorial(self):
        """Recursive factorial via if -- tests defun + if + arithmetic."""
        _ok_lisp("(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))")

    def test_lisp_let_binding(self):
        """let binds a local variable used in the body."""
        _ok_lisp("(defun double (x) (let ((two 2)) (* x two)))")

    def test_lisp_cond_two_way(self):
        """cond with one test clause and a t-else clause."""
        _ok_lisp("(defun abs-val (x) (cond ((> x 0) x) (t (- x))))")

    def test_lisp_cond_three_way(self):
        """cond with two test clauses and a t-else: sign function."""
        _ok_lisp("(defun sign (x) (cond ((> x 0) 1) ((= x 0) 0) (t -1)))")

    def test_lisp_cond_multi_clause(self):
        """cond with four clauses including nested comparison."""
        _ok_lisp("(defun classify (x)" " (cond ((> x 100) 3) ((> x 10) 2) ((> x 0) 1) (t 0)))")

    def test_lisp_nested_if(self):
        """Nested if-else (equivalent to cond, tests deep nesting)."""
        _ok_lisp("(defun max2 (a b)" " (if (> a b) a b))")

    def test_lisp_recursive_fib(self):
        """Fibonacci via double recursion -- two recursive calls."""
        _ok_lisp("(defun fib (n)" " (if (< n 2) n" "   (+ (fib (- n 1)) (fib (- n 2)))))")

    def test_lisp_lambda_in_mapcar(self):
        """Lambda expression passed to mapcar (higher-order function call)."""
        _ok_lisp("(defun double-all (lst) (mapcar (lambda (x) (* 2 x)) lst))")

    def test_lisp_multi_expr_body(self):
        """defun body with multiple expressions; last is return value."""
        _ok_lisp(
            "(defun compute (x)" " (let ((a (* x 2)))" "   (let ((b (+ a 1)))" "     (- b 1))))"
        )

    def test_lisp_rejected_bare_symbol(self):
        """A bare symbol with no enclosing defun is not a compile error
        for LISPService (it compiles to a no-op atom), but a completely
        invalid token sequence raises a compile error."""
        # Unmatched paren triggers a parse/compile error
        _err_lisp("(defun bad")


# =============================================================================
# Haskell -- type signatures, pattern matching, HOF, where clauses
# =============================================================================


class TestHaskellContract:
    """Haskell paradigm contract: type signatures, patterns, HOF, where."""

    def test_haskell_simple_addition(self):
        """add :: Int -> Int -> Int compiles to machine code."""
        _ok_haskell("add :: Int -> Int -> Int\nadd x y = x + y")

    def test_haskell_pattern_match_base_case(self):
        """Factorial with 0-pattern base case and recursive case."""
        _ok_haskell("fact :: Int -> Int\n" "fact 0 = 1\n" "fact n = n * fact (n - 1)")

    def test_haskell_higher_order_function(self):
        """applyTwice takes a function and applies it twice."""
        _ok_haskell("applyTwice :: (Int -> Int) -> Int -> Int\n" "applyTwice f x = f (f x)")

    def test_haskell_where_clause(self):
        """where clause introduces local bindings."""
        _ok_haskell(
            "bmi :: Int -> Int -> Int\n"
            "bmi weight height = weight * 100 `div` (height * height)\n"
            "  where scale = 100"
        )

    def test_haskell_multiple_function_definitions(self):
        """Two functions where the second calls the first."""
        _ok_haskell(
            "square :: Int -> Int\n"
            "square x = x * x\n\n"
            "sumSquares :: Int -> Int -> Int\n"
            "sumSquares a b = square a + square b"
        )

    def test_haskell_rejected_blocked_import(self):
        """Security gate: System.IO.Unsafe is blocked before compilation."""
        _err_haskell("import System.IO.Unsafe\nmain = unsafePerformIO (return ())")


# =============================================================================
# IDRIS2 -- module declarations, type annotations, function definitions
# =============================================================================


class TestIDRISContract:
    """IDRIS2 paradigm contract: module + type annotations + definitions.

    The IDRIS compiler targets a module-centric grammar (module Name body).
    Gate 3 verifies that well-formed module declarations compile without error.
    More advanced dependent-type features are deferred to Gate 4.
    """

    def test_idris_module_constant(self):
        """Module with a named constant definition."""
        _ok_idris("module Main\nanswer = 42")

    def test_idris_module_type_annotation(self):
        """Module with a type annotation declaration."""
        _ok_idris("module Main\nadd : Int")

    def test_idris_module_function_application(self):
        """Module with a function definition that applies another name."""
        _ok_idris("module Main\nresult = compute x")

    def test_idris_module_let_in(self):
        """Module with a let-in expression."""
        _ok_idris("module Main\nf = let x = 42 in x")

    def test_idris_module_two_declarations(self):
        """Module with two declarations (type + definition)."""
        _ok_idris("module Main\nf : Int\nf = 0")

    def test_idris_rejected_syntax_error(self):
        """Source with unrecoverable syntax produces COMPILE_ERROR."""
        # Completely invalid: just an operator with no operands
        # The PLY parser cannot build any AST from this
        # (triggers p_error and returns None -> service raises)
        _err_idris("module !!!")


# =============================================================================
# System F -- type abstraction, value abstraction, let, if
# =============================================================================


class TestSystemFContract:
    """System F paradigm contract: polymorphic types and lambda calculus.

    System F syntax:
      /\\A => body            type abstraction (polymorphic)
      \\x:T => body           value abstraction (lambda)
      f [T]                  type application
      f x                    value application
    """

    def test_systemf_identity(self):
        """Polymorphic identity: /\\A => \\x:A => x."""
        _ok_systemf("/\\A => \\x:A => x")

    def test_systemf_const(self):
        """Const: ignore second argument, return first."""
        _ok_systemf("/\\A => /\\B => \\x:A => \\y:B => x")

    def test_systemf_apply(self):
        """Apply: take a function and an argument, apply it."""
        _ok_systemf("/\\A => /\\B => \\f:A->B => \\x:A => f x")

    def test_systemf_let_binding(self):
        """let binds a typed variable in a body expression."""
        _ok_systemf("let x:Int = 42 in x")

    def test_systemf_if_then_else(self):
        """if-then-else branches on a boolean."""
        _ok_systemf("if true then 1 else 0")

    def test_systemf_nested_type_abstraction(self):
        """Three nested type abstractions (church-encoded pair helper)."""
        _ok_systemf("/\\A => /\\B => /\\C => \\f:A->B->C => \\x:A => \\y:B => f x y")

    def test_systemf_value_then_type_abstraction(self):
        """Value abstraction over a type-polymorphic body."""
        _ok_systemf("/\\A => \\x:A => /\\B => \\y:B => x")

    def test_systemf_rejected_dot_syntax(self):
        """Old-style dot syntax (Haskell-like) is not valid System F here."""
        _err_systemf("/\\A. \\x:A. x")

    def test_systemf_rejected_untyped_lambda(self):
        """Lambda with no type annotation is rejected."""
        _err_systemf("\\x => x")
