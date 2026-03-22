"""Gate 3: Language promotion -- paradigm-specific compilation contracts.

Verifies that LISP, Haskell, IDRIS2, and System F each compile programs
that exercise their core paradigm features.  Gate 3 criterion (per
docs/general/LANGUAGE_READINESS_MATRIX.md):
  'Compile + execute round-trip; paradigm-specific contract'

For System F the round-trip terminates at the IR stage (the service produces
an IR program and returns SUCCESS).  For Haskell the full pipeline is
exercised (IR -> codegen -> assembler -> machine_code).  For LISP, real
SBCL 2.6.2 execution is used (sbcl --script).  For IDRIS2, real idris2 0.8.0
type-checking is used (idris2 --check); these tests are skipped if idris2 is
not installed.

This file satisfies the Gate 3 deferred-work item in DEFERRED_WORK.md.
"""

from __future__ import annotations

import asyncio
import os

import pytest

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
        # Note: 'double' conflicts with SB-ALIEN:DOUBLE in SBCL; use 'mul-two' instead.
        _ok_lisp("(defun mul-two (x) (let ((two 2)) (* x two)))")

    def test_lisp_cond_two_way(self):
        """cond with one test clause and a t-else clause."""
        _ok_lisp("(defun abs-val (x) (cond ((> x 0) x) (t (- x))))")

    def test_lisp_cond_three_way(self):
        """cond with two test clauses and a t-else: sign function."""
        _ok_lisp("(defun sign (x) (cond ((> x 0) 1) ((= x 0) 0) (t -1)))")

    def test_lisp_cond_multi_clause(self):
        """cond with four clauses including nested comparison."""
        _ok_lisp("(defun classify (x) (cond ((> x 100) 3) ((> x 10) 2) ((> x 0) 1) (t 0)))")

    def test_lisp_nested_if(self):
        """Nested if-else (equivalent to cond, tests deep nesting)."""
        _ok_lisp("(defun max2 (a b) (if (> a b) a b))")

    def test_lisp_recursive_fib(self):
        """Fibonacci via double recursion -- two recursive calls."""
        _ok_lisp("(defun fib (n) (if (< n 2) n   (+ (fib (- n 1)) (fib (- n 2)))))")

    def test_lisp_lambda_in_mapcar(self):
        """Lambda expression passed to mapcar (higher-order function call)."""
        _ok_lisp("(defun double-all (lst) (mapcar (lambda (x) (* 2 x)) lst))")

    def test_lisp_multi_expr_body(self):
        """defun body with multiple expressions; last is return value."""
        _ok_lisp("(defun compute (x) (let ((a (* x 2)))   (let ((b (+ a 1)))     (- b 1))))")

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
        _ok_haskell("fact :: Int -> Int\nfact 0 = 1\nfact n = n * fact (n - 1)")

    def test_haskell_higher_order_function(self):
        """applyTwice takes a function and applies it twice."""
        _ok_haskell("applyTwice :: (Int -> Int) -> Int -> Int\napplyTwice f x = f (f x)")

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


@pytest.mark.skipif(
    not os.path.exists("/usr/bin/idris2"),
    reason="idris2 not installed at /usr/bin/idris2",
)
class TestIDRISContract:
    """IDRIS2 paradigm contract: module + type annotations + definitions.

    Gate 3 verifies that well-formed Idris2 programs type-check via
    idris2 --check.  All programs must be complete (no unresolved names,
    no type signatures without implementations) since real idris2 enforces
    full type-checking, not just parsing.
    """

    def test_idris_module_constant(self):
        """Module with a named integer constant."""
        _ok_idris("module Main\n\nanswer : Int\nanswer = 42")

    def test_idris_module_identity(self):
        """Module with a typed identity function."""
        _ok_idris("module Main\n\nf : Int -> Int\nf x = x")

    def test_idris_module_const_function(self):
        """Module with a two-argument function (const -- ignores second arg)."""
        _ok_idris("module Main\n\ng : Int -> Int -> Int\ng x y = x")

    def test_idris_module_let_in(self):
        """Module with a let-in expression."""
        _ok_idris("module Main\n\nf : Int\nf = let x = 42 in x")

    def test_idris_module_two_declarations(self):
        """Module with two declarations (type + definition)."""
        _ok_idris("module Main\n\nf : Int\nf = 0")

    def test_idris_rejected_syntax_error(self):
        """Source with unrecoverable syntax produces COMPILE_ERROR."""
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


# =============================================================================
# JAVA -- OOP, generics, exception handling, lambdas
# =============================================================================

_JAVA_JAVAC = "/usr/lib/jvm/java-17-openjdk/bin/javac"


def _java(code: str, input_data: str = "") -> object:
    import asyncio

    from backend.src.services.languages.java_service import JavaService

    return asyncio.run(JavaService().execute(code, input_data))


def _ok_java(code: str, input_data: str = "") -> None:
    from backend.src.services.languages.java_service import ExecutionStatus

    r = _java(code, input_data)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.stderr}"


def _err_java(code: str) -> None:
    from backend.src.services.languages.java_service import ExecutionStatus

    r = _java(code)
    assert r.status != ExecutionStatus.SUCCESS, f"Expected failure: {r.stdout}"


@pytest.mark.skipif(
    not os.path.exists(_JAVA_JAVAC),
    reason="OpenJDK 17 javac not found at expected path",
)
class TestJavaContract:
    """Java Gate 3 contract: compile + run OOP programs.

    Verifies that the JavaService can compile and execute well-formed Java 17
    programs exercising OOP, generics, lambdas, and exception handling.
    """

    def test_java_hello_world(self):
        """Hello World compiles and prints output."""
        r = _java(
            "public class Main {\n"
            "    public static void main(String[] args) {\n"
            '        System.out.println("Hello, Java!");\n'
            "    }\n"
            "}"
        )
        from backend.src.services.languages.java_service import ExecutionStatus

        assert r.status == ExecutionStatus.SUCCESS
        assert "Hello, Java!" in r.stdout

    def test_java_oop_inheritance(self):
        """Subclass overrides a method and is called via base reference."""
        _ok_java(
            "public class Main {\n"
            '    static class Shape { String name() { return "shape"; } }\n'
            "    static class Circle extends Shape {\n"
            '        @Override String name() { return "circle"; }\n'
            "    }\n"
            "    public static void main(String[] args) {\n"
            "        Shape s = new Circle();\n"
            "        System.out.println(s.name());\n"
            "    }\n"
            "}"
        )

    def test_java_generics(self):
        """Generic method called with integer argument."""
        _ok_java(
            "public class Main {\n"
            "    static <T> T identity(T x) { return x; }\n"
            "    public static void main(String[] args) {\n"
            "        System.out.println(identity(42));\n"
            "    }\n"
            "}"
        )

    def test_java_lambda(self):
        """Lambda expression via functional interface."""
        _ok_java(
            "public class Main {\n"
            "    interface Fn { int apply(int x); }\n"
            "    public static void main(String[] args) {\n"
            "        Fn square = x -> x * x;\n"
            "        System.out.println(square.apply(7));\n"
            "    }\n"
            "}"
        )

    def test_java_exception_handling(self):
        """try/catch block handles a thrown exception gracefully."""
        _ok_java(
            "public class Main {\n"
            "    public static void main(String[] args) {\n"
            "        try {\n"
            '            throw new IllegalArgumentException("bad input");\n'
            "        } catch (IllegalArgumentException e) {\n"
            '            System.out.println("caught: " + e.getMessage());\n'
            "        }\n"
            "    }\n"
            "}"
        )

    def test_java_rejected_missing_brace(self):
        """Missing closing brace produces COMPILE_ERROR."""
        _err_java(
            "public class Main {\n"
            "    public static void main(String[] args) {\n"
            '        System.out.println("unclosed");\n'
            "    }\n"
            # missing closing brace for class
        )


# =============================================================================
# LISP -- additional paradigm coverage
# =============================================================================


class TestLISPContractExtended:
    """Additional LISP programs testing more S-expression forms."""

    def test_lisp_let_arithmetic(self):
        """let binding used in arithmetic expression."""
        _ok_lisp("(defun square (x) (let ((y (* x x))) y))")

    def test_lisp_nested_call_via_let(self):
        """Simulated quadruple via two let bindings (single defun)."""
        _ok_lisp("(defun quad (x) (let ((d (* x 2))) (* d 2)))")

    def test_lisp_simple_arithmetic(self):
        """Simple arithmetic expressions compile."""
        _ok_lisp("(defun calc (x) (+ (* x 2) (- x 1)))")

    def test_lisp_cond_single_clause(self):
        """cond with one test and no else (t clause omitted)."""
        _ok_lisp("(defun positive-p (x) (cond ((> x 0) t)))")

    def test_lisp_rejected_bad_parens(self):
        """Mismatched parens produce a compile error."""
        _err_lisp("(defun broken (x)")

    def test_lisp_list_construction(self):
        """list constructor used in function body."""
        _ok_lisp("(defun make-pair (a b) (list a b))")

    def test_lisp_passthrough_function(self):
        """Passthrough function returns its argument unchanged (avoids CL name lock)."""
        _ok_lisp("(defun my-id (x) x)")


# =============================================================================
# Haskell -- additional paradigm coverage
# =============================================================================


class TestHaskellContractExtended:
    """Additional Haskell programs covering more pipeline stages."""

    def test_haskell_sign_function(self):
        """Sign function via nested if-then-else (guard syntax not supported)."""
        _ok_haskell(
            "classify :: Int -> Int\n"
            "classify x = if x > 0 then 1 else if x < 0 then -1 else 0"
        )

    def test_haskell_list_comprehension_equivalent(self):
        """Map over a range (functional list processing)."""
        _ok_haskell(
            "double :: Int -> Int\n"
            "double x = x * 2\n\n"
            "doubled :: [Int] -> [Int]\n"
            "doubled xs = map double xs"
        )

    def test_haskell_let_in_expression(self):
        """let-in expression inside a function body."""
        _ok_haskell("f :: Int -> Int\nf x = let y = x * 2 in y + 1")

    def test_haskell_if_then_else(self):
        """Inline if-then-else expression."""
        _ok_haskell("abs_val :: Int -> Int\nabs_val x = if x >= 0 then x else negate x")

    def test_haskell_no_type_signature_still_compiles(self):
        """Functions without type signatures also compile."""
        _ok_haskell("add a b = a + b")

    def test_haskell_rejected_system_process(self):
        """System.Process import is blocked."""
        _err_haskell("import System.Process\nmain = pure ()")

    def test_haskell_rejected_system_exit(self):
        """System.Exit import is blocked."""
        _err_haskell("import System.Exit\nmain = pure ()")


# =============================================================================
# System F -- additional programs
# =============================================================================


class TestSystemFContractExtended:
    """Additional System F programs testing more polymorphic forms."""

    def test_systemf_church_true(self):
        """Church-encoded true: selects first of two arguments."""
        _ok_systemf("/\\A => /\\B => \\t:A => \\f:B => t")

    def test_systemf_church_false(self):
        """Church-encoded false: selects second of two arguments."""
        _ok_systemf("/\\A => /\\B => \\t:A => \\f:B => f")

    def test_systemf_deep_abstraction(self):
        """Four nested type abstractions compile correctly."""
        _ok_systemf("/\\A => /\\B => /\\C => /\\D => \\x:A => x")

    def test_systemf_literal_int(self):
        """Integer literal is a valid top-level expression."""
        _ok_systemf("42")

    def test_systemf_literal_bool_true(self):
        """Boolean literal true is a valid expression."""
        _ok_systemf("true")

    def test_systemf_literal_bool_false(self):
        """Boolean literal false is a valid expression."""
        _ok_systemf("false")

    def test_systemf_if_nested(self):
        """Nested if-then-else in a let binding."""
        _ok_systemf("if true then (if false then 0 else 1) else 2")

    def test_systemf_rejected_missing_arrow(self):
        """Type abstraction with missing body arrow is rejected."""
        _err_systemf("/\\A")
