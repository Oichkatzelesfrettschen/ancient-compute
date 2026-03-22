"""Gate 2: C freestanding subset -- end-to-end compilation programs.

Verifies that 5+ non-trivial programs compile fully from C source through
IR generation, code generation, and assembly to Babbage machine code via
CService.execute().  Satisfies the Gate 2 completion criterion:
  'end-to-end compilation for at least 5 non-trivial programs using the
   existing subset'  (docs/general/FREESTANDING_C_SUBSET_PROFILE.md)
"""

from __future__ import annotations

import asyncio

import pytest

from backend.src.services.languages.c_service import CService, ExecutionStatus


def _run(code: str) -> object:
    return asyncio.run(CService().execute(code))


def _ok(code: str) -> None:
    result = _run(code)
    assert result.status == ExecutionStatus.SUCCESS, (
        f"Expected SUCCESS, got {result.status}. Errors:\n{result.stderr}"
    )
    assert len(result.machine_code) > 0, "Machine code output is empty"


# =============================================================================
# Gate 2 -- 5 non-trivial end-to-end programs
# =============================================================================


class TestGate2EndToEndPrograms:
    """5 non-trivial programs exercising the full C -> Babbage pipeline."""

    def test_program_1_factorial_for_loop(self):
        """Factorial via for-loop with declaration init (int i = 1)."""
        _ok("""
        int factorial(int n) {
            int result;
            result = 1;
            for (int i = 1; i <= n; i = i + 1) {
                result = result * i;
            }
            return result;
        }
        """)

    def test_program_2_fibonacci_while_loop(self):
        """Fibonacci via while loop with two local variables."""
        _ok("""
        int fib(int n) {
            int a;
            int b;
            int tmp;
            a = 0;
            b = 1;
            while (n > 0) {
                tmp = b;
                b = a + b;
                a = tmp;
                n = n - 1;
            }
            return a;
        }
        """)

    def test_program_3_max_two_functions(self):
        """Two functions: helper max() called from classify()."""
        _ok("""
        int max(int a, int b) {
            if (a > b) {
                return a;
            }
            return b;
        }

        int classify(int x, int y) {
            int m;
            m = max(x, y);
            if (m > 10) {
                return 1;
            }
            return 0;
        }
        """)

    def test_program_4_nested_if_else(self):
        """Multi-level nested if/else returning different constants."""
        _ok("""
        int sign_category(int x) {
            if (x > 0) {
                if (x > 100) {
                    return 3;
                }
                if (x > 10) {
                    return 2;
                }
                return 1;
            } else {
                if (x < 0) {
                    return -1;
                }
                return 0;
            }
        }
        """)

    def test_program_5_sum_while_with_multiple_vars(self):
        """Sum integers 1..n with while loop and two accumulators."""
        _ok("""
        int sum_and_count(int n) {
            int total;
            int count;
            total = 0;
            count = 0;
            while (count < n) {
                count = count + 1;
                total = total + count;
            }
            return total;
        }
        """)


# =============================================================================
# For-loop init declaration variants (the pre-existing parser bug, now fixed)
# =============================================================================


class TestForLoopInitDeclaration:
    """Parser now handles 'int i = 0' in for-loop init."""

    def test_for_init_int_decl_with_initializer(self):
        _ok("""
        int loop_sum(int n) {
            int total;
            total = 0;
            for (int i = 0; i < n; i = i + 1) {
                total = total + i;
            }
            return total;
        }
        """)

    def test_for_init_int_decl_no_initializer_body(self):
        """for (int i = 1; ...) where init value is 1."""
        _ok("""
        int product(int n) {
            int result;
            result = 1;
            for (int k = 1; k <= n; k = k + 1) {
                result = result * k;
            }
            return result;
        }
        """)

    def test_for_init_expression_still_works(self):
        """Existing assignment-expression for-init is not broken."""
        _ok("""
        int countdown(int n) {
            int i;
            for (i = n; i > 0; i = i - 1) {
                n = n - 1;
            }
            return n;
        }
        """)

    def test_for_empty_init(self):
        """for (;condition;incr) with no init clause compiles."""
        _ok("""
        int spin(int n) {
            for (; n > 0; n = n - 1) {
            }
            return n;
        }
        """)


# =============================================================================
# Full freestanding subset coverage: every supported feature compiles
# =============================================================================


class TestFreestandardingSubsetCoverage:
    """Each of the 7 supported features from FREESTANDING_C_SUBSET_PROFILE.md
    compiles individually end-to-end."""

    def test_feature_1_function_definition_scalar_params(self):
        _ok("""
        int add(int a, int b) {
            return a + b;
        }
        """)

    def test_feature_2_local_scalar_declaration_and_assignment(self):
        _ok("""
        int f() {
            int x;
            float y;
            x = 5;
            y = 3.14;
            return x;
        }
        """)

    def test_feature_3_integer_and_float_literals(self):
        _ok("""
        float get_pi() {
            return 3.14159;
        }
        int get_answer() {
            return 42;
        }
        """)

    def test_feature_4_arithmetic_expressions(self):
        _ok("""
        int arith(int a, int b) {
            int r;
            r = a + b;
            r = r - a;
            r = r * 2;
            r = r / b;
            return r;
        }
        """)

    def test_feature_5_comparisons_and_if(self):
        _ok("""
        int cmp(int a, int b) {
            if (a == b) {
                return 0;
            }
            if (a < b) {
                return -1;
            }
            return 1;
        }
        """)

    def test_feature_6_while_and_for_loops(self):
        _ok("""
        int while_loop(int n) {
            while (n > 0) {
                n = n - 1;
            }
            return n;
        }
        int for_loop(int n) {
            int s;
            s = 0;
            for (int i = 0; i < n; i = i + 1) {
                s = s + i;
            }
            return s;
        }
        """)

    def test_feature_7_function_calls_and_scalar_returns(self):
        _ok("""
        int square(int x) {
            return x * x;
        }
        int sum_squares(int a, int b) {
            return square(a) + square(b);
        }
        """)


# =============================================================================
# Diagnostic contract: all 6 rejected features produce correct error messages
# =============================================================================


class TestDiagnosticContract:
    """Rejected features carry the freestanding profile diagnostic contract."""

    @pytest.mark.parametrize(
        "label, code",
        [
            (
                "preprocessor directives",
                "#include <stdio.h>\nint main() { return 0; }",
            ),
            (
                "struct/union/enum types",
                "int f() { struct S { int x; }; return 0; }",
            ),
            (
                "switch statements",
                "int f(int x) { switch (x) { case 1: return 1; default: return 0; } }",
            ),
            (
                "goto statements",
                "int f() { goto end; end: return 0; }",
            ),
            (
                "array declarators",
                "int f() { int arr[10]; return arr[0]; }",
            ),
            (
                "pointer declarators",
                "int f(int *p) { return 0; }",
            ),
        ],
    )
    def test_rejected_feature_diagnostic(self, label: str, code: str) -> None:
        result = asyncio.run(CService().execute(code))
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "Unsupported C feature(s) for freestanding subset" in result.stderr
        assert label in result.stderr
        assert "FREESTANDING_C_SUBSET_PROFILE.md" in result.stderr


# =============================================================================
# CompilationResult contract
# =============================================================================


class TestCompilationResultContract:
    """CService.execute() always returns a well-formed CompilationResult."""

    def test_success_result_has_machine_code(self) -> None:
        result = asyncio.run(CService().execute("int f() { return 1; }"))
        assert result.status == ExecutionStatus.SUCCESS
        assert isinstance(result.machine_code, str)
        assert len(result.machine_code) > 0

    def test_success_result_has_ir_text(self) -> None:
        result = asyncio.run(CService().execute("int f() { return 1; }"))
        assert result.status == ExecutionStatus.SUCCESS
        assert isinstance(result.ir_text, str)
        assert len(result.ir_text) > 0

    def test_success_result_has_assembly_text(self) -> None:
        result = asyncio.run(CService().execute("int f() { return 1; }"))
        assert result.status == ExecutionStatus.SUCCESS
        assert isinstance(result.assembly_text, str)
        assert len(result.assembly_text) > 0

    def test_success_result_has_compilation_time(self) -> None:
        result = asyncio.run(CService().execute("int f() { return 1; }"))
        assert result.status == ExecutionStatus.SUCCESS
        assert result.compilation_time >= 0.0

    def test_compile_error_has_nonempty_stderr(self) -> None:
        result = asyncio.run(CService().execute("#include <stdio.h>\nint main() { return 0; }"))
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert len(result.stderr) > 0

    def test_compile_error_machine_code_empty(self) -> None:
        result = asyncio.run(CService().execute("int f() { goto end; end: return 0; }"))
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert result.machine_code == ""

    def test_status_is_execution_status_enum(self) -> None:
        result = asyncio.run(CService().execute("int f() { return 0; }"))
        assert isinstance(result.status, ExecutionStatus)


# =============================================================================
# Additional end-to-end programs: control flow and arithmetic depth
# =============================================================================


class TestAdditionalEndToEndPrograms:
    """Additional non-trivial programs covering more of the freestanding subset."""

    def test_program_power_function(self) -> None:
        """Iterative integer power: base^exp via for-loop."""
        _ok("""
        int power(int base, int exp) {
            int result;
            result = 1;
            for (int i = 0; i < exp; i = i + 1) {
                result = result * base;
            }
            return result;
        }
        """)

    def test_program_absolute_value(self) -> None:
        """Single if-return pattern: abs(x)."""
        _ok("""
        int abs_val(int x) {
            if (x < 0) {
                return 0 - x;
            }
            return x;
        }
        """)

    def test_program_min_two_values(self) -> None:
        """min() via if/else."""
        _ok("""
        int min(int a, int b) {
            if (a < b) {
                return a;
            }
            return b;
        }
        """)

    def test_program_gcd_euclidean(self) -> None:
        """GCD via subtraction-based Euclidean algorithm (while loops)."""
        _ok("""
        int gcd(int a, int b) {
            while (a != b) {
                if (a > b) {
                    a = a - b;
                } else {
                    b = b - a;
                }
            }
            return a;
        }
        """)

    def test_program_three_functions_chain(self) -> None:
        """Three functions where third calls both first and second."""
        _ok("""
        int double_val(int x) {
            return x * 2;
        }
        int triple_val(int x) {
            return x * 3;
        }
        int combined(int x) {
            return double_val(x) + triple_val(x);
        }
        """)

    def test_program_nested_for_loops(self) -> None:
        """Nested for-loops: outer * inner iterations."""
        _ok("""
        int count_pairs(int n) {
            int count;
            count = 0;
            for (int i = 0; i < n; i = i + 1) {
                for (int j = 0; j < n; j = j + 1) {
                    count = count + 1;
                }
            }
            return count;
        }
        """)

    def test_program_early_return_in_loop(self) -> None:
        """Return from inside a while loop (early exit)."""
        _ok("""
        int find_first_zero(int n) {
            int i;
            i = n;
            while (i > 0) {
                if (i == 1) {
                    return i;
                }
                i = i - 1;
            }
            return 0;
        }
        """)

    def test_program_multiple_local_vars(self) -> None:
        """Function with 5 local variables, all assigned and used."""
        _ok("""
        int five_vars(int x) {
            int a;
            int b;
            int c;
            int d;
            int e;
            a = x + 1;
            b = a * 2;
            c = b - x;
            d = c + a;
            e = d * b;
            return e;
        }
        """)

    def test_program_float_arithmetic(self) -> None:
        """Float local variable with arithmetic expressions."""
        _ok("""
        float area(float r) {
            float pi;
            pi = 3.14159;
            return pi * r * r;
        }
        """)

    def test_program_zero_param_function(self) -> None:
        """Function with no parameters returns a constant."""
        _ok("""
        int get_answer() {
            return 42;
        }
        """)


# =============================================================================
# For-loop edge cases and while-loop variants
# =============================================================================


class TestLoopEdgeCases:
    """Edge cases for loop forms supported by the freestanding subset."""

    def test_while_loop_single_body_statement(self) -> None:
        _ok("""
        int dec(int n) {
            while (n > 0) {
                n = n - 1;
            }
            return n;
        }
        """)

    def test_for_loop_decrement(self) -> None:
        """Decrementing for loop with i = i - 1 update."""
        _ok("""
        int sum_down(int n) {
            int s;
            s = 0;
            for (int i = n; i > 0; i = i - 1) {
                s = s + i;
            }
            return s;
        }
        """)

    def test_nested_while_loops(self) -> None:
        """Two while loops nested: inner resets per outer iteration."""
        _ok("""
        int matrix_sum(int n) {
            int total;
            int i;
            int j;
            total = 0;
            i = 0;
            while (i < n) {
                j = 0;
                while (j < n) {
                    total = total + 1;
                    j = j + 1;
                }
                i = i + 1;
            }
            return total;
        }
        """)

    def test_for_loop_no_body(self) -> None:
        """For loop with empty body (spin-loop)."""
        _ok("""
        int spin(int n) {
            for (; n > 0; n = n - 1) {
            }
            return n;
        }
        """)

    def test_while_loop_with_if_inside(self) -> None:
        """While loop with conditional increment inside body."""
        _ok("""
        int count_even(int n) {
            int count;
            int i;
            count = 0;
            i = 0;
            while (i < n) {
                if (i == 0) {
                    count = count + 1;
                }
                i = i + 1;
            }
            return count;
        }
        """)


# =============================================================================
# Comparison operator coverage
# =============================================================================


class TestComparisonOperators:
    """All 6 comparison operators compile correctly."""

    def test_less_than(self) -> None:
        _ok("int f(int a, int b) { if (a < b) { return 1; } return 0; }")

    def test_less_than_or_equal(self) -> None:
        _ok("int f(int a, int b) { if (a <= b) { return 1; } return 0; }")

    def test_greater_than(self) -> None:
        _ok("int f(int a, int b) { if (a > b) { return 1; } return 0; }")

    def test_greater_than_or_equal(self) -> None:
        _ok("int f(int a, int b) { if (a >= b) { return 1; } return 0; }")

    def test_equal(self) -> None:
        _ok("int f(int a, int b) { if (a == b) { return 1; } return 0; }")

    def test_not_equal(self) -> None:
        _ok("int f(int a, int b) { if (a != b) { return 1; } return 0; }")


# =============================================================================
# Arithmetic operator coverage
# =============================================================================


class TestArithmeticOperators:
    """All 4 arithmetic operators compile correctly."""

    def test_addition(self) -> None:
        _ok("int f(int a, int b) { return a + b; }")

    def test_subtraction(self) -> None:
        _ok("int f(int a, int b) { return a - b; }")

    def test_multiplication(self) -> None:
        _ok("int f(int a, int b) { return a * b; }")

    def test_division(self) -> None:
        _ok("int f(int a, int b) { return a / b; }")

    def test_chained_arithmetic(self) -> None:
        _ok("int f(int a, int b, int c) { return a + b * c - a / b; }")

    def test_negative_literal(self) -> None:
        _ok("int f() { int x; x = 0 - 5; return x; }")
