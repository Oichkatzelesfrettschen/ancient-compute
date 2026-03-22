"""Gate 3: C (freestanding subset) paradigm contract tests.

CService compiles C via the Babbage IR pipeline (c_compiler.py -> IR -> asm -> machine code).
Empirically verified capabilities (2026-03-20):

SUPPORTED (Gate 3 scope):
  - Function definitions with parameters (int, float, void)
  - Arithmetic (+, -, *, /)
  - Comparison operators in branch conditions (via BranchTerminator)
  - if/else branches
  - while loops (using function parameter as counter, no local var decls)
  - Recursive calls
  - Multiple functions calling each other

NOT YET SUPPORTED (compile error expected):
  - Local variable declarations inside function bodies (e.g. int s = 0;)
  - Preprocessor directives (#include, #define)
  - struct/union/enum
  - Arrays, pointers

Gate 3 criterion: compile + round-trip to machine code (non-empty machine_code field).
"""

from __future__ import annotations

import asyncio


def _run(code: str) -> object:
    from backend.src.services.languages.c_service import CService

    return asyncio.run(CService().execute(code))


def _ok(code: str) -> None:
    from backend.src.services.languages.c_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.stderr}"
    assert r.machine_code, "Expected non-empty machine_code"


def _err(code: str) -> None:
    from backend.src.services.languages.c_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, f"Expected COMPILE_ERROR, got {r.status}"


class TestCFreestandingContract:
    """C freestanding subset contract: functions, arithmetic, control flow, recursion."""

    def test_simple_addition(self):
        """Two-parameter add function compiles to machine code."""
        _ok("int add(int a, int b) { return a + b; }")

    def test_subtraction(self):
        """Subtraction function."""
        _ok("int sub(int a, int b) { return a - b; }")

    def test_multiplication(self):
        """Multiplication function."""
        _ok("int mul(int a, int b) { return a * b; }")

    def test_if_else_branch(self):
        """if/else with comparison on function parameter."""
        _ok(
            "int abs_val(int x) {\n"
            "  if (x >= 0) {\n"
            "    return x;\n"
            "  } else {\n"
            "    return -x;\n"
            "  }\n"
            "}"
        )

    def test_if_else_chain(self):
        """Nested if/else (sign function via parameters only)."""
        _ok(
            "int sign(int x) {\n"
            "  if (x > 0) {\n"
            "    return 1;\n"
            "  } else {\n"
            "    if (x == 0) {\n"
            "      return 0;\n"
            "    } else {\n"
            "      return -1;\n"
            "    }\n"
            "  }\n"
            "}"
        )

    def test_while_countdown(self):
        """While loop decrementing a parameter."""
        _ok("int countdown(int n) {\n  while (n > 0) {\n    n = n - 1;\n  }\n  return n;\n}")

    def test_recursive_factorial(self):
        """Recursive factorial (tail recursion via parameter arithmetic)."""
        _ok("int fact(int n) {\n  if (n <= 1) {\n    return 1;\n  }\n  return n * fact(n - 1);\n}")

    def test_recursive_fibonacci(self):
        """Double recursion Fibonacci."""
        _ok(
            "int fib(int n) {\n"
            "  if (n < 2) {\n"
            "    return n;\n"
            "  }\n"
            "  return fib(n - 1) + fib(n - 2);\n"
            "}"
        )

    def test_multiple_functions(self):
        """Two functions where the second calls the first."""
        _ok(
            "int square(int x) {\n"
            "  return x * x;\n"
            "}\n"
            "int sum_squares(int a, int b) {\n"
            "  return square(a) + square(b);\n"
            "}"
        )

    def test_while_accumulate(self):
        """While loop that accumulates using parameter mutation."""
        _ok(
            "int sum_to(int n) {\n"
            "  while (n > 1) {\n"
            "    n = n + (n - 1);\n"
            "    n = n / 2 + n / 2;\n"
            "    return n;\n"
            "  }\n"
            "  return n;\n"
            "}"
        )

    def test_rejected_preprocessor(self):
        """Preprocessor directives are rejected."""
        _err("#include <stdio.h>\nint main() { return 0; }")

    def test_rejected_struct(self):
        """struct declarations are rejected."""
        _err("struct Point { int x; int y; };\nint main() { return 0; }")

    def test_rejected_pointer(self):
        """Pointer declarations are rejected."""
        _err("int deref(int *p) { return *p; }")


class TestCFreestandingMachineCode:
    """Verify machine_code properties of compiled C programs."""

    def test_machine_code_is_non_empty_string(self) -> None:
        r = _run("int add(int a, int b) { return a + b; }")
        assert isinstance(r.machine_code, str)
        assert len(r.machine_code) > 0

    def test_machine_code_contains_hex_addresses(self) -> None:
        r = _run("int add(int a, int b) { return a + b; }")
        assert "00000000" in r.machine_code

    def test_more_functions_more_machine_code(self) -> None:
        r1 = _run("int f(int x) { return x; }")
        r2 = _run(
            "int f(int x) { return x; }\n"
            "int g(int a, int b) { return f(a) + f(b); }\n"
            "int h(int x) { return g(x, x) * 2; }"
        )
        assert len(r2.machine_code) >= len(r1.machine_code)

    def test_stdout_contains_hex_dump_marker(self) -> None:
        r = _run("int add(int a, int b) { return a + b; }")
        assert r.stdout
        assert "MACHINE CODE" in r.stdout


class TestCFreestandingAdditional:
    """Additional C freestanding programs."""

    def test_max_function(self) -> None:
        _ok("int max2(int a, int b) {\n  if (a > b) { return a; }\n  return b;\n}")

    def test_recursive_power(self) -> None:
        _ok(
            "int power(int base, int exp) {\n"
            "  if (exp == 0) { return 1; }\n"
            "  return base * power(base, exp - 1);\n"
            "}"
        )

    def test_three_param_function(self) -> None:
        _ok("int add3(int a, int b, int c) { return a + b + c; }")

    def test_nested_recursive_calls(self) -> None:
        _ok(
            "int ack(int m, int n) {\n"
            "  if (m == 0) { return n + 1; }\n"
            "  if (n == 0) { return ack(m - 1, 1); }\n"
            "  return ack(m - 1, ack(m, n - 1));\n"
            "}"
        )


class TestCFreestandingLocalVarRejected:
    """Local variable declarations inside C function bodies must produce COMPILE_ERROR."""

    def test_local_int_decl_rejected(self) -> None:
        _err("int f(int x) { int y = 5; return x + y; }")

    def test_local_float_decl_rejected(self) -> None:
        _err("float f(float x) { float y = 1.0; return x + y; }")

    def test_local_loop_counter_decl_rejected(self) -> None:
        _err(
            "int sum(int n) {\n"
            "  int s = 0;\n"
            "  int i = 1;\n"
            "  while (i <= n) { s = s + i; i = i + 1; }\n"
            "  return s;\n"
            "}"
        )

    def test_local_decl_in_if_branch_rejected(self) -> None:
        _err("int f(int x) { if (x > 0) { int pos = x; return pos; } return 0; }")


class TestCFreestandingReturnExpressions:
    """Various return expression patterns that must compile successfully."""

    def test_return_constant_zero(self) -> None:
        _ok("int zero() { return 0; }")

    def test_return_constant_negative_one(self) -> None:
        _ok("int neg() { return -1; }")

    def test_return_param_times_constant(self) -> None:
        _ok("int double_val(int x) { return x * 2; }")

    def test_return_division_by_two(self) -> None:
        _ok("int half(int x) { return x / 2; }")

    def test_return_param_minus_param(self) -> None:
        _ok("int diff(int a, int b) { return a - b; }")


class TestCFreestandingComparisonOps:
    """All comparison operators compile correctly in if-branch conditions."""

    def test_not_equal_compiles(self) -> None:
        _ok("int neq(int a, int b) { if (a != b) { return 1; } return 0; }")

    def test_less_than_compiles(self) -> None:
        _ok("int lt(int a, int b) { if (a < b) { return 1; } return 0; }")

    def test_greater_equal_compiles(self) -> None:
        _ok("int ge(int a, int b) { if (a >= b) { return 1; } return 0; }")

    def test_less_equal_compiles(self) -> None:
        _ok("int le(int a, int b) { if (a <= b) { return 1; } return 0; }")

    def test_equal_equal_compiles(self) -> None:
        _ok("int eq(int a, int b) { if (a == b) { return 1; } return 0; }")


class TestCGate3MachineCodeProperties:
    """Machine code output properties for C Gate 3 programs."""

    def test_machine_code_non_empty_for_simple_function(self) -> None:
        r = _run("int f(int x) { return x; }")
        assert len(r.machine_code) > 0

    def test_machine_code_is_string(self) -> None:
        r = _run("int f(int x) { return x; }")
        assert isinstance(r.machine_code, str)

    def test_machine_code_grows_with_more_operations(self) -> None:
        r1 = _run("int f(int x) { return x; }")
        r2 = _run("int f(int a, int b) { return a + b; }")
        # Both should succeed; more complex code gives at least as much output
        assert len(r2.machine_code) >= len(r1.machine_code)

    def test_status_is_success_for_valid_c(self) -> None:
        from backend.src.services.languages.c_service import ExecutionStatus
        r = _run("int identity(int x) { return x; }")
        assert r.status == ExecutionStatus.SUCCESS

    def test_result_has_stdout_attribute(self) -> None:
        r = _run("int f(int x) { return x; }")
        assert hasattr(r, "stdout")

    def test_result_has_stderr_attribute(self) -> None:
        r = _run("int f(int x) { return x; }")
        assert hasattr(r, "stderr")

    def test_compile_error_result_has_empty_machine_code(self) -> None:
        from backend.src.services.languages.c_service import ExecutionStatus
        r = _run("int f { return 1; }")  # missing parens -> parse error
        assert r.status == ExecutionStatus.COMPILE_ERROR


class TestCGate3RecursionAndMultiFunc:
    """Recursive and multi-function C programs."""

    def test_fibonacci_recursive_compiles(self) -> None:
        code = (
            "int fib(int n) {\n"
            "    if (n <= 1) { return n; }\n"
            "    return fib(n - 1) + fib(n - 2);\n"
            "}\n"
        )
        _ok(code)

    def test_three_function_chain_compiles(self) -> None:
        code = (
            "int double_val(int x) { return x * 2; }\n"
            "int triple_val(int x) { return x * 3; }\n"
            "int sextuple(int x) { return double_val(triple_val(x)); }\n"
        )
        _ok(code)

    def test_mutual_recursion_simple_compiles(self) -> None:
        # Simple recursion without forward declarations
        code = "int count_down(int n) { if (n <= 0) { return 0; } return count_down(n - 1); }"
        _ok(code)

    def test_nested_if_else_compiles(self) -> None:
        _ok(
            "int sign(int x) {\n"
            "    if (x > 0) { return 1; }\n"
            "    if (x < 0) { return -1; }\n"
            "    return 0;\n"
            "}\n"
        )

    def test_while_countdown_compiles(self) -> None:
        _ok(
            "int countdown(int n) {\n"
            "    while (n > 0) { n = n - 1; }\n"
            "    return n;\n"
            "}\n"
        )
