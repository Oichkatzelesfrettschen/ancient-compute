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
        _ok(
            "int countdown(int n) {\n"
            "  while (n > 0) {\n"
            "    n = n - 1;\n"
            "  }\n"
            "  return n;\n"
            "}"
        )

    def test_recursive_factorial(self):
        """Recursive factorial (tail recursion via parameter arithmetic)."""
        _ok(
            "int fact(int n) {\n"
            "  if (n <= 1) {\n"
            "    return 1;\n"
            "  }\n"
            "  return n * fact(n - 1);\n"
            "}"
        )

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
        _ok("int max2(int a, int b) {\n" "  if (a > b) { return a; }\n" "  return b;\n" "}")

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
