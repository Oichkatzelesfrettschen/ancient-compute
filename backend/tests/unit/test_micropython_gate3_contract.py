"""Gate 3: MicroPython paradigm contract tests.

MicroPythonService runs source via micropython 1.27.0 Unix port.
Empirically verified (2026-03-20):
  - micropython v1.27.0, Python 3.4.0 compatible subset
  - Functions, recursion, list comprehensions, generators all work
  - exit 0 on success, exit 1 + stderr on error (SyntaxError, NameError, etc.)

Gate 3 criterion: execute program to completion (ExecutionStatus.SUCCESS).
"""

from __future__ import annotations

import asyncio

import pytest


def _run(code: str) -> object:
    from backend.src.services.languages.micropython_service import MicroPythonService

    return asyncio.run(MicroPythonService().execute(code))


def _ok(code: str) -> None:
    from backend.src.services.languages.micropython_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.SUCCESS, (
        f"Expected SUCCESS: {r.stderr}"
    )


def _err_compile(code: str) -> None:
    from backend.src.services.languages.micropython_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, (
        f"Expected COMPILE_ERROR, got {r.status}"
    )


class TestMicroPythonContract:
    """MicroPython paradigm contract: functions, control flow, recursion, comprehensions."""

    def test_print_hello(self):
        """Minimal program: print a string."""
        _ok('print("hello")')

    def test_arithmetic(self):
        """Integer arithmetic."""
        _ok("x = 6 * 7\nprint(x)")

    def test_function_definition(self):
        """Function with two parameters."""
        _ok("def add(a, b): return a + b\nprint(add(3, 4))")

    def test_if_else(self):
        """if/else branch."""
        _ok(
            "def sign(x):\n"
            "    if x > 0:\n"
            "        return 1\n"
            "    elif x == 0:\n"
            "        return 0\n"
            "    else:\n"
            "        return -1\n"
            "print(sign(-5))"
        )

    def test_while_loop(self):
        """While loop with accumulator."""
        _ok(
            "s = 0\n"
            "i = 1\n"
            "while i <= 10:\n"
            "    s += i\n"
            "    i += 1\n"
            "print(s)"
        )

    def test_for_range_loop(self):
        """For loop over range()."""
        _ok(
            "total = 0\n"
            "for i in range(5):\n"
            "    total += i\n"
            "print(total)"
        )

    def test_recursive_factorial(self):
        """Recursive factorial."""
        _ok(
            "def fact(n):\n"
            "    if n <= 1:\n"
            "        return 1\n"
            "    return n * fact(n - 1)\n"
            "print(fact(5))"
        )

    def test_list_comprehension(self):
        """List comprehension (supported in MicroPython)."""
        _ok("squares = [x*x for x in range(5)]\nprint(squares)")

    def test_generator(self):
        """Generator function with yield."""
        _ok(
            "def gen(n):\n"
            "    for i in range(n):\n"
            "        yield i\n"
            "print(list(gen(4)))"
        )

    def test_closure(self):
        """Closure capturing outer variable."""
        _ok(
            "def make_adder(n):\n"
            "    def adder(x):\n"
            "        return x + n\n"
            "    return adder\n"
            "add5 = make_adder(5)\n"
            "print(add5(3))"
        )

    def test_multiple_functions(self):
        """Two functions where one calls the other."""
        _ok(
            "def square(x):\n"
            "    return x * x\n\n"
            "def hypotenuse(a, b):\n"
            "    return square(a) + square(b)\n\n"
            "print(hypotenuse(3, 4))"
        )

    def test_rejected_syntax_error(self):
        """Unclosed parenthesis is a SyntaxError (COMPILE_ERROR)."""
        _err_compile("def f(\n    pass")

    def test_rejected_invalid_indentation(self):
        """Mixed indentation or bad indent is a SyntaxError (COMPILE_ERROR)."""
        _err_compile("def f():\nreturn 1")
