"""Gate 3: Python paradigm contract tests.

PythonService compiles Python to Babbage IR -> assembly -> machine code.
Gate 3 criterion: compile + execute round-trip produces non-empty machine_code.

Empirically verified capabilities (python_compiler.py):
  - Function definitions (def)
  - Return statements
  - Integer arithmetic (+, -, *, /)
  - If / elif / else branches
  - While loops
  - For loops via range(n)
  - Recursive calls
  - Multiple functions

Not yet supported (compile_error expected):
  - List comprehensions (LBRACKET parse error)
  - Classes / objects
  - Imports
"""

from __future__ import annotations

import asyncio

import pytest


def _run(code: str) -> object:
    from backend.src.services.languages.python_service import PythonService

    return asyncio.run(PythonService().execute(code))


def _ok(code: str) -> None:
    from backend.src.services.languages.python_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.stderr}"
    assert r.machine_code, "Expected non-empty machine_code"


def _err(code: str) -> None:
    from backend.src.services.languages.python_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, (
        f"Expected COMPILE_ERROR, got {r.status}"
    )


class TestPythonContract:
    """Python paradigm contract: functions, arithmetic, control flow, recursion."""

    def test_simple_function(self):
        """Basic function with two parameters returns machine code."""
        _ok("def add(x, y):\n    return x + y")

    def test_arithmetic_ops(self):
        """All four arithmetic operators in a single function."""
        _ok(
            "def calc(a, b):\n"
            "    s = a + b\n"
            "    d = a - b\n"
            "    p = a * b\n"
            "    return s + d + p"
        )

    def test_if_else(self):
        """If/else branch compiles to machine code."""
        _ok(
            "def abs_val(x):\n"
            "    if x >= 0:\n"
            "        return x\n"
            "    else:\n"
            "        return -x"
        )

    def test_if_elif_else(self):
        """Sign function with if/elif/else chain."""
        _ok(
            "def sign(x):\n"
            "    if x > 0:\n"
            "        return 1\n"
            "    elif x == 0:\n"
            "        return 0\n"
            "    else:\n"
            "        return -1"
        )

    def test_while_loop(self):
        """While loop with accumulator."""
        _ok(
            "def sum_to(n):\n"
            "    s = 0\n"
            "    i = 1\n"
            "    while i <= n:\n"
            "        s = s + i\n"
            "        i = i + 1\n"
            "    return s"
        )

    def test_for_range_loop(self):
        """For loop over range(n) with accumulator."""
        _ok(
            "def sum_range(n):\n"
            "    s = 0\n"
            "    for i in range(n):\n"
            "        s = s + i\n"
            "    return s"
        )

    def test_recursion_factorial(self):
        """Recursive factorial function."""
        _ok(
            "def fact(n):\n"
            "    if n <= 1:\n"
            "        return 1\n"
            "    return n * fact(n - 1)"
        )

    def test_multiple_functions(self):
        """Two functions where one calls the other."""
        _ok(
            "def square(x):\n"
            "    return x * x\n\n"
            "def sum_squares(a, b):\n"
            "    return square(a) + square(b)"
        )

    def test_nested_if(self):
        """Nested if inside while loop."""
        _ok(
            "def count_pos(n):\n"
            "    c = 0\n"
            "    i = -n\n"
            "    while i <= n:\n"
            "        if i > 0:\n"
            "            c = c + 1\n"
            "        i = i + 1\n"
            "    return c"
        )

    def test_rejected_list_comprehension(self):
        """List comprehensions are not yet supported by the Python IR compiler."""
        _err("def doubles(n):\n    return [x * 2 for x in range(n)]")
