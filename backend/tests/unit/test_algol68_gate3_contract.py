"""Gate 3: ALGOL68 paradigm contract tests.

Uses a68g 3.10.12 (/usr/bin/a68g) -- empirically verified:
  - a68g --check exits 0 for valid source, 1 for syntax errors
  - Comments require paired # markers (#...#)
  - Procedures: PROC name = (TYPE param) TYPE: body
  - Conditionals: IF ... THEN ... ELSE ... FI
  - Loops: FOR i FROM n TO m DO ... OD
  - Struct types: MODE TYPENAME = STRUCT(TYPE field, ...)
  - Field access: field OF struct_var
"""

from __future__ import annotations

import asyncio

import pytest


def _run(code: str) -> object:
    from backend.src.services.languages.algol68_service import ALGOL68Service

    return asyncio.run(ALGOL68Service().execute(code))


def _ok(code: str) -> None:
    from backend.src.services.languages.algol68_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.stderr}"


def _err(code: str) -> None:
    from backend.src.services.languages.algol68_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, (
        f"Expected COMPILE_ERROR, got {r.status}"
    )


class TestALGOL68Contract:
    """ALGOL68 paradigm contract: BEGIN/END, PROC, IF/FI, FOR/OD, MODE STRUCT."""

    def test_hello_world(self):
        """Minimal program: print a string."""
        _ok('BEGIN print(("hello", newline)) END')

    def test_integer_arithmetic(self):
        """Integer variable declaration and arithmetic."""
        _ok("BEGIN INT x := 6; INT y := 7; print((x * y, newline)) END")

    def test_proc_definition(self):
        """PROC with a return type and a parameter."""
        _ok(
            "BEGIN\n"
            "  PROC double = (INT n) INT: n * 2;\n"
            "  print((double(21), newline))\n"
            "END"
        )

    def test_conditional_if_fi(self):
        """IF ... THEN ... ELSE ... FI conditional."""
        _ok(
            "BEGIN\n"
            "  INT n := 5;\n"
            "  IF n > 0 THEN print((1, newline)) ELSE print((0, newline)) FI\n"
            "END"
        )

    def test_for_loop(self):
        """FOR ... FROM ... TO ... DO ... OD loop."""
        _ok(
            "BEGIN\n"
            "  INT s := 0;\n"
            "  FOR i FROM 1 TO 10 DO s +:= i OD;\n"
            "  print((s, newline))\n"
            "END"
        )

    def test_recursive_factorial(self):
        """Recursive PROC (factorial)."""
        _ok(
            "BEGIN\n"
            "  PROC fact = (INT n) INT:\n"
            "    IF n <= 1 THEN 1 ELSE n * fact(n - 1) FI;\n"
            "  print((fact(5), newline))\n"
            "END"
        )

    def test_mode_struct(self):
        """MODE (struct) declaration and field access via OF."""
        _ok(
            "BEGIN\n"
            "  MODE POINT = STRUCT(INT x, y);\n"
            "  POINT p = (3, 4);\n"
            "  print((x OF p + y OF p, newline))\n"
            "END"
        )

    def test_proc_two_params(self):
        """PROC with two parameters."""
        _ok(
            "BEGIN\n"
            "  PROC add = (INT a, INT b) INT: a + b;\n"
            "  print((add(10, 32), newline))\n"
            "END"
        )

    def test_array_declaration(self):
        """Array ([N] TYPE) declaration and indexed access."""
        _ok(
            "BEGIN\n"
            "  [5] INT arr;\n"
            "  FOR i FROM 1 TO 5 DO arr[i] := i OD;\n"
            "  print((arr[5], newline))\n"
            "END"
        )

    def test_nested_if(self):
        """Nested IF/FI (sign function)."""
        _ok(
            "BEGIN\n"
            "  INT n := -3;\n"
            "  IF n > 0 THEN print((1, newline))\n"
            "  ELIF n = 0 THEN print((0, newline))\n"
            "  ELSE print((-1, newline))\n"
            "  FI\n"
            "END"
        )

    def test_rejected_unclosed_paren(self):
        """Unclosed parenthesis is a syntax error."""
        _err("BEGIN print(( END")

    def test_rejected_invalid_token(self):
        """Invalid token sequence is rejected."""
        _err("BEGIN @@@ END")

    def test_rejected_unclosed_begin(self):
        """BEGIN without END is a syntax error."""
        _err("BEGIN INT x := 1")
