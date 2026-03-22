"""ALGOL68 paradigm contract tests + extended service / abacus backend tests.

Uses a68g 3.10.12 (/usr/bin/a68g) -- empirically verified:
  - a68g --check exits 0 for valid source, 1 for syntax errors
  - Comments require COMMENT...COMMENT or paired # markers (#...#)
  - Procedures: PROC name = (TYPE param) TYPE: body
  - Conditionals: IF ... THEN ... ELSE ... FI
  - ELIF chains supported
  - Loops: FOR i FROM n TO m DO ... OD; WHILE cond DO ... OD
  - CASE n IN ..., ..., ... ESAC  (1-based selector)
  - Struct types: MODE TYPENAME = STRUCT(TYPE field, ...)
  - Field access: field OF struct_var
  - REAL arithmetic; BOOL type (TRUE/FALSE, prints as T/F)
  - ABS operator; UPB for string/array upper bound
  - STRING concatenation with +:= ; UPB s gives length
  - Runtime errors: "runtime error" in stderr + exit 1
"""

from __future__ import annotations

import asyncio
import pathlib
from typing import Any

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _run(code: str) -> Any:
    from backend.src.services.languages.algol68_service import ALGOL68Service

    return asyncio.run(ALGOL68Service().execute(code))


def _run_full(code: str, stdin: str = "") -> Any:
    from backend.src.services.languages.algol68_service import ALGOL68Service

    return asyncio.run(ALGOL68Service().execute_full(code, stdin))


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


def _out(code: str, stdin: str = "") -> str:
    """Run code and return stdout."""
    return _run_full(code, stdin).stdout


_A68_SOURCE = str(
    pathlib.Path(__file__).resolve().parents[3]
    / "tools"
    / "algol68"
    / "abacus.a68"
)


# ---------------------------------------------------------------------------
# Gate 3 contract: well-formed programs pass syntax check
# ---------------------------------------------------------------------------


class TestALGOL68Contract:
    """ALGOL68 paradigm contract: BEGIN/END, PROC, IF/FI, FOR/OD, MODE STRUCT."""

    def test_hello_world(self) -> None:
        _ok('BEGIN print(("hello", newline)) END')

    def test_integer_arithmetic(self) -> None:
        _ok("BEGIN INT x := 6; INT y := 7; print((x * y, newline)) END")

    def test_proc_definition(self) -> None:
        _ok("BEGIN\n  PROC double = (INT n) INT: n * 2;\n  print((double(21), newline))\nEND")

    def test_conditional_if_fi(self) -> None:
        _ok(
            "BEGIN\n"
            "  INT n := 5;\n"
            "  IF n > 0 THEN print((1, newline)) ELSE print((0, newline)) FI\n"
            "END"
        )

    def test_for_loop(self) -> None:
        _ok("BEGIN\n  INT s := 0;\n  FOR i FROM 1 TO 10 DO s +:= i OD;\n  print((s, newline))\nEND")

    def test_recursive_factorial(self) -> None:
        _ok(
            "BEGIN\n"
            "  PROC fact = (INT n) INT:\n"
            "    IF n <= 1 THEN 1 ELSE n * fact(n - 1) FI;\n"
            "  print((fact(5), newline))\n"
            "END"
        )

    def test_mode_struct(self) -> None:
        _ok(
            "BEGIN\n"
            "  MODE POINT = STRUCT(INT x, y);\n"
            "  POINT p = (3, 4);\n"
            "  print((x OF p + y OF p, newline))\n"
            "END"
        )

    def test_proc_two_params(self) -> None:
        _ok("BEGIN\n  PROC add = (INT a, INT b) INT: a + b;\n  print((add(10, 32), newline))\nEND")

    def test_array_declaration(self) -> None:
        _ok(
            "BEGIN\n"
            "  [5] INT arr;\n"
            "  FOR i FROM 1 TO 5 DO arr[i] := i OD;\n"
            "  print((arr[5], newline))\n"
            "END"
        )

    def test_nested_if(self) -> None:
        _ok(
            "BEGIN\n"
            "  INT n := -3;\n"
            "  IF n > 0 THEN print((1, newline))\n"
            "  ELIF n = 0 THEN print((0, newline))\n"
            "  ELSE print((-1, newline))\n"
            "  FI\n"
            "END"
        )

    def test_rejected_unclosed_paren(self) -> None:
        _err("BEGIN print(( END")

    def test_rejected_invalid_token(self) -> None:
        _err("BEGIN @@@ END")

    def test_rejected_unclosed_begin(self) -> None:
        _err("BEGIN INT x := 1")


# ---------------------------------------------------------------------------
# Output verification: actual stdout matches expected values
# ---------------------------------------------------------------------------


class TestALGOL68OutputVerification:
    """Verify actual stdout output from ALGOL68 programs (execute_full)."""

    def test_print_42(self) -> None:
        r = _run_full("BEGIN print((42, newline)) END")
        assert "42" in r.stdout

    def test_factorial_5_outputs_120(self) -> None:
        r = _run_full(
            "BEGIN\n"
            "  PROC fact = (INT n) INT:\n"
            "    IF n <= 1 THEN 1 ELSE n * fact(n - 1) FI;\n"
            "  print((fact(5), newline))\n"
            "END"
        )
        assert "120" in r.stdout

    def test_for_loop_sum_1_to_10(self) -> None:
        r = _run_full(
            "BEGIN\n  INT s := 0;\n  FOR i FROM 1 TO 10 DO s +:= i OD;\n  print((s, newline))\nEND"
        )
        assert "55" in r.stdout

    def test_true_struct_field_sum(self) -> None:
        r = _run_full(
            "BEGIN\n"
            "  MODE POINT = STRUCT(INT x, y);\n"
            "  POINT p = (3, 4);\n"
            "  print((x OF p + y OF p, newline))\n"
            "END"
        )
        assert "7" in r.stdout

    def test_status_is_success(self) -> None:
        from backend.src.services.languages.algol68_service import ExecutionStatus

        r = _run("BEGIN print((1, newline)) END")
        assert r.status == ExecutionStatus.SUCCESS


# ---------------------------------------------------------------------------
# Additional rejection cases
# ---------------------------------------------------------------------------


class TestALGOL68RejectedPrograms:
    """Rejection cases for ALGOL68 syntax / type errors."""

    def test_rejected_no_begin(self) -> None:
        _err("INT x := 5")

    def test_rejected_bad_proc_syntax(self) -> None:
        _err("BEGIN\n  PROC bad = INT: 42\nEND")

    def test_rejected_undeclared_variable(self) -> None:
        _err("BEGIN print((undefined_var, newline)) END")


# ---------------------------------------------------------------------------
# Data types: REAL, STRING, BOOL, CHAR
# ---------------------------------------------------------------------------


class TestALGOL68DataTypes:
    """Verify REAL, STRING, BOOL, and CHAR types parse and execute."""

    def test_real_variable(self) -> None:
        _ok("BEGIN REAL x := 3.14; print((x, newline)) END")

    def test_real_arithmetic(self) -> None:
        out = _out("BEGIN REAL x := 3.0; print((x * x, newline)) END")
        assert "9" in out

    def test_real_abs(self) -> None:
        out = _out("BEGIN REAL x := -4.5; print((ABS x, newline)) END")
        assert "4.5" in out

    def test_string_variable(self) -> None:
        out = _out('BEGIN STRING s := "hello"; print((s, newline)) END')
        assert "hello" in out

    def test_string_upb_length(self) -> None:
        out = _out('BEGIN STRING s := "hello"; print((UPB s, newline)) END')
        assert "5" in out

    def test_string_concatenation(self) -> None:
        out = _out('BEGIN STRING s := "foo"; s +:= "bar"; print((s, newline)) END')
        assert "foobar" in out

    def test_bool_true(self) -> None:
        out = _out("BEGIN BOOL b := TRUE; print((b, newline)) END")
        assert "T" in out

    def test_bool_false(self) -> None:
        out = _out("BEGIN BOOL b := FALSE; print((b, newline)) END")
        assert "F" in out

    def test_char_abs_gives_ascii(self) -> None:
        out = _out('BEGIN CHAR c := "A"; print((ABS c, newline)) END')
        assert "65" in out

    def test_int_over_is_integer_division(self) -> None:
        out = _out("BEGIN INT x := 17 OVER 3; print((x, newline)) END")
        assert "5" in out

    def test_int_mod_gives_remainder(self) -> None:
        out = _out("BEGIN INT x := 17 MOD 3; print((x, newline)) END")
        assert "2" in out

    def test_real_declaration_ok(self) -> None:
        _ok("BEGIN REAL pi := 3.14159; REAL e := 2.71828; print((pi + e, newline)) END")


# ---------------------------------------------------------------------------
# Operators
# ---------------------------------------------------------------------------


class TestALGOL68Operators:
    """Arithmetic, comparison, and logical operators."""

    def test_add(self) -> None:
        out = _out("BEGIN print((3 + 4, newline)) END")
        assert "7" in out

    def test_subtract(self) -> None:
        out = _out("BEGIN print((10 - 4, newline)) END")
        assert "6" in out

    def test_multiply(self) -> None:
        out = _out("BEGIN print((6 * 7, newline)) END")
        assert "42" in out

    def test_integer_division(self) -> None:
        out = _out("BEGIN print((10 OVER 3, newline)) END")
        assert "3" in out

    def test_modulo(self) -> None:
        out = _out("BEGIN print((10 MOD 3, newline)) END")
        assert "1" in out

    def test_comparison_equal(self) -> None:
        out = _out("BEGIN print((3 = 3, newline)) END")
        assert "T" in out

    def test_comparison_not_equal(self) -> None:
        out = _out("BEGIN print((3 /= 4, newline)) END")
        assert "T" in out

    def test_comparison_less_than(self) -> None:
        out = _out("BEGIN print((2 < 5, newline)) END")
        assert "T" in out

    def test_comparison_greater_equal(self) -> None:
        out = _out("BEGIN print((5 >= 5, newline)) END")
        assert "T" in out

    def test_logical_and(self) -> None:
        out = _out("BEGIN print((TRUE AND FALSE, newline)) END")
        assert "F" in out

    def test_logical_or(self) -> None:
        out = _out("BEGIN print((TRUE OR FALSE, newline)) END")
        assert "T" in out

    def test_logical_not(self) -> None:
        out = _out("BEGIN print((NOT TRUE, newline)) END")
        assert "F" in out


# ---------------------------------------------------------------------------
# Control flow: WHILE, CASE, ELIF, FOR BY
# ---------------------------------------------------------------------------


class TestALGOL68ControlFlow:
    """Extended control flow: WHILE, CASE, ELIF chains, FOR BY."""

    def test_while_loop_sum(self) -> None:
        out = _out(
            "BEGIN INT i:=1; INT s:=0; WHILE i<=5 DO s+:=i; i+:=1 OD; print((s,newline)) END"
        )
        assert "15" in out

    def test_case_selector_1(self) -> None:
        out = _out(
            "BEGIN INT n:=1; CASE n IN print((\"one\",newline)), print((\"two\",newline)) ESAC END"
        )
        assert "one" in out

    def test_case_selector_2(self) -> None:
        out = _out(
            "BEGIN INT n:=2; CASE n IN print((\"one\",newline)), print((\"two\",newline)) ESAC END"
        )
        assert "two" in out

    def test_elif_chain(self) -> None:
        out = _out(
            "BEGIN INT n:=-3;\n"
            "IF n>0 THEN print((1,newline))\n"
            "ELIF n=0 THEN print((0,newline))\n"
            "ELSE print((-1,newline)) FI END"
        )
        assert "-1" in out

    def test_for_by_negative(self) -> None:
        out = _out(
            "BEGIN INT s:=0; FOR i FROM 5 BY -1 TO 1 DO s+:=i OD; print((s,newline)) END"
        )
        assert "15" in out

    def test_for_from_1_to_0_skips(self) -> None:
        out = _out(
            "BEGIN INT s:=99; FOR i FROM 1 TO 0 DO s+:=i OD; print((s,newline)) END"
        )
        assert "99" in out

    def test_nested_while(self) -> None:
        out = _out(
            "BEGIN\n"
            "  INT s := 0; INT i := 1;\n"
            "  WHILE i <= 3 DO\n"
            "    INT j := 1;\n"
            "    WHILE j <= 3 DO s +:= 1; j +:= 1 OD;\n"
            "    i +:= 1\n"
            "  OD;\n"
            "  print((s, newline))\n"
            "END"
        )
        assert "9" in out

    def test_if_without_else(self) -> None:
        out = _out(
            "BEGIN INT n:=5;\n"
            "IF n > 0 THEN print((\"pos\", newline)) FI END"
        )
        assert "pos" in out


# ---------------------------------------------------------------------------
# Procedures: BOOL return, nested calls, higher-order patterns
# ---------------------------------------------------------------------------


class TestALGOL68Procedures:
    """PROC definitions, BOOL return, nested calls."""

    def test_proc_returning_bool(self) -> None:
        out = _out("BEGIN PROC even=(INT n)BOOL: n MOD 2=0; print((even(4),newline)) END")
        assert "T" in out

    def test_proc_returning_bool_odd(self) -> None:
        out = _out("BEGIN PROC even=(INT n)BOOL: n MOD 2=0; print((even(7),newline)) END")
        assert "F" in out

    def test_nested_proc_calls(self) -> None:
        out = _out(
            "BEGIN PROC sq=(INT n)INT: n*n;\n"
            "PROC cube=(INT n)INT: n*sq(n);\n"
            "print((cube(3),newline)) END"
        )
        assert "27" in out

    def test_proc_with_real_arg(self) -> None:
        out = _out("BEGIN PROC sq=(REAL x)REAL: x*x; print((sq(1.5),newline)) END")
        assert "2.25" in out

    def test_recursive_fib(self) -> None:
        out = _out(
            "BEGIN\n"
            "  PROC fib=(INT n)INT:\n"
            "    IF n<=1 THEN n ELSE fib(n-1)+fib(n-2) FI;\n"
            "  print((fib(10),newline))\n"
            "END"
        )
        assert "55" in out

    def test_proc_no_params(self) -> None:
        out = _out("BEGIN PROC greet=VOID: print((\"hi\",newline)); greet END")
        assert "hi" in out

    def test_proc_used_in_if(self) -> None:
        out = _out(
            "BEGIN PROC pos=(INT n)BOOL: n>0;\n"
            "INT x:=3;\n"
            "IF pos(x) THEN print((\"yes\",newline)) ELSE print((\"no\",newline)) FI END"
        )
        assert "yes" in out

    def test_proc_multiple_calls(self) -> None:
        out = _out(
            "BEGIN PROC double=(INT n)INT: n*2;\n"
            "print((double(double(3)),newline)) END"
        )
        assert "12" in out


# ---------------------------------------------------------------------------
# Arrays: INT, REAL, multi-dim (via multiple arrays)
# ---------------------------------------------------------------------------


class TestALGOL68Arrays:
    """Array declarations, indexed access, and loop-fill."""

    def test_int_array_fill_and_read(self) -> None:
        out = _out(
            "BEGIN [5] INT a;\n"
            "FOR i FROM 1 TO 5 DO a[i]:=i*i OD;\n"
            "print((a[3],newline)) END"
        )
        assert "9" in out

    def test_real_array(self) -> None:
        out = _out(
            "BEGIN [3] REAL arr;\n"
            "arr[1]:=1.1; arr[2]:=2.2; arr[3]:=3.3;\n"
            "print((arr[2],newline)) END"
        )
        assert "2.2" in out

    def test_array_sum(self) -> None:
        out = _out(
            "BEGIN [4] INT a; a[1]:=10; a[2]:=20; a[3]:=30; a[4]:=40;\n"
            "INT s:=0;\n"
            "FOR i FROM 1 TO 4 DO s +:= a[i] OD;\n"
            "print((s, newline)) END"
        )
        assert "100" in out

    def test_array_reverse_print(self) -> None:
        out = _out(
            "BEGIN [3] INT a; a[1]:=1; a[2]:=2; a[3]:=3;\n"
            "FOR i FROM 3 BY -1 TO 1 DO print((a[i],\" \")) OD;\n"
            "print(newline) END"
        )
        assert "3" in out

    def test_array_single_element(self) -> None:
        out = _out("BEGIN [1] INT a; a[1]:=99; print((a[1],newline)) END")
        assert "99" in out


# ---------------------------------------------------------------------------
# ALGOL68Service attributes and API
# ---------------------------------------------------------------------------


class TestALGOL68ServiceAttributes:
    """ALGOL68Service constructor and attribute contracts."""

    def test_default_timeout(self) -> None:
        from backend.src.services.languages.algol68_service import (
            _DEFAULT_TIMEOUT,
            ALGOL68Service,
        )

        svc = ALGOL68Service()
        assert svc.timeout == _DEFAULT_TIMEOUT

    def test_custom_timeout(self) -> None:
        from backend.src.services.languages.algol68_service import ALGOL68Service

        svc = ALGOL68Service(timeout=30.0)
        assert svc.timeout == 30.0

    def test_execution_status_values(self) -> None:
        from backend.src.services.languages.algol68_service import ExecutionStatus

        assert ExecutionStatus.SUCCESS.value == "success"
        assert ExecutionStatus.COMPILE_ERROR.value == "compile_error"
        assert ExecutionStatus.RUNTIME_ERROR.value == "runtime_error"
        assert ExecutionStatus.TIMEOUT.value == "timeout"

    def test_execution_result_fields(self) -> None:
        from backend.src.services.languages.algol68_service import (
            ExecutionResult,
            ExecutionStatus,
        )

        r = ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="hello",
            stderr="",
        )
        assert r.stdout == "hello"
        assert r.stderr == ""
        assert r.execution_time == 0.0
        assert r.exit_code == 0

    def test_result_has_execution_time(self) -> None:
        from backend.src.services.languages.algol68_service import ALGOL68Service

        r = asyncio.run(ALGOL68Service().execute("BEGIN print((1,newline)) END"))
        assert r.execution_time > 0.0

    def test_result_has_exit_code_zero_on_success(self) -> None:
        from backend.src.services.languages.algol68_service import ALGOL68Service

        r = asyncio.run(ALGOL68Service().execute("BEGIN print((1,newline)) END"))
        assert r.exit_code == 0

    def test_result_has_nonzero_exit_on_error(self) -> None:
        from backend.src.services.languages.algol68_service import ALGOL68Service

        r = asyncio.run(ALGOL68Service().execute("BEGIN @@@ END"))
        assert r.exit_code != 0

    def test_execute_returns_execution_result(self) -> None:
        from backend.src.services.languages.algol68_service import (
            ALGOL68Service,
            ExecutionResult,
        )

        r = asyncio.run(ALGOL68Service().execute("BEGIN print((1,newline)) END"))
        assert isinstance(r, ExecutionResult)


# ---------------------------------------------------------------------------
# Runtime error detection and classification
# ---------------------------------------------------------------------------


class TestALGOL68RuntimeErrors:
    """Verify runtime errors are classified as RUNTIME_ERROR (not COMPILE_ERROR)."""

    def test_division_by_zero_int_is_runtime_error(self) -> None:
        from backend.src.services.languages.algol68_service import (
            ALGOL68Service,
            ExecutionStatus,
        )

        code = "BEGIN INT x:=10; INT y:=0; print((x OVER y, newline)) END"
        r = asyncio.run(ALGOL68Service().execute_full(code))
        assert r.status == ExecutionStatus.RUNTIME_ERROR

    def test_division_by_zero_stderr_contains_runtime_error(self) -> None:
        from backend.src.services.languages.algol68_service import ALGOL68Service

        code = "BEGIN INT x:=5; INT y:=0; print((x OVER y, newline)) END"
        r = asyncio.run(ALGOL68Service().execute_full(code))
        assert "runtime error" in r.stderr.lower()

    def test_syntax_error_is_compile_error(self) -> None:
        from backend.src.services.languages.algol68_service import (
            ALGOL68Service,
            ExecutionStatus,
        )

        r = asyncio.run(ALGOL68Service().execute_full("BEGIN @@@ END"))
        assert r.status == ExecutionStatus.COMPILE_ERROR

    def test_classify_error_runtime_pattern(self) -> None:
        from backend.src.services.languages.algol68_service import (
            ExecutionStatus,
            _classify_error,
        )

        assert _classify_error("runtime error: division by zero") == ExecutionStatus.RUNTIME_ERROR

    def test_classify_error_non_runtime_gives_compile_error(self) -> None:
        from backend.src.services.languages.algol68_service import (
            ExecutionStatus,
            _classify_error,
        )

        assert _classify_error("syntax error at line 1") == ExecutionStatus.COMPILE_ERROR

    def test_classify_error_stack_overflow(self) -> None:
        from backend.src.services.languages.algol68_service import (
            ExecutionStatus,
            _classify_error,
        )

        assert _classify_error("stack overflow") == ExecutionStatus.RUNTIME_ERROR


# ---------------------------------------------------------------------------
# Stdin interaction: execute_with_input and execute_full with input_data
# ---------------------------------------------------------------------------


class TestALGOL68StdinInteraction:
    """Programs that read from stdin (execute_full / execute_with_input)."""

    def test_stdin_square(self) -> None:
        code = "BEGIN INT n; read((n)); print((n*n, newline)) END"
        out = _out(code, stdin="7")
        assert "49" in out

    def test_stdin_sum_two(self) -> None:
        code = "BEGIN INT a; INT b; read((a)); read((b)); print((a+b, newline)) END"
        out = _out(code, stdin="13 29")
        assert "42" in out

    def test_execute_with_input_alias(self) -> None:
        from backend.src.services.languages.algol68_service import ALGOL68Service

        code = "BEGIN INT n; read((n)); print((n, newline)) END"
        r = asyncio.run(ALGOL68Service().execute_with_input(code, "99"))
        assert "99" in r.stdout

    def test_stdin_loop_product(self) -> None:
        code = (
            "BEGIN\n"
            "  INT n; read((n));\n"
            "  INT p:=1;\n"
            "  FOR i FROM 1 TO n DO p*:=i OD;\n"
            "  print((p, newline))\n"
            "END"
        )
        out = _out(code, stdin="5")
        assert "120" in out

    def test_stdin_conditional(self) -> None:
        code = (
            "BEGIN INT n; read((n));\n"
            "IF n > 0 THEN print((\"pos\",newline)) ELSE print((\"neg\",newline)) FI END"
        )
        assert "pos" in _out(code, stdin="3")
        assert "neg" in _out(code, stdin="-1")

    def test_execute_full_no_stdin(self) -> None:
        from backend.src.services.languages.algol68_service import (
            ALGOL68Service,
            ExecutionStatus,
        )

        r = asyncio.run(ALGOL68Service().execute_full("BEGIN print((7, newline)) END"))
        assert r.status == ExecutionStatus.SUCCESS
        assert "7" in r.stdout


# ---------------------------------------------------------------------------
# File-based execution: execute_file and execute_file_check
# ---------------------------------------------------------------------------


class TestALGOL68FileExecution:
    """execute_file and execute_file_check on existing .a68 source files."""

    def test_execute_file_check_abacus_is_valid(self) -> None:
        from backend.src.services.languages.algol68_service import (
            ALGOL68Service,
            ExecutionStatus,
        )

        r = asyncio.run(ALGOL68Service().execute_file_check(_A68_SOURCE))
        assert r.status == ExecutionStatus.SUCCESS

    def test_execute_file_abacus_load_add(self) -> None:
        from backend.src.services.languages.algol68_service import (
            ALGOL68Service,
            ExecutionStatus,
        )

        stdin = "1 100\n2 23\n0\n"
        r = asyncio.run(ALGOL68Service().execute_file(_A68_SOURCE, input_data=stdin))
        assert r.status == ExecutionStatus.SUCCESS
        assert "123" in r.stdout

    def test_execute_file_abacus_reset(self) -> None:
        from backend.src.services.languages.algol68_service import ALGOL68Service

        stdin = "1 999\n4\n0\n"
        r = asyncio.run(ALGOL68Service().execute_file(_A68_SOURCE, input_data=stdin))
        assert "total=0" in r.stdout

    def test_execute_file_abacus_sub_floors_at_zero(self) -> None:
        from backend.src.services.languages.algol68_service import ALGOL68Service

        stdin = "1 5\n3 10\n0\n"  # 5 - 10 -> floor at 0
        r = asyncio.run(ALGOL68Service().execute_file(_A68_SOURCE, input_data=stdin))
        assert "total=0" in r.stdout

    def test_execute_file_check_accepts_valid_source(self) -> None:
        import os
        import tempfile

        from backend.src.services.languages.algol68_service import (
            ALGOL68Service,
            ExecutionStatus,
        )

        code = "BEGIN print((1, newline)) END"
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".a68", delete=False, encoding="utf-8"
        ) as fh:
            fh.write(code)
            tmp = fh.name
        try:
            r = asyncio.run(ALGOL68Service().execute_file_check(tmp))
            assert r.status == ExecutionStatus.SUCCESS
        finally:
            os.unlink(tmp)

    def test_execute_file_check_rejects_bad_source(self) -> None:
        import os
        import tempfile

        from backend.src.services.languages.algol68_service import (
            ALGOL68Service,
            ExecutionStatus,
        )

        code = "BEGIN @@@ END"
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".a68", delete=False, encoding="utf-8"
        ) as fh:
            fh.write(code)
            tmp = fh.name
        try:
            r = asyncio.run(ALGOL68Service().execute_file_check(tmp))
            assert r.status == ExecutionStatus.COMPILE_ERROR
        finally:
            os.unlink(tmp)


# ---------------------------------------------------------------------------
# Parse utility: _parse_abacus_a68_output
# ---------------------------------------------------------------------------


class TestParseAbacusA68Output:
    """Unit tests for _parse_abacus_a68_output."""

    def _parse(self, text: str) -> dict[str, Any]:
        from backend.src.emulator.adapter import _parse_abacus_a68_output

        return _parse_abacus_a68_output(text)

    def test_parse_zero(self) -> None:
        r = self._parse("total=0\nheaven=0 0 0 0 0 0 0 0 0\nearth=0 0 0 0 0 0 0 0 0\n")
        assert r["total"] == 0
        assert r["heaven"] == [0] * 9
        assert r["earth"] == [0] * 9

    def test_parse_known_value(self) -> None:
        r = self._parse(
            "total=12812\n"
            "heaven=0 0 1 0 0 0 0 0 0\n"
            "earth=2 1 3 2 1 0 0 0 0\n"
        )
        assert r["total"] == 12812
        assert r["heaven"][2] == 1  # hundreds column has 1 heaven bead
        assert r["earth"][0] == 2  # units column has 2 earth beads

    def test_parse_nine_in_each_column(self) -> None:
        r = self._parse(
            "total=999999999\n"
            "heaven=1 1 1 1 1 1 1 1 1\n"
            "earth=4 4 4 4 4 4 4 4 4\n"
        )
        assert r["total"] == 999999999
        assert all(h == 1 for h in r["heaven"])
        assert all(e == 4 for e in r["earth"])

    def test_parse_empty_returns_zero(self) -> None:
        r = self._parse("")
        assert r["total"] == 0

    def test_parse_returns_last_total_block(self) -> None:
        # Two print_state outputs stacked; should return the last total
        text = (
            "total=100\nheaven=0 0 0 0 0 0 0 0 0\nearth=0 0 1 0 0 0 0 0 0\n"
            "total=200\nheaven=0 0 0 0 0 0 0 0 0\nearth=0 0 2 0 0 0 0 0 0\n"
        )
        r = self._parse(text)
        assert r["total"] == 200

    def test_parse_heaven_list_length(self) -> None:
        r = self._parse(
            "total=5\nheaven=1 0 0 0 0 0 0 0 0\nearth=0 0 0 0 0 0 0 0 0\n"
        )
        assert len(r["heaven"]) == 9

    def test_parse_earth_list_length(self) -> None:
        r = self._parse(
            "total=3\nheaven=0 0 0 0 0 0 0 0 0\nearth=3 0 0 0 0 0 0 0 0\n"
        )
        assert len(r["earth"]) == 9

    def test_parse_heaven_encodes_five(self) -> None:
        r = self._parse(
            "total=5\nheaven=1 0 0 0 0 0 0 0 0\nearth=0 0 0 0 0 0 0 0 0\n"
        )
        assert r["heaven"][0] == 1  # column 1 has heaven bead activated


# ---------------------------------------------------------------------------
# ALGOL68 abacus backend integration
# ---------------------------------------------------------------------------


class TestALGOL68AbacusBackend:
    """AbacusAdapter.execute_algol68_ops integration tests."""

    def _make_adapter(self) -> Any:
        from backend.src.emulator.abacus import AbacusEmulator
        from backend.src.emulator.adapter import AbacusAdapter

        return AbacusAdapter(AbacusEmulator())

    def test_load_sets_value(self) -> None:
        a = self._make_adapter()
        r = a.execute_algol68_ops([{"op": "load", "value": 42}])
        assert r["total"] == 42

    def test_add_increments_value(self) -> None:
        a = self._make_adapter()
        a.execute_algol68_ops([{"op": "load", "value": 100}])
        r = a.execute_algol68_ops([{"op": "add", "value": 23}])
        assert r["total"] == 123

    def test_sub_decrements_value(self) -> None:
        a = self._make_adapter()
        a.execute_algol68_ops([{"op": "load", "value": 100}, {"op": "sub", "value": 37}])
        assert a.machine.state()["value"] == 63

    def test_sub_floors_at_zero(self) -> None:
        a = self._make_adapter()
        r = a.execute_algol68_ops([{"op": "load", "value": 5}, {"op": "sub", "value": 10}])
        assert r["total"] == 0

    def test_reset_clears_board(self) -> None:
        a = self._make_adapter()
        a.execute_algol68_ops([{"op": "load", "value": 9999}, {"op": "reset"}])
        r = a.execute_algol68_ops([{"op": "add", "value": 0}])
        assert r["total"] == 0

    def test_machine_synced_after_ops(self) -> None:
        a = self._make_adapter()
        a.execute_algol68_ops([{"op": "load", "value": 777}])
        assert a.machine.state()["value"] == 777

    def test_heaven_bead_set_for_five(self) -> None:
        a = self._make_adapter()
        r = a.execute_algol68_ops([{"op": "load", "value": 5}])
        assert r["heaven"][0] == 1  # units column: heaven bead activated

    def test_earth_beads_set_for_four(self) -> None:
        a = self._make_adapter()
        r = a.execute_algol68_ops([{"op": "load", "value": 4}])
        assert r["earth"][0] == 4  # units column: 4 earth beads

    def test_nine_in_units(self) -> None:
        a = self._make_adapter()
        r = a.execute_algol68_ops([{"op": "load", "value": 9}])
        assert r["heaven"][0] == 1
        assert r["earth"][0] == 4

    def test_ops_counter_incremented(self) -> None:
        a = self._make_adapter()
        initial = a.get_cycle_count()
        a.execute_algol68_ops([{"op": "load", "value": 1}, {"op": "add", "value": 1}])
        assert a.get_cycle_count() == initial + 2

    def test_snapshot_includes_heaven_earth(self) -> None:
        a = self._make_adapter()
        a.execute_algol68_ops([{"op": "load", "value": 5}])
        snap = a.get_snapshot()
        assert "heaven" in snap
        assert "earth" in snap

    def test_register_values_include_bead_state(self) -> None:
        a = self._make_adapter()
        a.execute_algol68_ops([{"op": "load", "value": 6}])
        rv = a.get_register_values()
        assert "value" in rv
        assert "heaven" in rv
        assert "earth" in rv

    def test_carry_propagation_in_add(self) -> None:
        a = self._make_adapter()
        r = a.execute_algol68_ops([{"op": "load", "value": 99}, {"op": "add", "value": 1}])
        assert r["total"] == 100

    def test_chain_of_adds(self) -> None:
        a = self._make_adapter()
        ops = [{"op": "add", "value": 10}] * 5  # 0 + 10*5 = 50
        r = a.execute_algol68_ops(ops)
        assert r["total"] == 50
