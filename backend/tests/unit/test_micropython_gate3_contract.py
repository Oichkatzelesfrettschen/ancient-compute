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


def _run(code: str) -> object:
    from backend.src.services.languages.micropython_service import MicroPythonService

    return asyncio.run(MicroPythonService().execute(code))


def _ok(code: str) -> None:
    from backend.src.services.languages.micropython_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.stderr}"


def _err_compile(code: str) -> None:
    from backend.src.services.languages.micropython_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, f"Expected COMPILE_ERROR, got {r.status}"


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
        _ok("s = 0\ni = 1\nwhile i <= 10:\n    s += i\n    i += 1\nprint(s)")

    def test_for_range_loop(self):
        """For loop over range()."""
        _ok("total = 0\nfor i in range(5):\n    total += i\nprint(total)")

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
        _ok("def gen(n):\n    for i in range(n):\n        yield i\nprint(list(gen(4)))")

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


class TestMicroPythonOutputVerification:
    """Verify stdout values from MicroPython programs."""

    def test_print_hello_stdout(self) -> None:
        r = _run('print("hello")')
        assert "hello" in r.stdout

    def test_arithmetic_result(self) -> None:
        r = _run("print(6 * 7)")
        assert "42" in r.stdout

    def test_factorial_5_is_120(self) -> None:
        r = _run(
            "def fact(n):\n"
            "    if n <= 1:\n"
            "        return 1\n"
            "    return n * fact(n - 1)\n"
            "print(fact(5))"
        )
        assert "120" in r.stdout

    def test_list_comprehension_squares(self) -> None:
        r = _run("print([x*x for x in range(4)])")
        assert "9" in r.stdout  # 3*3 is last element

    def test_for_range_sum(self) -> None:
        r = _run("s = 0\nfor i in range(5):\n    s += i\nprint(s)")
        assert "10" in r.stdout

    def test_while_countdown_result(self) -> None:
        r = _run("n = 5\nwhile n > 0:\n    n -= 1\nprint(n)")
        assert "0" in r.stdout


class TestMicroPythonRejectedPrograms:
    """Additional rejection cases."""

    def test_rejected_missing_colon(self) -> None:
        _err_compile("def f(x)\n    return x")

    def test_rejected_invalid_keyword(self) -> None:
        _err_compile("x = @invalid")

    def test_status_success_for_empty_program(self) -> None:
        from backend.src.services.languages.micropython_service import ExecutionStatus

        r = _run("x = 1")
        assert r.status == ExecutionStatus.SUCCESS


class TestMicroPythonContractExtended:
    """Additional MicroPython program patterns that must succeed."""

    def test_string_concatenation(self) -> None:
        _ok('print("hello" + " " + "world")')

    def test_integer_floor_division(self) -> None:
        _ok("print(7 // 2)")

    def test_boolean_and_expression(self) -> None:
        _ok("print(True and False)")

    def test_tuple_indexing(self) -> None:
        _ok("t = (10, 20, 30)\nprint(t[1])")

    def test_dict_access(self) -> None:
        _ok('d = {"key": 42}\nprint(d["key"])')

    def test_list_len(self) -> None:
        _ok("lst = [1, 2, 3, 4, 5]\nprint(len(lst))")

    def test_power_operator(self) -> None:
        _ok("print(2 ** 10)")


class TestMicroPythonOutputValues:
    """Verify correct stdout values from MicroPython programs."""

    def test_string_concat_output(self) -> None:
        r = _run('print("foo" + "bar")')
        assert "foobar" in r.stdout

    def test_floor_division_result(self) -> None:
        r = _run("print(7 // 2)")
        assert "3" in r.stdout

    def test_power_result_1024(self) -> None:
        r = _run("print(2 ** 10)")
        assert "1024" in r.stdout

    def test_tuple_index_result(self) -> None:
        r = _run("t = (10, 20, 30)\nprint(t[1])")
        assert "20" in r.stdout

    def test_dict_access_result(self) -> None:
        r = _run('d = {"x": 99}\nprint(d["x"])')
        assert "99" in r.stdout

    def test_recursive_fibonacci_8_is_21(self) -> None:
        r = _run(
            "def fib(n):\n"
            "    if n < 2:\n"
            "        return n\n"
            "    return fib(n-1) + fib(n-2)\n"
            "print(fib(8))"
        )
        assert "21" in r.stdout

    def test_list_sum_15(self) -> None:
        r = _run("s = 0\nfor x in [1, 2, 3, 4, 5]:\n    s += x\nprint(s)")
        assert "15" in r.stdout
