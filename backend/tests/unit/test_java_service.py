"""Unit tests for JavaService.

Verifies compile + run pipeline against OpenJDK 17.
Skipped automatically if javac is not installed at the expected path.
"""

from __future__ import annotations

import asyncio
import os

import pytest

_JAVAC = "/usr/lib/jvm/java-17-openjdk/bin/javac"

pytestmark = pytest.mark.skipif(
    not os.path.exists(_JAVAC),
    reason="OpenJDK 17 javac not found at expected path",
)


def _run(code: str, input_data: str = "") -> object:
    from backend.src.services.languages.java_service import JavaService

    return asyncio.run(JavaService().execute(code, input_data))


def _check(code: str) -> object:
    from backend.src.services.languages.java_service import JavaService

    return asyncio.run(JavaService().execute_check(code))


def _ok(code: str, input_data: str = "") -> None:
    from backend.src.services.languages.java_service import ExecutionStatus

    r = _run(code, input_data)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.stderr}"


def _compile_error(code: str) -> None:
    from backend.src.services.languages.java_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, f"Expected COMPILE_ERROR: {r.stdout}"


def _runtime_error(code: str) -> None:
    from backend.src.services.languages.java_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.RUNTIME_ERROR, f"Expected RUNTIME_ERROR: {r.stdout}"


# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

_HELLO = """\
public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
"""

_ADD = """\
public class Main {
    public static void main(String[] args) {
        int a = 6;
        int b = 7;
        System.out.println(a + b);
    }
}
"""

_FIBONACCI = """\
public class Main {
    static int fib(int n) {
        if (n <= 1) return n;
        return fib(n - 1) + fib(n - 2);
    }

    public static void main(String[] args) {
        for (int i = 0; i < 8; i++) {
            System.out.print(fib(i));
            if (i < 7) System.out.print(" ");
        }
        System.out.println();
    }
}
"""

_STDIN_ECHO = """\
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        while (sc.hasNextLine()) {
            System.out.println(sc.nextLine());
        }
    }
}
"""

_GENERICS = """\
import java.util.ArrayList;
import java.util.List;

public class Main {
    public static void main(String[] args) {
        List<Integer> nums = new ArrayList<>();
        for (int i = 1; i <= 5; i++) nums.add(i);
        int sum = nums.stream().mapToInt(Integer::intValue).sum();
        System.out.println(sum);
    }
}
"""

_INTERFACE = """\
public class Main {
    interface Greeter { String greet(String name); }

    public static void main(String[] args) {
        Greeter g = name -> "Hello, " + name + "!";
        System.out.println(g.greet("Ada"));
    }
}
"""


class TestHappyPath:
    def test_hello_world(self) -> None:
        r = _run(_HELLO)
        from backend.src.services.languages.java_service import ExecutionStatus

        assert r.status == ExecutionStatus.SUCCESS
        assert "Hello, World!" in r.stdout

    def test_arithmetic(self) -> None:
        r = _run(_ADD)
        from backend.src.services.languages.java_service import ExecutionStatus

        assert r.status == ExecutionStatus.SUCCESS
        assert "13" in r.stdout

    def test_fibonacci(self) -> None:
        r = _run(_FIBONACCI)
        from backend.src.services.languages.java_service import ExecutionStatus

        assert r.status == ExecutionStatus.SUCCESS
        assert "0 1 1 2 3 5 8 13" in r.stdout

    def test_stdin_echo(self) -> None:
        r = _run(_STDIN_ECHO, "Babbage\nLovelace")
        from backend.src.services.languages.java_service import ExecutionStatus

        assert r.status == ExecutionStatus.SUCCESS
        assert "Babbage" in r.stdout
        assert "Lovelace" in r.stdout

    def test_generics_and_streams(self) -> None:
        r = _run(_GENERICS)
        from backend.src.services.languages.java_service import ExecutionStatus

        assert r.status == ExecutionStatus.SUCCESS
        assert "15" in r.stdout

    def test_lambda_and_interface(self) -> None:
        r = _run(_INTERFACE)
        from backend.src.services.languages.java_service import ExecutionStatus

        assert r.status == ExecutionStatus.SUCCESS
        assert "Hello, Ada!" in r.stdout

    def test_execution_time_recorded(self) -> None:
        r = _run(_HELLO)
        assert r.execution_time > 0.0


# ---------------------------------------------------------------------------
# Compile errors
# ---------------------------------------------------------------------------

_MISSING_SEMICOLON = """\
public class Main {
    public static void main(String[] args) {
        int x = 5
        System.out.println(x);
    }
}
"""

_UNDECLARED_VARIABLE = """\
public class Main {
    public static void main(String[] args) {
        System.out.println(undeclared);
    }
}
"""

_WRONG_CLASS_NAME = """\
public class WrongName {
    public static void main(String[] args) {
        System.out.println("wrong");
    }
}
"""

_TYPE_MISMATCH = """\
public class Main {
    public static void main(String[] args) {
        int x = "not an int";
        System.out.println(x);
    }
}
"""


class TestCompileErrors:
    def test_missing_semicolon(self) -> None:
        _compile_error(_MISSING_SEMICOLON)

    def test_undeclared_variable(self) -> None:
        _compile_error(_UNDECLARED_VARIABLE)

    def test_wrong_class_name(self) -> None:
        # javac requires public class name to match filename (Main.java)
        _compile_error(_WRONG_CLASS_NAME)

    def test_type_mismatch(self) -> None:
        _compile_error(_TYPE_MISMATCH)

    def test_empty_source(self) -> None:
        # Empty Java source compiles (javac accepts it), but java fails to find Main
        from backend.src.services.languages.java_service import ExecutionStatus

        r = _run("")
        assert r.status in (ExecutionStatus.COMPILE_ERROR, ExecutionStatus.RUNTIME_ERROR)

    def test_compile_error_has_stderr(self) -> None:
        r = _run(_MISSING_SEMICOLON)
        assert r.stderr or r.stdout  # javac writes diagnostics to stderr


# ---------------------------------------------------------------------------
# Runtime errors
# ---------------------------------------------------------------------------

_ARRAY_OUT_OF_BOUNDS = """\
public class Main {
    public static void main(String[] args) {
        int[] arr = new int[3];
        System.out.println(arr[10]);
    }
}
"""

_NULL_POINTER = """\
public class Main {
    public static void main(String[] args) {
        String s = null;
        System.out.println(s.length());
    }
}
"""

_EXPLICIT_EXIT_NONZERO = """\
public class Main {
    public static void main(String[] args) {
        System.out.println("before");
        System.exit(1);
    }
}
"""


class TestRuntimeErrors:
    def test_array_out_of_bounds(self) -> None:
        _runtime_error(_ARRAY_OUT_OF_BOUNDS)

    def test_null_pointer(self) -> None:
        _runtime_error(_NULL_POINTER)

    def test_explicit_nonzero_exit(self) -> None:
        _runtime_error(_EXPLICIT_EXIT_NONZERO)


# ---------------------------------------------------------------------------
# compile_check only
# ---------------------------------------------------------------------------


class TestCompileCheck:
    def test_check_valid_source(self) -> None:
        from backend.src.services.languages.java_service import ExecutionStatus

        r = _check(_HELLO)
        assert r.status == ExecutionStatus.SUCCESS

    def test_check_invalid_source(self) -> None:
        from backend.src.services.languages.java_service import ExecutionStatus

        r = _check(_MISSING_SEMICOLON)
        assert r.status == ExecutionStatus.COMPILE_ERROR


# ---------------------------------------------------------------------------
# Gate 3 contract
# ---------------------------------------------------------------------------


class TestGate3Contract:
    """Paradigm-specific Gate 3 verification for Java."""

    def test_oop_inheritance(self) -> None:
        _ok("""\
public class Main {
    static class Animal {
        String name;
        Animal(String n) { this.name = n; }
        String speak() { return name + " speaks"; }
    }
    static class Dog extends Animal {
        Dog(String n) { super(n); }
        @Override String speak() { return name + " barks"; }
    }
    public static void main(String[] args) {
        Animal a = new Dog("Rex");
        System.out.println(a.speak());
    }
}
""")

    def test_exception_handling(self) -> None:
        _ok("""\
public class Main {
    public static void main(String[] args) {
        try {
            int result = Integer.parseInt("not-a-number");
            System.out.println(result);
        } catch (NumberFormatException e) {
            System.out.println("caught: " + e.getMessage());
        }
    }
}
""")

    def test_checked_exception_declared(self) -> None:
        _ok("""\
public class Main {
    static int parse(String s) throws NumberFormatException {
        return Integer.parseInt(s);
    }
    public static void main(String[] args) {
        try {
            System.out.println(parse("42"));
        } catch (NumberFormatException e) {
            System.out.println("error");
        }
    }
}
""")

    def test_hello_world_compiles_and_runs(self) -> None:
        r = _run(_HELLO)
        from backend.src.services.languages.java_service import ExecutionStatus

        assert r.status == ExecutionStatus.SUCCESS
        assert "Hello, World!" in r.stdout
