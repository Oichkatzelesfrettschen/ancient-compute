"""Gate 3: C++ paradigm contract tests.

CppService uses g++ 15.2.1 (/usr/bin/g++) with -std=c++20 -fsyntax-only.
Empirically verified (2026-03-20):
  - g++ -std=c++20 -fsyntax-only exits 0 for valid C++, 1 for errors
  - Classes, templates, lambdas, RAII all compile with GCC 15

Gate 3 criterion: syntax-check well-formed C++20 programs via -fsyntax-only.
"""

from __future__ import annotations

import asyncio


def _run(code: str) -> object:
    from backend.src.services.languages.cpp_service import CppService

    return asyncio.run(CppService().execute(code))


def _ok(code: str) -> None:
    from backend.src.services.languages.cpp_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.SUCCESS, f"Expected SUCCESS: {r.stderr}"


def _err(code: str) -> None:
    from backend.src.services.languages.cpp_service import ExecutionStatus

    r = _run(code)
    assert r.status == ExecutionStatus.COMPILE_ERROR, f"Expected COMPILE_ERROR, got {r.status}"


class TestCppContract:
    """C++20 paradigm contract: classes, templates, lambdas, RAII."""

    def test_free_function(self):
        """Simple freestanding function compiles."""
        _ok("int add(int a, int b) { return a + b; }\nint main() { return add(1, 2); }")

    def test_class_with_constructor(self):
        """Class with constructor and method."""
        _ok(
            "class Counter {\n"
            "    int count;\n"
            "public:\n"
            "    Counter() : count(0) {}\n"
            "    void inc() { count++; }\n"
            "    int get() const { return count; }\n"
            "};\n"
            "int main() { Counter c; c.inc(); return c.get(); }"
        )

    def test_function_template(self):
        """Generic function template (T max)."""
        _ok(
            "template<typename T>\n"
            "T tmax(T a, T b) { return a > b ? a : b; }\n"
            "int main() { return tmax(3, 5) - 5; }"
        )

    def test_class_template(self):
        """Generic class template (Pair<T, U>)."""
        _ok(
            "template<typename T, typename U>\n"
            "struct Pair {\n"
            "    T first;\n"
            "    U second;\n"
            "    Pair(T f, U s) : first(f), second(s) {}\n"
            "};\n"
            "int main() {\n"
            "    Pair<int, int> p(1, 2);\n"
            "    return p.first + p.second - 3;\n"
            "}"
        )

    def test_lambda_expression(self):
        """Lambda expression assigned to auto variable."""
        _ok(
            "int main() {\n"
            "    auto add = [](int a, int b) { return a + b; };\n"
            "    return add(2, 3) - 5;\n"
            "}"
        )

    def test_raii_destructor(self):
        """RAII: destructor runs when guard goes out of scope."""
        _ok(
            "int val = 0;\n"
            "struct Guard {\n"
            "    Guard() { val = 1; }\n"
            "    ~Guard() { val = 0; }\n"
            "};\n"
            "int main() { { Guard g; } return val; }"
        )

    def test_constexpr_function(self):
        """Compile-time constexpr function."""
        _ok("constexpr int sq(int x) { return x * x; }\n" "int main() { return sq(5) - 25; }")

    def test_inheritance(self):
        """Base class and derived class with virtual method."""
        _ok(
            "struct Shape {\n"
            "    virtual int area() const = 0;\n"
            "    virtual ~Shape() = default;\n"
            "};\n"
            "struct Square : Shape {\n"
            "    int side;\n"
            "    explicit Square(int s) : side(s) {}\n"
            "    int area() const override { return side * side; }\n"
            "};\n"
            "int main() { Square s(4); return s.area() - 16; }"
        )

    def test_auto_type_deduction(self):
        """auto type deduction in variable and return type."""
        _ok(
            "auto mul(auto a, auto b) { return a * b; }\n"
            "int main() { auto x = mul(3, 4); return x - 12; }"
        )

    def test_rejected_undeclared_variable(self):
        """Reference to undeclared variable is a compile error."""
        _err("int main() {\n" "    undeclared = 42;\n" "    return 0;\n" "}")

    def test_rejected_type_mismatch(self):
        """Passing wrong type to function is a compile error."""
        _err(
            "void takes_int(int x) {}\n"
            "int main() {\n"
            '    takes_int("not an int");\n'
            "    return 0;\n"
            "}"
        )
