"""Gate 2 freestanding C subset tests and diagnostics."""

from __future__ import annotations

import asyncio

import pytest

from backend.src.services.languages.c_service import CService, ExecutionStatus


def _execute(code: str):
    return asyncio.run(CService().execute(code))


def test_freestanding_subset_accepts_scalar_core_program() -> None:
    code = """
    int main() {
        int x;
        x = 10;
        return x;
    }
    """
    result = _execute(code)
    assert result.status == ExecutionStatus.SUCCESS


@pytest.mark.parametrize(
    "expected_label, code",
    [
        (
            "switch statements",
            """
            int classify(int x) {
                switch (x) {
                    case 1: return 1;
                    default: return 0;
                }
            }
            """,
        ),
        (
            "struct/union/enum types",
            """
            int main() {
                struct S { int x; };
                return 0;
            }
            """,
        ),
        (
            "preprocessor directives",
            """
            #include <stdio.h>
            int main() {
                return 0;
            }
            """,
        ),
        (
            "pointer declarators",
            """
            int deref(int *p) {
                return 0;
            }
            """,
        ),
    ],
)
def test_freestanding_subset_rejects_unsupported_features(expected_label: str, code: str) -> None:
    result = _execute(code)
    assert result.status == ExecutionStatus.COMPILE_ERROR
    assert "Unsupported C feature(s) for freestanding subset" in result.stderr
    assert expected_label in result.stderr
    assert "FREESTANDING_C_SUBSET_PROFILE.md" in result.stderr


class TestCFreestandingSubsetSupported:
    """Features that the freestanding subset DOES support."""

    def test_two_parameter_function_compiles(self) -> None:
        result = _execute("int add(int a, int b) { return a + b; }")
        assert result.status == ExecutionStatus.SUCCESS

    def test_recursive_function_compiles(self) -> None:
        result = _execute(
            "int fact(int n) {\n" "  if (n <= 1) { return 1; }\n" "  return n * fact(n - 1);\n" "}"
        )
        assert result.status == ExecutionStatus.SUCCESS

    def test_if_else_compiles(self) -> None:
        result = _execute(
            "int abs_val(int x) {\n" "  if (x >= 0) { return x; } else { return -x; }\n" "}"
        )
        assert result.status == ExecutionStatus.SUCCESS

    def test_while_loop_compiles(self) -> None:
        result = _execute(
            "int countdown(int n) {\n" "  while (n > 0) { n = n - 1; }\n" "  return n;\n" "}"
        )
        assert result.status == ExecutionStatus.SUCCESS

    def test_multiple_functions_compile(self) -> None:
        result = _execute(
            "int square(int x) { return x * x; }\n" "int cube(int x) { return x * square(x); }"
        )
        assert result.status == ExecutionStatus.SUCCESS

    def test_three_parameter_function(self) -> None:
        result = _execute("int add3(int a, int b, int c) { return a + b + c; }")
        assert result.status == ExecutionStatus.SUCCESS


class TestCFreestandingSubsetRejected:
    """Features that the freestanding subset rejects."""

    def test_switch_rejected_with_label(self) -> None:
        result = _execute("int f(int x) { switch (x) { case 1: return 1; default: return 0; } }")
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "switch statements" in result.stderr

    def test_struct_rejected_with_label(self) -> None:
        result = _execute("struct Point { int x; int y; };\nint main() { return 0; }")
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "struct/union/enum types" in result.stderr

    def test_preprocessor_rejected_with_label(self) -> None:
        result = _execute("#include <stdio.h>\nint main() { return 0; }")
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "preprocessor directives" in result.stderr

    def test_pointer_rejected_with_label(self) -> None:
        result = _execute("int f(int *p) { return 0; }")
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "pointer declarators" in result.stderr

    def test_stderr_always_references_profile_doc(self) -> None:
        result = _execute("#include <stdio.h>\nint main() { return 0; }")
        assert "FREESTANDING_C_SUBSET_PROFILE.md" in result.stderr

    def test_rejection_status_is_compile_error(self) -> None:
        result = _execute("struct S { int x; };\nint main() { return 0; }")
        assert result.status == ExecutionStatus.COMPILE_ERROR
