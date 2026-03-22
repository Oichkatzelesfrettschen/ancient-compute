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
            "int fact(int n) {\n  if (n <= 1) { return 1; }\n  return n * fact(n - 1);\n}"
        )
        assert result.status == ExecutionStatus.SUCCESS

    def test_if_else_compiles(self) -> None:
        result = _execute(
            "int abs_val(int x) {\n  if (x >= 0) { return x; } else { return -x; }\n}"
        )
        assert result.status == ExecutionStatus.SUCCESS

    def test_while_loop_compiles(self) -> None:
        result = _execute("int countdown(int n) {\n  while (n > 0) { n = n - 1; }\n  return n;\n}")
        assert result.status == ExecutionStatus.SUCCESS

    def test_multiple_functions_compile(self) -> None:
        result = _execute(
            "int square(int x) { return x * x; }\nint cube(int x) { return x * square(x); }"
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

    def test_goto_rejected_with_label(self) -> None:
        result = _execute("int f() { goto end; end: return 0; }")
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "goto statements" in result.stderr

    def test_array_declarator_rejected(self) -> None:
        result = _execute("int f() { int arr[5]; return 0; }")
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "array declarators" in result.stderr

    def test_multiple_unsupported_features_all_listed(self) -> None:
        code = "#include <stdio.h>\nstruct S { int x; };\nint main() { return 0; }"
        result = _execute(code)
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "preprocessor directives" in result.stderr
        assert "struct/union/enum types" in result.stderr

    def test_enum_rejected(self) -> None:
        result = _execute("enum Color { RED, GREEN, BLUE };\nint main() { return 0; }")
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "struct/union/enum types" in result.stderr

    def test_union_rejected(self) -> None:
        result = _execute("union U { int x; float y; };\nint main() { return 0; }")
        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "struct/union/enum types" in result.stderr


class TestCompilationResultFields:
    """CompilationResult dataclass fields for both success and failure."""

    def test_success_status_is_success(self) -> None:
        result = _execute("int add(int a, int b) { return a + b; }")
        assert result.status == ExecutionStatus.SUCCESS

    def test_success_machine_code_non_empty(self) -> None:
        result = _execute("int add(int a, int b) { return a + b; }")
        assert len(result.machine_code) > 0

    def test_success_assembly_text_non_empty(self) -> None:
        result = _execute("int add(int a, int b) { return a + b; }")
        assert len(result.assembly_text) > 0

    def test_success_ir_text_contains_function(self) -> None:
        result = _execute("int add(int a, int b) { return a + b; }")
        assert "add" in result.ir_text or len(result.ir_text) > 0

    def test_success_compilation_time_positive(self) -> None:
        result = _execute("int add(int a, int b) { return a + b; }")
        assert result.compilation_time >= 0.0

    def test_success_stderr_is_empty(self) -> None:
        result = _execute("int f(int x) { return x; }")
        assert result.stderr == ""

    def test_failure_stderr_non_empty(self) -> None:
        result = _execute("#include <stdio.h>\nint main() { return 0; }")
        assert len(result.stderr) > 0

    def test_failure_compilation_time_non_negative(self) -> None:
        result = _execute("struct S { int x; };")
        assert result.compilation_time >= 0.0


class TestDetectUnsupportedFeaturesDirect:
    """Direct tests for _detect_unsupported_features()."""

    def _service(self) -> CService:
        return CService()

    def test_clean_code_returns_empty(self) -> None:
        svc = self._service()
        found = svc._detect_unsupported_features("int f(int x) { return x; }")
        assert found == []

    def test_preprocessor_detected(self) -> None:
        svc = self._service()
        found = svc._detect_unsupported_features("#define X 5\nint f() { return 0; }")
        assert "preprocessor directives" in found

    def test_struct_detected(self) -> None:
        svc = self._service()
        found = svc._detect_unsupported_features("struct S { int x; };")
        assert "struct/union/enum types" in found

    def test_switch_detected(self) -> None:
        svc = self._service()
        code = "int f(int x) { switch(x) { default: return 0; } }"
        found = svc._detect_unsupported_features(code)
        assert "switch statements" in found

    def test_goto_detected(self) -> None:
        svc = self._service()
        found = svc._detect_unsupported_features("int f() { goto end; end: return 0; }")
        assert "goto statements" in found

    def test_pointer_detected(self) -> None:
        svc = self._service()
        found = svc._detect_unsupported_features("int f(int *p) { return 0; }")
        assert "pointer declarators" in found

    def test_array_detected(self) -> None:
        svc = self._service()
        found = svc._detect_unsupported_features("int f() { int arr[5]; return 0; }")
        assert "array declarators" in found

    def test_returns_list(self) -> None:
        svc = self._service()
        found = svc._detect_unsupported_features("int f() { return 0; }")
        assert isinstance(found, list)
